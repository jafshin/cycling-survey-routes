# set of alternative routes to routes used by survey participants, 
# for route choice modelling

# the script creates alternative routes in 3 different ways, in each case using
# the dijkstra least-cost algorithm as implemented by pgrouting :
# - prefer a specific attribute (eg cycling infrastructure)
# - omit links (BFSLE: breadth-first search on link elimination)
# - randomly alter link length


# 0 Setup ----
# -----------------------------------------------------------------------------#

# This section sets up the script for processing.
# - Section 0.1 loads the required libraries.
# - Section 0.2 establishes a connection to the database.  It also contains the
#   lines to establish the database (a once-only step, for each computer on
#   which this code is used, to be run in the Terminal).
# - Section 0.3 loads the required functions (in the 'functions' folder).
# - Section 0.4 loads data file locations and parameters.


## 0.1 Libraries ----
## ------------------------------------# 

library(tidyverse)
library(sf)
library(fs)
library(igraph)  # used in largestConnectedComponent
library(RPostgres)  # Because RPostgreSQL doesn't include SCRAM-SHA-256 authentication required by newer version of PostgreSQL
library(ggplot2)
library(ggspatial) ## map tiles
library(doSNOW)
library(parallel)
library(foreach)


## 0.2 Postgres ----
## ------------------------------------# 

# setup of network database in postgres (uses bash) 
# run lines below in the terminal (or can use pgadmin)
## createdb -U postgres choiceset
## psql -U postgres -d choiceset -c 'CREATE EXTENSION IF NOT EXISTS postgis'
## psql -U postgres -d choiceset -c 'CREATE EXTENSION IF NOT EXISTS pgrouting'

con = dbConnect(RPostgres::Postgres(), dbname = "choiceset", user = "postgres", 
                password = "postgres", host = "localhost", port = 5432)


## 0.3 Functions ----
## ------------------------------------# 

dir_walk(path="./functions/",source, recurse=T, type = "file")


## 0.4 Locations and parameters ----
## ------------------------------------# 

# network file
NETWORK_FILE <- "../network v20250828 unsimplified/network.sqlite"  # note - all one way, contains LTS and other impedances
LINK_LAYER <- "links"
NODE_LAYER <- "nodes"

# survey route file
SURVEY_ROUTE_FILE <- "../Bendigo survey/routes_networked.sqlite"
SURVEY_ROUTE_LAYER <- "survey"

# commonality ceiling (max level of commonality), targets, etc
# COMMONALITY.CEILING <- 0.95  # ie at least 5% difference
COMMONALITY.CEILING <- 0.8  # ie at least 20% difference
BFSLE.TARGET <- 5  # no of BFSLE routes to find
RAND.TARGET <- 5  # no of random weight routes to find
RAND.MAX.ITERATIONS <- 500  # no of iterations of random loop before exit

# output directories and files
OUTPUT.DIR <- "../Bendigo survey/choice set"
OUTPUT.MAP.SUBDIR <- "/output maps"
if (!dir.exists(OUTPUT.DIR)) dir.create(OUTPUT.DIR)
if (!dir.exists(paste0(OUTPUT.DIR, OUTPUT.MAP.SUBDIR))) dir.create(paste0(OUTPUT.DIR, OUTPUT.MAP.SUBDIR))
OUTPUT.CHOICE.SET.FILE <- paste0(OUTPUT.DIR, "/choice_set.sqlite")
OUTPUT.BFSLE.DISCARD.FILE <- paste0(OUTPUT.DIR, "/bfsle_discards.csv")
OUTPUT.RAND.DISCARD.FILE <- paste0(OUTPUT.DIR, "/rand_discards.csv")
CHOICE.SET.EXPANDED.FILE <- paste0(OUTPUT.DIR, "/choice_set_expanded.csv")

# 1 Load and set up data ----
# -----------------------------------------------------------------------------#

# This section loads and prepares the data used to find the alternative routes.
# - Section 1.1 prepares the table of links used to find alternative routes:
#   - loads the network of links and nodes, created from OSM using the code
#     at https://github.com/rmit-astm/network
#   - excludes links such as motorways that cannot be cyclede on
#   - retains only the largest connected networl
#   - calculates weights required for routes that prefer specific attributes
#   - adds the output to the database
# - Section 1.2 loads the processed survey route results (that is, the routes
#   marked by the survey participants, as mapped on to the network, by
#   'process routes.R'), and adds the start and end nodes to the database

# In section 1.1, the specific attributes that are preferred are:
# shortest route
# any cycleway infrastructure, onroad or offroad
# speed <= 40 km/h
# slope <= 2%, including all downhill (however steep)
# tree canopy coverage >= 25%
# LTS level 1 or 2


## 1.1 Links ----
## ------------------------------------# 

# load network
all.links <- st_read(NETWORK_FILE, layer = LINK_LAYER)
all.nodes <- st_read(NETWORK_FILE, layer = NODE_LAYER)

# exclude all non-cyclable links
all.cyclable.links <- all.links %>%
  filter(is_cycle == 1)

all.cyclable.nodes <- all.nodes %>%
  filter(id %in% all.cyclable.links$from_id | id %in% all.cyclable.links$to_id)

# keep largest connected network
largest.component <- largestConnectedComponent(all.cyclable.nodes, all.cyclable.links)
cyclable.links <- largest.component[[2]]
cyclable.nodes <- largest.component[[1]]
cyclable.from.nodes <- cyclable.nodes %>% filter(id %in% cyclable.links$from_id)
cyclable.to.nodes <- cyclable.nodes %>% filter(id %in% cyclable.links$to_id)

# prepare links with required weights
links <- cyclable.links %>%
  
  # omit links without slope - as slope was calculated only for non-PT links within
  # Greater Bendigo buffered to 10km, this has the effect of omitting all links
  # outside the study area (as well as PT links within the study area)
  filter(!is.na(slope_pct)) %>%
  
  # there are some duplicate links (shouldn't be!); eliminate
  group_by(link_id) %>%
  slice(1) %>%
  ungroup() %>%
  

  # calculate weights as required by specific attribute preferences
  mutate(short = length,
         infra = ifelse(!is.na(cycleway), length * 0.001, length),  # any cycleway infrastructure, onroad or offroad
         speed = ifelse(freespeed <= 40 / 3.6, length * 0.001, length),  # speed <= 40 km/h
         flat = ifelse(slope_pct <= 2, length * 0.001, length),  # slope <= 2%, including all downhill (however steep)
         green = ifelse(tcc_percent >= 10, length * 0.001, length),  # tree canopy coverage >= 10%
         lts = ifelse(lvl_traf_stress %in% c(1, 2), length * 0.001, length)  # LTS level 1 or 2
  ) %>%
  
  # add 'length_adjusted' column which is used for BFSLE and random routing
  mutate(length_adjusted = length) %>%
  
  # set geometry column as expected
  st_set_geometry("geom")

# vector of weight field names
weight_fields <- c("short", "infra", "speed", "flat", "green", "lts")

# get the crs of the network (so routes can be in same crs)
networkCrs <- st_crs(links)

# add links to database as 'links'
dbExecute(con, "DROP TABLE IF EXISTS links;")
st_write(links %>%
           mutate(id = as.integer(link_id),
                  source = as.integer(from_id),
                  target = as.integer(to_id)) %>%
           dplyr::select(id, source, target, length, length_adjusted, 
                         any_of(weight_fields)), 
         con, layer = "links")
dbExecute(con, "CREATE INDEX links_gix ON links USING GIST (geom);")


## 1.2 Survey routes ----
## ------------------------------------# 

# load survey routes, as networked - but adjust starting point if not cyclable
survey_routes <- st_read(SURVEY_ROUTE_FILE, layer = SURVEY_ROUTE_LAYER) %>%
  
  # extract first and last nodes (node string broken at string followed by optional spaces)
  mutate(start_node = as.integer(stringr::word(network_nodes, 1, sep = ",\\s*")),
         end_node = as.integer(stringr::word(network_nodes, -1, sep = ",\\s*")))

# but, if not in cyclable nodes, then pick closest replacement (eg where starts/ends on footpath)
for (i in 1:nrow(survey_routes)) {
  
  if (!survey_routes$start_node[i] %in% cyclable.to.nodes$id) {
    old.node <- all.nodes %>% filter(id == survey_routes$start_node[i])
    new.node <- cyclable.from.nodes[st_nearest_feature(old.node, cyclable.from.nodes), ]
    survey_routes$start_node[i] <- new.node$id
  }
  
  if (!survey_routes$end_node[i] %in% cyclable.from.nodes$id) {
    old.node <- all.nodes %>% filter(id == survey_routes$end_node[i])
    new.node <- cyclable.to.nodes[st_nearest_feature(old.node, cyclable.to.nodes), ]
    survey_routes$end_node[i] <- new.node$id
  }
}  

# add survey routes to database as 'combinations'
dbExecute(con, "DROP TABLE IF EXISTS combinations;")
st_write(survey_routes %>%
           st_drop_geometry() %>%
           mutate(source = as.integer(start_node), target = as.integer(end_node)) %>%
           dplyr::select(routeid, source, target),
         con, layer = "combinations")


## 1.3 Remove network preparation dataframes where not needed, to save memory
## ------------------------------------# 

rm(all.links, all.cyclable.links, all.cyclable.nodes, largest.component, 
   cyclable.links, cyclable.nodes, cyclable.from.nodes, cyclable.to.nodes)


# 2 Routing - preferred attributes ----
# -----------------------------------------------------------------------------#

# This section finds routes using the weights for preferred attributes as calculated
# in section 1.1.

# find routes for preferred attributes and add to choice set
for (i in seq_along(weight_fields)) {
  
  # report
  print(paste(Sys.time(), "| Finding routes for preferred attribute:", weight_fields[i]))
  
  # construct the least-cost path statement
  routing.statement <- paste("
        SELECT * FROM pgr_dijkstra('SELECT id, source, target,", weight_fields[i], "AS cost FROM links',
                           'SELECT source, target FROM combinations',
                           directed => true); 
       ")
  
  # run the statement
  routing.output <- dbGetQuery(con, routing.statement) %>%
    mutate(start_vid = as.numeric(start_vid), end_vid = as.numeric(end_vid),
           node = as.numeric(node), edge = as.numeric(edge))
  
  # convert output paths to rows
  output.routes <- outputToRowCombo(routing.output, survey_routes, links, weight_fields[i])
  
  # add to choice set
  if (i == 1) {
    choice_set <- output.routes
  } else {
    choice_set <- bind_rows(choice_set, output.routes)
  }
  
}


# 3 Routing - BFSLE ----
# -----------------------------------------------------------------------------#

# This section finds routes using breadth-first search on link elimination (BFSLE)

# dataframe to hold details of found and discard numbers
bfsle.discards <- data.frame(routeid = numeric(), 
                             depth = numeric(),
                             found = numeric(),
                             discards = numeric())

# loop to find BFSLE routes
for (i in 1:nrow(survey_routes)) {
  
  # routeid and report
  route_no <- survey_routes$routeid[i]
  print(paste(Sys.time(), "|", "Finding BFSLE routes for survey route no", route_no))
  
  depth <- 1  # for reporting only
  print(paste("Searching at depth level:", depth))
  
  # route choice set for the route
  route.choice.set <- choice_set %>%
    filter(routeid == route_no)
  
  # shortest route and its endnodes and links (as list)
  route <- route.choice.set %>%
    filter(str_detect(routeid_type, "short"))
  
  start_node = as.integer(stringr::word(route$network_nodes, 1, sep = ",\\s*"))
  end_node = as.integer(stringr::word(route$network_nodes, -1, sep = ",\\s*"))
  
  link.list <- as.list(as.numeric(unlist(str_split(route$network_edges, ", ")))) %>%
    # randomise order
    sample()
  
  # initialise counters and next list
  found <- 0
  discards <- 0
  next.link.list <- list()
  
  # loop to find BFSLE routes
  while (found < BFSLE.TARGET) {
    
    for (j in seq_along(link.list)) {
      
      # report
      print(paste("Finding routes by removing link(s)", paste(link.list[[j]], collapse = ", ")))
      
      # refresh links database by setting length_adjusted equal to length
      dbExecute(con, "
        UPDATE links 
        SET length_adjusted = length;
      ")
      
      # remove links from network, by setting adjusted length to -1
      for (k in seq_along(link.list[[j]])) {
        
        link_from <- links %>% filter(link_id == link.list[[j]][k]) %>% pull(from_id)
        link_to <- links %>% filter(link_id == link.list[[j]][k]) %>% pull(to_id)
        
        update.statement <- paste0("
          UPDATE links
          SET length_adjusted = -1
          WHERE source = ", link_from, " AND target = ", link_to, ";
        ")
        
        dbExecute(con, update.statement)
        
      }  # end k-loop
      
      # new shortest path
      routing.statement <- paste("
        SELECT * FROM pgr_dijkstra('SELECT id, source, target, length_adjusted AS cost FROM links',",
                                 start_node, ",", end_node, ",
                           directed => true); 
       ")
      
      routing.output <- dbGetQuery(con, routing.statement) %>%
        mutate(start_vid = as.numeric(start_vid), end_vid = as.numeric(end_vid),
               node = as.numeric(node), edge = as.numeric(edge))
      
      # only proceed if route is found (if not, do nothing)
      if (nrow(routing.output) > 0) {
        
        # convert new shortest path to row
        output.route <- outputToRow(routing.output, links)
        
        # test commonality of new route against route.choice.set
        commonality <- testCommonality(output.route, route.choice.set,
                                       links, COMMONALITY.CEILING)
        
        # if new route meets the commonality test, add to route.choice.set
        if (commonality <= COMMONALITY.CEILING) {
          
          # increment 'found' and report
          found <- found + 1
          print(paste("New route found: routes found", found))
          
          # add route to route.choice.set for route i
          route.choice.set <- 
            bind_rows(route.choice.set,
                      output.route %>%
                        mutate(routeid_type = paste0(route_no, "-bfsle-", found)))
          
          if (found >= BFSLE.TARGET) break  # breaks out of j-loop
          
        } else {
          
          # increment 'discards' and report
          discards <- discards + 1
          print(paste("Route discarded: routes discarded", discards))
          
        }
        
        # whether or not route meets commonality test, add links omitted in 
        # this iteration, plus each link in the new route, to next.link.list
        output.route.links <- as.numeric(unlist(str_split(output.route$network_edges, ", ")))
        for (m in seq_along(output.route.links)) {
          next.link.list <- append(next.link.list,
                                   list(c(link.list[[j]], output.route.links[m])))
          
        }  # end m-loop
        
      }  # end if-statement (route found)
      
    }  # end j-loop
    
    if (found < BFSLE.TARGET) {
      
      # move to next level by setting next.link.list as link.list, random order
      link.list <- next.link.list %>% sample()
      
      # exit if link.list is empty (no more links can be found)
      if (length(link.list) == 0) break  # breaks out of while-loop 
      
      # re- initialise next.link.list
      next.link.list <- list() %>%
        # randomise order
        sample()
      
      # report depth
      depth <- depth + 1
      print(paste("Searching at depth level:", depth))
      
    }  # end if-statement (found < target)
    
  }  # end while-loop
  
  # add found routes to the choice set 
  choice_set <- bind_rows(choice_set,
                          route.choice.set %>%
                            filter(str_detect(routeid_type, "bfsle")))
  
  # update discard table
  bfsle.discards <- bind_rows(bfsle.discards, 
                              as.data.frame(cbind(routeid = route_no, depth,
                                                  found, discards)))
  
}  # end of i-loop

# write discards
write.csv(bfsle.discards, OUTPUT.BFSLE.DISCARD.FILE, row.names = FALSE)
          

# 4 Routing - random weight perturbation ----
# -----------------------------------------------------------------------------#

# This section finds routes using random multipliers for link length

# dataframe to hold details of found and discard numbers
rand.discards <- data.frame(routeid = unique(choice_set$routeid),
                            found = 0,
                            discards = 0)

# update 'combinations' so it only contains routes for which a shortest path 
# has been found
routes.to.find <- paste(unique(choice_set$routeid), collapse = ", ")

delete.statement <- paste0("
  DELETE FROM combinations
  WHERE routeid NOT IN (", routes.to.find, ");
")

dbExecute(con, delete.statement)

# iteration counter
iterations <- 0

# loop to find random weight routes (depends on number of rows in 'combinations')
while (iterations < RAND.MAX.ITERATIONS) {
  
  # alter length randomly by multiplying by 1, 2 or 3:
  # random() generates float in [0, 1); random() * 3 gives a float in [0, 3);
  # floor(random() * 3) gives 0, 1 or 2; +1 shifts it to 1, 2 or 3
  update.statement <- paste0("
      UPDATE links
      SET length_adjusted = length * (floor(random() * 3) + 1);
    ")
  
  dbExecute(con, update.statement)
  
  # report
  iterations <- iterations + 1

  print(paste(Sys.time(), "| Iteration no", iterations, ": Finding random routes for", 
              dbGetQuery(con, 'SELECT COUNT(*) FROM combinations')[[1]], "survey routes"))
  
  # construct the least-cost path statement
  routing.statement <- paste("
        SELECT * FROM pgr_dijkstra('SELECT id, source, target, length_adjusted AS cost FROM links',
                           'SELECT source, target FROM combinations',
                           directed => true); 
       ")
  
  # run the statement
  routing.output <- dbGetQuery(con, routing.statement) %>%
    mutate(start_vid = as.numeric(start_vid), end_vid = as.numeric(end_vid),
           node = as.numeric(node), edge = as.numeric(edge))
  
  # convert output paths to rows
  output.routes <- outputToRowCombo(routing.output, survey_routes, links, "rand")
  
  # for each route, test commonality against route choice set
  for (i in 1:nrow(output.routes)) {
    
    output.route <- output.routes[i,]
    output.routeid <- output.route$routeid
    
    route.choice.set <- choice_set %>%
      filter(routeid == output.routeid)
    
    print(paste("Checking commonality for routeid:", output.routeid))
    
    # test commonality of new route against route.choice.set
    commonality <- testCommonality(output.route, route.choice.set,
                                   links, COMMONALITY.CEILING)
    
    # if new route meets the commonality test, add to route.choice.set
    if (commonality <= COMMONALITY.CEILING) {
      
      # increment 'found' and report
      rand.discards[rand.discards$routeid == output.routeid, "found"] <- 
        rand.discards[rand.discards$routeid == output.routeid, "found"] + 1
      routesfound <- rand.discards[rand.discards$routeid == output.routeid, "found"]
      print(paste("New route found for routeid", output.routeid, ":", routesfound,
                  "route(s) found"))
      
      # add found route to the choice set 
      choice_set <- bind_rows(choice_set,
                              output.route %>%
                                mutate(routeid_type = paste0(output.routeid, "-rand-", routesfound)))

    } else {
      
      # increment 'discards' and report
      rand.discards[rand.discards$routeid == output.routeid, "discards"] <- 
        rand.discards[rand.discards$routeid == output.routeid, "discards"] + 1
      routediscards <- rand.discards[rand.discards$routeid == output.routeid, "discards"]
      print(paste("Route discarded for routeid", output.routeid, ":", routediscards,
                  "route(s) discarded"))
      
    }

  }  # end i-loop
  
  # update 'combinations' so it only contains routes for which the required
  # number of random routes hasn't yet been found
  routes.to.find <- paste(rand.discards %>%
                            filter(found < 5) %>%
                            .$routeid,
                          collapse = ", ")
  
  delete.statement <- paste0("
    DELETE FROM combinations
    WHERE routeid NOT IN (", routes.to.find, ");
  ")
  
  dbExecute(con, delete.statement)
  
  # exit if no rows left in combinations
  if (dbGetQuery(con, "SELECT COUNT(*) FROM combinations")[[1]] == 0) break

}  # end while-loop (iterations)


# write discards
write.csv(rand.discards, OUTPUT.RAND.DISCARD.FILE, row.names = FALSE)

# write choice set
st_write(choice_set, OUTPUT.CHOICE.SET.FILE, delete_layer = TRUE)


# 5 Visualise outputs ----
# -----------------------------------------------------------------------------#

# This section prints a set of maps, one for each survey route, showing its choice set

# reload survey and choice set routes 
survey.routes <- st_read(SURVEY_ROUTE_FILE, layer = SURVEY_ROUTE_LAYER) %>%
  st_set_geometry("geom")
choice_set <- st_read(OUTPUT.CHOICE.SET.FILE) %>%
  st_set_geometry("geom")

# setup for parallel processing - detect available cores and create cluster
cores <- min(detectCores(), 8)  # reduce value if memory problems
cluster <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cluster)

# report
print(paste(Sys.time(), "| Printing maps for", length(unique(choice_set$routeid)), 
            "choice set maps; parallel processing with", cores, "cores"))

# set up progress reporting
# https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
pb <- txtProgressBar(max = length(unique(choice_set$routeid)), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# create and save map for each route 
output <- 
  foreach(i = 1:length(unique(choice_set$routeid)),
          # foreach(i = 1:20,
          .packages = c("dplyr", "sf", "stringr", "ggplot2", "ggspatial"),
          .options.snow = opts) %dopar% {
            
            # selected routeid and routes
            route.id <- unique(choice_set$routeid)[i]
            
            survey.route <- survey.routes %>% filter(routeid == route.id)
            
            routes <- choice_set %>% filter(routeid == route.id) %>%
              
              # convert geometry to link geometries
              rowwise() %>%
              mutate(
                geom = {
                  edge_ids <- as.numeric(unlist(str_split(network_edges, ", ")))
                  edge_geom <- links %>%
                    filter(link_id %in% edge_ids) %>%
                    pull(geom)
                  st_union(edge_geom)
                }
              ) %>%
              ungroup() %>%
              st_as_sf() %>%
              
              # assign groups and identifier (for colours)
              mutate(
                group = case_when(
                  str_detect(routeid_type, "bfsle") ~ "bfsle",
                  str_detect(routeid_type, "rand") ~ "rand",
                  TRUE ~ "pref"
                )
              ) %>%
              group_by(group) %>%
              mutate(identifier = paste0(group, "_", row_number())) %>%
              ungroup()
            
            map.title <- paste0("Choice set for routeID ", route.id)
            map.filename <- paste0("map_choice_set_routeid", route.id)
            
            # bounding box
            route_bbox <- st_bbox(st_union(bind_rows(routes, survey.route))) %>%
              st_as_sfc() %>%
              st_buffer(., sqrt(as.numeric(st_area(.)))/10)
            
            # map zoom
            if (as.numeric(st_area(route_bbox)) < 500000) {
              map.zoom = 17
            } else if (as.numeric(st_area(route_bbox)) < 4000000) {
              map.zoom = 15
            } else {
              map.zoom = 13
            }
            
            # create color palette, darkest out of palette of 9 (take 9, reverse them so darkest first,
            # select first 5 or 6 of the 9, then reverse again so lightest is first)
            colors <- c(
              rev(rev(RColorBrewer::brewer.pal(n = 9, name = "Greens"))[1:nrow(routes %>% filter(group == "pref"))]),
              rev(rev(RColorBrewer::brewer.pal(n = 9, name = "Oranges"))[1:nrow(routes %>% filter(group == "bfsle"))]),
              rev(rev(RColorBrewer::brewer.pal(n = 9, name = "Purples"))[1:nrow(routes %>% filter(group == "rand"))])
            )
            names(colors) <- routes$identifier
            
            # starting and ending point
            route.short <- routes %>%
              filter(str_detect(routeid_type, "short"))
            start_node = as.integer(stringr::word(route.short$network_nodes, 1, sep = ",\\s*"))
            end_node = as.integer(stringr::word(route.short$network_nodes, -1, sep = ",\\s*"))
            
            starting.point <- all.nodes %>% filter(id == start_node)
            ending.point <- all.nodes %>% filter(id == end_node)
            
            # map
            map <- ggplot() +
              annotation_map_tile(type = "cartolight", zoom = map.zoom) +
              
              # routes, in grouped colours
              geom_sf(data = routes, aes(colour = identifier), linewidth = 1.5) +
              scale_color_manual(values = colors, name = "generated routes") +
              
              # survey route and start / end points
              geom_sf(data = survey.route,
                      aes(color = "surveyed route"), linewidth = 0.5) +
              geom_sf(data = starting.point, aes(color = "start/end point"), size = 3) +
              geom_sf(data = ending.point, aes(color = "start/end point"), size = 3) +
              
              # legend
              scale_color_manual(
                values = c(
                  colors, 
                  "surveyed route" = "black",
                  "start/end point" = "black"
                ),
                breaks = c(
                  names(colors), "surveyed route", "start/end point"
                ),
                name = "Route Elements"  # title of the full legend
              ) +
              
              theme(axis.ticks = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank()) +
              
              labs(title = map.title)

            # map  # to display 

            # save the map
            ggsave(paste0(OUTPUT.DIR, OUTPUT.MAP.SUBDIR, "/", map.filename ,".png"),
                   map,
                   width = 30, height = 24, units = "cm")

          }

# close the progress bar and cluster
close(pb)
stopCluster(cluster)


# 6 Discard analysis plots ----
# -----------------------------------------------------------------------------#

# This section prints plots of the distribution of the numbrs of discards 
# in finding the BFSLE and Rand routes

# read in discard files
bfsle.discards <- read.csv(OUTPUT.BFSLE.DISCARD.FILE)
rand.discards <- read.csv(OUTPUT.RAND.DISCARD.FILE)

# plot function
discard.plot <- function(discard.file, mytitle) {
  ggplot(discard.file, aes(x = discards)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
    labs(
      title = mytitle,
      x = "Number of Discards",
      y = "Frequency"
    ) +
    theme_bw()
}

# create and save plots
bflse.discard.plot <- discard.plot(bfsle.discards, 
                                   "Distribution of discards - BFSLE")
rand.discard.plot <- discard.plot(rand.discards, 
                                   "Distribution of discards - random weights")

ggsave(paste0(OUTPUT.DIR, "/bfsle_discard_plot.png"), bflse.discard.plot, 
       width = 15, height = 12, units = "cm")

ggsave(paste0(OUTPUT.DIR, "/rand_discard_plot.png"), rand.discard.plot, 
       width = 15, height = 12, units = "cm")


# 7 Expanded choice set routes  ----
# -----------------------------------------------------------------------------#
# This section creates atable which is an expanded version of 'choice_set', 
# with one row per link in each trip, and network details attached

# read in routes_networked and network links
choice_set <- st_read(OUTPUT.CHOICE.SET.FILE) %>%
  st_set_geometry("geom")
all.links <- st_read(NETWORK_FILE, layer = LINK_LAYER)

# expand routes_network by adding details listed below from links
choice_set_expanded <- 
  expandRoutes(choice_set %>%
                 st_drop_geometry() %>%
                 dplyr::select(any_of(c("routeid", "routeid_type", "network_edges"))),
               all.links %>% 
                 st_drop_geometry() %>%
                 dplyr::select(any_of(c("link_id", "length", "highway", "cycleway", "freespeed",
                                        "surface", "slope_pct", "ndvi", "ndvi_md", "ndvi_75", "ndvi_90",
                                        "tcc_buffer", "tcc_percent",
                                        "adt", "lvl_traf_stress"))))


# write output
write.csv(choice_set_expanded, CHOICE.SET.EXPANDED.FILE, row.names = FALSE)


