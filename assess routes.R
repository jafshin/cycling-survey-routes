# file to assess suitability of survey routes, when networked, using 3 measures:
# - visual inspection
# - check for same start/end nodes (too short or circularity)
# - Hausdorff distance
# - length compared to Euclidean distance

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
# library(igraph)  # used in largestConnectedComponent
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

# city
city <- "Bendigo"

# survey route file
SURVEY_ROUTE_FILE <- "../Bendigo survey/routes_networked.sqlite"
SURVEY_ROUTE_LAYER <- "survey"
NETWORKED_ROUTE_LAYER <- "networked"

# network links
NETWORK_FILE <- "../network v20250512 unsimplified/network.sqlite" 
LINK_LAYER <- "links"
NODE_LAYER <- "nodes"

# output directory for maps
OUTPUT_MAP_DIR <- "../Bendigo survey/output maps"

# 1 Load and set up data ----
# -----------------------------------------------------------------------------#

# This section loads the survey routes

survey_routes <- st_read(SURVEY_ROUTE_FILE, layer = SURVEY_ROUTE_LAYER) %>%
  st_set_geometry("geom")
networked_routes <- st_read(SURVEY_ROUTE_FILE, layer = NETWORKED_ROUTE_LAYER) %>%
  st_set_geometry("geom")

# load links and nodes (used in maps)
links <- st_read(NETWORK_FILE, layer = LINK_LAYER)
nodes <- st_read(NETWORK_FILE, layer = NODE_LAYER)

# 2 Visual inspection ----
# -----------------------------------------------------------------------------#

# This section loads comments as a result of visual inspection

# comments for Bendigo routes (determined by manual inspection)
if (city == "Bendigo") {
  survey_routes <- survey_routes %>%
    mutate(visual = case_when(
      routeid %in% c(8, 9, 11, 15, 17, 33, 68, 75, 77, 79, 83, 93, 106, 123, 125, 
                     146, 171, 178, 180, 197, 199, 200, 203, 209, 251, 259, 268, 
                     283, 287, 288, 289, 297, 300, 301, 303, 307, 311, 313, 316, 
                     321, 331, 332, 350, 351, 353, 356, 357, 358, 359, 360, 375, 
                     377, 379, 380, 383, 387, 388, 389, 390, 395, 397, 415, 419, 
                     434, 435) ~ "unreliable straight lines",
      routeid %in% c(144, 176, 314, 374) ~ "unreliable scribble"
    ))
} else {
  survey_routes <- survey_routes %>% mutate(visual = NA)
}


# 3 Same start/end nodes ----
# -----------------------------------------------------------------------------#

# This section checks whether start and end nodes are the same; if they are, it 
# can be because (1) the route is so short that the start and end point snap
# to the same node with no links in the route, or (2) the route is circular,
# and follows a path of links but ends up where it starts

survey_routes <- survey_routes %>%
  
  # extract first and last nodes (node string broken at string followed by optional spaces)
  mutate(start_node = as.integer(stringr::word(network_nodes, 1, sep = ",\\s*")),
         end_node = as.integer(stringr::word(network_nodes, -1, sep = ",\\s*"))) %>%
  
  # add comment on start/end nodes
  mutate(start_end = case_when(
    start_node == end_node & network_edges == "" ~ "same start/end node, no links",
    start_node == end_node ~ "same start/end node, circular"
  )) %>%
  
  # remove unused fields
  dplyr::select(-c(start_node, end_node))


# 4 Hausdorff distance and length/euclid_dist----
# -----------------------------------------------------------------------------#

# add survey and networked routes to database
dbExecute(con, "DROP TABLE IF EXISTS survey;")
st_write(survey_routes, con, layer = "survey")
dbExecute(con, "CREATE INDEX survey_gix ON survey USING GIST (geom);")

dbExecute(con, "DROP TABLE IF EXISTS networked;")
st_write(networked_routes, con, layer = "networked")
dbExecute(con, "CREATE INDEX networked_gix ON networked USING GIST (geom);")

# using postgres, add columns for hausdorff distance and directness ratio
haus.dir <- dbGetQuery(con, "
  SELECT 
    s.routeid,
    ST_HausdorffDistance(s.geom, n.geom, 0.01) AS hausdorff_dist,
    CASE
      WHEN ST_Distance(ST_StartPoint(s.geom), ST_EndPoint(s.geom)) = 0
      THEN NULL
      ELSE ST_Length(s.geom) / ST_Distance(ST_StartPoint(s.geom), ST_EndPoint(s.geom))
    END AS directness_ratio
  FROM survey s
  JOIN networked n
    ON s.routeid = n.id; 
")

survey_routes <- survey_routes %>%
  left_join(haus.dir, by = "routeid")


# 5 Save output, and re-write maps ----
# -----------------------------------------------------------------------------#

# This section saves the output, including maps (overriding those created in 'process routes.R')

# save output
st_write(survey_routes, SURVEY_ROUTE_FILE, layer = SURVEY_ROUTE_LAYER, delete_layer = TRUE)


# re-write maps, adding assessment details in caption
# setup for parallel processing - detect available cores and create cluster
cores <- min(detectCores(), 8)  # reduce value if memory problems
cluster <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cluster)

# report
print(paste(Sys.time(), "| Printing maps for", nrow(survey_routes), 
            "network maps; parallel processing with", cores, "cores"))

# set up progress reporting
# https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
pb <- txtProgressBar(max = nrow(survey_routes), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# create and save map for each route 
output <- 
  foreach(i = 1:nrow(survey_routes),
          # foreach(i = 1:8,
          .packages = c("dplyr", "sf", "stringr", "ggplot2", "ggspatial"),
          .options.snow = opts) %dopar% {
            
            # selected route
            route <- survey_routes[i,]
            
            map.title <- paste0("Map no ", i, ",  routeID ", route$routeid)
            map.filename <- paste0("map_", i, "_routeID_", route$routeid)
            
            # surrounding roads
            route_bbox <- st_bbox(route) %>%
              st_as_sfc() %>%
              st_buffer(., sqrt(as.numeric(st_area(.)))/10)
            surrounding_edges <- st_intersection(links, route_bbox)
            
            # map zoom
            if (as.numeric(st_area(route_bbox)) < 500000) {
              map.zoom = 17
            } else if (as.numeric(st_area(route_bbox)) < 4000000) {
              map.zoom = 15
            } else {
              map.zoom = 13
            }
            
            # networked route
            network_edges <- as.numeric(unlist(str_split(route$network_edges, ", ")))
            networked_route <- links %>%
              filter(link_id %in% network_edges)
            
            # caption - exclusion reasons
            map.caption <- paste0(
              "Hausdorff distance: ", round(route$hausdorff_dist, 2), " m\n",
              "Directness ratio: ", round(route$directness_ratio, 2)
            )
            if (!is.na(route$directness_ratio)) {
              if (route$directness_ratio > pi) {
                map.caption <- paste0(map.caption, " (exceeds π)")
              } else if (route$directness_ratio > (pi / 2)) {
                map.caption <- paste0(map.caption, " (exceeds π/2, but not π)")
              }
            }
            if (!is.na(route$start_end)) {
              map.caption <- paste0(map.caption, "\nCircularity: ", route$start_end)
            }
            if (!is.na(route$visual)) {
              map.caption <- paste0(map.caption, "\nVisual inspection: ", route$visual)
            }
            
            
            if (nrow(networked_route) > 0) {
              # starting point
              starting_node <- route$network_nodes[1] %>%
                str_split(., ", ") %>%
                unlist() %>%
                .[1] %>%
                as.numeric()
              starting_point <- nodes %>%
                filter(id == starting_node)
              
              map <- ggplot() +
                annotation_map_tile(type = "osm", zoom = map.zoom, alpha = 0.8) +
                geom_sf(data = surrounding_edges, colour = "black", linewidth = 0.25) +
                geom_sf(data = route, aes(colour = "Survey route"), linewidth = 2, alpha = 0.8) +
                geom_sf(data = starting_point, aes(colour = "Starting point"), size = 4) +
                geom_sf(data = networked_route, aes(colour = "Networked route"), linewidth = 2, alpha = 0.8) +
                
                
                scale_color_manual(name = "",
                                   values = c("Networked route" = "blue",
                                              "Starting point" = "blue",
                                              "Survey route" = "red")) +
                
                guides(color = guide_legend(override.aes = list(linetype = c("solid", "blank", "solid"),
                                                                shape = c(NA, 16, NA)))) +
                
                theme(axis.ticks = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      plot.caption = element_text(hjust = 0, size = 11)) +
                
                labs(title = map.title,
                     caption = map.caption)
              
              
            } else {
              map <- ggplot() +
                annotation_map_tile(type = "osm", zoom = map.zoom, alpha = 0.8) +
                geom_sf(data = surrounding_edges, colour = "black", linewidth = 0.25) +
                geom_sf(data = route, aes(colour = "Survey route"), linewidth = 2, alpha = 0.8) +
                
                scale_color_manual(name = "",
                                   values = c("Survey route" = "red")) +
                
                # guides(color = guide_legend(override.aes = list(linetype = c("solid", "blank", "solid"),
                #                                                 shape = c(NA, 16, NA)))) +
                # 
                theme(axis.ticks = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank()) +
                
                labs(title = map.title,
                     subtitle = "No networked route found")
              
            }
            
            map
            
            # save the map
            ggsave(paste0(OUTPUT_MAP_DIR, "/", map.filename ,".png"),
                   map,
                   width = 15, height = 12, units = "cm")
            
          }

# close the progress bar and cluster
close(pb)
stopCluster(cluster)

