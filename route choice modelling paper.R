# preparation for figures and tables for Bendigo route choice modelling paper

library(tidyverse)
library(sf)
library(stringr)
library(readxl)
library(fs)
library(scales)

# 1 Link counts ----
# -----------------------------------------------------------------------------#
# Maps number of survey routes using each link (with links between the same
# pair of nodes treated as a single 'link')


# 1.1 Link counts for all base routes (first paper, but not used) ----
# -------------------------------------#
# read in network and routes
links <- st_read("../network v20250828 unsimplified/network.sqlite", layer = "links") %>%
  st_set_geometry("geom")
nodes <- st_read("../network v20250828 unsimplified/network.sqlite", layer = "nodes") %>%
  st_set_geometry("geom")

routes <- st_read("../Bendigo survey/routes_networked.sqlite", layer = "survey") 

# extract and tally route edges
edge_counts <- str_split(routes$network_edges, ", ") %>% 
  
  # route edges in dataframe
  unlist() %>% 
  as.numeric() %>%
  data.frame(link_id = .) %>%
  
  # omit NAs (from routes with no links)
  filter(!is.na(link_id)) %>%

  # join from and to nodes
  left_join(links %>% st_drop_geometry() %>% dplyr::select(link_id, from_id, to_id),
            by = "link_id") %>%
  
  # order from_and to_ids, so order is ignored
  mutate(node1 = pmin(from_id, to_id),
         node2 = pmax(from_id, to_id)) %>%
  
  # group by node pairs, and tally
  group_by(node1, node2) %>%
  summarise(n = n()) %>%
  ungroup()

# create line geometries for each node pair
edges_sf <- edge_counts %>%
  rowwise() %>%
  mutate(
    geometry = st_sfc(
      st_linestring(
        rbind(
          st_coordinates(nodes$geom[nodes$id == node1]),
          st_coordinates(nodes$geom[nodes$id == node2])
        )
      )
    )
  ) %>%
  ungroup() %>%
  select(n, geometry) %>%
  st_as_sf(crs = st_crs(nodes))

# save output file
st_write(edges_sf, "../GIS/link counts.sqlite")


# 1.2 Link counts for selected base, shortest and impedance routes (second paper) ----
# -------------------------------------#

# read in network
links <- st_read("../network v20250828 unsimplified/network.sqlite", layer = "links") %>%
  st_set_geometry("geom")
nodes <- st_read("../network v20250828 unsimplified/network.sqlite", layer = "nodes") %>%
  st_set_geometry("geom")

# read in 'new routes' (shortest and impedance-based modelled routes)
new.routes <- read.csv("../Bendigo survey/routed_ODs_10_02.csv")

# read in survey routes, and filter to those selected for analysis in 'new routes'
base.routes <- st_read("../Bendigo survey/routes_networked.sqlite", layer = "survey") %>%
  filter(routeid %in% new.routes$routeID)


# extract and tally route edges, for each group
edge_counts_base <- str_split(base.routes$network_edges, ", ") %>% 
  
  # route edges in dataframe
  unlist() %>% 
  as.numeric() %>%
  data.frame(link_id = .) %>%
  
  # omit NAs (from routes with no links)
  filter(!is.na(link_id)) %>%
  
  # join from and to nodes
  left_join(links %>% st_drop_geometry() %>% dplyr::select(link_id, from_id, to_id),
            by = "link_id") %>%
  
  # order from_and to_ids, so order is ignored
  mutate(node1 = pmin(from_id, to_id),
         node2 = pmax(from_id, to_id)) %>%
  
  # group by node pairs, and tally
  group_by(node1, node2) %>%
  summarise(n = n()) %>%
  ungroup()

edge_counts_short <- str_split(new.routes$pathLinkIds_Shortest, "\\|") %>% 
  
  # route edges in dataframe
  unlist() %>% 
  as.numeric() %>%
  data.frame(link_id = .) %>%
  
  # omit NAs (from routes with no links)
  filter(!is.na(link_id)) %>%
  
  # join from and to nodes
  left_join(links %>% st_drop_geometry() %>% dplyr::select(link_id, from_id, to_id),
            by = "link_id") %>%
  
  # order from_and to_ids, so order is ignored
  mutate(node1 = pmin(from_id, to_id),
         node2 = pmax(from_id, to_id)) %>%
  
  # group by node pairs, and tally
  group_by(node1, node2) %>%
  summarise(n = n()) %>%
  ungroup()


edge_counts_short <- str_split(new.routes$pathLinkIds_Shortest, "\\|") %>% 
  
  # route edges in dataframe
  unlist() %>% 
  as.numeric() %>%
  data.frame(link_id = .) %>%
  
  # omit NAs (from routes with no links)
  filter(!is.na(link_id)) %>%
  
  # join from and to nodes
  left_join(links %>% st_drop_geometry() %>% dplyr::select(link_id, from_id, to_id),
            by = "link_id") %>%
  
  # order from_and to_ids, so order is ignored
  mutate(node1 = pmin(from_id, to_id),
         node2 = pmax(from_id, to_id)) %>%
  
  # group by node pairs, and tally
  group_by(node1, node2) %>%
  summarise(n = n()) %>%
  ungroup()


edge_counts_imped <- str_split(new.routes$pathLinkIds_impedance, "\\|") %>% 
  
  # route edges in dataframe
  unlist() %>% 
  as.numeric() %>%
  data.frame(link_id = .) %>%
  
  # omit NAs (from routes with no links)
  filter(!is.na(link_id)) %>%
  
  # join from and to nodes
  left_join(links %>% st_drop_geometry() %>% dplyr::select(link_id, from_id, to_id),
            by = "link_id") %>%
  
  # order from_and to_ids, so order is ignored
  mutate(node1 = pmin(from_id, to_id),
         node2 = pmax(from_id, to_id)) %>%
  
  # group by node pairs, and tally
  group_by(node1, node2) %>%
  summarise(n = n()) %>%
  ungroup()


# combine the results and create line geometries for each node pair
edges_sf_combined  <- bind_rows(edge_counts_base %>% dplyr::select(node1, node2),
                        edge_counts_short %>% dplyr::select(node1, node2),
                        edge_counts_imped %>% dplyr::select(node1, node2)) %>%
  # distinct node pairs from any of the 3 route types
  distinct() %>%
  # create line geometry between the two nodes
  rowwise() %>%
  mutate(
    geometry = st_sfc(
      st_linestring(
        rbind(
          st_coordinates(nodes$geom[nodes$id == node1]),
          st_coordinates(nodes$geom[nodes$id == node2])
        )
      )
    )
  ) %>%
  ungroup() %>%
  st_as_sf(crs = st_crs(nodes)) %>%
  # join in the 3 sets of node counts
  left_join(edge_counts_base %>% rename(n_base = n), by = c("node1", "node2")) %>%
  left_join(edge_counts_short %>% rename(n_shorte = n), by = c("node1", "node2")) %>%
  left_join(edge_counts_imped %>% rename(n_imped = n), by = c("node1", "node2"))

# save output file
st_write(edges_sf_combined, "../GIS/link counts combined.sqlite")




# 2 Demographic table ----
# -----------------------------------------------------------------------------#
# Table of demographic data, following the logic in report.Rmd

## Read in data (same as .Rmd) ----
surveyFile <- "../Bendigo survey/responses-8gi6nyx69dt6-2025-07-10T04_40_41.798Z.xlsx"
respondents <- read_excel(surveyFile, sheet = "Respondents")


## Get valid responses (only cyclists) ----
categorise <- function(x) {
  case_when(
    str_detect(x, "once a week")  ~ "Weekly",
    str_detect(x, "once a month") ~ "Monthly",
    str_detect(x, "occasionally") ~ "Occasionally",
    str_detect(x, "not cycled")   ~ "Not cycled",
    is.na(x)                      ~ "Did not answer",
    TRUE                          ~ NA_character_
  )
}

q <- respondents %>% select(starts_with("Are you a bicycle rider")) %>% names()

valid_responses <- respondents %>%
  mutate(category = categorise(.data[[q]])) %>%
  filter(category %in% c("Weekly", "Monthly", "Occasionally"))

## Gender (Q1.1) ----
q <- respondents %>% select(starts_with("1.1")) %>% names()
gender_table <- valid_responses %>%
  select(all_of(q)) %>%
  filter(!is.na(.data[[q]])) %>%
  mutate(category = factor(.data[[q]],
                           levels = c("Woman/Female", "Man/Male", 
                                      "Other", "Prefer not to say"))) %>%
  count(category, name = "number") %>%
  mutate(`%` = round(100 * number / sum(number), 1),
         attribute = "Gender") %>%
  select(attribute, category, number, `%`)


## Age (Q1.2) ----
q <- respondents %>% select(starts_with("1.2")) %>% names()

age_table <- valid_responses %>%
  select(all_of(q)) %>%
  filter(!is.na(.data[[q]])) %>%
  
  # anomalous answers - assign midpoint
  mutate(age_num = case_when(
    str_detect(.data[[q]], "Mid 40s") ~ 45,
    str_detect(.data[[q]], "50’s") ~ 55,
    TRUE ~ suppressWarnings(as.numeric(.data[[q]]))
  )) %>%
  
  # ambiguous answers/errors (e.g. "60+", "565") - exclude
  filter(!is.na(age_num) & age_num <= 100) %>%
  
  # bucket into age groups
  mutate(category = cut(
    age_num,
    breaks = c(17, 20, 30, 40, 50, 60, 70, 80, Inf),
    labels = c("18-20", "21-30", "31-40", "41-50",
               "51-60", "61-70", "71-80", "80+"),
    right = TRUE,
    include.lowest = TRUE
  )) %>%
  
  # summarise counts and percentages
  count(category, name = "number") %>%
  mutate(`%` = round(100 * number / sum(number), 1),
         attribute = "Age") %>%
  select(attribute, category, number, `%`)


## Living arrangements (Q1.3) ----
q <- respondents %>% select(starts_with("1.3")) %>% names()

living_table <- valid_responses %>%
  select(all_of(q)) %>%
  filter(!is.na(.data[[q]])) %>%
  mutate(category = .data[[q]] %>%
           fct_infreq() %>%  # order by frequency (largest first)
           fct_relevel("Prefer not to say", after = Inf)) %>%
  count(category, name = "number") %>%
  mutate(`%` = round(100 * number / sum(number), 1),
         attribute = "Living arrangements") %>%
  select(attribute, category, number, `%`)


## Cars (Q1.4) ----
q <- respondents %>% select(starts_with("1.4")) %>% names()

cars_table <- valid_responses %>%
  select(all_of(q)) %>%
  filter(!is.na(.data[[q]])) %>%
  mutate(category = ifelse(.data[[q]] <= 5, .data[[q]], "6+"))  %>%
  count(category, name = "number") %>%
  mutate(`%` = round(100 * number / sum(number), 1),
         attribute = "No of cars in household") %>%
  select(attribute, category, number, `%`)


## Bicycles (Q1.5) ----
q <- respondents %>% select(starts_with("1.5")) %>% names()

bicycles_table <- valid_responses %>%
  select(all_of(q)) %>%
  filter(!is.na(.data[[q]])) %>%
  mutate(category = ifelse(.data[[q]] <= 5, .data[[q]], "6+")) %>%
  count(category, name = "number") %>%
  mutate(`%` = round(100 * number / sum(number), 1),
         attribute = "No of bicycles in household") %>%
  select(attribute, category, number, `%`)


## Educational qualifications (Q1.6) ----

categorise <- function(x, y) {
  case_when(
    str_detect(x, "Year 10")         ~ "Year 10 or less",
    x == "Other" & (str_detect(y, "11") | str_detect(y, "12")) ~ "Year 11 or 12",
    str_detect(x, "Diploma") ~ "Diploma/certificate",
    str_detect(x, "Undergraduate")   ~ "Undergraduate",
    str_detect(x, "Postgraduate")    ~ "Postgraduate",
    x == "Other"                     ~ "Other",
    TRUE                             ~ NA_character_
  )
}

q <- respondents %>% select(starts_with("1.6")) %>% names()
col_names <- names(respondents)
next_col <- col_names[which(col_names == q) + 1]

educ_table <- valid_responses %>%
  select(all_of(c(q, next_col))) %>%
  filter(!is.na(.data[[q]])) %>%
  mutate(category = categorise(.data[[q]], .data[[next_col]])) %>%
  mutate(category = factor(category,
                         levels = c("Year 10 or less", "Year 11 or 12", "Diploma/certificate",
                                    "Undergraduate", "Postgraduate", "Other"))) %>%
  count(category, name = "number") %>%
  mutate(`%` = round(100 * number / sum(number), 1),
         attribute = "Educational qualifications") %>%
  select(attribute, category, number, `%`)


## Income (Q1.8) ----
q <- respondents %>% select(starts_with("1.8")) %>% names()

format_income <- function(x) {
  str_replace_all(x, 
                  "(\\d+)",  # match any sequence of digits
                  function(y) paste0("$", comma(as.numeric(y))))
}

income_table <- valid_responses %>%
  select(all_of(q)) %>%
  filter(!is.na(.data[[q]])) %>%
  mutate(income_label = format_income(.data[[q]])) %>%
  mutate(income_label = ifelse(income_label == "I prefer not to say",
                               "Prefer not to say", income_label)) %>%
  mutate(income_order = case_when(
    str_detect(income_label, "Below") ~ 0,
    str_detect(income_label, "Above") ~ 200000,  # arbitrarily above highest range
    str_detect(income_label, "Prefer") ~ Inf,
    TRUE ~ as.numeric(str_extract(income_label, "\\d+"))
  )) %>%
  mutate(category = fct_reorder(income_label, income_order)) %>%
  count(category, name = "number") %>%
  mutate(`%` = round(100 * number / sum(number), 1),
         attribute = "Annual household income") %>%
  select(attribute, category, number, `%`)


## Combine all demographics ---=
demographic_table <- bind_rows(
  gender_table,
  age_table,
  living_table,
  cars_table,
  bicycles_table,
  educ_table,
  income_table
)


## Save as CSV ----
write_csv(demographic_table, "../Bendigo survey/demographic table.csv")


# 3 Purposes table ----
# -----------------------------------------------------------------------------#
# Table of cycling purposes, following the logic in report.Rmd

surveyFile <- "../Bendigo survey/responses-8gi6nyx69dt6-2025-07-10T04_40_41.798Z.xlsx"

sheet_names <- excel_sheets(surveyFile)

spot_sheet_names <- c("Most stressful intersection(s) ",
                      "My favorite spot(s) along the r",
                      "My least favorite spot(s) along")

route_sheet_names <- sheet_names[!sheet_names %in% c("Respondents", 
                                                     spot_sheet_names)]

respondents <- read_excel(surveyFile, sheet = "Respondents")

routes <- lapply(route_sheet_names, function(sheet) {
  data <- read_excel(surveyFile, sheet = sheet)
  data$destination <- sheet  # add sheet name as a new column
  return(data)
}) %>%
  bind_rows()


## Route purpose
clean_destination <- function(x) {
  x <- case_when(
    x == "To public transport stop-statio" ~ "To public transport stop/station",
    TRUE ~ x
  )
  x <- gsub("-", "/", x)
  x <- gsub(", ", "/", x)
  x <- factor(
    x,
    levels = c(
      "To work/commute", "To college/university/classes",
      "To shops/tasks/errands", "To visit family/friends",
      "To public transport stop/station", "To other destinations"
    )
  )
  return(x)
}

purpose_table <- routes %>% 
  dplyr::select(`Respondent ID`, destination, `Please tell us the type of destination:`) %>%
  mutate(category = clean_destination(destination)) %>%
  count(category, name = "number") %>%
  mutate(`%` = round(100 * number / sum(number), 1)) %>%
  select(category, number, `%`)


## Save as CSV ----
write_csv(purpose_table, "../Bendigo survey/purpose table.csv")



# 4 Observed vs shortest and least-impedance metrics ----
# -----------------------------------------------------------------------------#
# Table of comparison of various aspects of observed, shortest and least-impedence routes

# Read in network links
links <- st_read("../network v20250828 unsimplified/network.sqlite", layer = "links") |>
  st_set_geometry("geom")

# Read in 'new routes' (shortest and impedance-based modelled routes)
new.routes <- read.csv("../Bendigo survey/routed_ODs_10_02.csv")

# Read in survey routes, and filter to those selected for analysis in 'new routes'
base.routes <- st_read("../Bendigo survey/routes_networked.sqlite", layer = "survey") |>
  filter(routeid %in% new.routes$routeID)

# Extract the individual links for each route
base_links <- base.routes %>%
  st_drop_geometry() %>%
  select(routeid, network_edges) %>%
  mutate(link_id = str_split(network_edges, ",\\s*")) %>%
  unnest(link_id) %>%
  mutate(link_id = as.numeric(link_id)) %>%
  dplyr::select(routeid, link_id) %>%
  mutate(type = "base")

short_links <- new.routes %>%
  st_drop_geometry() %>%
  select(routeid = routeID, pathLinkIds_Shortest) %>%
  mutate(link_id = str_split(pathLinkIds_Shortest, "\\|")) %>%
  unnest(link_id) %>%
  mutate(link_id = as.numeric(link_id)) %>%
  dplyr::select(routeid, link_id) %>%
  mutate(type = "short")

imped_links <- new.routes %>%
  st_drop_geometry() %>%
  select(routeid = routeID, pathLinkIds_impedance) %>%
  mutate(link_id = str_split(pathLinkIds_impedance, "\\|")) %>%
  unnest(link_id) %>%
  mutate(link_id = as.numeric(link_id)) %>%
  dplyr::select(routeid, link_id) %>%
  mutate(type = "imped")

# Combined table of link id's, with link details joined
route_links <- bind_rows(base_links, short_links, imped_links) %>%
  left_join(links %>% st_drop_geometry(), by = "link_id")

# Table of metrics

# Average trip length
trip_length <- route_links %>%
  # length for each type and routeid
  group_by(type, routeid) %>%
  summarise(length = sum(length), .groups = "drop") %>%
  # average length for each type, in km
  group_by(type) %>%
  summarise(avg_length = mean(length) / 1000, .groups = "drop") %>%
  # reshape
  mutate(metric = "trip length") %>%
  pivot_wider(names_from = type, values_from = avg_length)

# LTS metrics
LTS_metrics <- route_links %>%
  # total length for each type and LTS level
  group_by(type, lvl_traf_stress) %>%
  summarise(length = sum(length), .groups = "drop") %>%
  # length for each LTS level as percentage of length for the type
  group_by(type) %>%
  mutate(pct_length = length / sum(length) * 100) %>%
  ungroup() %>%
  # reshape
  dplyr::select(type, metric = lvl_traf_stress, pct_length) %>%
  mutate(metric = paste0("LTS", metric)) %>%
  pivot_wider(names_from = type, values_from = pct_length)

# Protected cycling infra
infra <- route_links %>%
  # add 'infra' column for protected status
  mutate(infra = case_when(
    cycleway %in% c("shared_path", "bikepath", "separated_lane") ~ "protected",
    cycleway %in% c("simple_lane", "inadequate_lane") ~ "painted",
    TRUE ~ "none")
    ) %>%
  # total length for each type and infra 
  group_by(type, infra) %>%
  summarise(length = sum(length), .groups = "drop") %>%
  # length for each infra as percentage of length for the type
  group_by(type) %>%
  mutate(pct_length = length / sum(length) * 100) %>%
  ungroup() %>%
  # reshape
  dplyr::select(type, metric = infra, pct_length) %>%
  pivot_wider(names_from = type, values_from = pct_length)

# Slope ##NEED TO CHANGE, IT SHOULD BE PERCENT
# check extent of NAs
slope.na <- sum(route_links %>% filter(is.na(slope_pct)) %>% .$length)  / 
  sum(route_links$length)
slope.na  # 0.0006079744 (0.06% of route length) - so just disregard

slope_avg <- route_links %>%
  # weighted average of slope by route length
  group_by(type) %>%
  summarise(slope_pct = weighted.mean(slope_pct, w = length, na.rm = TRUE)) %>%
  # reshape
  mutate(metric = "slope_pct") %>%
  pivot_wider(names_from = type, values_from = slope_pct)


slope_pct <- route_links %>%
  # add slope category
  mutate(slope_cat = case_when(
           slope_pct >= 2 & slope_pct <= 6 ~ "gentle",
           slope_pct > 6 ~ "steep", 
           TRUE ~ "other"
           )) %>%
  # total length for each type and slope_cat 
  group_by(type, slope_cat) %>%
  summarise(length = sum(length), .groups = "drop") %>%
  # length for each slope_cat as percentage of length for the type
  group_by(type) %>%
  mutate(pct_length = length / sum(length) * 100) %>%
  ungroup() %>%
  # reshape
  dplyr::select(type, metric = slope_cat, pct_length) %>%
  pivot_wider(names_from = type, values_from = pct_length)

# Speed limit
# check extent of NAs
speed.na <- sum(route_links %>% filter(is.na(freespeed)) %>% .$length)  / 
  sum(route_links$length)
speed.na  # 0 - none

speed <- route_links %>%
  # convert freespeed (m/s) to km/h
  mutate(speed_limit = freespeed * 3.6) %>%
  # weighted average of speed by route length
  group_by(type) %>%
  summarise(speed = weighted.mean(speed_limit, w = length, na.rm = TRUE)) %>%
  # reshape
  mutate(metric = "speed") %>%
  pivot_wider(names_from = type, values_from = speed)

# Traffic
# check extent of NAs
traffic.na <- sum(route_links %>% filter(is.na(adt)) %>% .$length)  / 
  sum(route_links$length)
traffic.na  # 0.4131315 - so 41%
route_links %>% 
       dplyr::select(highway, length, adt) %>%
       group_by(highway, adt) %>%
       summarise(length = sum(length), .groups = "drop")
# highway             adt   length
# <chr>             <dbl>    <dbl>
# 1  corridor             NA     35.9
# 2  cycleway             NA 901642. 
# 3  footway              NA  70446. 
# 4  living_street       375  10479. 
# 5  path                 NA  18334. 
# 6  pedestrian           NA   5877. 
# 7  primary              NA 111458. 
# 8  proposed_cycleway    NA   1489. 
# 9  residential         375 860372. 
# 10 secondary          5000 332450. 
# 11 secondary_link     5000   1320. 
# 12 service             375  75612. 
# 13 tertiary           1500 583224. 
# 14 tertiary_link      1500    364. 
# 15 track                NA 115554. 
# 16 trunk                NA 243303. 
# 17 trunk_link           NA   3466. 
# 18 unclassified        375 226646.

# There's a lot of primary and trunk without adt values; I don't think it's 
# particularly safe to use

# Combine
metric_table <- bind_rows(trip_length, LTS_metrics, infra,
                          slope_avg, slope_pct, speed) %>%
  dplyr::select(metric, base, short, imped)
  
  
## Save as CSV ----
write.csv(metric_table, "../Bendigo survey/metric table.csv", row.names = FALSE)

# output for copy & paste into Overleaf
metric_table %>%
  # round to desired number
  mutate(across(c(base, short, imped),
                ~ case_when(metric == "slope_pct" ~ round(., 3),
                            metric == "steep" ~ round(., 2),
                            TRUE ~ round(., 1)))) %>%
  # add a &-separated column for pasting to overleaf
  mutate(overleaf = paste(base, "&", short, "&", imped))

# metric       base  short  imped overleaf               
# <chr>       <dbl>  <dbl>  <dbl> <chr>                  
# 1 trip length  4.9   4.3    4.6   4.9 & 4.3 & 4.6        
# 2 LTS1        31.7  13.9   47.8   31.7 & 13.9 & 47.8     
# 3 LTS2        35.9  39.5   26.9   35.9 & 39.5 & 26.9     
# 4 LTS3        16.7  21.1   10.4   16.7 & 21.1 & 10.4     
# 5 LTS4        15.7  25.5   14.9   15.7 & 25.5 & 14.9     
# 6 none        48.6  56.5   36.5   48.6 & 56.5 & 36.5     
# 7 painted     25.8  32.4   21.9   25.8 & 32.4 & 21.9     
# 8 protected   25.7  11.1   41.7   25.7 & 11.1 & 41.7     
# 9 slope_pct   -0.07 -0.071 -0.067 -0.07 & -0.071 & -0.067
# 10 gentle      12.5  13.8   12.7   12.5 & 13.8 & 12.7     
# 11 other       86.9  85.4   86.3   86.9 & 85.4 & 86.3     
# 12 steep        0.64  0.81   0.97  0.64 & 0.81 & 0.97      
# 13 speed       44.6  51.3   41.7   44.6 & 51.3 & 41.7     