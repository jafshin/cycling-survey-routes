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

# read in network and routes
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


