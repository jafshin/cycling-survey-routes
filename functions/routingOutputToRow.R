# function to convert shortest path output from pgrouting to row

# single row
outputToRow <- function(routing.output, links) {

  output.route <- routing.output %>%
    
    # join to links to add geometry
    left_join(links %>% dplyr::select(link_id, length), by = c("edge" = "link_id")) %>%
    
    # combine to get geometry, and add route_id
    summarise(length = sum(length, na.rm = TRUE),  # NAs are the last edges which are -1 
              geom = st_union(geom)) %>%
    mutate(routeid = route_no) %>%
    
    # join to lists of nodes and edges
    left_join(routing.output %>%
                
                # combine lists of nodes and edges (remove last edge '-1' in each route)
                reframe(
                  network_nodes = toString(node), 
                  network_edges = toString(edge[edge != -1])
                ) %>%
                
                # add route id 
                mutate(routeid = route_no),
              by = "routeid")
  
  return(output.route)
  
}

# multiple rows, resulting from pgrouting with combination
outputToRowCombo <- function(routing.output, survey_routes, links, type) {
  
  # join the output to route ids and link geometries
  routing.output.joined <- survey_routes %>%
    st_drop_geometry() %>%
    dplyr::select(routeid, start_node, end_node) %>%
    left_join(routing.output, by = c("start_node" = "start_vid",
                                     "end_node" = "end_vid"),
              relationship = "many-to-many") %>%
    # join to links to add geometry
    left_join(links %>% dplyr::select(link_id, length), by = c("edge" = "link_id"))

  # combine geometries for each routeid
  routing.output.geoms <- routing.output.joined %>%
    group_by(routeid) %>%
    summarise(length = sum(length, na.rm = TRUE),  # NAs are the last edges which are -1 
              geom = st_union(geom))
  
  # combine lists of nodes and edges, and add the combined geometries
  output.routes <- routing.output.joined %>%
    st_drop_geometry() %>%
    
    # combine lists of nodes and edges (remove last edge '-1' in each route)
    group_by(routeid) %>%
    reframe(
      network_nodes = toString(node), 
      network_edges = toString(edge[edge != -1])
    ) %>%
    
    # join the geometries
    left_join(routing.output.geoms, by = "routeid") %>%
    
    # update route id and set as st object
    mutate(routeid_type = paste0(routeid, "-", type),
           .after = "routeid") %>%
    st_as_sf() %>%
    
    # filter out empty geometries (where no route is found)
    filter(!st_is_empty(geom))
  
  return(output.routes)
  
}