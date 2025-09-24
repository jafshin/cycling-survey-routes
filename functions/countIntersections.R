# function to count number of intersections, and number of high-stress intersections
# (LTS 3 or 4), in a survey route

# omits intersections that only connect to 1 or 2 other nodes (not real intersections),
# or where intersects only with paths that don't allow car traffic

countIntersections <- function(input.routes, input.links, input.nodes) {
  
  # input.routes <- routes_networked %>%
  #   st_drop_geometry() %>%
  #   dplyr::select(any_of(c("routeid", 
  #                          "network_nodes", "network_edges")))
  # input.links <- links %>% 
  #   st_drop_geometry() %>%
  #   filter(!modes %in% c("bus", "train")) %>%
  #   dplyr::select(any_of(c("link_id", "from_id", "to_id",  
  #                          link_stress = "lvl_traf_stress", "is_car")))
  # input.nodes <- nodes %>%
  #   st_drop_geometry() %>%
  #   dplyr::select(any_of(c("id", "type")))
  
  # empty dataframe to hold output
  intersection_counts <- data.frame()
  
  # count degree (number of directly connected nodes) of nodes
  node_degree <- rbind(input.links %>%
                         dplyr::select(id = from_id, other_id = to_id),
                       input.links %>%
                         dplyr::select(id = to_id, other_id = from_id)) %>%
    distinct() %>%
    group_by(id) %>%
    summarise(degree = n()) %>%
    ungroup()
  
  # assemble details of nodes
  node_details <- rbind(input.links %>%
                          dplyr::select(id = from_id, LTS = link_stress, is_car),
                        input.links %>%
                          dplyr::select(id = to_id, LTS = link_stress, is_car)) %>%
    group_by(id) %>%
    
    # for connecting links, find highest level of LTS, whether car
    summarise(max_LTS = max(LTS),
              is_car = max(is_car)) %>%  
    ungroup() %>%
    
    # join node degree 
    left_join(node_degree, by = "id") %>%
    
    # join intersection status
    left_join(input.nodes, by = "id") %>%
    
    # calculate level(low if signalised or highest is 1/2; high if
    # unsignalised and highest is 3/4)
    mutate(hi_stress = ifelse(str_detect(type, "signalised") | max_LTS <= 2,
                              0, 1))
  
  
  # loop through routes, counting intersections and high LTS
  for (i in 1:nrow(input.routes)) {
    
    # individual route
    route <- input.routes[i,]
    
    # vector of network nodes (assumed to be in 'network_nodes' field)
    network_nodes <- str_split(route$network_nodes, ", ") %>% 
      unlist() %>% 
      as.numeric() %>%
      # omit the first node
      .[-1]
    
    # assemble nodes in table
    intersection.count <- data.frame(id = network_nodes) %>%
      
      # add node details
      left_join(node_details, by = "id") %>%
      
      # remove non-intersections (no car, or degree <= 2)
      filter(is_car == 1 & degree > 2) %>%
      
      # tally intersections and high-stress intersections
      summarise(no_intersec = n(),
                no_high_lts = sum(hi_stress == 1)) %>%
      
      # add route id
      mutate(routeid = route$routeid, .before = 1)
    
    # add to output table
    intersection_counts <- bind_rows(intersection_counts, intersection.count)
    
  }
  
  return(intersection_counts)
  
}
