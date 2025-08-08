# function to test commonality of a route against each existing route in route choice set

# 'commonality' between routes a and b is Cab = Lab / sqrt(La * Lb), 
# where Lab is the length of the shared portion between the two routes 
# and La and Lb are the length of routes a and b respectively: see 
# Tahlyan, D & Pinjari, AR 2020, 'Performance evaluation of choice set 
# generation algorithms for analyzing truck route choice: insights from 
# spatial aggregation for the breadth first search link elimination 
# (BFS-LE) algorithm', Transportmetrica A: Transport Science, 
# vol. 16, no. 3, pp. 1030-61. 

testCommonality <- function(output.route, route.choice.set,
                            links, COMMONALITY.CEILING) {
  
  # initialise commonality
  commonality <- 0
  
  # links for the found route ('a.links')
  a.links <- as.numeric(unlist(str_split(output.route$network_edges, ", ")))
  
  for (i in 1:nrow(route.choice.set)) {
    
    # length of routes a and b
    La <- output.route$length
    Lb <- route.choice.set$length[i]
    
    # common link length (Lab)
    b.links <- route.choice.set[i, ] %>%
      st_drop_geometry() %>%
      .$network_edges %>%
      strsplit(., ", ") %>%
      unlist() %>%
      as.numeric()
    
    common.links <- intersect(a.links, b.links)
    
    if (length(common.links) > 0) {
      Lab <- links %>%
        filter(link_id %in% common.links) %>%
        summarise(Lab = sum(length, na.rm = TRUE)) %>%
        pull(Lab)
    } else {
      Lab <- 0
    }
    
    Cab <- Lab / sqrt(La * Lb)
    
    # report
    print(paste("Commonality tested: commonality", round(Cab, 2)))
    
    # update commonality, if it exceeds previous commonality
    commonality <- max(commonality, Cab)
    
    if (commonality > COMMONALITY.CEILING) break  # breaks out of i-loop
    
  }  # end i-loop
  
  return(commonality)
  
}
