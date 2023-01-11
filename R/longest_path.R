
#### Longest Path ##############################################################

longest_path <- function(the_grid){
  
  
  the_grid$distance <- ifelse(the_grid$distance == max(the_grid$distance),
                              1,
                              0)
  
  
  while(any(the_grid$distance == 0)){
    frontier <- the_grid[which(the_grid$distance == max(the_grid$distance)),]
    frontier_north <- frontier %>% mutate(y = y + sign(north))
    frontier_south <- frontier %>% mutate(y = y - sign(south))
    frontier_east <- frontier %>% mutate(x = x + sign(east))
    frontier_west <- frontier %>% mutate(x = x - sign(west))
    frontier <- rbind(frontier_north, frontier_south, frontier_east, frontier_west)
    
    
    the_grid$distance[paste0(the_grid$x," ",the_grid$y) %in% paste0(frontier$x," ",frontier$y) &
                        the_grid$distance==0] <- max(the_grid$distance) + 1
    
  }
  
  the_grid$cluster <- ifelse(the_grid$distance == max(the_grid$distance),
                             1,
                             0)
  
  
  while(any(the_grid$cluster == 0)){
    frontier <- the_grid[which(the_grid$cluster == max(the_grid$cluster)),]
    frontier_north <- frontier %>% mutate(y = y + sign(north))
    frontier_south <- frontier %>% mutate(y = y - sign(south))
    frontier_east <- frontier %>% mutate(x = x + sign(east))
    frontier_west <- frontier %>% mutate(x = x - sign(west))
    frontier <- rbind(frontier_north, frontier_south, frontier_east, frontier_west)
    frontier <- the_grid[paste0(the_grid$x," ",the_grid$y) %in% paste0(frontier$x," ",frontier$y) &
                          the_grid$cluster==0,] 
    frontier <- frontier[which(frontier$distance == min(frontier$distance)),]
    the_grid$cluster[paste0(the_grid$x," ",the_grid$y) %in% paste0(frontier$x," ",frontier$y) &
                       the_grid$cluster ==0] <- max(the_grid$cluster, na.rm = TRUE) + 1
    if (frontier$distance == 1){
      the_grid$cluster[the_grid$cluster == 0] <- 1
      
    }
  }
  return(the_grid)
  
}
