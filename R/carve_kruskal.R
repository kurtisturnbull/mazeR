
carve_kruskal <- function(maze_grid, vbias = 50){
  maze <- maze_grid

  # Loop unitl there is only one cell
  while(length(unique(maze$cell) > 1)){
    # assign cursor
    maze[, cursor := 0][sample(.N, 1), cursor := 1]
    
    maze[, cursor := sample(1:4, .N, replace = TRUE)]
    setkey(maze, y)
    maze[data.table::shift(cluster, type = 'lead') == cluster, east := TRUE
         ][data.table::shift(east, type = 'lag') == TRUE, west := TRUE]
    setkey(maze, x)
    maze[data.table::shift(cluster, type = 'lead') == 1, north := TRUE
         ][data.table::shift(north, type = 'lag') == TRUE, south := TRUE]
    return(maze)
    
    
    
    
  }
  maze_grid %>% 
    mutate(cluster = sample(c(1:4),n(), replace = TRUE)) %>%
    arrange(y, x) %>%
    mutate(east = ifelse(lead(cluster) == sample(1:4,1), TRUE, FALSE),
           west = ifelse(lag(east) == TRUE, TRUE, FALSE),
           west = ifelse(lag(cluster) == sample(1:4,1), TRUE, FALSE),
           east = ifelse(lead(east) == TRUE, TRUE, FALSE)) %>%
    arrange(x, y) %>%
    mutate(north = ifelse(lead(cluster) == sample(1:4,1), TRUE, FALSE),
           south = ifelse(lag(north) == TRUE, TRUE, FALSE),
           south = ifelse(lag(cluster) == sample(1:4,1), TRUE, FALSE),
           north = ifelse(lead(north) == TRUE, TRUE, FALSE),
           cluster = NA)
}
  
  
