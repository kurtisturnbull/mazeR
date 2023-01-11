dijkstra_map <- function(maze_grid){
  maze <- maze_grid
  
  maze$distance <- 0
  maze[, distance := 0
       ][, distance := sample(c(1, rep(0, .N - 1)), .N)]
  
  while(any(maze$distance == 0)){
    frontier <- maze[distance == max(distance)]
    frontier_north <- copy(frontier)
    frontier_south <- copy(frontier)
    frontier_east <- copy(frontier)
    frontier_west <- copy(frontier)
    frontier_north[, y := y + sign(north)]
    frontier_south[, y := y - sign(south)]
    frontier_east[, x := x + sign(east)]
    frontier_west[, x := x - sign(west)]
    frontier <- rbind(frontier_north, frontier_south, frontier_east, frontier_west)
    
    
    maze[paste0(x,"|",y) %in% paste0(frontier$x,"|",frontier$y) & distance == 0
         & cursor == 0, 
       `:=`(cursor = 1, distance = max(frontier$distance) + 1)]

  }
  return(maze)
  
}