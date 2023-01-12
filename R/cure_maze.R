


cure_maze <- function(the_maze){
  maze <- the_maze
  north_tiles <- copy(maze[north == TRUE])
  east_tiles <- copy(maze[east == TRUE])
  north_tiles <- north_tiles[, y := y + 0.5]
  east_tiles <- east_tiles[, x := x + 0.5]

  maze <- rbind(maze, north_tiles, east_tiles)

  potential_walls <- expand.grid(
    x = seq(min(maze$x) - 0.5,
            max(maze$x) + 0.5,
            by = 0.5),
    y = seq(min(maze$y) - 0.5,
            max(maze$y) + 0.5,
            by = 0.5)
  )

  the_walls <- setdiff(potential_walls, unique(maze[,1:2]))
  the_walls$code <- "wall"
  maze$code <- "hall"
  rbind(the_walls, maze, fill = TRUE)
}
