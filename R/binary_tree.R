#' Binary tree algorithm
#'
#' Generates a perfect maze using a binary tree algorithm that carves west or
#' south of each grid cell. Must be used with a full, unmasked rectangular grid
#' object.
#'
#' @param maze_grid An unmasked rectangular maze grid object.
#' @param bias Probability of each cell carving west or south; default of 0.5.
#'
#' @return A data table representing a carved maze grid.
#' @export
#'
#' @examples
#' create_grid(4, 4) %>%
#'  binary_tree(0.5)


binary_tree <- function(maze_grid, bias = 0.5){
  maze <- maze_grid
  maze[, cluster := rbinom(.N, 1, bias)
        ][y == min(y), cluster := 0
          ][x == min(x), cluster := 1]
  setkey(maze, y)
  maze[data.table::shift(cluster, type = 'lead') == 0, east := TRUE
        ][data.table::shift(east, type = 'lag') == TRUE, west := TRUE]
  setkey(maze, x)
  maze[data.table::shift(cluster, type = 'lead') == 1, north := TRUE
        ][data.table::shift(north, type = 'lag') == TRUE, south := TRUE]
  return(maze)
}
