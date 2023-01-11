#' Sidewinder algorithm
#'
#' Generates a perfect maze using a sidewinder algorithm that carves clusters of
#' adjacent cells in each row and then carves south from one random cell in each
#' cluster. Must be used with a full, unmasked rectangular grid object.
#'
#' @param maze_grid A rectangular maze grid object.
#' @param bias Percent vertical bias in algorithm; default value of 0.5.
#'
#' @return A data table representing a carved maze grid.
#' @export
#'
#' @examples
#' create_grid(4, 4) %>%
#'  side_winder(0.5)

side_winder <- function(maze_grid, bias = 0.5){
  maze <- maze_grid
  maze[, cluster := rbinom(.N, 1, bias)
       ][x == min(x), cluster := 1
         ][y == min(y), cluster := 0]
  setkey(maze, y)
  maze[shift(cluster, type = 'lead') == 0, east := TRUE
       ][shift(east, type = 'lag') == TRUE, west := TRUE
         ][, cluster := cumsum(cluster)]
  nward <- maze[, .SD[sample(.N, 1)], by = cluster]
  setkey(maze, x)
  maze[paste0(x, "|", y) %chin% paste0(nward$x, "|", nward$y), cluster := 3
       ][shift(cluster, type = 'lead') == 3, north := TRUE
         ][shift(north, type = 'lag') == TRUE, south := TRUE
           ][y == max(y), north := FALSE]
  return(maze)
}



