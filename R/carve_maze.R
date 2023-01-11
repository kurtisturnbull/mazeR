#' Carve maze
#'
#' Generates a maze using a specified maze-carving algorithm.
#'
#' @param maze_grid An unmasked rectangular maze grid object.
#' @param method Algorithm to carve maze.
#' @param bias Probability of each cell carving west or south; default of 0.5
#'
#' @return A data table representing a carved maze grid.
#' @export
#'
#' @examples
#' create_grid(4, 4) %>%
#'  carve_maze('binarytree')

carve_maze <- function(maze_grid, method, bias = 0.5){
  if (method == "binarytree"){
    return(binary_tree(maze_grid, bias))
  }
  else if (method == "sidewinder"){
    return(side_winder(maze_grid, bias))
  }
  else (
    stop(paste0(
        as.character(maze_grid),
        "is not a valid maze_grid. Valid maze_grid options include 'binarytree'
        or 'sidewinder'."
      ))
  )
}
