#' Carve maze
#' 
#' Generates a maze using a specified maze-carving algorithm.
#' 
#' @param maze_grid An unmasked rectangular maze grid object.
#' @param method Algorithm to carve maze.
#' @param bias Probability of each cell carving west or south; default of 0.5

carve_maze <- function(maze_grid, method, bias = 0.5){
  if (method == "binarytree"){
    return(binary_tree(maze_grid, bias))
  }
  if (method == "sidewinder"){
    return(side_winder(maze_grid, bias))
  }
  if (method == "aldousbroder"){
    maze_grid <- aldous_broder(maze_grid)
  }
}