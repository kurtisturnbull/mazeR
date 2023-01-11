#' Aldous-Broder algorithm
#' 
#' Generates a perfect maze using the Aldous-Broder algorithm that carves in a 
#' random-walk, connecting unvisited cells. Aldous-Border is slow but unbiased.
#' 
#' @param maze_grid A maze grid object.
#' @param
#' 
#' @return A maze grid object with carved passages.
#'
#' @seealso 
#' 
#' @examples
#'
#' @export

aldous_broder <- function(maze_grid){
  maze <- maze_grid
  maze[, cluster := sample(c(1, rep(0, .N - 1)), .N)
       ][cluster == 1, cursor := 1]

  while(any(maze$cluster == 0)){
    target <- maze[cursor == 1]
    frontier <- maze[sqrt((x - target$x) ^ 2 + (y - target$y) ^2 ) == 1
                     ][cursor == 0
                       ][sample(.N, 1)]
   
    if (frontier$cluster == 0) {
      maze[cursor == 1 & north == FALSE, north :=  target$y < frontier$y
           ][cursor == 1 & south == FALSE, south :=  target$y > frontier$y
           ][cursor == 1 & east == FALSE, east :=  target$x < frontier$x
           ][cursor == 1 & west == FALSE, west :=  target$x > frontier$x
           ][paste0(x, " ", y) %chin% paste0(frontier$x, " ", frontier$y), 
             cursor := 2][
             cursor == 2 & north == FALSE, north := frontier$y < target$y
           ][cursor == 2 & south == FALSE, south :=  frontier$y > target$y
           ][cursor == 2 & east == FALSE, east :=  frontier$x < target$x
           ][cursor == 2 & west == FALSE, west :=  frontier$x > target$x
           ][cursor == 2, cluster :=  1]
    }
    maze[, cursor := 0
         ][paste0(x, " ", y) %chin% paste0(frontier$x, " ", frontier$y), 
         cursor := 1]
  }
  return(maze)
}



