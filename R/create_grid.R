#' Generate a rectangular grid for maze carving
#'
#' Generates a rectangular grid of specified dimensions for maze carving.
#'
#' @param cols The number of columns (grid width); default value of 4.
#' @param rows The number of rows (grid height); default value of 4.
#'
#' @export

create_grid <- function(cols = 4, rows = 4){
  cbind(CJ(x = 1:cols, y = 1:rows),
        data.table(cell = 1:(cols * rows),
                   cluster = 0,
                   cursor = 0,
                   north = FALSE,
                   south = FALSE,
                   east = FALSE,
                   west = FALSE))
}
