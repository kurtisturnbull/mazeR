
mazer <- function(n_x = 20,
                  n_y = 20,
                  method = "binarytree",
                  bias = 0.5,
                  palette = NULL) {
  create_grid(n_x, n_y) %>%
    carve_maze(method, bias) %>%
    dijkstra_map() %>%
    cure_maze() %>%
    plot_maze(palette = palette)
}


