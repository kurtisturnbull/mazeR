require(data.table)
require(tidyverse)
require(plotly)
source("R/create_grid.R") # document
source("R/binary_tree.R")
source("R/side_winder.R")
source("R/carve_maze.R")
source("R/dijkstra_map.R")
source("R/cure_maze.R")
source("R/plot_maze.R")
source("R/mazer.R")


set.seed(3)
mazer(method = "binarytree")
mazer(method = "sidewinder")







