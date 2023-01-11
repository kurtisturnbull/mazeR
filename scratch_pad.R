require(data.table)
require(tidyverse)
source("R/create_grid.R") # document
source("R/binary_tree.R")
source("R/side_winder.R")
source("R/carve_maze.R")
source("R/dijkstra_map.R")
source("R/longest_path.R")
create_grid(4, 4) %>%
  carve_maze("binarytree") %>%
  dijkstra_map() %>%
  longest_path()
