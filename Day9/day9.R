library(tidyverse)

input <- readLines("Day9/input.txt")

source("Day9/functions.R")

# part 1  ####
tail_positions <- record_simulation(input, n_knots=2)
tail_positions %>%
  purrr::reduce(rbind) %>%
  unique() %>%
  nrow()

# part 2 ####
tail_positions <- record_simulation(input, n_knots=10)
tail_positions %>%
  purrr::reduce(rbind) %>%
  unique() %>%
  nrow()

# Visualization ####

#' save the whote rope in the env, instead of only the tail
#' and you'll have the positions of each knot at each move


