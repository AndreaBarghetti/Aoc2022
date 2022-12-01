library(tidyverse)

input <- readLines("Day1/input.txt")


# part1 ####
elfs <- 0

for (value in input) {
  if (value=="") {
    elfs <- c(elfs,0)
  }
  else {
    elfs[length(elfs)] <- elfs[length(elfs)] + as.integer(value)
  }
}

max(elfs)

# part2 ####
sum(sort(elfs, decreasing = T)[1:3])

