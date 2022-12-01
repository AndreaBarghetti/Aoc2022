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


# Visualization ####
image <- ggplot(data=NULL,aes(x=seq_along(elfs), y=sort(elfs))) +
  geom_point(size=.1,) +
  theme_void() +
  theme(panel.background = element_rect(fill = "darkgreen",
                                        colour = "black",
                                        size = .5, linetype = "solid")) +
  labs(x=NULL, y="Snacks")

ggsave(plot = image,
       filename = "image.png",
       device = "png",
       path = "Day1/",
       width = 4,
       height = 4, units = "cm")


