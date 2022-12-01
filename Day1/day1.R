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
image <- ggplot(data=tibble(elfs),
                aes(xmin=lag(seq_along(elfs),default = 0),
                    xmax=(seq_along(elfs)),
                    ymin=sort(elfs)+(sort(elfs)/3),
                    ymax=sort(elfs)-(sort(elfs)/3),
                    col=sort(elfs),
                    fill=sort(elfs))) +
  geom_rect(show.legend = F, stat="identity", size=.1) +
  theme_void() +
  labs(x=NULL, y="Snacks") +
  scale_fill_gradient(low = "black", high = "darkgreen") +
  coord_polar(theta = "x")

image

ggsave(plot = image,
       dpi = 300,
       filename = "day1.png",
       device = "png",
       path = "Day1/",
       width = 4,
       height = 4, units = "cm")


