library(tidyverse)

input <- read_lines("Day15/example.txt")
# input <- read_lines("Day15/input.txt")

source("Day15/functions.R")

# part 1 ####
coordinates <- read_input(input)

coord_df <- get_coord_df(coordinates)

# set_y <- 2000000
set_y <- 10

covered_positions_x <- scan_row(set_y = set_y,coord_df)

range(covered_positions_x) %>% diff()


# part 2 ####
# target_range=c(0,4000000)
target_range=c(0,20)

plot_space(coord_df, target_range)

beacons <- coord_df %>%
  filter(center=="beacon")

plot_space(coord_df, target_range, center = "beacon")


# all the mess from now:




# get all the lower (south) and upper (north) corners of each sensor area
sensors1 <- coord_df %>%
  filter(center=="sensor") %>%
  dplyr::select(i1=i,x1=x,y1=y,dist1=dist)
sensors2 <- coord_df %>%
  filter(center=="sensor") %>%
  dplyr::select(i2=i,x2=x,y2=y,dist2=dist)

dist1_pairs <- crossing(sensors1,sensors2) %>%
  group_by(i1,i2) %>%
  summarize(dist = man_edge_dist(slope=1, c(x1,y1,dist1),c(x2,y2,dist2))) %>%
  filter(dist==1)

dist2_pairs <- crossing(sensors1,sensors2) %>%
  group_by(i1,i2) %>%
  summarize(dist = man_edge_dist(slope=-1, c(x1,y1,dist1),c(x2,y2,dist2))) %>%
  filter(dist==1)

keep_sensors <- union(c(dist1_pairs$i1,dist1_pairs$i2),c(dist2_pairs$i1,dist2_pairs$i2))

sensor_df <- coord_df %>%
  dplyr::filter(i%in%keep_sensors | center=="beacon")

plot_space(coord_df = sensor_df, target_range, center_on = "sensor")



#didn't work...



# find the couple of sides that have distance =1
dist1_pairs <- crossing(lower_corners,upper_corners) %>%
  group_by(i1,i2) %>%
  mutate(dist = man_side_dist(c(x1,y1),c(x2,y2), slope = 1)) %>%
  filter(dist==1)
dist2_pairs <- crossing(lower_corners,upper_corners) %>%
  group_by(i1,i2) %>%
  mutate(dist = man_side_dist(c(x1,y1),c(x2,y2), slope = -1)) %>%
  filter(dist==1)

keep_sensors <- c(dist1_pairs$i1,dist1_pairs$i2,dist2_pairs$i1,dist2_pairs$i2) %>% unique()


sensor_df <- coord_df %>%
  dplyr::filter(i%in%keep_sensors | center=="beacon")

plot_space(coord_df = sensor_df, target_range)


map_dbl(1:1000, function(i){
  scan_row2(i, sensor_df) %>% nrow()})




ranges
IRanges::IRanges(start = ranges[,1],end=ranges[,2]) %>%
  IRanges::reduce()



ordered_ranges <- ranges[order(ranges[,1]),]
values <- ordered_ranges[1]
current_max <- ordered_ranges[2]
for (i in 1:(nrow(ordered_ranges)-1)) {
  if (ordered_ranges[i,2]<=ordered_ranges[i+1,2]) {}
}

map(target_range[1]:target_range[2], function(x) {scan_row(set_y = x, coord_df)  %>%
  purrr::reduce(c) %>%
  sort() %>% intersect(target_range[1]:target_range[2]) %>%
  setdiff(target_range[1]:target_range[2],.)}) %>%
  unlist()

coord_df %>%
  ggplot(aes(x=x,y=y, fill=center)) +
  geom_point(shape=21,
             size=2,
             alpha=.5,
             show.legend = F) +
  scale_fill_manual(values=c("red","green")) +
  theme_bw() +
  coord_equal() +
  annotate(geom = 'rect', ymin=target_range[1],ymax=target_range[2],
           xmin=target_range[1],xmax=target_range[2], fill="blue", alpha=.1,
           col="black", size=.1)


# test vector graphic
poligon1 <- rbind(c(0,0),
                  c(0,10),
                  c(10,10),
                  c(10,0))

poligon2 <- rbind(c(10,5),
                  c(10,15),
                  c(20,15),
                  c(20,5))

poly_inf <- rbind(c(-Inf,-Inf),
                  c(-Inf,Inf),
                  c(Inf,-Inf),
                  c(Inf,Inf))


sum_areas <- function(poligon1,poligon2) {


}

