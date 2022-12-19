read_input <- function(input) {

  map(input, function(input) {
    ls <- input[[1]] %>%
      str_split(":", simplify = T)

    coordinates <- setNames(ls, c("sensor", "beacon")) %>%
      map(function(x) {
        x %>% str_extract_all("[-+]*\\d+", simplify = T) %>%
          as.integer()
      })

    return(coordinates)
  })
}

get_coord_df <- function(coordinates) {
  sensor_x <- coordinates %>% map_dbl(~.x[[1]][1])
  sensor_y <- coordinates %>% map_dbl(~.x[[1]][2])
  beacon_x <- coordinates %>% map_dbl(~.x[[2]][1])
  beacon_y <- coordinates %>% map_dbl(~.x[[2]][2])

  df <- tibble(sensor_x=sensor_x,
               sensor_y=sensor_y,
               beacon_x=beacon_x,
               beacon_y=beacon_y)
  tidy_df <- df %>%
    mutate(i = row_number()) %>%
    pivot_longer(cols = 1:4, names_to = c("center","axis"),names_sep = "_", values_to = "value") %>%
    spread(axis, value) %>%
    group_by(i) %>%
    mutate(dist_x = abs(diff(x)),
           dist_y = abs(diff(y)),
           dist = dist_x+dist_y) %>%
    ungroup()
}

scan_row <- function(set_y, coord_df) {
  coord_df %>%
    filter(center=="sensor") %>%
    mutate(overlap = dist-abs(y-set_y)) %>%
    filter(overlap>0) %>%
    pmap(function(overlap, x,...) {
      range(x-overlap,x+overlap)
    }) %>%
    purrr::reduce(rbind) %>%
    reduce_ranges()
}

get_corners <- function(center, dist, addx=0) {
  dist=dist+addx
  c1 <- c(center[1]+dist,center[2])
  c2 <- c(center[1],center[2]-dist)
  c3 <- c(center[1]-dist,center[2])
  c4 <- c(center[1],center[2]+dist)
  mat <- rbind(c1,c2,c3,c4)
  tibble(x=mat[,1],y=mat[,2])
}

# find the manh distance between two parallel lines
# with slope 1 or -1
# passing by the given points (corners of a square)
man_side_dist <- function(corner1,corner2,slope=1) {
  p1 <- c(corner2[1],corner1[2])
  diff <- corner2[2]-corner1[2]
  p1[1]<-p1[1]-diff*slope
  return(p1[1]-corner1[1])
}

man_edge_dist <- function(sensor1, sensor2,slope) {
  corners1 <- get_corners(sensor1[1:2], dist = sensor1[3])
  corners2 <- get_corners(sensor2[1:2], dist = sensor2[3])

  # if(corners1[1,1] <= corners2[3,1]) {return(c(0))}
  # if(corners1[2,2] >= corners2[4,2]) {return(c(0))}

  man_side_dist(corners1[2,] %>% as.numeric(),corners2[4,]%>% as.numeric(), slope = slope)
}

reduce_ranges <- function(ranges) {
  IRanges::IRanges(start = ranges[,1],end=ranges[,2]) %>%
    IRanges::reduce() %>%
    {cbind(start(.), end(.))}
}


plot_space <- function(coord_df,target_range, center_on="sensor") {
  coord_df %>%
    mutate(dist = ifelse(center==center_on,dist,0)) %>%
    group_by(i,center) %>%
    summarise(corners = get_corners(c(x,y),dist,add=.5)) %>%
    unique() %>%
    ggplot(aes(x=corners$x,y=corners$y, fill=center)) +
    geom_polygon(aes(group=interaction(center,i), fill=center), alpha=.3, size=0,
                 show.legend = F) +
    theme_void() +
    coord_equal() +
    annotate(geom = 'rect', ymin=target_range[1],ymax=target_range[2],
             xmin=target_range[1],xmax=target_range[2], fill="transparent",
             col="black", size=.1) +
    geom_point(data=coord_df, aes(x=x,y=y, fill=center),
               alpha=.1,
               shape=22,
               show.legend = F) +
    scale_fill_manual(values=c("red","green"))
}


