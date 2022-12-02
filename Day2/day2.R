library(tidyverse)

input <- readLines("Day2/input.txt")

# part1 ####

rules <- c(RR = 1+3, RP = 2+6, RS = 3+0,
           PR = 1+0, PP = 2+3, PS = 3+6,
           SR = 1+6, SP = 2+0, SS = 3+3)

part1 <- function(input) {
  games <- input %>% str_replace_all(c("A"="R","B"="P","C"="S",
                                       "X"="R","Y"="P","Z"="S")) %>%
    str_remove_all(" ")

  points <- recode(games,!!!rules)
  sum(points)
}
part1(input)


# part2 ####
part2 <- function(input) {
  games <- input %>% str_replace_all(c("A"="R","B"="P","C"="S"))%>%
    str_remove_all(" ")

  strategy <- c(RX = "RS", RY = "RR", RZ = "RP",
                PX = "PR", PY = "PP", PZ = "PS",
                SX = "SP", SY = "SS", SZ = "SR")

  recode(games,!!!strategy) %>%
    recode(!!!rules) %>% sum()
}

part2(input)


# other solution
part1 <- function(input) {
  get_points <- function(game) {
    hands <- str_split(game," ", simplify = T)
    hands <- recode(hands, "A"=1,"B"=2,"C"=3,"X"=1,"Y"=2,"Z"=3)
    points <- hands[2]
    if(all(hands==c(1,2))|
       all(hands==c(2,3))|
       all(hands==c(3,1))) {points<-points+6}
    if(hands[1]==hands[2]) {points<-points+3}
    points
  }

  map_dbl(input, get_points) %>% sum()
}

part2 <-  function(input) {
  get_points <- function(game) {
    hands <- str_split(game," ", simplify = T)
    hands <- recode(hands, "A"=1,"B"=2,"C"=3,"X"=0,"Y"=3,"Z"=6)
    points <- hands[2]
    if(hands[2]==3) {points<-points+hands[1]}
    if(hands[2]==6) {
      if(hands[1]==1) {points<-points+2}
      if(hands[1]==2) {points<-points+3}
      if(hands[1]==3) {points<-points+1}
    }
    if(hands[2]==0) {
      if(hands[1]==1) {points<-points+3}
      if(hands[1]==2) {points<-points+1}
      if(hands[1]==3) {points<-points+2}
    }
    points
  }

  map_dbl(input, get_points) %>% sum()
}

part1(input)
part2(input)


