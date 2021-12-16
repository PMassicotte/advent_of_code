library(tidyverse)
library(here)

find_neighbor <- function(m, row, col) {

  n_col <- ncol(m)
  n_row <- nrow(m)

  top <- NULL
  right <- NULL
  left <- NULL
  bottom <- NULL

  if (row > 1) {
    top <-c(row - 1, col)
  }

  if (col < n_col) {
    right <- c(row, col + 1)
  }

  if (row < n_row) {
    bottom <- c(row + 1, col)
  }

  if (col > 1) {
    left <- c(row, col - 1)
  }

  rbind(top, right, bottom, left)

}

m <- read_lines(here("m.txt")) %>%
  strsplit("") %>%
  map(., as.numeric) %>%
  do.call(rbind, .)

m <- read_lines(here("m2.txt")) %>%
  strsplit("") %>%
  map(., as.numeric) %>%
  do.call(rbind, .)


m <- read_lines(here("data/input/2021/15_input")) %>%
  strsplit("") %>%
  map(., as.numeric) %>%
  do.call(rbind, .)

visited <- m == Inf
distance <- m
distance[] <- Inf

n_col <- ncol(m)
n_row <- nrow(m)

queue <- 1:(n_row * n_col)

distance[1, 1] <- 0

row <- 1
col <- 1

while (!all(visited)) {
  # browser()
  # if(row == 4 & col == 5) browser()


  current_distance <- distance[row, col]

  neighbors <- find_neighbor(m, row, col)
  neighbors <- matrix(neighbors[!visited[neighbors], drop = FALSE], ncol = 2)

  if (length(neighbors) != 0) {

    tentative_distances <- current_distance + m[neighbors]

    i <- which(tentative_distances < distance[neighbors])

    distance[matrix(neighbors[i, ], ncol = 2)] <- tentative_distances[i]

  }

  visited[row, col] <- TRUE

  if (all(visited)) break

  smallest <- which(distance == min(distance[!visited]) & !visited, arr.ind = TRUE)

  row <- smallest[1, 1]
  col <- smallest[1, 2]



  cat(glue::glue("Moving to {row} and {col}"), "\n")

}
