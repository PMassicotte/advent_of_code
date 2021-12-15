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

m <- read_lines(here("~/Desktop/m.txt")) %>%
  strsplit("") %>%
  map(., as.numeric) %>%
  do.call(rbind, .)

# m <- read_lines(here("data/input/2021/15_input")) %>%
#   strsplit("") %>%
#   map(., as.numeric) %>%
#   do.call(rbind, .)

visited <- m == Inf
distance <- m
distance[] <- Inf

n_col <- ncol(m)
n_row <- nrow(m)

distance[1, 1] <- 0
# col <- 1
# row <- 1

destination_row <- n_row
destination_col <- n_col


for (col in 1:n_col) {

  for (row in 1:n_row) {
    if(row == 1 & col == 4) browser()
    # browser()
    current_distance <- distance[row, col]

    neighbors <- find_neighbor(m, row, col)
    # neighbors <- matrix(neighbors[!visited[neighbors], drop = FALSE], ncol = 2)

    edges <- m[neighbors]

    destination_distances <- distance[neighbors]

    i <- which((current_distance + edges) < destination_distances)

    distance[matrix(neighbors[i, ], ncol = 2)] <- current_distance + edges[i]

    visited[row, col] <- TRUE

    if (row == destination_col & col == destination_col) break

  }

}

# write.table(m, "~/Desktop/m.txt", sep = ",", row.names = F, col.names = F)
