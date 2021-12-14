library(tidyverse)
library(here)

file <- "~/Desktop/input"

template <- read_lines(file, n_max = 1)

rules <- read_delim(file, skip = 2, col_names = c("pair", "insert"), delim = " -> ")

lut <- deframe(rules)

split_pair <- function(s) {
  # s <- template

  l <- str_length(s)

  s <- map(1:(l - 1), ~ str_sub(s, .x, .x + 1))
}


insert_element <- function(pair) {

  # pair <- polymer[[1]]

  next_element <- unname(lut[pair])

  paste0(str_sub(pair, 1, 1), next_element, str_sub(pair, 2, 2))
}

combine_element <- function(elements) {

  elements <- res

  n <- length(elements)


  paste0(elements)

}

polymer <- split_pair(template)

res <- map(polymer, insert_element)

reduce(res, )
