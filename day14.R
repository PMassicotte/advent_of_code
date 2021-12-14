library(tidyverse)
library(here)

file <- "~/Desktop/input"

# file <- here("data", "input", "2021", "14_input")
polymer <- read_lines(file, n_max = 1)

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

for (i in 1:4) {
  # print(i)
  polymer <- split_pair(polymer)

  res <- map(polymer, insert_element)

  polymer <- reduce(res, ~ paste0(..1, str_sub(..2, 2, -1)))

  print(polymer)
}

element_count <- table(strsplit(polymer, ""))
max(element_count) - min(element_count)


# Part 2 ------------------------------------------------------------------

options(digits = 20)

file <- "~/Desktop/input"

# file <- here("data", "input", "2021", "14_input")
polymer <- read_lines(file, n_max = 1)

rules <- read_delim(
  file,
  skip = 2,
  col_names = c("pair", "insert"),
  delim = " -> "
)

lut <- rules %>%
  mutate(
    left = paste0(str_sub(pair, 1, 1), insert),
    right = paste0(insert, str_sub(pair, 2, 2))
  ) %>%
  pivot_longer(c("left", "right")) %>%
  select(pair, value)

# Initial count
df_count <- rules %>%
  mutate(n = str_count(polymer, pair)) %>%
  filter(n > 0)

df_count

for (i in 1:1) {
  df_count <- df_count %>%
    full_join(lut, by = "pair") %>%
    group_by(pair = value) %>%
    summarise(n = sum(n, na.rm = TRUE))
}

df_count


