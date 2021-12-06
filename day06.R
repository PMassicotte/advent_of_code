library(tidyverse)
library(here)

v <- c(3, 4, 3, 1, 2)

# v <- scan(here("data/input/2021/06"), what = "integer", sep = ",") %>%
#   parse_integer()

reproduce <- function(v, day, max_day) {
  v2 <- v - 1

  i <- v2 < 0
  v2[i] <- 6

  v3 <- c(v2, rep(8, sum(i)))

  if (day == max_day) {
    return(v3)
  }

  reproduce(v3, day = day + 1, max_day = max_day)
}

res <- reproduce(v, 1, 80)

res

length(res)


# Part 2 ------------------------------------------------------------------

v <- c(3, 4, 3, 1, 2)

count_fish <- function(v) {

  fc <- rle(sort(v))

  age <- fc$values
  fish_count <- fc$lengths

  fish_counts <- rep(0, 9)

  fish_counts[age + 1] <- fish_count

  fish_counts <- set_names(fish_counts, 0:8)

}

reproduce <- function(v, fish_count_current = count_fish(v), day, max_day) {

  # fish_count_current <- count_fish(v)

  # if(day == 3) browser()
  v <- v - 1
  i <- v < 0
  v[i] <- 6

  fish_count_next <- count_fish(v)

  fish_count_next[7] <- fish_count_current[1] + fish_count_current[8]
  fish_count_next[8] <- fish_count_current[7]
  fish_count_next[9] <- fish_count_current[1]

  if (day == max_day) {
    return(fish_count_next)
  }

  reproduce(v, fish_count_next, day = day + 1, max_day = max_day)

}

reproduce(v, count_fish(v), 1, 5)


