library(tidyverse)
library(here)

options(digits = 20)

reproduction <- function(ndays, fishes) {
  for (i in seq_len(ndays)) {

    fishes

    new_fishes <- fishes[1]

    fishes_next <- lead(fishes, default = 0)[-9]
    fishes_next

    fishes_next[9] <- new_fishes
    fishes_next[7] <- new_fishes + fishes[8]

    fishes_next

    fishes <- fishes_next


  }

  return(fishes)
}

# v <- c(3, 4, 3, 1, 2)
v <- scan(here("data/input/2021/06"), what = "integer", sep = ",") %>%
  parse_integer()

fishes <- rep(0, 9)
fc <- rle(sort(v))
fishes[fc$values + 1] <- fc$lengths

## Part 1
res <- reproduction(ndays = 80, fishes)
sum(res)

## Part 2
res <- reproduction(ndays = 256, fishes)
sum(res)

