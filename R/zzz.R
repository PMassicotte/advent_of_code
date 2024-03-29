library(tidyverse)
library(httr)
library(glue)
library(here)
library(fs)
library(rvest)

get_inputs <- function(year, day) {
  cookie <- Sys.getenv("AOC_COOKIE")

  cookie <- httr::set_cookies(session = cookie)
  url <- glue("https://adventofcode.com/{year}/day/{day}/input")

  res <- httr::GET(url, config = cookie)
  input <- httr::content(res, type = "text/plain", encoding = "UTF-8")

  dirpath <- path(here(), glue("data/input/{year}"))
  filepath <- path(dirpath, glue("{str_pad(day, width = 2, pad = '0', side = 'left')}_input"))

  if (!dir_exists(dirpath)) {
    dir_create(dirpath)
  }

  write_lines(input, filepath, sep = "")
}

create_template <- function(template_file = here("inst", "template.Rmd"), year, day, overwrite = FALSE) {

  url <- glue("https://adventofcode.com/{year}/day/{day}")

  subtitle <- read_html(url) %>%
    html_node("h2") %>%
    html_text()

  day <- str_pad(day, width = 2, pad = "0", side = "left")

  dirpath <- path(here(), glue("_posts/aoc-{year}-day-{day}"))
  filepath <- path(dirpath, glue("day-{day}.Rmd"))

  if (!dir_exists(dirpath)) {
    dir_create(dirpath)
  }

  file_copy(template_file, filepath, overwrite = overwrite)

  xfun::gsub_file(filepath, pattern = "{$year}", replacement = year, fixed = TRUE)
  xfun::gsub_file(filepath, pattern = "{$day}", replacement = day, fixed = TRUE)
  xfun::gsub_file(filepath, pattern = "{$subtitle}", replacement = subtitle, fixed = TRUE)

  return(filepath)
}

year <- 2015
day <- 11

filepath <- create_template(year = year, day = day)
get_inputs(year = year, day = day)

rstudioapi::navigateToFile(filepath)
