library(here)
library(glue)

create_file <- function(file) {
  if (!file.exists(file)) {
    file.create(file)
  }
}

new_challenge <- function(year, day, language = "R") {
  if (day %in% 1:9) {
    day <- glue("0{day}")
  }

  dir <- here(year, glue("day_{day}"))
  ex_file <- here(year, glue("day_{day}"), "example.txt")
  input_file <- here(year, glue("day_{day}"), "input.txt")
  challenge_file <- here(year, glue("day_{day}"), glue("day_{day}.{language}"))
  blank_files <- list(ex_file, input_file, challenge_file)

  dir.create(dir)
  purrr::walk(blank_files, create_file)

}

# new_challenge(2024, 2)
