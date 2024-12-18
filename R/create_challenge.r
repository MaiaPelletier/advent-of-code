
create_file <- function(file) {
  if (!file.exists(file)) {
    file.create(file)
  }
}

rmarkdown_template <- function(year, day) {
  glue::glue("
  ---
  title: \"{year} - Day {day}\"
  output: github_document
  ---

  ```{r setup, include=FALSE}
  knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
  ```
  ")
}

new_challenge <- function(year, day, language = "R") {
  if (day %in% 1:9) {
    day <- glue::glue("0{day}")
  }

  dir <- here::here(year, glue::glue("day_{day}"))
  ex_file <- here::here(year, glue::glue("day_{day}"), "example.txt")
  input_file <- here::here(year, glue::glue("day_{day}"), "input.txt")
  challenge_file <- here::here(year, glue::glue("day_{day}"), glue::glue("day_{day}.{language}"))
  readme_file <- here::here(year, glue::glue("day_{day}"), "README.Rmd")
  blank_files <- list(ex_file, input_file, challenge_file)

  dir.create(dir)
  purrr::walk(blank_files, create_file)
  readr::write_lines(rmarkdown_template(year, day), readme_file)

}

# new_challenge(2024, 2)
