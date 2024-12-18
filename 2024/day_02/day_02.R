# Read puzzle input ----
input <- here::here("2024", "day_02", "input.txt")
input <- readr::read_lines(input)
input <-  purrr::map(input, \(x) x |> stringr::str_split(" ") |> unlist() |> as.numeric())

# Functions ----
check_line_safety <- function(line) {
  steps <- diff(line)

  check_inc_dec <- all(steps > 0) | all(steps < 0)
  check_step_size <- all(abs(steps) > 0 & abs(steps) <= 3)

  if (check_inc_dec & check_step_size) {
    safe <- TRUE
  } else {
    safe <- FALSE
  }

  return(safe)
}

check_with_problem_dampener <- function(line) {
  # initialize while loop
  line_length <- length(line)
  safe <- check_line_safety(line)
  index <- 1

  while (!safe & index <= line_length) {
    safe <- check_line_safety(line[-index])
    index <- index + 1
  }

  return(safe)
}

# Part 1 ----
safety_check <- input |> purrr::map(check_line_safety) |> unlist()
sum(safety_check)

# Part 2 ----
unsafe <- input[which(!safety_check)]
dampener_safety_check <- purrr::map(unsafe, check_with_problem_dampener) |> unlist()
sum(safety_check) + sum(dampener_safety_check)
