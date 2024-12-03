library(here)
library(glue)

new_challenge <- function(year, day, language = "R") {
    if (day %in% 1:9) {
        day <- glue("0{day}")
    }

    dir <- here(year, glue("day_{day}"))
    ex_file <- here(year, glue("day_{day}"), "example.txt")
    input_file <- here(year, glue("day_{day}"), "input.txt")
    challenge_file <- here(year, glue("day_{day}"), glue("day_{day}.{language}"))

    if (!dir.exists(dir)) {
        dir.create(dir)
    }

    if (!file.exists(ex_file)) {
        file.create(ex_file)
    }

    if (!file.exists(input_file)) {
        file.create(input_file)
    }

    if (!file.exists(challenge_file)) {
        file.create(challenge_file)
    }
}

# new_challenge(2024, 2)
