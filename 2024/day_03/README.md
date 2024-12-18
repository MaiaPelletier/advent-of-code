2024 - Day 3
================

## Set-up and functions

``` r
# Load used libraries ----
library(stringr)

# Read puzzle input ----
input <- here::here("2024", "day_03", "input.txt")
input <- readr::read_lines(input)

# Define functions ----
mul <- function(x,y) x*y
eval_mul <- function(mul) eval(parse(text = mul))

sum_products <- function(input) {
  str_extract_all(input, "mul\\([0-9]{1,3},[0-9]{1,3}\\)") |>
    unlist() |>
    purrr::map(eval_mul) |>
    unlist() |>
    sum()
}
```

## Part 1

``` r
sum_products(input)
```

    ## [1] 174960292

## Part 2

``` r
cleaned_input <- input |>
  str_flatten() |>
  str_remove_all("don't\\(\\)(.*?)do\\(\\)")

sum_products(cleaned_input)
```

    ## [1] 56275602
