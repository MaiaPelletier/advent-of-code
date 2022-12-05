# load used libraries
library(dplyr)
library(stringr)
library(purrr)

# function to split string of characters and return sub string if start and end
# points are given
split_items <- function(str, start = NULL, end = NULL) {
  
  if (is.null(start)) unlist(str_split(str, ""))
  else unlist(str_split(str_sub(str, start = start, end = end), ""))
  
}

# function to return duplicate item in each elves bag
duplicate_item <- function(str) {
  
  len <- str_length(str)
  split <- len/2
  
  compartment_1 <- split_items(str, start = 1, end = split)
  compartment_2 <- split_items(str, start = split + 1, end = len)
  
  dup <- intersect(compartment_1, compartment_2)
  
  return(dup)
  
}

# function to find which badge group of elves have
badge_finder <- function(items) {
  
  items <- map(items, split_items)
  intersect(items[[3]], intersect(items[[1]], items[[2]]))
  
}

# read input
input <- readLines(file.path("2022", "day03", "input.txt"))

# create codeset of the items and their priorities
priorities <- data.frame(
  item = c(letters, LETTERS),
  priority = seq_along(c(letters, LETTERS))
)

# part 1 solution = 7917
data.frame(input = input) %>% 
  mutate(
    duplicate = map_chr(input, duplicate_item)
  ) %>% 
  left_join(priorities, by = c("duplicate" = "item")) %>% 
  summarise(total = sum(priority))

# part 2 solution = 2585
data.frame(input = input) %>% 
  mutate(
    group = rep(seq(1, nrow(.)/3), each = 3)
  ) %>% 
  group_by(group) %>% 
  summarise(input = list(input), .groups = "drop") %>% 
  mutate(
    badge = map_chr(input, badge_finder)
  ) %>% 
  left_join(priorities, by = c("badge" = "item")) %>% 
  summarise(total = sum(priority))
  
