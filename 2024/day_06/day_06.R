# Load used libraries
library(stringr)

# Read puzzle input
input <- here::here("2024", "day_06", "input.txt")
input <- readr::read_lines(input)

# Part 1 ----

# Grid is a square so this is the number of cols and rows
grid_size <- str_length(input[1])

# Split the characters in each line of text
grid_list <-
  input |>
  str_split("")

# Create a matrix of the spaces/obstacles
grid_matrix <-
  grid_list |>
  unlist() |>
  matrix(ncol = grid_size, byrow = TRUE)

# Functions ----

# Function finds the guards position in the given grid passed
find_guard <- function(grid, guard) {
  grid |>
  str_detect(guard) |>
  which()
}

# Function finds an obstacle in the path passed
find_obstacle <- function(path) str_detect(path, "#")

# Find the first obstacle guard will encounter in their path
find_stop <- function(path, direction) {
  switch(direction,
    "up" = max(which(find_obstacle(path))),
    "down" = min(which(find_obstacle(path)))
  )
}

count_spaces <- function(path) {
  length(path) - 2 # Subtract 2 for current pos and obstacle
}

update_position_matrix <- function(rows, cols) {
  pos_matrix[rows, cols] <- "X"
}

# Initialize ----
guard <- "\\^" # Starting guard orientation
pos_matrix <- grid_matrix # To update the guard's traveled positions on the grid
row <- find_guard(grid_list, guard) # Initial guard row
col <- find_guard(grid_list[[row]], guard) # Initial guard col
off_grid <- FALSE # Loop flag
pos_matrix[row, col] <- "X" # Mark starting position on position tracker
rotations <- 0 # Loop counter


while (!off_grid) {
  if (guard == "\\^") {
    # Guard traveling in positive Y direction
    guard_path <- grid_matrix[1:row, col]
    if(any(find_obstacle(guard_path))) {
      stop <- find_stop(guard_path, "up")
      spaces_to_move <- count_spaces(guard_path[stop:row])
      pos_matrix[(row - spaces_to_move):row, col] <- "X"
      row <- row - spaces_to_move
      rotations <- rotations + 1
      guard <- ">"
    } else {
      pos_matrix[1:row, col] <- "X"
      off_grid <- TRUE
    }
  } else if (guard == ">") {
    # Guard traveling in positive X direction
    guard_path <- grid_matrix[row, (col:grid_size)]
    if(any(find_obstacle(guard_path))) {
      stop <- find_stop(guard_path, "down")
      spaces_to_move <- count_spaces(guard_path[1:stop])
      pos_matrix[row, (col:(col + spaces_to_move))] <- "X"
      col <- col + spaces_to_move
      rotations <- rotations + 1
      guard <- "v"
    } else {
      pos_matrix[row, col:grid_size] <- "X"
      off_grid <- TRUE
    }
  } else if (guard == "v") {
    guard_path <- grid_matrix[row:grid_size, col]
    if(any(find_obstacle(guard_path))) {
      stop <- find_stop(guard_path, "down")
      spaces_to_move <- count_spaces(guard_path[1:stop])
      pos_matrix[(row:(row + spaces_to_move)), col] <- "X"
      row <- row + spaces_to_move
      rotations <- rotations + 1
      guard <- "<"
    } else {
      pos_matrix[row:grid_size, col] <- "X"
      off_grid <- TRUE
    }
  } else if (guard == "<") {
    guard_path <- grid_matrix[row, 1:col]
    if(any(find_obstacle(guard_path))) {
      stop <- find_stop(guard_path, "up")
      spaces_to_move <- count_spaces(guard_path[stop:col])
      pos_matrix[row, (col - spaces_to_move):col] <- "X"
      col <- col - spaces_to_move
      rotations <- rotations + 1
      guard <- "\\^"
    } else {
      pos_matrix[row, 1:col] <- "X"
      off_grid <- TRUE
    }
  }
}

print(rotations)
sum(as.vector(pos_matrix) == "X")
# Ans = 4977


