# library(stringr)
library(purrr)

# read input
input <- readLines(file.path("2022", "day08", "example.txt"))
input

tree_grid <- map(strsplit(input, ""), as.numeric)
outer_trees <- (length(tree_grid) - 1) * (length(tree_grid[[1]]) - 1)

grid_len <- length(tree_grid[[1]])
grid_hgt <- length(tree_grid)

# between 2 and length - 1
# i <- 2
# tree <- tree_grid[[3]][i]
# 
# 
# down <- unlist(map(tree_grid[(i+1):grid_hgt], i))
# up <- unlist(map(tree_grid[1:i], i))
# 
# left <- tree_grid[[3]][1:(i-1)]
# right <- tree_grid[[3]][(i+1):grid_len]
# 
# any(c(
#   all(tree > down),
#   all(tree > up),
#   all(tree > right),
#   all(tree > left)
# ))
# 
test <- c()

for (i in seq(2, grid_len-1)) {
  
  for (j in seq(2, grid_hgt-1))
  
  # tree <- tree_grid[[j]][i]
  print(paste0("tree", "[", i, ",", j, "]"))
  
  down <- unlist(map(tree_grid[(i+1):grid_hgt], i))
  up <- unlist(map(tree_grid[1:i], i))
  
  left <- tree_grid[[3]][1:(i-1)]
  right <- tree_grid[[3]][(i+1):grid_len]
  
  visible <- any(
    c(
      all(tree_grid[[j]][i] > down),
      all(tree_grid[[j]][i] > up),
      all(tree_grid[[j]][i] > right),
      all(tree_grid[[j]][i] > left)
    )
  )
  
  print(visible)
  
  test <- c(test, visible)
  
}









