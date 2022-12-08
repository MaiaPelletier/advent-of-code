library(purrr)

# read input
input <- readLines(file.path("2022", "day08", "input.txt"))

tree_grid <- map(strsplit(input, ""), as.numeric)
visible_trees <- (length(tree_grid) - 1) * (length(tree_grid[[1]]) - 1) # these are the outer trees

grid_len <- length(tree_grid[[1]])
grid_hgt <- length(tree_grid)

test <- c()

for (i in seq(2, grid_len-1)) {
  
  for (j in seq(2, grid_hgt-1)) {
    
    down <- unlist(map(tree_grid[(i+1):grid_hgt], i))
    up <- unlist(map(tree_grid[1:i], i))
    
    left <- tree_grid[[j]][1:(i-1)]
    right <- tree_grid[[j]][(i+1):grid_len]
    
    visible <- any(
      c(
        all(tree_grid[[j]][i] > down),
        all(tree_grid[[j]][i] > up),
        all(tree_grid[[j]][i] > right),
        all(tree_grid[[j]][i] > left)
      )
    )
    
    if (visible) visible_trees <- visible_trees + 1
    
  }
  
}

visible_trees







