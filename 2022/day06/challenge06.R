# read input
input <- readr::read_file(file.path("2022", "day06", "input.txt"))

# change this to 13 for solution to part 2
n_chars <- 3 

for (i in seq(1, nchar(input))) {
  
  received <- unlist(strsplit(substr(input, i, i+n_chars), ""))
  
  if (length(unique(received)) == (1 + n_chars)) {
    marker <- i+n_chars
    break
  }
  
}

# solution 1 = 1109, solution 2 = 3965
marker
