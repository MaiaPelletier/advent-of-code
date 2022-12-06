library(stringr)
library(purrr)

# read input
input <- readr::read_lines(file.path("2022", "day05", "example.txt"))

# split boxes from instructions
boxes <- input[1:(which(nchar(input) == 0)-1)]
instructions <- input[(which(nchar(input) == 0)+1):length(input)]

boxes

nchar(boxes[length(boxes)])


format_boxes_list <- function(box_input) {
  
  row <- unlist(str_split(box_input, ""))
  
  for (i in seq(4, length(row), by = 4)) {
    
    row[i] <- "-"
    
  }
  
  return(unlist(str_split(paste0(row, collapse = ""), "-")))
  
}

box_list <- map(boxes[1:3], format_boxes_list)
map(box_list, str_extract, "[A-Z]")

unlist(box_list)

tail(unlist(box_list), 1)

box_matrix <- 
  matrix(
    unlist(box_list),
    ncol = length(box_list[[1]]),
    nrow = length(box_list),
    byrow = TRUE
  )


