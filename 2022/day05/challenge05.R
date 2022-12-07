library(stringr)
library(purrr)

# read input
input <- readr::read_lines(file.path("2022", "day05", "input.txt"))

# split boxes from instructions
boxes <- input[1:(which(nchar(input) == 0)-1)]
instructions_raw <- input[(which(nchar(input) == 0)+1):length(input)]

# format boxes ------------------------------------------------------------

format_boxes_list <- function(box_input) {
  row <- unlist(str_split(box_input, ""))
  for (i in seq(4, length(row), by = 4)) {
    row[i] <- "-"
  }
  return(unlist(str_split(paste0(row, collapse = ""), "-")))
}

box_list <- map(boxes[1:(length(boxes)-1)], format_boxes_list)
box_vectors <- list_along(box_list[[1]])

for (i in seq(1, length(box_vectors))) {
  box_vectors[[i]] <- rev(unlist(map(box_list, i)))
  box_vectors[[i]] <- box_vectors[[i]][which(box_vectors[[i]] != "   ")]
}

# format instructions -----------------------------------------------------

instructions <- str_extract_all(instructions_raw, "[0-9]+")
instructions <- map(instructions, as.numeric)

# move boxes around -------------------------------------------------------

rev <- TRUE # true = sol 1, false = sol 2

for (i in seq(1, length(instructions))) {

  move <- instructions[[i]][1]
  from <- instructions[[i]][2]
  to <- instructions[[i]][3]
  
  stack <- tail(box_vectors[[from]], move)

  if (rev) {
    box_vectors[[to]] <- append(box_vectors[[to]], rev(stack))
  } else {
    box_vectors[[to]] <- append(box_vectors[[to]], stack)
  }

  
  if (length(box_vectors[[from]]) - move == 0) {
    box_vectors[[from]] <- character(0)
  } else {
    box_vectors[[from]] <-
      box_vectors[[from]][1:(length(box_vectors[[from]]) - move)] 
  }
  
}

top_boxes <- str_extract(unlist(map(box_vectors, tail, 1)), "[A-Z]")

# solution 1 = GRTSWNJHH, solution 2 = QLFQDBBHM
paste0(top_boxes, collapse = "")



