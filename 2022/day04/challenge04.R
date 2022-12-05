library(purrr)
library(stringr)

section_check <- function(pair, total_overlap = TRUE) {
  
  assignments <- list(
    elf1 = eval(parse(text = pair[1])),
    elf2 = eval(parse(text = pair[2]))
  )
  
  if (total_overlap) {
    
    sections <- map(assignments, length)
    
    if (length(unique(sections)) == 1) {
      return(all(assignments$elf1 %in% assignments$elf2))
    } else {
      return(all(assignments[[which.min(sections)]] %in% assignments[[which.max(sections)]]))
    }
    
    
  } else {
    return(!identical(intersect(assignments$elf1, assignments$elf2), integer(0)))
  }
  
}

# read input
input <- readLines(file.path("2022", "day04", "input.txt"))

pairs <- str_split(input, ",")
pairs <- map(pairs, str_replace, "-", ":")

# solution 1 = 424
sum(map_lgl(pairs, section_check, total_overlap = TRUE))

# solution 2 = 804
sum(map_lgl(pairs, section_check, total_overlap = FALSE))






