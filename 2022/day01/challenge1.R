# Read input (each item is a line from the input)
input <- readLines(file.path("2022", "day01", "input.txt"))

# For blank lines, replace with "new line" character (could be any character,
# just used to split value)
input[which(input=="")] <- "\n"

# collapse input with a space between values
input <- paste0(input, collapse = " ")

# split collapsed string into a list by new line character
input <- stringr::str_split(input, "\n")

# trim whitespace around values
input <- lapply(input, trimws)

# now split values by spaces between them
input <- lapply(input, function(x) stringr::str_split(x, " "))

# convert values from charcter to numeric values
input <- lapply(input[[1]], as.numeric)

# sum all values for each elf
input <- lapply(input, sum)

# unlist totals
input <- unlist(input)

# solution 1: which elf had the most calories
output1 <- which(input == max(input))
output2 <- max(input)
print(paste0("Elf ", output1, " had ", output2, " calories"))

# solution 2: how many cals top 3 elves had
output3 <- sum(sort(input, TRUE)[1:3])
print(paste0("The total calories of the 3 elves who had the most is ", output3))


