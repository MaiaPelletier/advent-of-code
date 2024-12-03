# Part 1 ----
input <- here::here("2024", "day_01", "input.txt")

input_df <- readr::read_delim(input, delim = "   ", col_names = FALSE)

sort_input_1 <- sort(input_df$X1)
sort_input_2 <- sort(input_df$X2)

sum(abs(sort_input_2 - sort_input_1))
# [1] 1341714

# Part 2 ----
calculate_score <- input_df[,1] |>
    dplyr::mutate(
        appearances = purrr::map_int(X1, \(x) sum(x == input_df$X2)),
        score = X1*appearances
    )

sum(calculate_score$score)
# [1] 27384707