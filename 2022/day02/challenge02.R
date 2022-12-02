# load packages
library(dplyr)
library(purrr)

# functions -------

# function to convert letters to choice value
rps_to_number <- function(choice) {
  if (choice %in% c("A", "X")) return(1)
  else if (choice %in% c("B", "Y")) return(2)
  else return(3)
}


# function to return a win choice value given the opponent's choice
win <- function(opp) {
  if (opp == 1) return(2)
  else if (opp == 2) return(3)
  else  return(1)
}


# function to return a lose choice value given the opponent's choice
lose <- function(opp) {
  if (opp == 1) return(3) 
  else if (opp == 2) return(1)
  else return(2)
}


# read input as a data.frame
input <-
  read.delim(
    "input.txt",
    sep = " ",
    header = FALSE,
    col.names = c("opp", "resp")
  )


# solution 1 = 13565
input %>% 
  mutate_all(~ map_dbl(.x, rps_to_number)) %>% 
  mutate(
    outcome = case_when(
      opp == resp ~ 3,
      opp == 1 & resp == 2 ~ 6,
      opp == 2 & resp == 3 ~ 6,
      opp == 3 & resp == 1 ~ 6,
      TRUE ~ 0
    )
  ) %>% 
  transmute(round_score = resp + outcome) %>% 
  summarise(total = sum(round_score))


# solution 2 = 1242
input %>% 
  rename(outcome = resp) %>%
  mutate(
    opp = purrr::map_dbl(opp, rps_to_number),
    outcome = case_when(
      outcome == "X" ~ 0,
      outcome == "Y" ~ 3,
      outcome == "Z" ~ 6
    ),
    resp = case_when(
      outcome == 3 ~ opp,
      outcome == 0 ~ map_dbl(opp, lose),
      outcome == 6 ~ map_dbl(opp, win)
      
    )
  ) %>% 
  transmute(round_score = resp + outcome) %>% 
  summarise(total = sum(round_score))
 
