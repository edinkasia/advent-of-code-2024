library(tidyverse)
library(here)

# loading in the data as a single string - note the new function here
computer_instructions <- read_file(here("day03", "day03_data.txt"))

# Regex created using an online resource: mul\(\d{1,3},\d{1,3}\)

# extract the uncorrupted instructions
instructions_clean <- str_extract_all(computer_instructions, 
                                      "mul\\(\\d{1,3},\\d{1,3}\\)")

instructions_clean_vector <- instructions_clean[[1]]

instructions_clean_initial_df <- tibble(instr = instructions_clean_vector)

instructions_clean_df <- instructions_clean_initial_df %>% 
  mutate(just_numbers = str_match(instr, "\\d{1,3},\\d{1,3}")[,1]) %>% 
  separate_wider_delim(just_numbers, delim = ",", 
                       names = c("number1", "number2"),
                       cols_remove = FALSE) %>% 
  mutate(across(starts_with("number"), as.numeric))

solution_df <- instructions_clean_df %>% 
  mutate(solution_column = number1 * number2)

sum(solution_df$solution_column)
