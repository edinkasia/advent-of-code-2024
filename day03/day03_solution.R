library(tidyverse)
library(here)

# loading in the data as a single string - note the new function here
computer_instructions <- read_file(here("day03", "day03_data.txt"))

# Regex created using an online resource: mul\(\d{1,3},\d{1,3}\)
# Needs double slashes because we're working in R

# extract the uncorrupted instructions into a list
instructions_clean <- str_extract_all(computer_instructions, 
                                      "mul\\(\\d{1,3},\\d{1,3}\\)")

# Simplify the list into a vector
instructions_clean_vector <- instructions_clean[[1]]

# Create a dataframe from the vector
instructions_clean_initial_df <- tibble(instr = instructions_clean_vector)

# Extract the two numbers from each mul command
instructions_clean_df <- instructions_clean_initial_df %>% 
  mutate(just_numbers = str_match(instr, "\\d{1,3},\\d{1,3}")[,1]) %>% 
  separate_wider_delim(just_numbers, delim = ",", 
                       names = c("number1", "number2"),
                       cols_remove = FALSE) %>% 
  mutate(across(starts_with("number"), as.numeric))

# multiply the numbers and put the result into a new column
solution_df <- instructions_clean_df %>% 
  mutate(solution_column = number1 * number2)

# Calculate the sum of numbers in that column
sum(solution_df$solution_column)

## Second star
