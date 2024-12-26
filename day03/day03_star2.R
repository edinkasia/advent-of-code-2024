# library(tidyverse)
# library(here)

# loading in the data as a single string - note the new function here
computer_instructions <- read_file(here("day03", "day03_data.txt"))

# test_instructions <- "xmul(2,4)&mul(3,7)!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

conditional_memory_calculation <- function(long_string){
  # remove new page symbols because they caused problems
  long_string <- str_replace_all(long_string, "[\r\n]" , "")
  # cut the instructions into sections ending with "do"
  result_list <- str_match_all(paste0(long_string,"do"),"(.*?)do")
  result_matrix <- result_list[[1]]
  # make a df with instruction sections in the first column
  initial_df <- tibble(inst_section = result_matrix[,1])
  result_df <- initial_df %>%
    # remove rows beginning with "n't" (the instruction there was don't)
    filter(!grepl("^n't", inst_section)) %>%
    # extract the mul(number1,number2) patterns - the result is a list column
    mutate(mul_instructions = str_extract_all(inst_section,
                    "mul\\(\\d{1,3},\\d{1,3}\\)")) %>%
    # unnest the list column
    unnest(cols = c(mul_instructions)) %>%
    # extract just the numbers
    mutate(just_numbers = str_match(mul_instructions, "\\d{1,3},\\d{1,3}")[,1]) %>%
    # separate the number pairs
    separate_wider_delim(just_numbers, delim = ",",
                         names = c("number1", "number2"),
                         cols_remove = FALSE) %>%
    # make sure the numbers are recognised as numeric
    mutate(across(starts_with("number"), as.numeric)) %>%
    # multiply the two numbers
    mutate(multiplication_column = number1 * number2)
  # calculate a sum of multiplications
  return(sum(result_df$multiplication_column, na.rm = TRUE))
}

test <- conditional_memory_calculation(computer_instructions)
