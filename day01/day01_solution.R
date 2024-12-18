library(tidyverse)
library(here)

# Loading the data
location_IDs <- read_csv(here("day01", "day01_data.csv"),
                         col_names = FALSE)
colnames(location_IDs) <- c("left_list", "right_list")

# First star
location_IDs <- location_IDs %>% 
  mutate(ordered_left_list = sort(left_list),
         ordered_right_list = sort(right_list)) %>% 
  mutate(diff_between_lists = abs(ordered_left_list - ordered_right_list))

day01_solution <- sum(location_IDs$diff_between_lists)
day01_solution

# Second star - create a function with a for-loop inside
find_repetition <- function(x_vector, y_vector){
  temp <- vector("double", length(x_vector))
  for (i in seq_along(x_vector)) {
    temp[[i]] <- x_vector[i]*sum(y_vector == x_vector[i])
  }
  return(temp)
}

# Check the function with example data
find_repetition(x_vector = c(3,4,2,1,3,3), y_vector = c(4,3,5,3,9,3))

# Use the function on my puzzle data
day01_solution_star2 <- find_repetition(x_vector = location_IDs$ordered_left_list,
                                        y_vector = location_IDs$ordered_right_list)

sum(day01_solution_star2)
