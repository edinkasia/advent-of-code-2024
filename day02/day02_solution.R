library(tidyverse)
library(here)

# Loading the data
reports <- read_csv(here("day02", "day02_data.csv"), 
                    col_names = FALSE)

# First star

# Function for checking if the report is safe
check_if_safe <- function(x_vector){
  # initialise with an empty vector
  temp <- vector("double", length = 0)
  for (i in seq_along(x_vector)) {
    # calculate the difference between consecutive vector elements
    temp[[i]] <- x_vector[i+1]-x_vector[i]
  }
  # remove NA (there is one at the end)
  temp <- temp[!is.na(temp)]
  # check if the differences between consecutive items meet the criteria
  result <- case_when((sum(temp > 0) == 0 | sum(temp > 0) == length(temp)) &
                      sum(abs(temp) %in% c(1, 2, 3)) == length(temp) ~ "safe",
                      .default = "unsafe")
  return(result)
}

# Test function on example data
check_if_safe(x_vector = c(7,6,4,2,1))

# Use the function on my data
result_column <- apply(reports, MARGIN = 1, FUN = check_if_safe)

# Calculate the final result
sum(result_column == "safe")
