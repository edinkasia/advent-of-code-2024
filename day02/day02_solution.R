library(tidyverse)
library(here)

# Loading the data
reports <- read_csv(here("day02", "day02_data.csv"), 
                    col_names = FALSE)

test_data <- read_csv(here("day02", "day02_test.csv"), 
                      col_names = FALSE)

# First star

# Function for checking if the report is safe
check_if_safe <- function(x_vector){
  # initialise with an empty vector
  differences <- vector("double", length = 0)
  for (i in 1:(length(x_vector)-1)) {
    # calculate the difference between consecutive vector elements
    differences[[i]] <- x_vector[i+1]-x_vector[i]
  }
  differences <- differences[!is.na(differences)]
  all_increase <- sum(differences > 0) == length(differences)
  all_decrease <- sum(differences < 0) == length(differences)
  all_between_one_three <- sum(abs(differences) %in% c(1, 2, 3)) == length(differences) 
  # check if the differences between consecutive items meet the criteria
  result <- case_when((all_increase | all_decrease) & all_between_one_three ~ "safe",
                      .default = "unsafe")
  return(result)
}

# Test function on example data
check_if_safe(x_vector = c(7,6,4,2,1,NA))

# Use the function on my data
result_column <- apply(reports, MARGIN = 1, FUN = check_if_safe)
# print(result_column)
# Calculate the final result
sum(result_column == "safe")
# 
# # Use the function on test data
# result_column <- apply(test_data, MARGIN = 1, FUN = check_if_safe)
# 
# ## Second star
# test_vector1 <- c(1,2,3,4,5)
# test_vector2 <- c(1,3,2,4,5)
# test_vector3 <- c(1,3,0,4,0)
# test_vectorall <- rbind(test_vector1,test_vector2,test_vector3)
# 
# apply(test_vectorall, MARGIN = 1, FUN = check_if_safe)

