library(tidyverse)
library(here)

# Loading the data
reports <- read_csv(here("day02", "day02_data.csv"), 
                    col_names = FALSE)

test_data <- read_csv(here("day02", "day02_test.csv"), 
                      col_names = FALSE)

# First star

# Function for checking if the report is safe c(1,2,3,4)
check_if_safe <- function(x_vector){
  # initialise with an empty vector
  differences <- vector("double", length = 0)
  for (i in 1:(length(x_vector)-1)) {
    # calculate the difference between consecutive vector elements
    differences[[i]] <- x_vector[i+1]-x_vector[i]
  }
  all_increase <- sum(differences > 0) == length(differences)
  all_decrease <- sum(differences < 0) == length(differences)
  all_between_one_three <- sum(abs(differences) %in% c(1, 2, 3)) == length(differences) 
  # check if the differences between consecutive items meet the criteria
  result <- case_when((all_increase | all_decrease) & all_between_one_three ~ "safe",
                      .default = "unsafe")
  return(result)
}

# Use the function on test data
# result_column <- apply(test_data, MARGIN = 1, FUN = check_if_safe)


get_all_forgiving_permutations <- function(x_vector){
  # add full vector to list_of_vectors_with_gaps
  list_of_vectors_with_gaps <- list(x_vector)
  for (i in 1:(length(x_vector))) {
    # greate a vector with gap at index i
    # add that new vector to vectors_with_gaps
    vector_with_gap <- x_vector[-c(i)]
    list_of_vectors_with_gaps <- append(list_of_vectors_with_gaps, list(vector_with_gap))
  }
  return(list_of_vectors_with_gaps)
}
  
check_if_safe_forgiving <- function(x_vector){
  x_vector <- x_vector[!is.na(x_vector)]
  list_of_vectors_with_gaps <- get_all_forgiving_permutations(x_vector)
  # print(list_of_vectors_with_gaps)
  which_are_safe <- list_of_vectors_with_gaps %>% map(check_if_safe)
  # print(which_are_safe)
  sum(which_are_safe == "safe") >=1
}

check_all_reactors <- function(list_of_vectors){
  
  list_of_safe_reactors <- apply(list_of_vectors, MARGIN = 1, FUN = check_if_safe_forgiving)
  print(list_of_safe_reactors)
  sum(list_of_safe_reactors)
}

check_all_reactors(reports)

## Second star
# test_vector1 <- c(1,2,10,4,1)
# test_vector2 <- c(1,3,2,4,5)
# test_vector3 <- c(1,3,0,4,0)
# test_vectorall <- list(test_vector1,test_vector2,test_vector3)

# get_all_forgiving_permutations(test_vector1)
# check_if_safe_forgiving(test_vector1)

# apply(test_vectorall, MARGIN = 1, FUN = check_if_safe)
# apply(test_vectorall, MARGIN = 1, FUN = check_if_safe_forgiving)

# check_all_reactors(test_data)


