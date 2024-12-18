library(tidyverse)
library(here)

# Loading the data
reports <- read_csv(here("day02", "day02_data.csv"), 
                    col_names = FALSE)
