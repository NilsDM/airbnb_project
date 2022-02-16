library(readr)
library(tidyr)
library(tibble)
library(dplyr) 
library(ggplot2)
library(R.utils)
library(RClickhouse)


# Read in and view initial data
listing_data <- read_csv("../data/listings.csv.gz")
dim(listing_data) # 17040 x 74
listing_data %>% view() 


# Deal with NA values 
# Taken from: https://rpubs.com/Argaadya/create_table_sql
# listing_data <- listing_data %>% 
#    mutate(across(,function(x) ifelse(x == "" | x == "None" | x == "N/A", NA, x)))
######################## Find a way to make this function work
