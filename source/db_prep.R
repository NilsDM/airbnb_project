# Data cleaning libraries
library(readr)
library(tidyr)
library(stringr)
library(tibble)
library(dplyr) 

# Database libraries
library(RClickhouse)
library(DBI)


#################### Read in and view initial data
listing_data <- read_csv("data/listings.csv.gz")
dim(listing_data) # 17040 x 74
# listing_data %>% view() 


#################### Deal with NA values 
listing_data <- 
    listing_data %>% 
    
    # Convert dates to characters for NA values
    mutate(last_scraped = as.character(last_scraped),
           host_since = as.character(host_since),
           calendar_last_scraped = as.character(calendar_last_scraped),
           first_review = as.character(first_review),
           last_review = as.character(last_review),
           ) %>% 
    
    # Homogenize NA values
    #*# Taken from: https://rpubs.com/Argaadya/create_table_sql
    mutate_all(function(x) ifelse(x == "" | x == "None" | x == "N/A", NA, x)) %>%  #*#
    
    # Convert character strings back to date type
    mutate(last_scraped = as.Date(last_scraped),
           host_since = as.Date(host_since),
           calendar_last_scraped = as.Date(calendar_last_scraped),
           first_review = as.Date(first_review),
           last_review = as.Date(last_review)) 

# listing_data %>% view()
 

#################### Host count  
# listing_data %>% 
#     count(host_id) %>% 
#     arrange(-n) %>% 
#     head(10)


#################### Extract host data
host_data <- listing_data %>% 
    select(host_id:host_identity_verified, 
           calculated_host_listings_count:calculated_host_listings_count_shared_rooms)


#################### Remove duplicate values
host_data <- host_data %>% distinct()


#################### Clean boolean values
host_data<- 
    host_data %>% 
    mutate_at(vars(host_is_superhost, host_has_profile_pic, host_identity_verified), 
                       function(x) case_when(x == "t" ~ TRUE, 
                                             x == "f" ~ FALSE,
                                             T ~ NA)) 


#################### Clean host verification column
host_data <- 
    host_data %>% 
    mutate(host_verifications = str_remove_all(host_verifications, "[\\'\\[\\]]"))


# host_data %>% 
#     select_if(is.character) %>% 
#     lapply(FUN = function(x) max(nchar(x, keepNA = F))) %>% 
#     as.data.frame() %>% 
#     pivot_longer(cols = names(.), 
#                  names_to = "column", 
#                  values_to = "maximum_length")


#################### 
# con <- dbConnect(RClickhouse::clickhouse()) 
con <- dbConnect(
    drv = clickhouse(),
    host = "localhost", 
    port = 9000, 
    db = "airdb", 
    user = "default",
    password = "", 
    # compression = "lz4",
    # config_paths = c("./RClickhouse.yaml", "~/.R/RClickhouse.yaml",
    #                  "/etc/RClickhouse.yaml"),
    # Int64 = c("integer64", "integer", "numeric", "character"),
    toUTF8 = TRUE
)

query <- "CREATE TABLE host_info(
    host_id INT, 
    host_url VARCHAR(50), 
    host_name VARCHAR(100), 
    host_since DATE,
    host_location VARCHAR(500), 
    host_about VARCHAR(10000),
    host_response_time VARCHAR(50),
    host_response_rate VARCHAR(50),
    host_acceptance_rate VARCHAR(50),
    host_is_superhost BOOLEAN,
    host_thumbnail_url VARCHAR(500),
    host_picture_url VARCHAR(500),
    host_neighbourhood VARCHAR(50),
    host_listings_count INT,
    host_total_listings_count INT,
    host_verifications VARCHAR(500),
    host_has_profile_pic BOOLEAN,
    host_identity_verified BOOLEAN,
    calculated_host_listings_count INT, 
    calculated_host_listings_count_entire_homes INT,
    calculated_host_listings_count_private_rooms INT,
    calculated_host_listings_count_shared_rooms INT,
    PRIMARY KEY(host_id)
    )"



