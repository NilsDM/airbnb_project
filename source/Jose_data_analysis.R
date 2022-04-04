# Jose's Data Analysis Queries

#################### Setup #################### 

#################### Build database
# Needs to be run only when initializing database
# If database already exists and needs to be removed, set: remove_old_database = TRUE
build_airbnb_database(con, listing_data, remove_old_database = FALSE)

# Load libraries and import functions (Always run) 
source("source/db_prep.R")


#################### Analysis #################### 

# Libraries
library(tidyr)
library(dplyr)
library(knitr)
library(scales)
library(ggplot2)
library(gridExtra)
library(tibble)

#################### Database Connection
con <- dbConnect(RSQLite::SQLite(), "airdb.SQLite")


#################### Load database tables back into R
# load host_info table 
res_host_info <- dbSendQuery(con, "select * from host_info")  
host_info <- fetch(res_host_info) 
dbClearResult(res_host_info)

# load listing table
res_listing <- dbSendQuery(con, "select * from listing")
listing <- fetch(res_listing)
dbClearResult(res_listing)

#################### Queries ####################  



#################### Query 1: What is the top and bottom 10 property type based on average price? (Room Listing)

q2<- listing%>%
    select(property_type,price)%>%
    group_by(property_type)%>%
    summarise(price = mean(price))%>%
    arrange(desc(price))
        
top_q2<- q2%>%
    arrange(desc(price))%>%
    top_n(10)%>% 
    ggplot(aes(x = price, y = property_type %>% reorder(price))) + 
    geom_col(fill = "Aquamarine4") + 
   
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Top Property Type by Average price",
        x        = "Average price",
        y        = "Property Type"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))


top_property_type_average_price <-q2 %>%
    arrange(desc(price))%>%
    top_n(10)%>% 
    knitr::kable(align = c("l", "c"),
    format.args = list(big.mark = ","),
    digits = 2)

top_property_type_average_price


bottom_q2<- q2%>%
    arrange((price))%>%
    top_n(-10)%>% 
    ggplot(aes(x = price, y = property_type %>% reorder(price))) + 
    geom_col(fill = "Skyblue3") + 
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Bottom Property Type by Average price",
        x        = "Average price",
        y        = "Property Type"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold")) 

bottom_property_type_average_price <-q2 %>%
    arrange(price)%>%
    top_n(-10)%>% 
    knitr::kable(align = c("l", "c"),
                 format.args = list(big.mark = ","),
                 digits = 2)

bottom_property_type_average_price

grid.arrange(top_q2, bottom_q2, ncol = 2)

#################### Query 2: What is the top and bottom 10 property type based on review score? (Room Listing)

q3<- listing%>%
    select(property_type,review_scores_rating)%>%
    group_by(property_type)%>%
    summarise(review_scores_rating = mean(review_scores_rating))%>%
    arrange(desc(review_scores_rating))

top_q3<- q3%>%
    arrange(desc(review_scores_rating))%>%
    top_n(10)%>% 
    ggplot(aes(x = review_scores_rating, y = property_type %>% reorder(review_scores_rating))) + 
    geom_col(fill = "Aquamarine4") + 
    
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Top Property Type by Review Score Rating",
        x        = "Review Score Rating",
        y        = "Property Type"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))


top_property_type_review_scores_rating <-q3 %>%
    arrange(desc(review_scores_rating))%>%
    top_n(10)%>% 
    knitr::kable(align = c("l", "c"),
                 format.args = list(big.mark = ","),
                 digits = 2)

top_property_type_review_scores_rating


bottom_q3<- q3%>%
    arrange((review_scores_rating))%>%
    top_n(-10)%>% 
    ggplot(aes(x = review_scores_rating, y = property_type %>% reorder(review_scores_rating))) + 
    geom_col(fill = "Skyblue3") + 
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Bottom Property Type by Review Score Rating",
        x        = "Review Score Rating",
        y        = "Property Type"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold")) 

bottom_property_type_review_scores_rating <-q3 %>%
    arrange(review_scores_rating)%>%
    top_n(-10)%>% 
    knitr::kable(align = c("l", "c"),
                 format.args = list(big.mark = ","),
                 digits = 2)

bottom_property_type_review_scores_rating

grid.arrange(top_q3, bottom_q3, ncol = 2)



#################### Query 3: What is the most common amenities provided? (Room Listing)
ncols_q4 <- max(stringr::str_count(listing$amenities, ",")) + 1
colmn_q4 <- paste("col", 1:ncols_q4)

q4<- listing%>%
    select(amenities)%>%
    tidyr::separate(
    col = amenities, 
    sep= ",", 
    into=colmn_q4,
    remove = FALSE)

q4<- pivot_longer(data=q4,
                  cols = 'col 1':'col 78',
                  names_to = "col_number",
                  values_to = "separated_amenities")
    
q4<- q4%>%
    select(separated_amenities)%>%
    group_by(separated_amenities)%>%
    summarise(amenities_count=n())%>%
    na.omit()%>%
    arrange(desc(amenities_count))%>%
    top_n (15)

 
 q4_plot<-q4%>%
    ggplot(aes(x = amenities_count, y = separated_amenities %>% reorder(amenities_count))) + 
    geom_col(fill = "Aquamarine4") + 
    
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Most Common Amenities",
        x        = "Count",
        y        = "Amenities"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))

most_common_amenities<- q4%>%
    knitr::kable(align = c("l", "c"),
                 format.args = list(big.mark = ","),
                 digits = 2)

most_common_amenities

#################### Query 4: What is the most commonly verified host information? (Host)

ncols_q10 <- max(stringr::str_count(na.omit(host_info$host_verifications), ",")) + 1
colmn_q10 <- paste("col", 1:ncols_q10)

q10<- host_info%>%
    select(host_verifications)%>%
    tidyr::separate(
        col = host_verifications, 
        sep= ",", 
        into=colmn_q10,
        remove = FALSE)

q10<- pivot_longer(data=q10,
                  cols = 'col 1':'col 12',
                  names_to = "col_number",
                  values_to = "separated_host_verifications")

q10<- q10%>%
    select(separated_host_verifications)%>%
    group_by(separated_host_verifications)%>%
    summarise(host_verifications_count=n())%>%
    na.omit()%>%
    arrange(desc(host_verifications_count))%>%
    top_n (10)


q10_plot<-q10%>%
    ggplot(aes(x = host_verifications_count, y = separated_host_verifications %>% reorder(host_verifications_count))) + 
    geom_col(fill = "Aquamarine4") + 
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Most Common Verified Information",
        x        = "Count",
        y        = "Verified Information"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))

q10_plot


q10 <-most_common_host_verifications<- q10%>%
    knitr::kable(align = c("l", "c"),
                 format.args = list(big.mark = ","),
                 digits = 2)

most_common_host_verifications


#################### Query 5: What is the most common room type available? (Room Listing)


q1<- listing%>%
    select(room_type,has_availability)%>%
    group_by(room_type)%>%
    filter(has_availability==1)%>%
    summarise(availability=n())%>%
    arrange(desc(availability))


q1_plot<-q1%>%
    ggplot(aes(x = availability, y = room_type %>% reorder(availability))) + 
    geom_col(fill = "Aquamarine4") + 
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Most Common Room Type Available",
        x        = "Availability",
        y        = "Room Type"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))

q1_plot


most_common_room_type_available <-q1%>% 
    knitr::kable(align = c("l", "c"),
                 format.args = list(big.mark = ","),
                 digits = 2)

most_common_room_type_available





availability_periods <- c('availability_30', 'availability_60', 'availability_90', 'availability_365')

for(col in availability_periods){
    tables<- paste('q1',col, sep='_')
    assign(tables,listing%>%
               select(room_type,col)%>%
               group_by(room_type)%>%
               summarise(mean=mean(.data[[col]])))
}




q1_availability_30_plot<-q1_availability_30%>%
    ggplot(aes(x = mean, y = room_type )) + 
    geom_col(fill = "Aquamarine4") + 
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Most Common Room Type Available 30 Days",
        x        = "Availability",
        y        = "Room Type"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))


q1_availability_60_plot<-q1_availability_60%>%
    ggplot(aes(x = mean, y = room_type )) + 
    geom_col(fill = "Aquamarine4") + 
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Most Common Room Type Available 60 Days",
        x        = "Availability",
        y        = "Room Type"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))


q1_availability_90_plot<-q1_availability_90%>%
    ggplot(aes(x = mean, y = room_type )) + 
    geom_col(fill = "Aquamarine4") + 
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Most Common Room Type Available 90 Days",
        x        = "Availability",
        y        = "Room Type"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))



q1_availability_365_plot<-q1_availability_365%>%
    ggplot(aes(x = mean, y = room_type )) + 
    geom_col(fill = "Aquamarine4") + 
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Most Common Room Type Available 365 Days",
        x        = "Availability",
        y        = "Room Type"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))


 
grid.arrange(q1_availability_30_plot, q1_availability_60_plot, q1_availability_90_plot,q1_availability_365_plot, ncol = 2)


###########################################################################################
    
    
    
    
    
    
