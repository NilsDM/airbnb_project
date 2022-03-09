# Nils' Data Analysis Queries

#################### Setup #################### 

#################### Build database
# Needs to be run only when initializing database
# If database already exists and needs to be removed, set: remove_old_database = TRUE
build_airbnb_database(con, listing_data, remove_old_database = FALSE)

# Load libraries and import functions (Always run) 
source("source/db_prep.R")


#################### Analysis #################### 

# Libraries
library(DBI) 
library(RSQLite)
library(tidyr)
library(tibble)
library(dbplyr)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(ggplot2)

library(tidyquant)
library(zoo)


#################### Database Connection
con <- dbConnect(RSQLite::SQLite(), "airdb.SQLite")

#################### Loads tables
host_info <- tbl(con, "host_info") %>% as.data.frame()
listing <- tbl(con, "listing") %>% as.data.frame()


#################### Queries ####################  



#################### Query 1: How is the number of host joined to Airbnb over time? (Room Listing)
q1 <- host_info %>% 
    left_join(listing, by = "host_id") %>% 
    select(host_id, host_since) %>% 
    mutate(host_since_date = as.Date(host_since)) %>% 
    separate("host_since", c("Year", "Month", "Day"), sep = "-") %>% 
    select(-Day) %>% 
    group_by(Year, Month) %>% 
    count(Year, Month) %>% 
    ungroup() %>% 
    mutate(year_month = paste0(Year, "-", Month, "-", "01"), 
           year_month_2 = paste0(Year, "-", Month), 
           joined = n) %>% 
    select(year_month, year_month_2, joined) %>% 
    mutate(year_month = as.Date(year_month)) %>% 
    drop_na()

q1 %>%
    ggplot(aes(x = year_month, y = joined)) +
    geom_line(size = 1.2, colour = "Aquamarine4") + 
    scale_x_date(breaks = waiver(), date_breaks = "6 months") + theme_tq() + 
    theme(axis.text.x = element_text(angle = 45, face = "bold", vjust = 0.65),
          axis.text.y = element_text(face = "bold")) + 
    labs(
        title    = "Number of hosts joined",
        subtitle = "Shows the frequency rate at which new posts sign up for airbnb",
        caption  = "",
        x        = "Joined",
        y        = "Year/Month")

q1 %>% 
    select(-year_month) %>% 
    mutate(year_month = as.yearmon(year_month_2)) %>% 
    select(-year_month_2) %>% 
    select(year_month, joined) %>% 
    arrange(desc(joined)) %>% 
    head(20) %>% knitr::kable()


#################### Query 2: Is there any correlation between room price and the review score? (Room Listing)
q2 <- 
    listing %>% select(price, review_scores_accuracy, 
                       review_scores_cleanliness, review_scores_checkin, 
                       review_scores_communication, review_scores_location, 
                       review_scores_rating, review_scores_value) %>% 
                drop_na() %>%  # reduces from 17040 to 9800, is this enough?
                filter(price != 13)


c1 <- cor(q2$price, q2$review_scores_accuracy)
c2 <- cor(q2$price, q2$review_scores_cleanliness)
c3 <- cor(q2$price, q2$review_scores_checkin)
c4 <- cor(q2$price, q2$review_scores_communication)
c5 <- cor(q2$price, q2$review_scores_location)
c6 <- cor(q2$price, q2$review_scores_rating)
c7 <- cor(q2$price, q2$review_scores_value)

c1 <- cor.test(q2$price, q2$review_scores_accuracy)
c2 <- cor.test(q2$price, q2$review_scores_cleanliness)
c3 <- cor.test(q2$price, q2$review_scores_checkin)
c4 <- cor.test(q2$price, q2$review_scores_communication)
c5 <- cor.test(q2$price, q2$review_scores_location)
c6 <- cor.test(q2$price, q2$review_scores_rating)
c7 <- cor.test(q2$price, q2$review_scores_value)

c1$conf.int
c2$conf.int
c3$conf.int
c4$conf.int
c5$conf.int
c6$conf.int
c7$conf.int

set.seed(54321)
q2 %>% 
    ggplot(aes(x = price,
               y = review_scores_rating)) +
    geom_jitter(color = "dodgerblue4", alpha = 0.5) + 
    scale_x_log10(label = scales::number_format(big.mark = ",")) +
    labs(x = "Price",
         y = "Review Scores Rating",
         title = "Price vs Overall Experience (Rating)")


#################### Query 3: Room listing geographical distribution (Room Listing)



#################### Query 4: Who are the top 10 host based on revenue? (Host)



#################### Query 5: Is there any difference in review score between superhost and normal host? (Host)



#################### Query 6: Is there any difference in response rate between superhost and normal host? (Host)


