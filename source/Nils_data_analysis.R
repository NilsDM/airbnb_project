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
# library(OpenStreetMap)
# library(osmdata)
# library(maps)
library(leaflet)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(tidyquant)
library(zoo)


#################### Database Connection
con <- dbConnect(RSQLite::SQLite(), "airdb.SQLite")

#################### Loads tables
host_info <- tbl(con, "host_info") %>% as.data.frame()
listing <- tbl(con, "listing") %>% as.data.frame()


# https://rpubs.com/Argaadya/airbnb_sql_analysis
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
    head(10) %>% knitr::kable(align = c("c", "c"))


#################### Query 2: Is there any correlation between room price and the review score? (Room Listing)

# Extract relevant columns
q2 <- 
    listing %>% select(price, review_scores_accuracy, 
                       review_scores_cleanliness, review_scores_checkin, 
                       review_scores_communication, review_scores_location, 
                       review_scores_rating, review_scores_value) %>% 
                drop_na() %>%  # reduces from 17040 to 9800, is this enough?
                filter(price != 13)

# Function defining correlation plot
c_plot <- function(df, y_val, y_name, clr = "dodgerblue4"){
    c_plot <- df %>% 
        ggplot(aes(x = price,
                   y = y_val)) +
            geom_jitter(color = clr, alpha = 0.5) + 
            scale_x_log10(label = scales::number_format(big.mark = ",")) +
            labs(x = "Price",
                 y = y_name,
                 title = y_name) + theme_tq()
    return(c_plot)
}

# Colours for correlation plot
c <- c("Aquamarine4", "Sienna3")

# Build correlation plots
q2_1 <- c_plot(q2, q2$review_scores_rating, "Rating vs Price") 
q2_2 <- c_plot(q2, q2$review_scores_accuracy, "Accuracy", clr = c[1]) 
q2_3 <- c_plot(q2, q2$review_scores_cleanliness, "Cleanliness", clr = c[1]) 
q2_4 <- c_plot(q2, q2$review_scores_checkin, "Check-in", clr = c[1]) 
q2_5 <- c_plot(q2, q2$review_scores_communication, "Communication", clr = c[2]) 
q2_6 <- c_plot(q2, q2$review_scores_location, "Location", clr = c[2]) 
q2_7 <- c_plot(q2, q2$review_scores_value, "Value", clr = c[2]) 

# Output correlation plots
q2_1
grid.arrange(q2_2, q2_3, q2_4, ncol = 3)
grid.arrange(q2_5, q2_6, q2_7, ncol = 3)

# Conduct numerical correlation test
c1 <- cor.test(q2$price, q2$review_scores_rating)
c2 <- cor.test(q2$price, q2$review_scores_accuracy)
c3 <- cor.test(q2$price, q2$review_scores_cleanliness)
c4 <- cor.test(q2$price, q2$review_scores_checkin)
c5 <- cor.test(q2$price, q2$review_scores_communication)
c6 <- cor.test(q2$price, q2$review_scores_location)
c7 <- cor.test(q2$price, q2$review_scores_value)

# Extract correlation test confidence intervals
c1_int <- paste0(round(c1$conf.int[1], 2), ", ", round(c1$conf.int[2], 2))
c2_int <- paste0(round(c2$conf.int[1], 2), ", ", round(c2$conf.int[2], 2))
c3_int <- paste0(round(c3$conf.int[1], 2), ", ", round(c3$conf.int[2], 2))
c4_int <- paste0(round(c4$conf.int[1], 2), ", ", round(c4$conf.int[2], 2))
c5_int <- paste0(round(c5$conf.int[1], 2), ", ", round(c5$conf.int[2], 2))
c6_int <- paste0(round(c6$conf.int[1], 2), ", ", round(c6$conf.int[2], 2))
c7_int <- paste0(round(c7$conf.int[1], 2), ", ", round(c7$conf.int[2], 2))

# Construct data frame of confidence intervals for correlation plots
confidence_intervals <- c(c1_int, c2_int, c3_int, c4_int, c5_int,
                          c6_int, c7_int)

review_category <- c("Rating", "Accuracy", "Cleanliness", "Check–in", 
                     "Communication", "Location", "Value")

correlation_df <- data.frame(review_category, confidence_intervals)
correlation_df %>% knitr::kable(align = c("c", "c"))


#################### Query 3: Room listing geographical distribution (Room Listing)

# res <- dbSendQuery(mydb,
#                    "SELECT id, name, listing_url, latitude, longitude, price, 
#                    review_scores_rating, number_of_reviews, neighbourhood_cleansed, listing.host_id, host_info.host_name
#                    FROM listing
#                    LEFT JOIN host_info
#                    ON listing.host_id = host_info.host_id
#                    LIMIT 5000"
# )
# 
# out_db <- fetch(res, n = -1)
# dbClearResult(res)

q3 <- listing %>%
    left_join(host_info, by = "host_id") %>%
    select(host_id, host_name, listing_url, latitude, longitude, price,
           review_scores_rating, number_of_reviews, neighbourhood_cleansed) %>%
    replace_na(list(name = "No Name", host_name = "No Host Name"))


popup <- paste0("<b>", q3$name, "</b><br>",
                "Listing ID: ", q3$id, "<br>",
                "Host Name: ", q3$host_name, "<br>",
                "Price: ", q3$price, "<br>",
                "Review Scores Rating: ", ifelse(is.na(q3$review_scores_rating), "No Review Yet", q3$review_scores_rating) , "<br>",
                "Number of Reviews: ", q3$number_of_reviews, "<br>",
                "<a href=", q3$listing_url, "> Click for more info</a>"
                )
leaflet(data = q3) %>% 
    addTiles() %>% 
    addMarkers(lng = ~longitude,
               lat = ~latitude, 
               popup = popup, 
               clusterOptions = markerClusterOptions())
# location <- opq("Bangkok")


#################### Query 4: Who are the top 10 host based on revenue? (Host)
q4 <- listing %>% 
    left_join(host_info, by = "host_id") %>% 
    select(host_id, host_name, price, review_scores_rating, minimum_nights, number_of_reviews) %>%
    mutate(total_earnings = price * review_scores_rating * minimum_nights) %>% 
    drop_na() %>% 
    group_by(host_id, host_name) %>% 
    mutate(number_of_listing = n(),
           average_price = mean(price)) %>% 
    ungroup() %>% 
    select(host_id, host_name, total_earnings, number_of_listing, average_price) %>% 
    arrange(desc(total_earnings))

top_host_by_listing <- 
    q4 %>% 
    arrange(desc(number_of_listing)) %>% 
    select(host_name, number_of_listing) %>% 
    distinct() %>% 
    head(15) %>% 
    ggplot(aes(x = number_of_listing, y = host_name %>% reorder(number_of_listing))) + 
    geom_col(fill = "Skyblue3") +
    labs(
        title    = "Top Host by Number of Listing",
        x        = "Number of Listing",
        y        = "Host Name"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold")) 
    
top_host_by_earning <- 
    q4 %>% 
    select(host_name, total_earnings) %>%
    arrange(desc(total_earnings)) %>% 
    filter(total_earnings != 16242500) %>% 
    head(15) %>% 
    ggplot(aes(x = total_earnings, y = host_name %>% reorder(total_earnings))) + 
    geom_col(fill = "Aquamarine4") + 
    scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
    labs(
        title    = "Top Host by Number of Listing",
        x        = "Total Eearning (in Baht)",
        y        = "Host Name"
    ) + 
    theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold")) 

grid.arrange(top_host_by_listing, top_host_by_earning, ncol = 2)


#################### Query 5: Is there any difference in review score between superhost and normal host? (Host)
q5 <- listing %>% 
    left_join(host_info, by = "host_id") %>% 
    select(host_id, host_name, review_scores_rating, host_is_superhost) %>% 
    drop_na() %>% 
    mutate(host_is_superhost = as.logical(host_is_superhost)) %>% 
    select(review_scores_rating, host_is_superhost)

q5_1 <- 
    q5[q5$host_is_superhost == FALSE, ] %>% 
    ggplot(aes(y = review_scores_rating, group = host_is_superhost)) + 
    geom_boxplot(fill = "Skyblue3") + 
    labs(
        title    = "Host Ratings",
        subtitle = "Ratings Distribution",
        x        = "Host",
        y        = "Rating"
    ) + theme_tq()  + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold")) 

q5_2 <- 
    q5[q5$host_is_superhost == TRUE, ] %>% 
    ggplot(aes(y = review_scores_rating, group = host_is_superhost)) + 
    geom_boxplot(fill = "Aquamarine3") + 
    labs(
        title    = "Superhost Ratings",
        subtitle = "Ratings Distribution",
        x        = "Superhost",
        y        = "Rating"
    ) + theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold")) 

grid.arrange(q5_1, q5_2, ncol = 2)

host <-  q5[q5$host_is_superhost == FALSE, ]
super_host <-  q5[q5$host_is_superhost == TRUE, ]

t.test(host$review_scores_rating, super_host$review_scores_rating)
#################### Query 6: Is there any difference in response rate between superhost and normal host? (Host)
res <- dbSendQuery(mydb,
                   "SELECT host_id, host_name, host_response_rate, host_acceptance_rate, host_is_superhost
                   FROM host_info
                   WHERE host_response_rate IS NOT NULL and host_acceptance_rate IS NOT NULL"
)

q6 <- listing %>% 
    left_join(host_info, by = "host_id") %>% 
    select(host_id, host_name, host_response_rate, host_acceptance_rate, host_is_superhost) %>%
    drop_na() %>% 
    mutate(host_is_superhost = as.logical(host_is_superhost), 
           # Transform acceptance rate and response rate
           host_response_rate = host_response_rate %>% 
               str_remove("[%]") %>% 
               as.numeric(),
           host_acceptance_rate = host_acceptance_rate %>% 
               str_remove("[%]") %>% 
               as.numeric()
    )


q6_1 <- 
    q6[q6$host_is_superhost == FALSE, ] %>% 
    ggplot(aes(y = host_response_rate, group = host_is_superhost)) + 
    geom_boxplot(fill = "Skyblue3") + 
    labs(
        title    = "Host Response Rate",
        subtitle = "Ratings Distribution",
        x        = "Host",
        y        = "Rating"
    ) + theme_tq()  + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold")) 

q6_2 <- 
    q6[q6$host_is_superhost == TRUE, ] %>% 
    ggplot(aes(y = host_response_rate, group = host_is_superhost)) + 
    geom_boxplot(fill = "Aquamarine3") + 
    labs(
        title    = "Superhost Response Rate",
        subtitle = "Ratings Distribution",
        x        = "Superhost",
        y        = "Rating"
    ) + theme_tq() + 
    theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold")) 

grid.arrange(q6_1, q6_2, ncol = 2)

host_2 <-  q6[q6$host_is_superhost == FALSE, ]
super_host_2 <-  q6[q6$host_is_superhost == TRUE, ]

t.test(host_2$host_response_rate, super_host_2$host_response_rate)



