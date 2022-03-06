# Nils' Data Analysis Queries

# Load libraries and import functions (Always run) 
source("source/db_prep.R")


#################### Database Connection
con <- dbConnect(RSQLite::SQLite(), "airdb.SQLite")


#################### Build database
# Needs to be run only when initializing database
# If database already exists and needs to be removed, set: remove_old_database = TRUE
build_airbnb_database(con, listing_data, remove_old_database = FALSE)


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



#################### Query 1: How is the number of host joined to Airbnb over time? (Room Listing)



#################### Query 2: Is there any correlation between room price and the review score? (Room Listing)



#################### Query 3: Room listing geographical distribution (Room Listing)



#################### Query 4: Who are the top 10 host based on revenue? (Host)



#################### Query 5: Is there any difference in review score between superhost and normal host? (Host)



#################### Query 6: Is there any difference in response rate between superhost and normal host? (Host)


