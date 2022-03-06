# Jose's Data Analysis Queries

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



#################### Query 1: What is the top and bottom 10 property type based on average price? (Room Listing)
 


#################### Query 2: What is the top and bottom 10 property type based on review score? (Room Listing)



#################### Query 3: What is the most common amenities provided? (Room Listing)



#################### Query 4: What is the most commonly verified host information? (Host)



#################### Query 5: What is the most common room type available? (Room Listing)



