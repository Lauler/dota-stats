library(readr)
library(ggplot2)

# Reads functions 
source("C:\\Users\\Faton\\Desktop\\R-ovning\\Game data mining\\Dota2\\dota stats\\dota_functions.R")

path <- "C:\\Users\\Faton\\Desktop\\R-ovning\\Game data mining\\Dota2\\data\\dota"

# Connect db
con <- connect_db()

# Get matchinfo
matchinfo <- download_matchinfo(dbcon = con, start_row = 1, nr_matches = 150)
head(matchinfo)
matchinfo$matchid

### ONLY USE THIS IF YOU ARE DOWNLOADING DATA FOR THE FIRST TIME ###
# First time reading in data, old_json_df and old_lane_data_df need to be NULL.
# This function saves the results to path as an .rdata file.
dota <- add_new_data(dbcon = con, matchids = matchinfo$matchid[1:5], old_json_df = NULL,
                     old_lane_data_df = NULL, path = path)

### Use this if you already have data on your harddrive ###
dota <- read_updated_data(path)
dota$lanes_data_df

# Updates any matchids which have not yet been processed, saves them to your drive,
# then reads the saved file and returns the result as output.
dota <- add_new_data(dbcon = con, matchids = matchinfo$matchid[1470:1497], old_json_df = dota$json_df,
                     old_lane_data_df = dota$lanes_data_df, path = path)


dota$lanes_data_df

ggplot(dota$lanes_data_df, aes(x = as.numeric(solo_rating))) +
  geom_histogram()


dota <- read_updated_data(path, output = "lane")


############

jso <- download_json(dbcon = con, matchids = 3134802295)
oview <- download_json(dbcon = con, matchids = 3134802295)

test <- join_herorank(dbcon = con, matchids = 3134802295)

con <- connect_db()

hero <- join_herorank(dbcon = con, matchids = 3134802295)


