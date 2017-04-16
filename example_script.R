library(readr)

# Reads functions 
source("C:\\Users\\Faton\\Desktop\\R-ovning\\Game data mining\\Dota2\\dota stats\\dota_functions.R")

path <- "C:\\Users\\Faton\\Desktop\\R-ovning\\Game data mining\\Dota2\\data\\dota.Rdata"

# Connect db
con <- connect_db()

# Get matchinfo
matchinfo <- download_matchinfo(dbcon = con, start_row = 1, nr_matches = 1500)
head(matchinfo)

### ONLY USE THIS IF YOU ARE DOWNLOADING DATA FOR THE FIRST TIME ###
# First time reading in data, old_json_df and old_lane_data_df need to be NULL.
# This function saves the results to path as an .rdata file.
dota <- add_new_data(dbcon = con, matchids = matchinfo$matchid[1:5], old_json_df = NULL,
                     old_lane_data_df = NULL, path = path)

### Use this if you already have data on your harddrive ###
dota <- read_rds(path) 


# Updates any matchids which have not yet been processed, saves them to your drive,
# then reads the saved file and returns the result as output.
dota <- add_new_data(dbcon = con, matchids = matchinfo$matchid[370:470], old_json_df = dota$json_df,
                     old_lane_data_df = dota$lanes_data_df, path = path)


dota$lanes_data_df

