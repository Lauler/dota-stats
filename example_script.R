library(readr)
library(anytime)
library(RCurl)

# Reads functions 
source("C:\\Users\\Faton\\Desktop\\R-ovning\\Game data mining\\Dota2\\dota stats\\dota_functions.R")

path <- "F:\\dota_data\\storage\\"

# Connect db
con <- connect_db()

# Get matchinfo
matchinfo <- download_matchinfo(dbcon = con, start_row = 1, nr_matches = 10000)
minfo <- as_data_frame(matchinfo)

minfo$time <- anytime(matchinfo$matchdatetime)

sum(minfo$time > "2017-04-09 22:07:58 CEST")

### Camera and action data mining
dota <- join_camera_latency(con, dota, minfo$matchid[6300:6500], path)
a <- dota %>%
  select(matchid, slot, nr_camera_changes, action_latency_median, action_latency_90th_percentile) %>%
  filter(!is.na(nr_camera_changes))


### ONLY USE THIS IF YOU ARE DOWNLOADING DATA FOR THE FIRST TIME ###
# First time reading in data, old_json_df and old_lane_data_df need to be NULL.
# The function saves the results to path as an .rdata file.
# NULL parameters overwrite old data.
dota <- add_new_data(dbcon = con, matchids = matchinfo$matchid[1:30], old_json_df = NULL,
                     old_df = NULL, path = path)

### Use this if you already have data on your harddrive ###
dota <- read_updated_data(path, output = "both")

# Updates any matchids which have not yet been processed, saves them to your drive,
# The save appends new data to the old data.
add_new_data(dbcon = con, matchids = matchinfo$matchid[1:100], old_json_df = dota$json_df,
             old_df = dota$dota_df, path = path)


### For starting new jsonchunks (when saved jsons become too big to fit in memory) ###
add_new_data(dbcon = con, matchids = matchinfo$matchid[4901:4920], old_json_df = NULL,
             old_df = dota$dota_df, path = path)


### Save and add data in chunks of 100.
for (i in 61:62){
  add_new_data(dbcon = con, matchids = matchinfo$matchid[(i*100+1):(i*100+100)], old_json_df = dota$json_df,
               old_df = dota$dota_df, path = path)
  
  dota <- read_updated_data(path, output = "both")
}

################################################
################################################

dota <- read_rds(paste0(path, "dota.rdata"))
# dota$solo_rating <- as.numeric(dota$solo_rating.x)
# dota$nr_camera_changes <- NA_integer_
# dota$action_latency_mean <- NA_integer_
# dota$action_latency_10th_percentile <- NA_integer_
# dota$action_latency_1st_quartile <- NA_integer_
# dota$action_latency_median <- NA_integer_
# dota$action_latency_3rd_quartile <- NA_integer_
# dota$action_latency_90th_percentile <- NA_integer_

json_df <- read_rds(paste0(path, "dota_json_2000-4000.rdata"))

dota$start_time <- anytime(as.numeric(dota$start_time))
dota <- dota[dota$start_time > "2017-04-10 00:00:00 CEST", ]
parsed <- json_df[json_df$matchid %in% unique(dota$matchid),]

dl_replays <- function(parsed){
  
  for (i in 601:700) {
    tryCatch( 
    expr = {
      url <- parsed$opendotajsondata[[i]]$replay_url
        
      download.file(c(parsed$opendotajsondata[[i]]$replay_url,parsed$opendotajsondata[[3]]$replay_url),
                    destfile = paste0("F:\\dota_data\\replays\\", basename(url)),
                    mode = "wb")
      
    }, 
    error=function(e) {
      cat("Download error:", conditionMessage(e), "\n")
    }
  )
  }
}

dl_replays(parsed)


#####


