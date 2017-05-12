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

### ONLY USE THIS IF YOU ARE DOWNLOADING DATA FOR THE FIRST TIME ###
# First time reading in data, old_json_df and old_lane_data_df need to be NULL.
# The function saves the results to path as an .rdata file.
# NULL parameters overwrite old data.
dota <- add_new_data(dbcon = con, matchids = matchinfo$matchid[1:20], old_json_df = NULL,
                     old_df = NULL, path = path)

### Use this if you already have data on your harddrive ###
dota <- read_updated_data(path, output = "both")

# Updates any matchids which have not yet been processed, saves them to your drive,
# The save appends new data to the old data.
add_new_data(dbcon = con, matchids = matchinfo$matchid[8030:8154], old_json_df = dota$json_df,
             old_df = dota$dota_df, path = path)


### For starting new jsonchunks (when saved jsons become too big to fit in memory) ###
add_new_data(dbcon = con, matchids = matchinfo$matchid[8000:8030], old_json_df = NULL,
             old_df = dota$dota_df, path = path)


### Save and add data in chunks of 100.
for (i in 80:82){
  add_new_data(dbcon = con, matchids = matchinfo$matchid[(i*100):(i*100+100)], old_json_df = dota$json_df,
               old_df = dota$dota_df, path = path)
  
  dota <- read_updated_data(path, output = "both")
}

################################################
################################################

dota <- read_rds(paste0(path, "dota_df.rdata"))
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
describe_db()
unique(dota$matchid)

camera <- download_playerentry(dbcon = con, matchids =3155961006)
action <- download_action(dbcon = con, matchids = 3155961006)

test <- camera %>%
  arrange(slot, tick) %>%
  filter(health > 0) %>%
  group_by(slot) %>%
  mutate(deltax = abs(cameracellx - lag(cameracellx)),
         deltatick = tick - lag(tick))

test %>% 
  filter(deltax > 10)

dota[dota$matchid == 3157307576,]

lowranks <- dota[!is.na(dota$solo_rating.x) & dota$solo_rating.x < 3000, ] %>%
  arrange(-matchid)

highranks <- dota[!is.na(dota$solo_rating.x) & dota$solo_rating.x > 4000, ] %>%
  arrange(-matchid)

print(lowranks, n=100)
print(highranks, n=100)

action %>%
  arrange(tick)

mids$matchid <- as.numeric(mids$match_id)
dota$matchid %in% mids$match_id

mids %>%
  left_join(dota, by = c("matchid", "slot")) %>%
  as_data_frame()
