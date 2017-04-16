library(RPostgreSQL)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)


connect_db <- function(){
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # Use "con" later in each connection to the database
  
  con <- dbConnect(drv, dbname = "dotaliu",
                   host = "dotaliu.cpuvi9hzbwlb.eu-west-1.rds.amazonaws.com", port = 5432,
                   user = "dotaliu", password = readline(prompt = "Enter password for dotaliu db: "))
  
  return(con)
}

describe_db <- function(){
  tables <- dbListTables(con)
  for (tble in tables){
    cat("Table: ", tble, "\n",
        "fields:", dbListFields(con, tble), "\n",
        "---------------------------------------", "\n")
  }
}


download_matchinfo <- function(dbcon, start_row, nr_matches){
  dbmatch <- dbSendQuery(dbcon, paste0("SELECT * FROM match limit ", nr_matches, 
                                       " offset ", start_row))
  match <- dbFetch(dbmatch)
  return(match)
}

download_json <- function(dbcon, matchids, partial_df=NULL){

  # remove matchids which are already in partial_df
  if (!is.null(partial_df)){
    matchids <- subset(matchids, !matchids %in% unique(partial_df$matchid))
  }
  
  if (length(matchids) == 0) {
    stop("All the supplied matchids have already been processed.")
  }
  
  # download from db and parse json column
  matchids <- paste0("'", matchids, "'", collapse = ", ")
  json_data <- dbSendQuery(dbcon, paste0("SELECT * FROM overview WHERE matchid IN (", matchids, ")"))
  json_df <- dbFetch(json_data)
  json_df$opendotajsondata <- lapply(json_df[, "opendotajsondata"], FUN = function(x) fromJSON(x))
  json_df$processed <- FALSE
  
  if (!is.null(partial_df)){
    json_df <- rbind(partial_df, json_df)
  }
  
  return(json_df)
}


clean_lane_info <- function(jsonmatch, lane_pos_list, player_nr){
  
  # Create a list of lane position first 10 minutes of game
  lane_pos_long <- as.data.frame(gather(lane_pos_list[player_nr, ]))
  
  lane_pos <- lane_pos_long %>%
    filter(!is.na(value)) # remove NA
  
  lane_pos <- lane_pos %>%
    cbind(data.frame(str_split_fixed(string = lane_pos$key,
                                     pattern = "\\.", n = 2))) %>% # split string into 2 columns
    select(x = X1, y = X2, value) %>%
    mutate(x = as.numeric(as.character(x)),
           y = as.numeric(as.character(y))) %>% # convert to numeric
    as_data_frame()
  
  return(lane_pos)
}

get_lane_info <- function(json_df){
  
  uncleaned_json <- json_df[json_df$processed == FALSE, ]
  lane_data_match <- list()
  lane_data_all <- list()
  
  for (match in 1:nrow(uncleaned_json)){
    jsonmatch <- uncleaned_json$opendotajsondata[[match]]
    
    # sanity check to see if opendota has parsed the replay
    if ( all(is.na(jsonmatch$players$lane_pos)) ) next 
    lane_pos_list <- flatten(jsonmatch$players$lane_pos)
    
    for (player_nr in 1:10){
      lane_pos <- clean_lane_info(jsonmatch, lane_pos_list, player_nr)
      
      # Calculate weighted mid point of the lane position. Greater weight if position has been 
      # visited more. Also calculate distance of each point to mid points and median points.
      
      lane_pos <- lane_pos %>%
        mutate(mean_x_pos = weighted.mean(x, value),
               mean_y_pos = weighted.mean(y, value),
               mean_dist = sqrt((x - mean_x_pos)^2 + (y - mean_y_pos)^2),
               avg_mean_dist = weighted.mean(mean_dist, value),
               median_x_pos = median(x),
               median_y_pos = median(y),
               median_dist = sqrt((x - median_x_pos)^2 + (y - median_y_pos)^2),
               avg_median_dist = weighted.mean(median_dist, value))
      
      player_lane_data <- data_frame(matchid = jsonmatch$match_id,
                                     accountid = jsonmatch$players$account_id[player_nr],
                                     slot = jsonmatch$players$player_slot[player_nr],
                                     hero_id = jsonmatch$players$hero_id[player_nr],
                                     solo_rating = jsonmatch$players$solo_competitive_rank[player_nr],
                                     mean_x_pos = lane_pos$mean_x_pos[1],
                                     mean_y_pos = lane_pos$mean_y_pos[1],
                                     avg_distance_travelled = lane_pos$avg_mean_dist[1],
                                     median_x_pos = lane_pos$median_x_pos[1],
                                     median_y_pos = lane_pos$median_y_pos[1],
                                     avg_median_dist = lane_pos$avg_median_dist[1],
                                     pos_dataframe = lane_pos %>% select(x, y, value) %>% list())
      
      lane_data_match[[player_nr]] <- player_lane_data
      
    }
    
    lane_data_all[[match]] <- do.call(rbind, lane_data_match)
  }
  
  return(do.call(rbind, lane_data_all))
}

read_updated_data <- function(path){
  dota <- read_rds(path)
  
  json_df <- as_data_frame(dota[1]) # Access jsons (don't print this)
  json_df <- json_df %>%
    bind_cols(dota[2], dota[3])
  
  lanes_data_df <- as_data_frame(do.call(cbind, dota[4:(length(dota)-1)]))
  lanes_data_df <- lanes_data_df %>%
    bind_cols(dota[length(dota)])
  
  return(list(json_df = json_df, lanes_data_df = lanes_data_df))
}

add_new_data <- function(dbcon, matchids, old_json_df, old_lane_data_df, path){
  # Downloads jsons from matchids which are not already in partial_df
  json_df <- download_json(dbcon, matchids, old_json_df)
  
  new_lane_data <- get_lane_info(json_df)
  json_df$processed <- TRUE
  
  updated_df <- rbind(old_lane_data_df, new_lane_data)
  saveRDS(object = c(json_df, updated_df), file = path)
  
  jsons_and_lanes_df <- read_updated_data(path)
  
  return(jsons_and_lanes_df)
}

