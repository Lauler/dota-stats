x
y <- c(x, "4091842")

y[!(y %in% x)]

subset(y, !y %in% x)

connect_db <- function(){
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # Use "con" later in each connection to the database
  
  con <- dbConnect(drv, dbname = "dotaliu",
                   host = "dotaliu.cpuvi9hzbwlb.eu-west-1.rds.amazonaws.com", port = 5432,
                   user = "dotaliu", password = readline(prompt = "Enter password for dotaliu db: "))
  
  return(con)
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
    matchids <- subset(matchids, !matchids %in% partial_df$matchid)
  }
  
  # download from db and parse json column
  matchids <- paste0("'", matchids, "'", collapse = ", ")
  json_data <- dbSendQuery(dbcon, paste0("SELECT * FROM overview WHERE matchid IN (", matchids, ")"))
  json_df <- dbFetch(json_data)
  json_df$opendotajsondata <- lapply(json_df[, "opendotajsondata"], FUN = function(x) fromJSON(x))
  
  if (!is.null(partial_df)){
    json_df <- rbind(partial_df, json_df)
  }
  
  return(json_df)
}


get_lane_info <- function(lane_pos_list, players){
  # Create a list of lane position first 10 minutes of game
  hero_lane_pos <- flatten(lane_pos_list[player_nr,])
  lane_pos_long <- as.data.frame(gather(hero_lane_pos))
  
  lane_pos <- lane_pos_long %>%
    filter(!is.na(value)) # remove NA
  
  lane_pos <- lane_pos %>%
    cbind(data.frame(str_split_fixed(string = lane_pos$key,
                                     pattern = "\\.", n = 2))) %>% # split string into 2 columns
    select(x = X1, y = X2, value) %>%
    mutate(x = as.numeric(as.character(x)),
           y = as.numeric(as.character(y))) %>% # convert to numeric
    as_data_frame()
  
  # Calculate weighted mid point of the lane position
  # greater weight if position has been visited more
  # Also calculate distance of each point to weighted mid point
  lane_pos <- lane_pos %>%
    mutate(mean_x_pos = weighted.mean(x, value),
           mean_y_pos = weighted.mean(y, value),
           med_dist = sqrt((x - mean_x_pos)^2 + (y - mean_y_pos)^2),
           avg_dist = weighted.mean(med_dist, value))
  
  return(lane_pos)
}