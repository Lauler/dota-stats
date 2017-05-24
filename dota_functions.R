library(RPostgreSQL)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)


connect_db <- function(){
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # Use "con" later in each connection to the database
  
  con <- dbConnect(drv, dbname = "dotaliu_v2",
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
  match <- dbGetQuery(dbcon, paste0("SELECT * FROM match limit ", nr_matches, 
                                      " offset ", start_row))
  
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
  json_df$processed_pos <- FALSE
  json_df$processed_vars <- FALSE
  
  if (!is.null(partial_df)){
    json_df <- rbind(partial_df, json_df)
  }
  
  return(json_df)
}

download_heroentry <- function(dbcon, matchids){
  
  matchids <- paste0("'", matchids, "'", collapse = ", ")
  hero_df <- dbGetQuery(dbcon, paste0("SELECT matchid, tick, slot, herocellx, herocelly, health 
                                      FROM heroentry WHERE matchid IN (", matchids, ")"))
  
  return(as_data_frame(hero_df))
}

download_action <- function(dbcon, matchids){
  
  matchids <- paste0("'", matchids, "'", collapse = ", ")
  action_df <- dbGetQuery(dbcon, paste0("SELECT matchid, tick, slot, type, key 
                                        FROM action WHERE matchid IN (", matchids, ")"))
  
  return(as_data_frame(action_df))
}

download_playerentry <- function(dbcon, matchids){
  
  matchids <- paste0("'", matchids, "'", collapse = ", ")
  player_df <- dbGetQuery(dbcon, paste0("SELECT matchid, tick, slot, type, cameracellx, cameracelly, health 
                                        FROM playerentry WHERE matchid IN (", matchids, ")"))
  
  return(as_data_frame(player_df))
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
  
  uncleaned_json <- json_df[json_df$processed_pos == FALSE, ]
  lane_data_match <- list()
  lane_data_all <- list()
  
  for (match in 1:nrow(uncleaned_json)){
    jsonmatch <- uncleaned_json$opendotajsondata[[match]]
    
    # sanity check to see if opendota has parsed the replay
    if ( all(is.na(jsonmatch$players$lane_pos)) ) next 
    if (jsonmatch$duration < 600) next
    
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
    lane_data_all[[match]]$slot[6:10] <- lane_data_all[[match]]$slot[6:10] - 123
    
  }
  
  return(do.call(rbind, lane_data_all))
}


get_camera_changes <- function(camera_df){
  # supply camera_df from download_playerentry()
  
  changes <- camera_df %>%
    arrange(slot, tick) %>%
    filter(health > 0) %>%
    group_by(slot) %>%
    mutate(deltax = abs(cameracellx - lag(cameracellx)),
           deltay = abs(cameracelly - lag(cameracelly)),
           deltatick = tick - lag(tick)) %>%
    filter((deltax >= 10 | deltay >= 10)) %>%
    mutate(dtick = tick - lag(tick)) %>% # deltadeltatick
    filter(dtick >= 4 & deltatick <= 12) %>% 
    select(matchid, tick, slot, type)
  
  
  changes_tally <- changes %>%
    tally() %>%
    rename(nr_camera_changes = n)
  
  changes_tally$matchid <- unique(camera_df$matchid)
  
  cam_lst <- list(cam_changes = changes,
                  tally = changes_tally)
  
  return(cam_lst)
}


get_action_latency <- function(action_df, cam_changes_df){
  # action_df from download_action
  # cam_changes_df from get_camera_changes()
  
  latency <- action_df %>%
    full_join(cam_changes_df, by = c("matchid","slot", "tick", "type")) %>%
    arrange(slot, tick) %>%
    group_by(slot) %>%
    mutate(action_latency = ifelse(type == "interval" & lead(type) == "action", yes = lead(tick) - tick, no = NA)) %>%
    summarise(action_latency_mean = mean(action_latency, na.rm=TRUE),
              action_latency_10th_percentile = quantile(action_latency, 0.10, na.rm = TRUE),
              action_latency_1st_quartile = quantile(action_latency, 0.25, na.rm = TRUE),
              action_latency_median = median(action_latency, na.rm = TRUE),
              action_latency_3rd_quartile = quantile(action_latency, 0.75, na.rm = TRUE),
              action_latency_90th_percentile = quantile(action_latency, 0.90, na.rm = TRUE))
  
  latency$matchid <- unique(action_df$matchid)
  
  return(latency)
}


read_updated_data <- function(path, output = "both"){
  json_df <- read_rds(paste0(path, "dota_json.rdata"))
  dota_df <- read_rds(paste0(path, "dota_df.rdata"))
  
  if (output == "both") {
    return(list(json_df = json_df, dota_df = dota_df))
  } 
  else if (output == "dota") {
    return(dota_df)
  }
  else if (output == "json"){
    return(json_df)
  }
}

add_new_data <- function(dbcon, matchids, old_json_df, old_df, path){
  # Downloads jsons from matchids which are not already in partial_df
  json_df <- download_json(dbcon, matchids, old_json_df)
  
  new_lane_data <- get_lane_info(json_df)
  new_df <- join_jsonvars(json_df)
  json_df$processed_pos <- TRUE
  json_df$processed_vars <- TRUE
  
  updated_df <- new_df %>%
    left_join(new_lane_data, by = c("matchid", "slot"))
  
  if (!is.null(old_df)){
    updated_df <- old_df %>%
      bind_rows(updated_df)
  }
  
  saveRDS(object = json_df, file = paste0(path, "dota_json.rdata"))
  saveRDS(object = updated_df, file = paste0(path, "dota_df.rdata"))
  
  print("Data added. Read with function read_updated_data().")
}

join_herorank <- function(dbcon, matchids) {
  # Needs partial_df
  
  json_df <- download_json(dbcon, matchids) # download jsons
  hero_df <- download_heroentry(dbcon, matchids) # download hero pos
  ranks_list <- list()
  
  for (match in 1:length(json_df[["matchid"]])) {
    rank_df <- data.frame(solo_rank = json_df$opendotajsondata[[match]]$players$solo_competitive_rank, 
                          slot = json_df$opendotajsondata[[match]]$players$player_slot,
                          matchid = json_df$matchid[[match]])
    
    rank_df$slot[6:10] <- rank_df$slot[6:10] - 123 # transform slots 128:133 to 5:9
    ranks_list[[match]] <- rank_df
  }
  
  rank_df <- do.call(rbind, ranks_list)
  
  combined_df <- hero_df %>%
    left_join(rank_df, by = c("matchid", "slot"))

  return(combined_df)
}


join_camera_latency <- function(dbcon, dota_df, matchids, path){
  # supply dota_df from read_updated_data()
  i <- 1
  
  for (matchid in matchids){
    camera <- download_playerentry(dbcon, matchids = matchid)
    action <- download_action(dbcon, matchids = matchid)
    
    if (nrow(camera) == 0 | nrow(action) == 0) next
    
    cam_changes <- get_camera_changes(camera)
    latency <- get_action_latency(action, cam_changes$cam_changes)
    
    dota_df <- dota_df %>% 
      mutate(matchid = as.character(matchid)) %>%
      left_join(cam_changes$tally, by = c("matchid", "slot")) %>%
      mutate(nr_camera_changes = coalesce(as.integer(nr_camera_changes.x), nr_camera_changes.y)) %>%
      left_join(latency, by = c("matchid", "slot"))  %>%
      mutate(action_latency_mean = coalesce(as.numeric(action_latency_mean.x), 
                                            as.numeric(action_latency_mean.y)),
             action_latency_10th_percentile = coalesce(as.numeric(action_latency_10th_percentile.x), 
                                                       as.numeric(action_latency_10th_percentile.y)),
             action_latency_1st_quartile = coalesce(as.numeric(action_latency_1st_quartile.x), 
                                                    as.numeric(action_latency_1st_quartile.y)),
             action_latency_median = coalesce(as.numeric(action_latency_median.x), 
                                              as.numeric(action_latency_median.y)),
             action_latency_3rd_quartile = coalesce(as.numeric(action_latency_3rd_quartile.x), 
                                                    as.numeric(action_latency_3rd_quartile.y)),
             action_latency_90th_percentile = coalesce(as.numeric(action_latency_90th_percentile.x), 
                                                       as.numeric(action_latency_90th_percentile.y))) %>%
      select(-ends_with(".y"), -ends_with(".x"))

    print(paste("Match", i, "of", length(matchids), "done."))
    i <- i + 1
    
    if (i %% 100 == 0){
      saveRDS(dota_df, paste0(path, "dota.rdata"))
    }
    
  }
  
  
  saveRDS(dota_df, paste0(path, "dota.rdata"))
  return(dota_df)
}

join_jsonvars <- function(json_df){
  
  uncleaned_json <- json_df[json_df$processed_var == FALSE, ]
  
  match_list <- list()
  match_nr <- 1
  
  for (match in uncleaned_json$opendotajsondata){
    
    # Check whether replay was parsed by opendota
    if ( all(is.na(match$players$lane_pos)) ) next 
    if (match$duration < 600) next
    
    matchid <- match$players$match_id
    start_time <- match$players$start_time # unix time in seconds
    region <- match$players$region # https://github.com/odota/ui/blob/master/src/lang/en-US.json#L861
    lobby_type <- match$players$lobby_type # https://github.com/odota/ui/blob/master/src/lang/en-US.json#L404
    accountid <- match$players$account_id
    slot <- match$players$player_slot
    slot[6:10] <- slot[6:10] - 123
    is_radiant <- match$players$isRadiant
    match_win <- match$players$win
    solo_rating <- match$players$solo_competitive_rank
    heroid <- match$players$hero_id
    leaver_status <- factor(match$players$leaver_status, 
                            levels = 0:6,
                            labels = c("None", "Left safely", "Abandoned (DC)",
                                       "Abandoned", "Abandoned (AFK)", "Never connected",
                                       "Never connected (timeout)"))
    abandons <- match$players$abandons
    duration <- match$players$duration/60 # minutes
    
    # 1: bottom, 2:mid, 3: top, 4: radiant jungle, 5: dire jungle
    lane <- match$players$lane
    lane <- factor(lane, levels = 1:5, labels = c("bot", "mid", "top", "radiant jungle", "dire jungle"))
     
    # 0: Unknown, 1: Safe, 2: Mid, 3: Off, 4: Jungle
    lane_role <- match$players$lane_role
    lane_role <- factor(lane_role, levels = 0:4, labels = c("unknown", "safe", "mid", "off", "jungle"))
    
    radiant_score <- rep(match$radiant_score, 5)
    dire_score <- rep(match$dire_score, 5)
    team_score <- c(radiant_score, dire_score)
    team_score_delta <- c(radiant_score - dire_score, dire_score - radiant_score)
    
    tower_kills <- parse_objectives(match)
    
    team_firstblood <- tower_kills$team_firstblood
    team_first_tower_kill <- tower_kills$team_first_tower_kill
    team_first_roshan <- tower_kills$team_first_roshan
    team_towerkills_at_10 <- tower_kills$towerkills_10$team_towerkills
    team_towerkills_at_10_delta <- tower_kills$towerkills_10$team_towerkills_delta
    team_towerkills_at_10_adj <- tower_kills$towerkills_10$team_towerkills_adj
    team_towerkills_at_10_delta_adj <- tower_kills$towerkills_10$team_towerkills_delta_adj
    player_towerkills <- match$players$tower_kills
    player_tower_damage <- match$players$tower_damage
    courier_kills <- match$players$courier_kills
    
    kills_per_min <- match$players$kills/duration
    deaths_per_min <- match$players$deaths/duration
    assists_per_min <- match$players$assists/duration
    neutral_kills_per_min <- match$players$neutral_kills/duration
    actions_per_min <- match$players$actions_per_min
    hero_dmg_pm <- match$players$hero_damage/duration
    hero_healing_pm <- match$players$hero_healing/duration
    buybacks <- match$players$buyback_count
    team_gold_adv_at_5 <- c(rep(match$radiant_gold_adv[5], 5), -rep(match$radiant_gold_adv[5], 5))
    team_gold_adv_at_10 <- c(rep(match$radiant_gold_adv[10], 5), -rep(match$radiant_gold_adv[10], 5))
    team_xp_adv_at_5 <- c(rep(match$radiant_xp_adv[5], 5), -rep(match$radiant_xp_adv[5], 5))
    team_xp_adv_at_10 <- c(rep(match$radiant_xp_adv[10], 5), -rep(match$radiant_xp_adv[10], 5))
    denies_at_5 <- do.call(rbind, match$players$dn_t)[,5]
    denies_at_10 <- do.call(rbind, match$players$dn_t)[,10]
    lasthits_at_5 <- do.call(rbind, match$players$dn_t)[,5]
    lasthits_at_10 <- do.call(rbind, match$players$dn_t)[,10]
    gpm_at_end <- match$players$gold_per_min
    gpm_at_5 <- do.call(rbind, match$players$gold_t)[,5]/5
    gpm_at_10 <- do.call(rbind, match$players$gold_t)[,10]/10
    xpm_at_end <- match$players$xp_per_min
    xpm_at_5 <- do.call(rbind, match$players$xp_t)[,5]/5
    xpm_at_10 <- do.call(rbind, match$players$xp_t)[,10]/10
    obswards_placed_pm <- match$players$obs_placed/duration
    sentwards_placed_pm <- match$players$sen_placed/duration
    rune_pickups_pm <- match$players$rune_pickups/duration
    camps_stacked <- match$players$camps_stacked
    
    # Check who picked up the first bounty runes spawning at 00:00
    first_bounty_pickup <- rep(FALSE, 10)
    j <- 1
    for (player in match$players$runes_log){
      first_bounty_pickup[j] <- any(player$time < 60)
      j <- j + 1
    }
    
    # Non bounty runes picked up at 15 minutes
    regular_rune_pickup_at_15 <- rep(0, 10)
    j <- 1
    for (player in match$players$runes_log){
      runes <- nrow(player[player$time < 900 & player$key != 5, ])
      
      if (!is.null(runes)){
        regular_rune_pickup_at_15[j] <- runes
      }
      
      j <- j + 1
    }
    
    pings_pm <- match$players$pings/duration
    bottle_purchased <- match$players$purchase$bottle
    gem_purchased <- match$players$purchase$gem
    infused_rdrop_purchased <- match$players$purchase$infished_raindrop
    faerie_fires_purchased <- match$players$purchase$faerie_fire
    smokes_purchased_pm <- match$players$purchase$smoke_of_deceit/duration
    tpscroll_purchased_pm <- match$players$purchase$tpscroll/duration
    dust_purchased_pm <- match$players$purchase$dust/duration
    travel_boots_uses <- match$players$item_uses$travel_boots
    travel_boots_minutes <- duration - match$players$first_purchase_time$travel_boots/60
    travel_boots_uses_pm <- travel_boots_uses/travel_boots_minutes
    phase_boots_uses <- match$players$item_uses$phase_boots
    phase_boots_minutes <- duration - match$players$first_purchase_time$phase_boots/60
    phase_boots_uses_pm <- phase_boots_uses/phase_boots_minutes
    power_treads_uses <- match$players$item_uses$power_treads
    power_treads_minutes <- duration - match$players$first_purchase_time$power_treads/60
    power_treads_uses_pm <- power_treads_uses/power_treads_minutes
    blink_uses <- match$players$item_uses$blink
    blink_minutes <- duration - match$players$first_purchase_time$blink/60
    blink_pm <- blink_uses/blink_minutes
    
    wand_uses <- match$players$item_uses$magic_wand
    stick_uses <- match$players$item_uses$magic_stick
    # Stick is always registered as bought before wand
    stick_minutes <- duration - match$players$first_purchase_time$magic_stick/60 
    wandstick_uses_pm <- (wand_uses + stick_uses)/stick_minutes
    force_staff_uses <- match$players$item_uses$force_staff
    hurricane_pike_uses <- match$players$item_uses$hurricane_pike
    force_minutes <- duration - match$players$first_purchase_time$force_staff/60
    force_hurri_uses_pm <- (force_staff_uses + hurricane_pike_uses)/stick_minutes
    medallion_uses <- match$players$item_uses$medallion_of_courage
    solar_crest_uses <- match$players$item_uses$solar_crest
    medalcrest_minutes <- duration - match$players$first_purchase_time$medallion_of_courage/60
    medalcrest_uses_pm <- (medallion_uses + solar_crest_uses)/medalcrest_minutes
    mekansm_uses <- match$players$item_uses$mekansm
    guardian_greaves_uses <- match$players$item_uses$guardian_greaves
    mekansm_guardian_minutes <- duration - match$players$first_purchase_time$medallion_of_courage/60
    mekansm_guardian_uses_pm <- (mekansm_uses + guardian_greaves_uses)/mekansm_guardian_minutes
    
    # Save these  vars if matches are longer than 15/20/30 minutes
    varlist <- list()
    j <- 1
    
    for (minutes in c(15, 20, 30)) {
      if (length(match$players$times[[1]]) >= minutes){
        team_gold_adv <- c(rep(match$radiant_gold_adv[minutes], 5), -rep(match$radiant_gold_adv[minutes], 5))
        team_xp_adv <- c(rep(match$radiant_xp_adv[minutes], 5), -rep(match$radiant_xp_adv[minutes], 5))
        lasthits <- do.call(rbind, match$players$lh_t)[, minutes]
        denies <- do.call(rbind, match$players$dn_t)[, minutes]
        gpm <- do.call(rbind, match$players$gold_t)[, minutes]/minutes
        xpm <- do.call(rbind, match$players$xp_t)[, minutes]/minutes
        team_towerkills <- tower_kills$towerkills_15$team_towerkills
        team_towerkills_delta <- tower_kills$towerkills_15$team_towerkills_delta
        team_towerkills_adj <- tower_kills$towerkills_15$team_towerkills_adj
        team_towerkills_delta_adj <- tower_kills$towerkills_15$team_towerkills_delta_adj
      } else {
        team_gold_adv <- c(rep(NA, 10))
        team_xp_adv <- c(rep(NA, 10))
        lasthits <- c(rep(NA, 10))
        denies <- c(rep(NA, 10))
        gpm <- c(rep(NA, 10))
        xpm <- c(rep(NA, 10))
        team_towerkills <- c(rep(NA, 10))
        team_towerkills_delta <- c(rep(NA, 10))
        team_towerkills_adj <- c(rep(NA, 10))
        team_towerkills_delta_adj <- c(rep(NA, 10))
      } 
      
     assign(paste0("team_gold_adv_at_", minutes), team_gold_adv)
     assign(paste0("team_xp_adv_at_", minutes), team_xp_adv)
     assign(paste0("lasthits_at_", minutes), lasthits)
     assign(paste0("denies_at_", minutes), denies)
     assign(paste0("gpm_at_", minutes), gpm)
     assign(paste0("xpm_at_", minutes), xpm)
     assign(paste0("team_towerkills_at_", minutes), team_towerkills)
     assign(paste0("team_towerkills_at_", minutes, "_delta"), team_towerkills_delta)
     assign(paste0("team_towerkills_at_", minutes, "_adj"), team_towerkills_adj)
     assign(paste0("team_towerkills_at_", minutes, "_delta_adj"), team_towerkills_delta_adj)
      
    }

    # Save all vars in a dataframe
    
    vars <- data_frame(matchid = matchid, start_time = start_time, region = region,
                       lobby_type = lobby_type, slot = slot, is_radiant = is_radiant,
                       match_win = match_win, solo_rating = solo_rating, heroid = heroid, 
                       leaver_status = leaver_status, abandons = abandons, duration = duration, 
                       lane = lane, lane_role = lane_role, courier_kills = courier_kills,
                       player_tower_damage = player_tower_damage, 
                       player_towerkills = check_nullrows(player_towerkills),
                       team_score = team_score, team_score_delta = team_score_delta,
                       team_first_roshan = team_first_roshan,
                       team_firstblood = team_firstblood, team_first_tower_kill = team_first_tower_kill,
                       team_towerkills_at_10 = team_towerkills_at_10, 
                       team_towerkills_at_10_delta = team_towerkills_at_10_delta,
                       team_towerkills_at_10_adj = team_towerkills_at_10_adj,
                       team_towerkills_at_10_delta_adj = team_towerkills_at_10_delta_adj,
                       team_towerkills_at_15 = team_towerkills_at_15, 
                       team_towerkills_at_15_delta = team_towerkills_at_15_delta,
                       team_towerkills_at_15_adj = team_towerkills_at_15_adj,
                       team_towerkills_at_15_delta_adj = team_towerkills_at_15_delta_adj,
                       team_towerkills_at_20 = team_towerkills_at_20, 
                       team_towerkills_at_20_delta = team_towerkills_at_20_delta,
                       team_towerkills_at_20_adj = team_towerkills_at_20_adj,
                       team_towerkills_at_20_delta_adj = team_towerkills_at_20_delta_adj,
                       team_towerkills_at_30 = team_towerkills_at_30, 
                       team_towerkills_at_30_delta = team_towerkills_at_30_delta,
                       team_towerkills_at_30_adj = team_towerkills_at_30_adj,
                       team_towerkills_at_30_delta_adj = team_towerkills_at_30_delta_adj,
                       kills_per_min = kills_per_min, deaths_per_min = deaths_per_min,
                       assists_per_min = assists_per_min, neutral_kills_per_min = neutral_kills_per_min,
                       actions_per_min = actions_per_min, hero_dmg_pm = hero_dmg_pm, 
                       hero_healing_pm = check_nullrows(hero_healing_pm), 
                       buybacks = check_nullrows(buybacks),
                       team_gold_adv_at_5 = team_gold_adv_at_5,
                       team_gold_adv_at_10 = team_gold_adv_at_10,
                       team_gold_adv_at_15 = team_gold_adv_at_15,
                       team_gold_adv_at_20 = team_gold_adv_at_20,
                       team_gold_adv_at_30 = team_gold_adv_at_30,
                       team_xp_adv_at_5 = team_xp_adv_at_5,
                       team_xp_adv_at_10 = team_xp_adv_at_10,
                       team_xp_adv_at_15 = team_xp_adv_at_15,
                       team_xp_adv_at_20 = team_xp_adv_at_20,
                       team_xp_adv_at_30 = team_xp_adv_at_30,
                       denies_at_5 = denies_at_5, denies_at_10 = denies_at_10, 
                       denies_at_15 = denies_at_15,
                       denies_at_20 = denies_at_20,
                       denies_at_30 = denies_at_30,
                       lasthits_at_5 = lasthits_at_5, lasthits_at_10 = lasthits_at_10,
                       lasthits_at_15 = lasthits_at_15,
                       lasthits_at_20 = lasthits_at_20, 
                       lasthits_at_30 = lasthits_at_30,
                       gpm_at_5 = gpm_at_5, gpm_at_10 = gpm_at_10, 
                       gpm_at_15 = gpm_at_15,
                       gpm_at_20 = gpm_at_20, 
                       gpm_at_30 = gpm_at_30, gpm_at_end, xpm_at_5 = xpm_at_5, 
                       xpm_at_10 = xpm_at_10, 
                       xpm_at_15 = xpm_at_15, 
                       xpm_at_20 = xpm_at_20,
                       xpm_at_30 = xpm_at_30, 
                       xpm_at_end = xpm_at_end,
                       camps_stacked = check_nullrows(camps_stacked),
                       obswards_placed_pm = check_nullrows(obswards_placed_pm),
                       sentwards_placed_pm = check_nullrows(sentwards_placed_pm), 
                       rune_pickups_pm = check_nullrows(rune_pickups_pm),
                       first_bounty_pickup = check_nullrows(first_bounty_pickup), 
                       regular_rune_pickup_at_15 = check_nullrows(regular_rune_pickup_at_15),
                       pings_pm = check_nullrows(pings_pm), bottle_purchased = check_nullrows(bottle_purchased), 
                       gem_purchased = check_nullrows(gem_purchased),
                       infused_rdrop_purchased = check_nullrows(infused_rdrop_purchased), 
                       faerie_fires_purchased = check_nullrows(faerie_fires_purchased),
                       smokes_purchased_pm = check_nullrows(smokes_purchased_pm), 
                       tpscroll_purchased_pm = check_nullrows(tpscroll_purchased_pm),
                       dust_purchased_pm = check_nullrows(dust_purchased_pm), 
                       travel_boots_uses_pm = check_nullrows(travel_boots_uses_pm),
                       phase_boots_uses_pm = check_nullrows(phase_boots_uses_pm), 
                       power_treads_uses_pm = check_nullrows(power_treads_uses_pm),
                       blink_pm = check_nullrows(blink_pm), wandstick_uses_pm = check_nullrows(wandstick_uses_pm), 
                       force_hurri_uses_pm = check_nullrows(force_hurri_uses_pm), 
                       medalcrest_uses_pm = check_nullrows(medalcrest_uses_pm),
                       mekansm_guardian_uses_pm = check_nullrows(mekansm_guardian_uses_pm))
    
    match_list[[match_nr]] <- vars
    
    match_nr <- match_nr+1
  }
  
  return(do.call(rbind, match_list))
}


check_nullrows <- function(var){
  
  if (is.null(var)){
    var <- c(rep(NA, 10))
  }
  
  if (is.numeric(var)) {
    if (length(var) == 0){
      var <- c(rep(NA, 10))
    }
  }
  
  return(var)
}

### Parse tower kill messages ###

parse_objectives <- function(match){
  # Firstblood
  
  team_firstblood <- rep(FALSE, 10)
  firstblood <- match$objectives[match$objectives$type == "CHAT_MESSAGE_FIRSTBLOOD",]
  
  if (length(firstblood$player_slot) != 0){
    if (firstblood$player_slot %in% c(0:4)){
      team_firstblood[1:5] <- TRUE
    } 
    else if (firstblood$player_slot %in% c(128:132)){
      team_firstblood[6:10] <- TRUE
    } else {
      warning(paste("player_slot not in 0:4 or 128:132. player_slot:", firstblood$player_slot))
    }
  } else {
    team_firstblood <- rep(NA, 10)
  }
  
  # First tower
  
  tower_killdeny <- match$objectives[match$objectives$type == "CHAT_MESSAGE_TOWER_KILL" 
                                     | match$objectives$type == "CHAT_MESSAGE_TOWER_DENY",]
  
  
  if (nrow(tower_killdeny) > 0){
    
    team_first_tower_kill <- rep("No", 10)
    
    if (tower_killdeny$type[1] == "CHAT_MESSAGE_TOWER_KILL"){
      if (tower_killdeny$team[1] == 2){
        team_first_tower_kill[1:5] <- "Yes"
      }
      if (tower_killdeny$team[1] == 3){
        team_first_tower_kill[6:10] <- "Yes"
      }
    } else if (tower_killdeny$type[1] == "CHAT_MESSAGE_TOWER_DENY"){
      # If player 0:4 denies tower, then opposite team gets first tower kill
      if (tower_killdeny$player_slot[1] %in% 0:4){
        team_first_tower_kill[6:10] <- "Yes (denied)"
      }
      if (tower_killdeny$player_slot[1] %in% 128:132){
        team_first_tower_kill[1:5] <- "Yes (denied)"
      }
    } else {
      warning("No objectives/tower kills in game")
    }
    
  } else{
    team_first_tower_kill <- rep("No", 10)
  }
  
  
  ### Team tower kill summary, and tower kill lead/deficit (delta) compared to other team ###
  ### at 10, 15, 20, and 30 minute mark.                                                  ###
  timestamps <- c(600, 900, 1200, 1800)
  towerkills <- list()
  i <- 1
  
  for (time in timestamps) {
    
    tower_killdeny_temp <- tower_killdeny[tower_killdeny$time < time, ]
    
    radiant_towerkills <- sum(tower_killdeny_temp$type %in% "CHAT_MESSAGE_TOWER_KILL" & tower_killdeny_temp$team == 2)
    dire_towerkills <- sum(tower_killdeny_temp$type %in% "CHAT_MESSAGE_TOWER_KILL" & tower_killdeny_temp$team == 3)
    
    # if dire denies a tower, that means radiant has gotten a tower kill denied
    radiant_towerkill_denied <- sum(tower_killdeny_temp$type %in% "CHAT_MESSAGE_TOWER_DENY" 
                                    & tower_killdeny_temp$player_slot %in% 128:132)
    
    dire_towerkill_denied <- sum(tower_killdeny_temp$type %in% "CHAT_MESSAGE_TOWER_DENY" 
                                 & tower_killdeny_temp$player_slot %in% 0:4)
    
    team_towerkills <- c(rep(radiant_towerkills + radiant_towerkill_denied, 5), 
                         rep(dire_towerkills + dire_towerkill_denied, 5))
    
    # Adjusted towerkills, denied towers only count as half a tower
    team_towerkills_adj <- c(rep(radiant_towerkills + radiant_towerkill_denied * 0.5, 5), 
                             rep(dire_towerkills + dire_towerkill_denied * 0.5, 5))
    
    radiant_towerkills_delta <- team_towerkills[1:5] - team_towerkills[6:10]
    dire_towerkills_delta <- team_towerkills[6:10] - team_towerkills[1:5]
    
    radiant_towerkills_delta_adj <- team_towerkills_adj[1:5] - team_towerkills_adj[6:10]
    dire_towerkills_delta_adj <- team_towerkills_adj[6:10] - team_towerkills_adj[1:5]
    
    team_towerkills_delta <- c(radiant_towerkills_delta, dire_towerkills_delta)
    team_towerkills_delta_adj <- c(radiant_towerkills_delta_adj, dire_towerkills_delta_adj)
    
    towerkills[[i]] <- list(team_towerkills = team_towerkills, 
                            team_towerkills_delta = team_towerkills_delta, 
                            team_towerkills_adj = team_towerkills_adj, 
                            team_towerkills_delta_adj = team_towerkills_delta_adj)
   
    first_roshan <- match$objectives[match$objectives$type == "CHAT_MESSAGE_ROSHAN_KILL",]
    
    team_first_roshan <- rep(NA, 10)
    
    if (nrow(first_roshan) > 0){
      if (first_roshan[1]$team == 2){
        team_first_roshan[1:5] <- TRUE
        team_first_roshan[6:10] <- FALSE
      } else {
        team_first_roshan[1:5] <- FALSE
        team_first_roshan[6:10] <- TRUE
      }
    }
    
    
    i <- i + 1
    
  }
  
  return(list(team_firstblood = team_firstblood, 
              team_first_tower_kill = team_first_tower_kill,
              towerkills_10 = towerkills[[1]], 
              towerkills_15 = towerkills[[2]],
              towerkills_20 = towerkills[[3]],
              towerkills_30 = towerkills[[4]],
              team_first_roshan = team_first_roshan))
}


