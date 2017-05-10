############
library(jsonlite)
library(dplyr)

heroes <- fromJSON("https://raw.githubusercontent.com/odota/dotaconstants/master/build/hero_names.json")
heroes <- do.call(rbind, heroes)
heroes[, "roles"] <- lapply(X = heroes[, "roles"], FUN = paste, collapse = " ")
heroes <- as_data_frame(apply(heroes, 2, unlist))

as.data.frame(heroes)
a <- as_data_frame(heroes[, -6])
heroes <- apply(heroes, 2, FUN = unlist)
do.call(rbind, heroes)


###################

rankbrackets <- cut(as.numeric(dota$dota_df$solo_rating.x), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 10000),
                    labels = c("0-1000", "1000-2000", "2000-3000", "3000-4000", "4000-5000", "5000-6000",
                               "6000-7000", "7000+"))

table(rankbrackets)


### Map actions ###

actions <- dota$json_df$opendotajsondata[[23]]$players$actions
dota$json_df$opendotajsondata[[23]]$players$benchmarks

acts <- fromJSON("https://raw.githubusercontent.com/odota/dotaconstants/master/build/order_types.json")
ord <- as_data_frame(do.call(rbind, acts))
ord$key <- names(acts)
names(ord) <- c("order", "key")

names(actions) <- ord$order[match(names(actions), as.numeric(ord$key))]
names(actions) <- str_sub(names(actions), start = 17)
actions$MOVE_TO_POSITION[5] / sum(actions[5,], na.rm = TRUE)
