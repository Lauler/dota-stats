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


dota$pos_dataframe[5]
