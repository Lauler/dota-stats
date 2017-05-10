library(readr)
library(ggplot2)

dota <- read_rds("C:\\Users\\Faton\\Desktop\\R-ovning\\Game data mining\\Dota2\\dota stats\\data\\dota_df.rdata")

ggplot(dota, aes(x = as.numeric(solo_rating.x))) +
  geom_histogram()

ggplot(dota %>% filter(lane_role == "mid"), 
       aes(x = as.numeric(solo_rating.x), y = lasthits_at_15, color = obswards_placed_pm)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis()

log_model <- glm(formula = match_win ~ as.factor(team_towerkills_at_20_delta),
                 family = binomial(link = "logit"),
                 data = dota)

pred <- predict(log_model, newdata = data.frame(team_towerkills_at_20_delta = seq(-7, 7, by = 1)))
pred <- data_frame(pred)
pred$x <- seq(-7, 7, 1)

ggplot(pred, aes(x = x, y = pred)) +
  geom_line()