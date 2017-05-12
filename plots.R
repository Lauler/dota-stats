library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(extrafont)
library(tidyr)
fonttable()

dota <- read_rds("C:\\Users\\Faton\\Desktop\\R-ovning\\Game data mining\\Dota2\\dota stats\\data\\dota_df.rdata")

ggplot(dota, aes(x = as.numeric(solo_rating.x))) +
  geom_histogram() 

ggplot(dota, aes(x = duration)) +
  geom_histogram(alpha = 0.7) +
  theme(text=element_text(family = "CMU sans serif")) +
  scale_x_continuous(breaks = 0:12*10)

ggplot(dota %>% filter(lane_role == "mid"), 
       aes(x = as.numeric(solo_rating.x), y = lasthits_at_15, color = obswards_placed_pm)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis() +
  theme(text=element_text(family = "Humor Sans")) +
  labs(title = "Lasthits at 15 minutes by solo rating (mid players, opendota classification)",
       x = "Solo rating",
       y = "Lasthits at 15 minutes",
       colour =  expression(atop(" Obswards placed \n (per min)"))) 


### Trying log regression ###

log_model <- glm(formula = match_win ~ as.factor(team_towerkills_at_20_delta),
                 family = binomial(link = "logit"),
                 data = dota)

pred <- predict(log_model, newdata = data.frame(team_towerkills_at_20_delta = seq(-7, 7, by = 1)))
pred <- data_frame(pred)
pred$x <- seq(-7, 7, 1)

ggplot(pred, aes(x = x, y = pred)) +
  geom_line()


### Correlation matrix ###

dota[, "solo_rating.x"] <- as.numeric(unlist(dota[, "solo_rating.x"]))

dota <- dota %>%
  select(-solo_rating.y, -team_first_tower_kill)

dota[is.na(dota$tpscroll_purchased_pm), "tpscroll_purchased_pm"] <- 0
options(scipen = 99)

cormat <- cor(dota %>%
      filter(!is.na(solo_rating.x) & !is.na(gpm_at_15)
             & !is.na(gpm_at_20) & !is.na(team_firstblood)) %>%
      select(solo_rating.x, match_win,
             gpm_at_10, gpm_at_15, gpm_at_20,
             lasthits_at_15, kills_per_min,
             actions_per_min, hero_dmg_pm,
             denies_at_10, obswards_placed_pm,
             sentwards_placed_pm, team_score_delta,
             team_towerkills_at_20, team_firstblood,
             neutral_kills_per_min, tpscroll_purchased_pm))

dota$t
cormat <- as.data.frame(cormat)
cormat$rownames <- rownames(cormat)
cormat <- cormat %>% 
  gather(rownames)


names(cormat) <- c("rownames", "colnames", "value")
cormat$textcol <- "grey80"
cormat[cormat$value > 0.7, "textcol"] <- "grey10"

as_data_frame(cormat)

ggplot(data = cormat, aes(x = rownames, y = colnames, fill = value)) +
  geom_tile() +
  scale_fill_viridis() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1.09, hjust = -0.08),
        text = element_text(family = "CMU Sans Serif")) +
  geom_text(aes(label = round(value, 2), color = textcol), size = 3) +
  scale_colour_manual(values = c("grey10" = "grey10", "grey80" = "white"))

dota


#####

skill_model <- lm(solo_rating.x ~ gpm_at_15 + lasthits_at_15 + 
                  actions_per_min + denies_at_15,
                  data = dota)

summary(skill_model)
sapply(dota, class)



#####

library(glmnet)

x <- dota %>%
  filter(!is.na(solo_rating.x) & !is.na(gpm_at_15)
         & !is.na(gpm_at_20) & !is.na(team_firstblood)) %>%
  select(match_win,
         gpm_at_10, gpm_at_15, gpm_at_20,
         lasthits_at_15, kills_per_min,
         actions_per_min, hero_dmg_pm,
         denies_at_10, obswards_placed_pm,
         sentwards_placed_pm, team_score_delta,
         team_towerkills_at_20, team_firstblood,
         neutral_kills_per_min, tpscroll_purchased_pm)

y <- dota %>%
  filter(!is.na(solo_rating.x) & !is.na(gpm_at_15)
         & !is.na(gpm_at_20) & !is.na(team_firstblood)) %>%
  select(solo_rating.x)


dota_lasso <- glmnet(x = as.matrix(x), y = as.matrix(y), alpha = 0.2,
                     weights = rep(1, 26518))

coef(dota_lasso, s = 20)
dota_lasso$lambda

plot(dota_lasso, xvar = "lambda")
