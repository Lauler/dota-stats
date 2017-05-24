library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)
library(glmnet)
library(leaps)
library(extrafont)
library(xtable)
options(scipen = 99)



##### Databearbetning
dota <- read_rds("F:\\dota_data\\storage\\dota.rdata")


dota[c("travel_boots_uses_pm", "phase_boots_uses_pm", "power_treads_uses_pm",
       "blink_pm", "wandstick_uses_pm", "force_hurri_uses_pm", "medalcrest_uses_pm",
       "mekansm_guardian_uses_pm", "smokes_purchased_pm", 
       "gem_purchased", "dust_purchased_pm", "pings_pm", 
       "tpscroll_purchased_pm", "bottle_purchased")][is.na(dota[c("travel_boots_uses_pm", "phase_boots_uses_pm", "power_treads_uses_pm",
                                                          "blink_pm", "wandstick_uses_pm", "force_hurri_uses_pm", "medalcrest_uses_pm",
                                                          "mekansm_guardian_uses_pm", "smokes_purchased_pm", "gem_purchased",
                                                          "dust_purchased_pm", "pings_pm", "tpscroll_purchased_pm",
                                                          "bottle_purchased")])] <- 0

dota$item_uses <- rowSums(dota[, c("travel_boots_uses_pm", "phase_boots_uses_pm", "power_treads_uses_pm",
                                   "blink_pm", "wandstick_uses_pm", "force_hurri_uses_pm", "medalcrest_uses_pm",
                                   "mekansm_guardian_uses_pm")], na.rm =TRUE)


dota_glm <- dota %>%
  filter_at(vars(solo_rating, gpm_at_20, team_firstblood,
                 denies_at_15, team_xp_adv_at_15, team_towerkills_at_20_delta,
                 match_win, tpscroll_purchased_pm, gpm_at_30, 
                 nr_camera_changes, action_latency_median), all_vars(!is.na(.)) ) %>%
  select(solo_rating, gpm_at_5, gpm_at_10, gpm_at_15, gpm_at_20, gpm_at_30,
         xpm_at_5, xpm_at_10, xpm_at_15, xpm_at_20, xpm_at_30,
         lasthits_at_10 ,lasthits_at_15, lasthits_at_20,
         kills_per_min, assists_per_min, deaths_per_min,
         actions_per_min, hero_dmg_pm, hero_healing_pm,
         denies_at_10, denies_at_15, denies_at_20,
         obswards_placed_pm,
         sentwards_placed_pm, team_score_delta,
         team_towerkills_at_20_delta,
         team_firstblood,
         neutral_kills_per_min, tpscroll_purchased_pm,
         team_xp_adv_at_15, team_gold_adv_at_15, denies_at_15,
         lasthits_at_10, player_towerkills, player_tower_damage,
         avg_distance_travelled, pings_pm,
         item_uses, nr_camera_changes, action_latency_median,
         action_latency_10th_percentile, action_latency_90th_percentile,
         action_latency_mean, match_win, lane_role,
         smokes_purchased_pm, gem_purchased, dust_purchased_pm,
         buybacks, camps_stacked, pings_pm) %>%
  mutate(match_win = as.factor(match_win),
         team_firstblood = as.factor(team_firstblood))


# x görs till model matrix eftersom paketet glmnet inte kan hantera
# faktorvariabler. Dessa måste göras om till dummies först.
x <- model.matrix(solo_rating~., dota_glm)

y <- dota_glm$solo_rating


#########################
##### Stepwise Regression

bestsub <- regsubsets(x = x[,-1], y = y, 
                      method = "forward", 
                      nbest = 1, 
                      nvmax = 49)

b <- summary(bestsub)

# Välj ut modellerna
which_models <- as_data_frame(b$which)
which_models$params <- rownames(b$which)

a <- which_models %>%
  gather(key, value, -params) %>%
  group_by(key) %>%
  mutate(n = row_number(),
         params = as.numeric(params))


#####
# Backwards Elimination 

# Inkluderade parametrar plot
ggplot(data = a, aes(x = key, y = as.factor(n), 
                     fill = factor(value, labels = c("Ej inkluderad", "Inkluderad")))) +
  geom_tile(colour = "white") +
  theme_minimal() +
  theme(text = element_text(family = "CMU Sans Serif")) +
  scale_y_discrete(breaks = 1:10*5, labels = as.character(1:10*5)) +
  # scale_fill_manual(values = c("indianred2", "steelblue3")) +
  labs(x = "Variabel", y = "Antal parametrar",
       title = "Bästa modellen för varje storlek via backward elimination",
       fill = "") +
  coord_flip()

# R^2 och Cp-plot. Ta bort text = element_text(family = ...)
# För att få det att funka på andra datorer.
bsub <- data_frame(params = as.numeric(rownames(b$which)), 
                   r2adj = b$adjr2, 
                   cp = b$cp)

ggplot(data = bsub, aes(x = params, y = r2adj)) +
  geom_point() +
  theme_minimal(base_size = 18) +
  labs(title = expression(paste("Adjusted ", R^2, " för modeller genom backward elimination")),
       x = "Parametrar",
       y = expression(paste("Adjusted ", R^2))) +
  theme(text = element_text(family = "CMU Sans Serif"),
        plot.background = element_rect(colour = "grey40", size = 2))

ggplot(data = bsub, aes(x = params, y = cp)) +
  geom_abline(slope = 1, colour = "red", linetype = 2) +
  geom_point() +
  theme_minimal(base_size = 18) +
  scale_x_continuous(limits = c(30, 50)) +
  scale_y_continuous(limits = c(0, 170),
                     breaks = 0:17*20,
                     labels = 0:17*20) +
  labs(title = expression(paste("Mallows ", C[p], " för modeller genom backward elimination")),
       x = "Parametrar",
       y = expression(paste("Mallows ", C[p]))) +
  theme(text = element_text(family = "CMU Sans Serif"),
        plot.background = element_rect(colour = "grey40", size = 2))

#####
# Forwards selection

ggplot(data = a, aes(x = key, y = as.factor(n), 
                     fill = factor(value, labels = c("Ej inkluderad", "Inkluderad")))) +
  geom_tile(colour = "white") +
  theme_minimal() +
  theme(text = element_text(family = "CMU Sans Serif")) +
  scale_y_discrete(breaks = 1:10*5, labels = as.character(1:10*5)) +
  # scale_fill_manual(values = c("indianred2", "steelblue3")) +
  labs(x = "Variabel", y = "Antal parametrar",
       title = "Bästa modellen för varje storlek via forward selection",
       fill = "") +
  coord_flip()


ggplot(data = bsub, aes(x = params, y = r2adj)) +
  geom_point() +
  theme_minimal(base_size = 18) +
  labs(title = expression(paste("Adjusted ", R^2, " för modeller genom forward selection")),
       x = "Parametrar",
       y = expression(paste("Adjusted ", R^2))) +
  theme(text = element_text(family = "CMU Sans Serif"),
        plot.background = element_rect(colour = "grey40", size = 2))

ggplot(data = bsub, aes(x = params, y = cp)) +
  geom_abline(slope = 1, colour = "red", linetype = 2) +
  geom_point() +
  theme_minimal(base_size = 18) +
  scale_x_continuous(limits = c(30, 50)) +
  scale_y_continuous(limits = c(0, 170),
                     breaks = 0:17*20,
                     labels = 0:17*20) +
  labs(title = expression(paste("Mallows ", C[p], " för modeller genom forward selection")),
       x = "Parametrar",
       y = expression(paste("Mallows ", C[p]))) +
  theme(text = element_text(family = "CMU Sans Serif"),
        plot.background = element_rect(colour = "grey40", size = 2))


###############################################
# Lasso- och ridgeregression med paketet glmnet.
test <- cv.glmnet(x = scale(x[,-1]), y = scale(y))
test
dota_lasso <- glmnet(x = scale(x[,-1]), y = scale(y), lambda = test$lambda.1se)
coef(dota_lasso)

df <- as.data.frame(as.matrix(dota_lasso$beta))
df$vars <- rownames(df)
names(df)[1] <- "std_coeff"

df %>%
  arrange(-abs(std_coeff)) %>%
  mutate(row_nr = row_number()) %>%
  as_data_frame() %>%
  filter(row_nr <= 10) %>%
  xtable(digits = 3) %>%
  print(include.rownames = FALSE)

df %>%
  arrange(-abs(std_coeff)) %>%
  mutate(row_nr = row_number(),
         dummy = 1,
         vars = factor(vars, rev(vars)),
         std_coeff_abs = as.factor(abs(std_coeff))) %>%
  as_data_frame() %>%
  filter(row_nr <= 10) %>%
  ggplot(aes(x = dummy, y = vars, fill = std_coeff_abs)) +
  geom_tile() +
  scale_fill_manual(values = palette)

# Ange test och s = ___ som argument för att få specifik modell
# för ett visst lambda-värde
coef(dota_lasso) 
coef(test, s = "lambda.min")
coef(test, s = "lambda.1se")
a <- lm(solo_rating ~ gpm_at_15 + gpm_at_20 + xpm_at_5 
        + lasthits_at_10 + lasthits_at_20 + kills_per_min 
        + assists_per_min + actions_per_min + hero_healing_pm 
        + obswards_placed_pm + sentwards_placed_pm 
        + neutral_kills_per_min + tpscroll_purchased_pm 
        + team_gold_adv_at_15 + player_tower_damage 
        + item_uses + nr_camera_changes + action_latency_10th_percentile 
        + action_latency_90th_percentile + lane_role
        + smokes_purchased_pm + gem_purchased + buybacks
        + camps_stacked, data = dota_glm)


vif(a)


dota_lasso_shrink <- glmnet(x = x, y = y, nlambda = 1000)
plot(dota_lasso_shrink, xvar = "lambda")
plot(test)

