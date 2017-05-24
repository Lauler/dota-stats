library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(extrafont)
library(tidyr)
fonttable()


dota <- read_rds("F:\\dota_data\\storage\\dota.rdata")

dota[c("travel_boots_uses_pm", "phase_boots_uses_pm", "power_treads_uses_pm",
       "blink_pm", "wandstick_uses_pm", "force_hurri_uses_pm", "medalcrest_uses_pm",
       "mekansm_guardian_uses_pm", "smokes_purchased_pm", 
       "gem_purchased", "dust_purchased_pm", 
       "pings_pm", "tpscroll_purchased_pm")][is.na(dota[c("travel_boots_uses_pm", "phase_boots_uses_pm", "power_treads_uses_pm",
                                                          "blink_pm", "wandstick_uses_pm", "force_hurri_uses_pm", "medalcrest_uses_pm",
                                                          "mekansm_guardian_uses_pm", "smokes_purchased_pm", "gem_purchased",
                                                          "dust_purchased_pm", "pings_pm", "tpscroll_purchased_pm")])] <- 0

dota$item_uses <- rowSums(dota[, c("travel_boots_uses_pm", "phase_boots_uses_pm", "power_treads_uses_pm",
                                   "blink_pm", "wandstick_uses_pm", "force_hurri_uses_pm", "medalcrest_uses_pm",
                                   "mekansm_guardian_uses_pm")], na.rm =TRUE)



### Fördelning
dota %>%
  filter(!is.na(solo_rating) & duration > 30) %>%
  ggplot(aes(x = solo_rating)) +
  geom_histogram(aes(y=..count../sum(..count..))) +
  theme_minimal(base_size = 18) +
  theme(text = element_text(family = "CMU Sans Serif"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Solo rating (MMR)", y = "Andel spelare",
       title = "MMR-fördelning i urval")


##### 
# Exploratory plots trying to see which mids are supports
ggplot(dota[1:15000,], aes(x = as.numeric(solo_rating))) +
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




#####
### Correlation matrix ###

cormat <- cor(dota %>%
      filter(!is.na(solo_rating) & !is.na(gpm_at_15)
             & !is.na(gpm_at_30) & !is.na(team_firstblood)
             & !is.na(tpscroll_purchased_pm)
             & !is.na(action_latency_median)) %>%
      select(solo_rating, match_win,
             gpm_at_15, gpm_at_20, 
             xpm_at_15, xpm_at_20,
             team_gold_adv_at_20, team_xp_adv_at_20,
             lasthits_at_15, kills_per_min, deaths_per_min,
             actions_per_min, hero_dmg_pm,
             denies_at_10, obswards_placed_pm,
             sentwards_placed_pm, team_score_delta, team_firstblood,
             item_uses, action_latency_median,
             nr_camera_changes))


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
  theme_bw(base_size = 14) +
  scale_fill_viridis() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1.09, hjust = -0.08),
        text = element_text(family = "CMU Sans Serif"),
        panel.border = element_rect(colour = FALSE)) +
  geom_text(aes(label = round(value, 2), color = textcol), size = 3) +
  scale_colour_manual(values = c("grey10" = "grey10", "grey80" = "white")) +
  labs(x = "", y = "", title = "Korrelationsmatris för ett urval av variabler",
       fill = "", color = "") +
  guides(colour = FALSE)

