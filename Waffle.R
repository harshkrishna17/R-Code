# Libraries

library(tidyverse)
library(worldfootballR)
library(ggtext)
library(extrafont)
library(waffle)
library(MetBrewer)

# Scraping

data <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st", stat_type = "goal_shot_creation")

# Data Wrangling 

data1 <- data %>%
filter(Team_or_Opponent == "team") %>%
select(Squad, Mins_Per_90, SCA_SCA, PassLive_SCA_Types, PassDead_SCA_Types, Drib_SCA_Types, Sh_SCA_Types, Fld_SCA_Types, Def_SCA_Types)

Squad <- data1$Squad
Mins <- data1$Mins_Per_90
data1 <- subset(data1, select = -c(Squad, Mins_Per_90))

for(i in 1:ncol(data1)) {
    data1[, i] <- data1[, i] / Mins
}

SCA <- data1$SCA_SCA 

for(i in 1:ncol(data1)) {
    data1[, i] <- (data1[, i] / SCA) * 100
}

data1$Squad <- Squad 

data1 <- data1 %>%
pivot_longer(!Squad, values_to = "SCAp90", names_to = "SCATypes") %>%
filter(!SCATypes == "SCA_SCA") %>%
count(Squad, SCATypes, wt = SCAp90)

# Custom theme function

theme_athletic <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
          panel.background = element_rect(colour = "#151515", fill = "#151515")) +
    theme(plot.title = element_text(colour = "white", size = 24, family = "Fried Chicken Bold", hjust = 0.5),
          plot.subtitle = element_markdown(colour = "#525252", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 15, hjust = 1),
          axis.title.x = element_text(colour = "white", face = "bold", size = 14),
          axis.title.y = element_text(colour = "white", face = "bold", size = 14),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"))
}

# Plotting

data1 %>%
ggplot(aes(fill = SCATypes, values = n)) +
geom_waffle(nrows = 10, size = 0.33, colour = "#151515", flip = TRUE) +
scale_fill_manual(values = met.brewer(name = "Signac", n = 6, type = "discrete")) +
facet_wrap(~Squad) +
labs(title = "Shot-Creating Actions Share",
     subtitle = "Premier League 2021/22",
     caption = "Data from FBref\nCreated by @placeholder2004") +
theme_athletic() +
theme(aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text = element_text(colour = "white", size = 14),
      legend.position = "bottom",
      legend.text = element_text(size = 12))

# Save

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("waffle.png", width = 2700, height = 2850, units = "px")
