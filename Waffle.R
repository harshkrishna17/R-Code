# Libraries

library(tidyverse)
library(worldfootballR)
library(ggtext)
library(extrafont)
library(waffle)
library(MetBrewer)

# Scraping

data <- fb_big5_advanced_season_stats(season_end_year = 2022, stat_type = "gca", team_or_player = "player")

# Data Wrangling 

data1 <- data %>%
filter(Comp == "Premier League") %>%
filter(Mins_Per_90 >= 9) %>%
select(Player, Mins_Per_90, SCA_SCA, PassLive_SCA, PassDead_SCA, Drib_SCA, Sh_SCA, Fld_SCA, Def_SCA)

data1 <- data1[order(as.numeric(data1$SCA_SCA),decreasing = TRUE),]
data1 <- data1[c(1:20),]

Player <- data1$Player
Mins <- data1$Mins_Per_90
data1 <- subset(data1, select = -c(Player, Mins_Per_90))

for(i in 1:ncol(data1)) {
    data1[, i] <- data1[, i] / Mins
}

SCA <- data1$SCA_SCA 

for(i in 1:ncol(data1)) {
    data1[, i] <- (data1[, i] / SCA) * 100
}

data1$Player <- Player 

data1 <- data1 %>%
pivot_longer(!Player, values_to = "SCAp90", names_to = "SCATypes") %>%
filter(!SCATypes == "SCA_SCA") %>%
count(Player, SCATypes, wt = SCAp90)

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
facet_wrap(~Player) +
labs(title = "Premier League Shot-Creating Actions Share [2021/22]",
     subtitle = "Top 20 Players with the most SCA so far",
     caption = "Minimum 9 90's Played\nData from FBref\nCreated by @placeholder2004") +
theme_athletic() +
theme(aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text = element_text(colour = "white", size = 14),
      legend.position = "bottom",
      legend.text = element_text(size = 12))

# Save

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("waffle.png", width = 2850, height = 3300, units = "px")
