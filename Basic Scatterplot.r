Basic Scatterplot

# Libraries

library(worldfootballR)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(extrafont)

# Scraping

data <- fb_big5_advanced_season_stats(season_end_year= 2022, stat_type= "gca", team_or_player= "player")

# Manipulation

data <- data %>%
filter(Mins_Per_90 >= 7) %>%
filter(Pos == "FW" | Pos == "FW,MF") %>%
mutate(Dribble = Drib_SCA/Mins_Per_90) %>%
mutate(Total = SCA_SCA/Mins_Per_90)

# Custom theme function

theme_athletic <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
          panel.background = element_rect(colour = "#151515", fill = "#151515")) +
    theme(plot.title = element_text(colour = "white", size = 24, family = "Fried Chicken Bold", hjust = 0.5),
          plot.subtitle = element_markdown(colour = "white", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 15, hjust = 1),
          axis.title.x = element_text(colour = "white", face = "bold", size = 14),
          axis.title.y = element_text(colour = "white", face = "bold", size = 14),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white", size = 12)) +
    theme(panel.grid.major = element_line(colour = "#525252", size = 0.7, linetype = "dashed"),
          panel.grid.minor = element_line(colour = "#525252", size = 0.7, linetype = "dashed")) +
    theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.7, linetype = "dashed"),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"))
}

# Plotting 

ggplot() +
geom_point(data = data, aes(x = Total, y = Dribble), colour = "#4292c6", size = 3) +
geom_point(data = data[111, ], aes(x = Total, y = Dribble), colour = "#fc9272", size = 9) +
geom_text_repel(data = data, aes(x = Total, y = Dribble, label = Player),
                   box.padding   = 0.35, 
                   point.padding = 1.5,
                   segment.color = "black",
                   colour = "white", 
                   alpha = 1) +
                   labs(title = "Shot Creating Actions",
                   subtitle = "Forwards | Big 5 Leagues 2021/22 | Minimum 7 90's played",
                   caption = "Data from FBref
                   Created by Harsh Krishna") +
                   theme_athletic()