# Loading required packages

library(worldfootballR)
library(ggplot2)
library(ggtext)
library(dplyr)
library(ggalt)

# Data manipulation

league_table <- get_season_team_stats(country = "FRA", gender = "M", season_end_year = "2021", tier = "1st", stat_type = "league_table_home_away")
data <- league_table[, c(5, 14, 19, 27, 32)]
data <- data %>%
  mutate(xGDiff_Home = GD_Home - xGD_Home,
         xGDiff_Away = GD_Away - xGD_Away)
data1 <- data[, c(1, 6, 7)]
data1$Squad <- factor(data1$Squad, levels=as.character(data1$Squad))

# Plotting

attach(data1)

DBplot <- ggplot(data1, aes(x=xGDiff_Home, xend=xGDiff_Away, y=Squad)) +
  geom_segment(aes(x=xGDiff_Home, 
                       xend=xGDiff_Away, 
                       y=Squad, 
                       yend=Squad), 
                   color="#b2b2b2", size=3) +
  geom_dumbbell(color="light blue",         
                    size_x=10,                  
                    size_xend =10,            
                    colour_x="#238b45",       
                    colour_xend = "#fe9929") +
  labs(x=NULL, y=NULL, 
           title="<b style='color:#ffffff'>Overperformance or Underperformance ? (2020/21)</b>", 
           subtitle="<b style='color:#ffffff'>How have Ligue 1 teams performed on their Goal Difference</b> 
<b style='color:#238b45'>Home</b> <b style='color:#ffffff'>&</b> 
<b style='color:#fe9929'>Away</b> <b style='color:#ffffff'>?</b>") +
  theme_classic(base_size = 24) +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1)) +
  labs(fill="",                                                                       
            caption = "Data from FBref via StatsBomb, WorldFootballR
       Created by Harsh Krishna") +
  theme(plot.background = element_rect(fill = "#000000", colour = "#000000")) +
  theme(panel.background = element_rect(fill = "#000000", colour = "#000000")) +
  theme(plot.caption = element_text(color = "white")) +
  labs(x = "GD - xGD") +
  theme(axis.title.x = element_text(colour = "#ffffff")) +
  theme(axis.text.x = element_text(colour = "#ffffff"),
        axis.text.y = element_text(colour = "#ffffff")) +
  theme(panel.grid.major = element_line(colour="grey", size = (0.1)),
                 panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(axis.line = element_line(size = 0.8, colour = "#ffffff"))
DBplot

DBplot + geom_rect(aes(xmin = 0, xmax = -Inf, ymin = -Inf, ymax = Inf),
                   fill = "red", alpha = 0.01) + 
  DBplot +
  theme(axis.text.x = element_text(colour = "#ffffff"),
        axis.text.y = element_text(colour = "#ffffff"))
