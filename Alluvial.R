# Libraries 

library(tidyverse)
library(worldfootballR)
library(ggalluvial)
library(extrafont)
library(ggtext)
library(MetBrewer)

# Scraping 

data <- understat_league_season_shots(league = "EPL", season_start_year = 2021)

# Data Wrangling 

data <- data %>%
mutate(isGoal = ifelse(result == "Goal", "Goal", "No Goal"))

data1 <- data %>%
filter(h_a == "h") %>%
rename(Squad = home_team) %>%
filter(Squad == "Manchester United" | Squad == "Chelsea" | 
       Squad == "Liverpool" | Squad == "Manchester City" |
       Squad == "Tottenham" | Squad == "Arsenal") %>%
select(Squad, isGoal, situation, xG)

data2 <- data %>%
filter(h_a == "a") %>%
rename(Squad = away_team) %>%
filter(Squad == "Manchester United" | Squad == "Chelsea" | 
       Squad == "Liverpool" | Squad == "Manchester City" |
       Squad == "Tottenham" | Squad == "Arsenal") %>%
select(Squad, isGoal, situation, xG)

data <- rbind(data1, data2)

data$Squad[data$Squad == "Manchester United"] <- "Man Utd"
data$Squad[data$Squad == "Manchester City"] <- "Man City"

# Custom theme function

theme_custom <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#0d1117", fill = "#0d1117"),
          panel.background = element_rect(colour = "#0d1117", fill = "#0d1117")) +
    theme(plot.title = element_text(colour = "white", size = 24, family = "Fried Chicken Bold", hjust = 0.5),
          plot.subtitle = element_markdown(colour = "white", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 12, hjust = 1),
          axis.title.x = element_text(colour = "white", face = "bold", size = 14),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "black"),
          legend.text = element_text(colour = "black"))
}

ggplot(data = data, aes(axis1 = Squad, axis2 = situation, axis3 = isGoal, y = xG)) +
  scale_x_discrete(limits = c("situation", "grouping", "result")) +
  geom_alluvium(aes(fill = Squad), alpha = 0.7) +
  scale_fill_manual(values = c("#9b3441", "#1f6e9c", "#633372", "#92c051", "#e87b89", "#fe9b00")) +
  geom_stratum(fill = "#0d1117", colour = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, colour = "white") +
  theme_custom() +
  theme(legend.position = "top",
        legend.title = element_text(colour = "white"),
        legend.text = element_text(colour = "white")) +
  labs(title = "Premier League Shot (For) Flows",
       subtitle = "Premier League | Big Six | 2021/22",
       caption = "Data from Understat\nCreated by Harsh Krishna",
       x = "The size of each block corresponds to the sum of the xG.\nThese Charts attempt to show the shooting trends of teams.")

# Save 

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("alluvialfor.png", width = 2350, height = 2150, units = "px")
