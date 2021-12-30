# Libraries

library(tidyverse)
library(worldfootballR)
library(ggtext)
library(extrafont)
library(MetBrewer)

# Scraping

data <- tm_matchday_table(country_name="England", start_year="2021", matchday=c(1:20))

# Data Wrangling

data <- data %>%
rename(Rank = rk) %>%
rename(Matchday = matchday) %>%
rename(Squad = squad)

data1 <- data %>%
filter(Squad == "Man Utd" | Squad == "Chelsea" | 
       Squad == "Liverpool" | Squad == "Man City" |
       Squad == "Spurs" | Squad == "Arsenal") 

data2 <- data %>%
filter(!Squad == "Man Utd") %>%
filter(!Squad == "Chelsea") %>%
filter(!Squad == "Liverpool") %>%
filter(!Squad == "Man City") %>%
filter(!Squad == "Arsenal") %>%
filter(!Squad == "Spurs")

# Custom Theme Function

theme_athletic <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
          panel.background = element_rect(colour = "#151515", fill = "#151515")) +
    theme(plot.title = element_text(colour = "white", size = 24, family = "Fried Chicken Bold", hjust = 0.5),
          plot.subtitle = element_markdown(colour = "#525252", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 12, hjust = 1),
          axis.title.x = element_text(colour = "white", face = "bold", size = 14),
          axis.title.y = element_text(colour = "white", face = "bold", size = 14),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white", size = 12)) +
    theme(panel.grid.major = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.grid.minor = element_line(colour = "#525252", size = 0.4, linetype = "dashed")) +
    theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"))
}

# Big 6

ggplot(data1, aes(x = Matchday, y = Rank, group = Squad)) +
geom_line(aes(colour = Squad), size = 2) +
geom_point(aes(colour = Squad), size = 4) +
scale_colour_manual(values = met.brewer(name="Juarez",n=6,type="discrete")) +
scale_y_reverse(breaks = c(1:20)) +
theme_athletic() +
labs(title = "Matchday Rankings",
subtitle = "Premier League Big 6 [2021/22]",
caption = "Created by Harsh Krishna\nData from Transfermrkt")

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("big6.png", width = 4000, height = 2000, units = "px")

# The other 14

ggplot(data2, aes(x = Matchday, y = Rank, group = Squad)) +
geom_line(aes(colour = Squad), size = 2) +
geom_point(aes(colour = Squad), size = 4) +
scale_colour_manual(values = met.brewer(name="Signac",n=14,type="discrete")) +
scale_y_reverse(breaks = c(1:20)) +
theme_athletic() +
labs(title = "Matchday Rankings",
subtitle = "Premier League Other 14 [2021/22]",
caption = "Created by Harsh Krishna\nData from Transfermrkt")

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("other14.png", width = 4000, height = 2000, units = "px")
