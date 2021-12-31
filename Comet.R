# Libraries

library(tidyverse)
library(ggforce)
library(worldfootballR)
library(ggtext)
library(extrafont)
library(MetBrewer)
library(ggrepel)

# Scraping 

data1 <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2021", tier = "1st", stat_type = "defense")
data2 <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st", stat_type = "defense")

# Data Wrangling

data1 <- data1 %>%
filter(Team_or_Opponent == "team") %>%
filter(!Squad == "Fulham") %>%
filter(!Squad == "Sheffield Utd") %>%
filter(!Squad == "West Brom") %>%
rename(fint = "Att 3rd_Pressures") %>%
rename("Succ1" = "_percent_Pressures") %>%
mutate("Fint1" = fint/Mins_Per_90)

data2 <- data2 %>%
filter(Team_or_Opponent == "team") %>%
filter(!Squad == "Brentford") %>%
filter(!Squad == "Norwich City") %>%
filter(!Squad == "Watford") %>%
rename(fint = "Att 3rd_Pressures") %>%
rename("Succ2" = "_percent_Pressures") %>%
mutate("Fint2" = fint/Mins_Per_90)

data1 <- data1[, c("Squad", "Fint1", "Succ1")]
data2 <- data2[, c("Fint2", "Succ2")]

data <- cbind(data1, data2)
Squad <- data$Squad

data <- subset(data, select = -Squad)

# Determining optimal number of clusters for k-means

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(data)

# Implememnting k-means

kmean <- kmeans(data, 4)
data$Squad <- Squad
data$Cluster <- kmean$cluster

data$Cluster[data$Cluster == 1] <- "Cluster 1"
data$Cluster[data$Cluster == 2] <- "Cluster 2"
data$Cluster[data$Cluster == 3] <- "Cluster 3"
data$Cluster[data$Cluster == 4] <- "Cluster 4"

# Custom theme function

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

# Plotting

ggplot(data) +
geom_link(aes(x = Fint1, y = Succ1, xend = Fint2, yend = Succ2, 
          colour = Cluster, alpha = stat(index), size = stat(index)), show.legend = FALSE) +
geom_point(aes(x = Fint2, y = Succ2, size=10, color=Cluster, fill=Cluster), shape=21) +
scale_colour_manual(values = met.brewer(name = "Homer2", n = 4, type = "discrete")) +
scale_fill_manual(values = met.brewer(name = "Homer2", n = 4, type = "discrete")) +
geom_text_repel(data = data, aes(x = Fint2, y = Succ2, label = Squad),
                   box.padding   = 1.5, 
                   point.padding = 1.5,
                   segment.color = "white", 
                   alpha = 0.8,
                   colour = "white") +
theme_athletic() +
theme(legend.position = "none") +
labs(x = "Final Third Pressures", y = "Successful Pressure %",
title = "Pressing Performance - Premier League",
subtitle = "2020/21 vs 2021/22",
caption = "Inspired by @johnspacemuller\nData from FBref\nCreated by Harsh Krishna")

# Save

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("comet.png", width = 4000, height = 2000, units = "px")
