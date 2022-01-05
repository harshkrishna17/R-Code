# Libraries

library(tidyverse)
library(worldfootballR)
library(extrafont)
library(ggtext)
library(MetBrewer)
library(ggpubr)

# Scraping 

data <- understat_league_season_shots(league = "Ligue 1", season_start_year = 2021)

# Data Wrangling 

data$minute <- as.numeric(data$minute)

data <- data %>%
mutate(Period = ifelse(minute <= 15, "0-15",
                       ifelse(minute > 15 & minute <= 30, "15-30",
                              ifelse(minute > 30 & minute <= 45, "30-45", 
                                     ifelse(minute > 45 & minute <= 60, "45-60",
                                            ifelse(minute > 60 & minute <= 75, "60-75",        
                                                   ifelse(minute > 75 & minute < 100, "75-90", NA)))))))

data1 <- data %>%
filter(h_a == "h") %>%
group_by(home_team, Period) %>%
summarise(xG = sum(xG)) %>%
spread(Period, xG) %>%
ungroup() %>%
rename(Squad = home_team)

data2 <- data %>%
filter(h_a == "a") %>%
group_by(away_team, Period) %>%
summarise(xG = sum(xG)) %>%
spread(Period, xG) %>%
ungroup() %>%
rename(Squad = away_team)

data3 <- data %>%
filter(h_a == "h") %>%
group_by(away_team, Period) %>%
summarise(xG = sum(xG)) %>%
spread(Period, xG) %>%
ungroup() %>%
rename(Squad = away_team)

data4 <- data %>%
filter(h_a == "a") %>%
group_by(home_team, Period) %>%
summarise(xG = sum(xG)) %>%
spread(Period, xG) %>%
ungroup() %>%
rename(Squad = home_team)

Squad <- data1$Squad

data1 <- subset(data1, select = -Squad)
data2 <- subset(data2, select = -Squad)
data3 <- subset(data3, select = -Squad)
data4 <- subset(data4, select = -Squad)

for(i in 1:ncol(data1)) {
       data1[, i] <- data1[, i] + data2[, i]
}

for(i in 1:ncol(data3)) {
       data3[, i] <- data3[, i] + data4[, i]
}

data1$Squad <- Squad
data3$Squad <- Squad

data1 <- data1 %>% pivot_longer(!Squad, names_to = "Period", values_to = "xG")
data3 <- data3 %>% pivot_longer(!Squad, names_to = "Period", values_to = "xG")

df1 <- data1 %>%
filter(Squad == "Angers")
df1 <- df1[order(df1$Period, decreasing = TRUE),]
data1$Period <- factor(data1$Period, levels= unique(df1$Period))

df3 <- data3 %>%
filter(Squad == "Angers")
df3 <- df3[order(df3$Period, decreasing = TRUE),]
data3$Period <- factor(data3$Period, levels= unique(df3$Period))

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

p1 <- ggplot(data1, aes(x = reorder(Squad, xG), y = xG, fill = Period)) +
geom_bar(stat="identity") +
scale_fill_manual(values = met.brewer(name = "Signac", n = 6, type = "discrete")) +
labs(title = "xG For", x = "Squad", y = "xG") +
coord_flip() +
theme_athletic()

p2 <- ggplot(data3, aes(x = reorder(Squad, -xG), y = xG, fill = Period)) +
geom_bar(stat="identity") +
scale_fill_manual(values = met.brewer(name = "Signac", n = 6, type = "discrete")) +
labs(title = "xG Against", x = "Squad", y = "xGA") +
coord_flip() +
theme_athletic() +
theme(axis.title.y = element_blank())

# Combining

fig <- ggarrange(p1, p2, ncol = 2, nrow = 1)
fig +
theme_athletic() +
labs(title = "Ligue 1 xG Performance by Time Period",
subtitle = "2021/22 Season", 
caption = "Data from Understat\nCreated by Harsh Krishna")

# Save

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("bar1.png", width = 4000, height = 2000, units = "px")
