# Libraries 

library(tidyverse)
library(worldfootballR)
library(ggtext)
library(extrafont)

# Data 

data <- fb_big5_advanced_season_stats(season_end_year = 2022, stat_type = "standard", team_or_player = "player")

# Data Wrangling 

data <- data %>%
  filter(Squad == "Arsenal")

data1 <- data %>%
  mutate(Ages = 2022 - Born) %>%
  mutate(Matches = ifelse(MP_Playing <= 10, "<10 Played",
                          ifelse(MP_Playing >= 20, ">20 Played",
                                 ifelse(MP_Playing > 10 && MP_Playing < 20, ">10 & <20 Played", ">10 & <20 Played")))) %>%
  mutate(AgeGroups = ifelse(Ages <= 23, "Youth (u23)",
                            ifelse(Ages >= 30, "Veteran (+30)",
                                   ifelse(Ages >= 24 && Ages <= 29, "Peak (24-29)", "Peak (24-29)")))) %>% 
  mutate(Position = case_when(Pos == "DF" |
                                Pos == "DF,MF" |
                                Pos == "DF,FW" ~ "Defenders",
                              Pos == "MF" |
                                Pos == "MF,FW" |
                                Pos == "MF,DF" ~ "Midfielders",
                              Pos == "FW" |
                                Pos == "FW,MF" |
                                Pos == "FW,DF" ~ "Forwards",
                              Pos == "GK" |
                                Pos == "GK,MF" ~ "Goalkeepers")) %>%
  select(Player, AgeGroups, Position, Matches)

list_df <- split(data1, list(data1$Position, data1$AgeGroups))
list2env(setNames(list_df,paste0("data", seq(list_df))),envir = .GlobalEnv)

index_col <- function(x) {
  x <- x %>%
    mutate(index = 1:nrow(x))
}

#' You might find an error in some of these. 
#' That's only because some datasets have 0 observations. 

data1 <- index_col(data1)
data2 <- index_col(data2)
data3 <- index_col(data3)
data4 <- index_col(data4)
data5 <- index_col(data5)
data6 <- index_col(data6)
data7 <- index_col(data7)
data8 <- index_col(data8)
data9 <- index_col(data9)
data10 <- index_col(data10)
data11 <- index_col(data11)
data12 <- index_col(data12)

data1 <- rbind(data1, data2, data3, data4, 
               data5, data6, data7, data8, 
               data9, data10, data11, data12)

data1$AgeGroups <- factor(data1$AgeGroups, levels = c("Youth (u23)", "Peak (24-29)", "Veteran (+30)"))
data1$Position <- factor(data1$Position, levels = c("Defenders", "Midfielders", "Forwards", "Goalkeepers"))

# Custom Theme Function

theme_custom <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#14171A", fill = "#14171A"),
          panel.background = element_rect(colour = "#14171A", fill = "#14171A")) +
    theme(plot.title = element_text(colour = "white", size = 20, hjust = 0.5, face = "bold"),
          plot.subtitle = element_markdown(colour = "#525252", size = 16, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 10, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    theme(strip.background = element_blank(),
          strip.text = element_text(colour = "grey", face = "bold", size = 15)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(panel.grid.major.x = element_blank(),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"))
}

# Plot

data1 %>%
  ggplot(aes(x = 1, y = index)) +
  geom_text(aes(label = Player, colour = Matches), fontface = "bold") +
  scale_colour_manual(values = c("white", "#F1C40F", "#CB4335")) +
  geom_rect(aes(xmin = 0, xmax = 2, ymin = 0, ymax = 9), fill = NA, colour = "#525252", linetype = "dashed", size = 1) +
  scale_y_reverse() +
  facet_grid(Position~AgeGroups, scales = "free") +
  theme_custom() +
  theme(legend.position = "top") +
  labs(title = "Arsenal Squad Composition",
       subtitle = "Premier League 2021/22",
       caption = "Data from FBref. Inspired by @Worville. Created by @veryharshtakes")

# Save

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("composition.png", bg = "#14171A", width = 4000, height = 2500, units = "px")
