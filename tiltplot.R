library(StatsBombR)

Comp <- FreeCompetitions() %>%
  filter(competition_id == 2)
Matches <- FreeMatches(Comp)
StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = TRUE)
data  <- allclean(StatsBombData)

library(tidyverse)
library(worldfootballR)
library(grid)
library(ggplotify)
library(cowplot)
library(ggtext)
library(MetBrewer)
library(ggrepel)
library(ggshakeR)

data <- data %>%
  rename("x" = "location.x",
         "y" = "location.y",
         "finalX" = "pass.end_location.x",
         "finalY" = "pass.end_location.y")

data <- data %>%
  ggshakeR::calculate_threat(dataType = "statsbomb")
data <- data %>%
  mutate(xT = xTEnd - xTStart)
data$xT[is.na(data$xT)] <- 0

data1 <- data %>%
  group_by(player.name) %>%
  summarise(xTPass = sum(xT))
data2 <- data %>%
  group_by(pass.recipient.name) %>%
  summarise(xTRec = sum(xT))

data <- cbind(data1, data2) %>%
  tidyr::drop_na(player.name, pass.recipient.name)

theme_athletic <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
          panel.background = element_rect(colour = "#151515", fill = "#151515")) +
    theme(plot.title = element_text(colour = "white", size = 16),
          plot.subtitle = element_markdown(colour = "white", size = 12),
          plot.caption = element_text(colour = "white", size = 10, hjust = 0),
          axis.title.x = element_text(colour = "white", size = 8),
          axis.title.y = element_text(colour = "white", size = 8, angle = -90),
          axis.text.x = element_text(colour = "white", size = 6),
          axis.text.y = element_text(colour = "white", size = 6)) +
    theme(panel.grid.major = element_line(colour = "#525252", size = 0.2, linetype = "dashed"),
          panel.grid.minor = element_line(colour = "#525252", size = 0.2, linetype = "dashed")) +
    theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.2, linetype = "dashed")) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white")) +
    theme(aspect.ratio = 1)
}

g <- ggplot(data, aes(x = ProgPassesp90, y = ProgCarriesp90, colour = Comp)) +
  geom_point(size = 2) +
  geom_text_repel(data = df, aes(x = ProgPassesp90, y = ProgCarriesp90, label = Player, angle = -45), colour = "white", size = 3) +
  scale_colour_manual(values = met.brewer(name = "Signac", n = 5)) +
  theme_athletic() +
  labs(x = "Progressive Passes / 90",
       y = "Progressive Carries / 90")

leg <- as.grob( ~ plot(get_legend(g + theme_athletic())))

g <- g + 
  theme(legend.position = "none")

grid.newpage()
plot <- print(g, vp = viewport(width = unit(0.8, "npc"),
                               height = unit(0.8, "npc"), angle = 45))
vp <- viewport(x = 0.87, y = 0.9, width = 0, height = 0)
pushViewport(vp)
grid.draw(leg)

plot <- grid.grab()
plot <- as.ggplot(plot)

plot +
  labs(title = "Progressive Passes Vs. Carries",
       subtitle = "Big 5 Leagues [2021/22]",
       caption = "More than 30 90's played\nCreated by @veryharshtakes") +
  theme(plot.title = element_text(colour = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(colour = "white", size = 10),
        plot.caption = element_text(colour = "white", size = 8, hjust = 1))

setwd("C:/Users/harsh_1mwi2o4/OneDrive/Documents")
ggsave("plot.png", bg = "#151515", width = 1500, height = 1500, units = "px")
