library(tidyverse)
library(grid)
library(ggplotify)
library(cowplot)
library(ggtext)
library(ggrepel)
library(StatsBombR)
library(ggshakeR)
library(MetBrewer)

Comp <- FreeCompetitions() %>%
  filter(competition_id == 2)
Matches <- FreeMatches(Comp)
StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = TRUE)
data  <- allclean(StatsBombData)

df <- data %>%
  rename("x" = "location.x",
         "y" = "location.y",
         "finalX" = "pass.end_location.x",
         "finalY" = "pass.end_location.y") %>%
  ggshakeR::calculate_epv(dataType = "statsbomb") %>%
  mutate(EPV = EPVEnd - EPVStart)
df$EPV[is.na(df$EPV)] <- 0

data1 <- df %>%
  group_by(player.name) %>%
  summarise(EPVPass = sum(EPV), Passes = n()) %>%
  mutate(Passes = Passes/100,
         EPVPass = EPVPass/Passes,
         EPVPass = percent_rank(EPVPass))

data2 <- df %>%
  group_by(pass.recipient.name) %>%
  summarise(EPVRec = sum(EPV), Passes = n()) %>%
  mutate(Passes = Passes/100,
         EPVRec = EPVRec/Passes,
         EPVRec = percent_rank(EPVRec))

data <- cbind(data1, data2) %>%
  tidyr::drop_na(player.name, pass.recipient.name) %>%
  select(player.name, EPVPass, EPVRec)

data <- data %>%
  mutate(colour = case_when(EPVPass <= 0.50 & EPVRec <= 0.50 ~ "Not Very Good",
                            EPVPass <= 0.50 & EPVRec >= 0.50 ~ "High Receiving EPV/100",
                            EPVPass >= 0.50 & EPVRec <= 0.50 ~ "High Pass EPV/100",
                            EPVPass >= 0.50 & EPVRec >= 0.50 ~ "High Pass & Rec EPV/100"))

df <- data %>%
  filter(EPVPass >= 0.9,
         EPVRec >= 0.9)

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
    theme(legend.title = element_blank(),
          legend.text = element_text(colour = "white")) +
    theme(aspect.ratio = 1)
}

g <- ggplot(data, aes(x = EPVPass, y = EPVRec)) +
  geom_point(aes(fill = colour), size = 2.5, shape = 21, colour = "black") +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 1.1, yend = 0.5), colour = "white", size = 0.5, linetype = "longdash") +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 0.5, yend = 1.1), colour = "white", size = 0.5, linetype = "longdash") +
  geom_text_repel(data = df, aes(x = EPVPass, y = EPVRec, label = player.name, angle = -45), fontface = "bold", colour = "white", size = 3.5) +
  scale_fill_manual(values = met.brewer(name = "Homer2", n = 4)) +
  theme_athletic() +
  labs(x = "(-) EPV Passed Ranking (+)",
       y = "(-) EPV Received Ranking (+)")

leg <- as.grob( ~ plot(get_legend(g + theme_athletic())))

g <- g + 
  theme(legend.position = "none")

grid.newpage()
plot <- print(g, vp = viewport(width = unit(0.8, "npc"),
                               height = unit(0.8, "npc"), angle = 45))
vp <- viewport(x = 0.82, y = 0.97, width = 0, height = 0)
pushViewport(vp)
grid.draw(leg)

plot <- grid.grab()
plot <- as.ggplot(plot)

plot +
  labs(title = "EPV Passed Vs. Received",
       subtitle = "Premier League [2003/04]",
       caption = "Data via StatsBomb\nCreated by @veryharshtakes") +
  theme(plot.title = element_text(colour = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(colour = "white", size = 10),
        plot.caption = element_text(colour = "white", size = 8, hjust = 1))

setwd("C:/Users/harsh_1mwi2o4/OneDrive/Documents")
ggsave("plot.png", bg = "#151515", width = 1500, height = 1500, units = "px")
