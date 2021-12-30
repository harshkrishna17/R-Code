# Libraries

library(tidyverse)
library(viridis)

# Data

setwd("C:/Users/harsh_1mwi2o4/Downloads")
data <- read.csv("dort.csv")

# Manipulation

data1 <- data %>%
  mutate(location.x = x * 1.2) %>%
  mutate(pass.end_location.x = endX * 1.2) %>%
  mutate(pass.end_location.y = endY * 0.8) %>%
  mutate(location.y = y * 0.8) %>%
  mutate(ystart = pass.end_location.y - location.y) %>%
  mutate(xstart = pass.end_location.x - location.x) %>%
  mutate(slope = ystart/xstart) %>%
  mutate(pass.angle = atan(slope)) %>%
  mutate(starty = ystart*ystart) %>%
  mutate(startx = xstart*xstart) %>%
  mutate(length = startx + starty) %>%
  mutate(pass.length = sqrt(length))

round.angle = 15

data2 <- data1 %>%
  mutate(angle.round=round(pass.angle*360/pi/round.angle)*round.angle)

sonar <- data2 %>%
  group_by(playerId)%>%
  mutate(N=n())%>%
  ungroup()%>%
  group_by(playerId, angle.round)%>%
  mutate(n.angle=n()/N)%>%
  ungroup()%>%
  group_by(playerId)%>%
  mutate(maxN=max(n.angle),
         angle.norm=n.angle/maxN)%>%
  ungroup()%>%
  group_by(angle.round, playerId,N)%>%
  summarize(angle.norm=mean(angle.norm),
            distance=mean(pass.length),
            distance=ifelse(distance>30, 30,distance))

# Plotting 

ggplot(data = sonar)+
  geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity")+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
  coord_polar(start = 80, direction = 1)+
  scale_fill_viridis("Distance (yards)", limits=c(0,30), na.value="#FDE725FF")+
  labs(x='', y='',title= "Sonar")+
  theme(plot.title = element_text(hjust=0.5),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))
