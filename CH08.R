library(tidyverse)
library(janitor)
library(patchwork)
library(png)

bucks <- readPNG("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/bucks.png",                  
                 native = TRUE)

hawks <- readPNG("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/hawks.png",
                 native = TRUE)

hornets <- readPNG("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/hornets.png",
                   native = TRUE)

nba <- readPNG("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba.png",
               native = TRUE)

pbp <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/pbp.csv")

glimpse(pbp) 

pbp$data_set <- as.factor(pbp$data_set)

levels(pbp$data_set)

pbp %>% 
  filter(data_set != "2019-20 Playoffs") -> pbp
dim(pbp) 

pbp %>%
  mutate(play_length2 = str_sub(play_length, -2, -1)) -> pbp
pbp$play_length2 <- as.numeric(pbp$play_length2)

pbp$event_type <- as.factor(pbp$event_type)
levels(pbp$event_type)

head(pbp$play_length)
head(pbp$play_length2)

pbp %>%
  group_by(event_type) %>%
  tally() -> tbl1
print(tbl1)

pbp$team <- as.factor(pbp$team)
summary(pbp$team) 

summary(pbp$points)
                                                
pbp %>% 
  filter(team == "MIL",
         play_length2 >= 5 & play_length2 <= 24,
         event_type == "shot" | event_type == "miss") -> MIL
dim(MIL)

MIL %>%
  select(event_type, points, play_length2) -> MIL
dim(MIL)

MIL %>%
  group_by(play_length2) %>%
  summarize(avg = mean(points)) -> MILx
print(MILx)

MIL %>%
  tabyl(play_length2, event_type) -> MILy

MILy %>%
  select(play_length2, shot, miss) %>%
  mutate(fg_pct = shot / (shot + miss)*100) -> MILy
MILy$fg_pct <- round(MILy$fg_pct, digits = 2)
print(MILy)

MILp1 <- ggplot(MILx, aes(x = play_length2, y = avg, group = 1)) +
  geom_line(aes(y = avg), color = "darkgreen", size = 2) +
  geom_point(color = "wheat2", size = 3) +
  labs(title = "Points Scored per Second Increment",
       subtitle = "2019-20 Milwaukee Bucks",
       caption = "regular season only",
       x = "Number of Seconds into Possession",
       y = "Average Number of Points Scored") +
  geom_smooth(method = lm, color = "blue", se = FALSE) +
  geom_smooth(method = lm, color = "gold", 
              data = MILx[MILx$play_length2 < 13,], se = FALSE) +
  geom_smooth(method = lm, color = "purple", 
              data = MILx[MILx$play_length2 > 11,], se = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  inset_element(bucks, left = 0.80, bottom = 0.80, 
                right = 0.95, top = 0.95) 

MILp2 <- ggplot(MILy, aes(x = play_length2, y = fg_pct, group = 1)) +
  geom_line(aes(y = fg_pct), color = "darkgreen", size = 2) +
  geom_point(color = "wheat2", size = 3) +
  labs(title = "Field Goal Percentage per Second Increment", 
       subtitle = "2019-20 Milwaukee Bucks",
       caption = "regular season only",
       x = "Number of Seconds into Possession",
       y = "Field Goal Percentage") +
  geom_smooth(method = lm, color = "blue", se = FALSE) +
  geom_smooth(method = lm, color = "gold", 
              data = MILy[MILy$play_length2 < 13,], se = FALSE) +
  geom_smooth(method = lm, color = "purple", 
              data = MILy[MILy$play_length2 > 11,], se = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  inset_element(bucks, left = 0.80, bottom = 0.80, 
                right = 0.95, top = 0.95) 

MILp1 + MILp2 + plot_layout(ncol = 2)

pbp %>% 
  filter(team == "ATL",
         play_length2 >= 5 & play_length2 <= 24,
         event_type == "shot" | event_type == "miss") -> ATL
dim(ATL) 

ATL %>%
  group_by(event_type) %>%
  tally()

MIL %>%
  group_by(event_type) %>%
  tally()

ATL %>%
  select(event_type, points, play_length2) -> ATL
dim(ATL)

ATL %>%
  group_by(play_length2) %>%
  summarize(avg = mean(points)) -> ATLx
print(ATLx)

ATL %>%
  tabyl(play_length2, event_type) -> ATLy
ATLy <- select(ATLy, play_length2, shot, miss)
ATLy %>%
  mutate(fg_pct = shot / (shot + miss)*100) -> ATLy
ATLy$fg_pct <- round(ATLy$fg_pct, digits = 2)
print(ATLy)

ATLp1 <- ggplot(ATLx, aes(x = play_length2, y = avg, group = 1)) +
  geom_line(aes(y = avg), color = "red", size = 2) +
  geom_point(color = "black", size = 3) +
  labs(title = "oints Scored per Second Increment",
       subtitle = "2019-20 Atlanta Hawks",
       caption = "regular season only",
       x = "Number of Seconds into Possession", 
       y = "Average Number of Points Scored") +
  geom_smooth(method = lm, color = "blue", se = FALSE) +
  geom_smooth(method = lm, color = "gold", 
              data = ATLx[ATLx$play_length2 < 13,], se = FALSE) +
  geom_smooth(method = lm, color = "purple", 
              data = ATLx[ATLx$play_length2 > 11,], se = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  inset_element(hawks, left = 0.78, bottom = 0.78, 
                right = 0.95, top = 0.95) 

ATLp2 <- ggplot(ATLy, aes(x = play_length2, y = fg_pct, group = 1)) +
  geom_line(aes(y = fg_pct), color = "red", size = 2) +
  geom_point(color = "black", size = 3) +
  labs(title = "Field Goal Percentage per Second Increment",
       subtitle = "2019-20 Atlanta Hawks",
       caption = "regular season only",
       x = "Number of Seconds into Possession", 
       y = "Field Goal Percentage") +
  geom_smooth(method = lm, color = "blue", se = FALSE) +
  geom_smooth(method = lm, color = "gold", 
              data = ATLy[ATLy$play_length2 < 13,], se = FALSE) +
  geom_smooth(method = lm, color = "purple", 
              data = ATLy[ATLy$play_length2 > 11,], se = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  inset_element(hawks, left = 0.62, bottom = 0.78, 
                right = 0.79, top = 0.95) 

ATLp1 + ATLp2 + plot_layout(ncol = 2)

pbp %>% 
  filter(team == "CHA",
         play_length2 >= 5 & play_length2 <= 24,
         event_type == "shot" | event_type == "miss") -> CHA

CHA %>%
  select(event_type, points, play_length2) -> CHA
dim(CHA) 

CHA %>%
  group_by(event_type) %>%
  tally()

CHA %>%
  group_by(play_length2) %>%
  summarise(avg = mean(points)) -> CHAx
print(CHAx)

CHA %>%
  tabyl(play_length2, event_type) -> CHAy
CHAy %>%
  select(play_length2, shot, miss) -> CHAy
CHAy %>%
  mutate(fg_pct = shot / (shot + miss)*100) -> CHAy
CHAy$fg_pct <- round(CHAy$fg_pct, digits = 2)
print(CHAy)

CHAp1 <- ggplot(CHAx, aes(x = play_length2, y = avg, group = 1)) +
  geom_line(aes(y = avg), color = "cyan3", size = 2) +
  geom_point(color = "black", size = 3) +
  labs(title = "Points Scored per Second Increment",
       subtitle = "2019-20 Charlotte Hornets",
       caption = "regular season only",
       x = "Number of Seconds into Possession", 
       y = "Average Number of Points Scored") +
  geom_smooth(method = lm, color = "blue", se = FALSE) +
  geom_smooth(method = lm, color = "gold", 
              data = CHAx[CHAx$play_length2 < 13,], se = FALSE) +
  geom_smooth(method = lm, color = "purple", 
              data = CHAx[CHAx$play_length2 > 11,], se = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  inset_element(hornets, left = 0.73, bottom = 0.73, 
                right = 0.95, top = 0.95) 

CHAp2 <- ggplot(CHAy, aes(x = play_length2, y = fg_pct, group = 1)) +
  geom_line(aes(y = fg_pct), color = "cyan3", size = 2) +
  geom_point(color = "black", size = 3) +
  labs(title = "Field Goal Percentage per Second Increment",
       subtitle = "2019-20 Charlotte Hornets",
       caption = "regular season only",
       x = "Number of Seconds into Possession", 
       y = "Field Goal Percentage") +
  geom_smooth(method = lm, color = "blue", se = FALSE) +
  geom_smooth(method = lm, color = "gold", 
              data = CHAy[CHAy$play_length2 < 13,], se = FALSE) +
  geom_smooth(method = lm, color = "purple", 
              data = CHAy[CHAy$play_length2 > 11,], se = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  inset_element(hornets, left = 0.73, bottom = 0.73, 
                right = 0.95, top = 0.95) 

CHAp1 + CHAp2 + plot_layout(ncol = 2)

pbp %>% 
  filter(team != "",
         play_length2 >= 5 & play_length2 <= 24,
         event_type == "shot" | event_type == "miss") -> NBA

NBA %>%
  select(event_type, points, play_length2) -> NBA
dim(NBA) 

NBA %>%
  group_by(play_length2) %>%
  summarise(avg = mean(points)) -> NBAx
print(NBAx)

NBA %>%
  tabyl(play_length2, event_type) -> NBAy
NBAy %>%
  select(play_length2, shot, miss) -> NBAy
NBAy %>%
  mutate(fg_pct = shot / (shot + miss)*100) -> NBAy
NBAy$fg_pct <- round(NBAy$fg_pct, digits = 2)
print(NBAy)

NBAp1 <- ggplot(NBAx, aes(x = play_length2, y = avg, group = 1)) +
  geom_line(aes(y = avg), color = "red", size = 2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Points Scored per Second Increment",
       subtitle = "2019-20 NBA Regular Season (all teams)",
       x = "Number of Seconds into Possession", 
       y = "Average Number of Points Scored") +
  geom_smooth(method = lm, color = "blue", se = FALSE) +
  geom_smooth(method = lm, color = "gold", 
              data = NBAx[NBAx$play_length2 < 13,], se = FALSE) +
  geom_smooth(method = lm, color = "purple", 
              data = NBAx[NBAx$play_length2 > 11,], se = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  inset_element(nba, left = 0.65, bottom = 0.65, right = 0.95, top = 0.95) 

NBAp2 <- ggplot(NBAy, aes(x = play_length2, y = fg_pct, group = 1)) +
  geom_line(aes(y = fg_pct), color = "red", size = 2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Field Goal Percentage per Second Increment",
       subtitle = "2019-20 NBA Regular Season (all teams)",
       x = "Number of Seconds into Possession", 
       y = "Field Goal Percentage") +
  geom_smooth(method = lm, color = "blue", se = FALSE) +
  geom_smooth(method = lm, color = "gold", 
              data = NBAy[NBAy$play_length2 < 13,], se = FALSE) +
  geom_smooth(method = lm, color = "purple",
              data = NBAy[NBAy$play_length2 > 11,], se = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  inset_element(nba, left = 0.65, bottom = 0.65, right = 0.95, top = 0.95) 

NBAp1 + NBAp2 + plot_layout(ncol = 2)



