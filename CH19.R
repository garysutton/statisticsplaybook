library(tidyverse)

dat1 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/seasons_stats.csv")

dim(dat1)

dat1 %>%
  select(Year, Player, Pos, Tm, G, PTS) -> dat1

dat1 %>%
  filter(Year <= 1999) -> dat1

glimpse(dat1)

dat1$Year <- as.factor(dat1$Year)
dat1$Pos <- as.factor(dat1$Pos)
dat1$Tm <- as.factor(dat1$Tm)

dat1 %>%
  distinct(Year, Player, Pos) -> test1
dim(test1)

dat1 %>%
  filter(Tm == "TOT") %>%
  tally()

dat1 %>%
  filter(Year == 1950, Player == "Ed Bartels") -> test2
print(test2)

dat1 %>%
  filter(Tm != "TOT") -> dat1
dim(dat1)

dat1 %>%
  group_by(Year, Player, Pos) %>%
  summarize(G = sum(G), PTS = sum(PTS)) -> dat2
dim(dat2)

dat2 %>%
  filter(Year == 1950, Player == "Ed Bartels") -> test3
print(test3)

dat1 %>%
  group_by(Year) %>%
  filter(Player == "George Mikan*") -> test4
colorDF::highlight(test4, test4$Year == 1950)

dat2 %>%
  group_by(Year) %>%
  filter(Player == "George Mikan*") -> test5
colorDF::highlight(test5, test5$Year == 1950)

arsenal::comparedf(test4, test5)

dat2 %>%
  mutate(PPG = format(PTS/G, digits = 1, nsmall = 1)) -> dat2
head(dat2)

dat2 %>%
  filter(PPG >= 2 & G >= 20) -> dat2
dim(dat2)

dat2 %>%
  select(Year, Player, PPG) -> dat2

dat2$PPG <- as.numeric(dat2$PPG)

dat2$Year_Player <- paste0(as.character(dat2$Year)," ", as.character(dat2$Player))

head(dat2, n = 3)
tail(dat2, n = 3)

dat2 %>%
  group_by(Year) %>%
  slice(which.max(PPG)) -> dat3

p1 <- ggplot(dat3, aes(x = Year_Player, y = PPG)) + 
  geom_bar(stat = "identity", color = "dodgerblue", fill = "dodgerblue") +
  geom_text(aes(label = PPG), 
            position = position_dodge(width = 0.8), vjust = -0.3,
            fontface = "bold", size = 2) +
  labs(title = "Leading Scorer (PPG) by Year", 
       subtitle = "1950 to 1999", 
       caption = "* Hall of Fame member",
       x = "Year and Leading Scorer",
       y = "Points per Game") +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90)) 
print(p1)

dat2 %>%
  group_by(Year) %>%
  mutate(z_ppg = round((PPG - mean(PPG)) / sd(PPG), digits = 1)) -> dat4a

head(dat4a, n = 3)
tail(dat4a, n = 3)

mean(dat4a$z_ppg)
var(dat4a$z_ppg)

dat4a %>%
  group_by(Year) %>%
  slice(which.max(z_ppg)) -> dat4b

p2 <- ggplot(dat4b, aes(x = Year_Player, y = z_ppg)) + 
  geom_bar(stat = "identity", color = "darkorange", fill = "darkorange") +
  geom_text(aes(label = z_ppg), 
            position = position_dodge(width = 0.8), vjust = -0.3,
            fontface = "bold", size = 2) +
  labs(title = "Leading Scorer (Z-Score) by Year", 
       subtitle = "1950 to 1999", 
       caption = "* Hall of Fame member",
       x = "Year and Leading Scorer",
       y = "Z-Score (standard deviations from mean)") +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90)) 
print(p2)

dat2 %>%
  group_by(Year) %>%
  mutate(sd_ppg = round((PPG / sd(PPG)), digits = 1)) -> dat5a
head(dat5a, n = 3)
tail(dat5a, n = 3)

var(dat5a$sd_ppg)

dat5a %>%
  group_by(Year) %>%
  slice(which.max(sd_ppg)) -> dat5b

p3 <- ggplot(dat5b, aes(x = Year_Player, y = sd_ppg)) + 
  geom_bar(stat = "identity", color = "salmon3", fill = "salmon3") +
  geom_text(aes(label = sd_ppg), 
            position = position_dodge(width = 0.8), vjust = -0.3,
            fontface = "bold", size = 2) +
  labs(title = "Leading Scorer (Standard Deviation Method) by Year", 
       subtitle = "1950 to 1999", caption = "* Hall of Fame member",
       x = "Year and Leading Scorer",
       y = "PPG / Standard Deviation") +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90)) 
print(p3)

dat2 %>%
  group_by(Year) %>%
  summarize(mean = round(mean(PPG), digits = 1)) -> dat6a
head(dat6a, n = 3)
tail(dat6a, n = 3)

left_join(dat3, dat6a, by = "Year") -> dat6b
head(dat6b, n = 3)
tail(dat6b, n = 3)

dat6b %>%
  mutate(c_ppg = PPG - mean) -> dat6b
head(dat6b, n = 3)
tail(dat6b, n = 3)

p4 <- ggplot(dat6b, aes(x = Year_Player, y = c_ppg)) + 
  geom_bar(stat = "identity", color = "aquamarine4", fill = "aquamarine4") +
  geom_text(aes(label = c_ppg), 
            position = position_dodge(width = 0.8), vjust = -0.3,
            fontface = "bold", size = 2) +
  labs(title = "Leading Scorer (Centering Method) by Year", 
       subtitle = "1950 to 2017", 
       caption = "* Hall of Fame member",
       x = "Year and Leading Scorer",
       y = "PPG - Annual Mean") +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90)) 
print(p4)

dat2 %>%
  group_by(Year) %>%
  mutate(r_ppg = round((PPG) / (max(PPG) - min(PPG)), digits = 1)) -> dat7a
head(dat7a, n = 3)
tail(dat7a, n = 3)

dat7a %>%
  group_by(Year) %>%
  slice(which.max(r_ppg)) -> dat7b
head(dat7b, n = 10)
tail(dat7b, n = 10)


