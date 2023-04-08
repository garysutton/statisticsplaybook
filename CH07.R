
library(tidyverse)
library(ggpubr)
library(sqldf)
library(effsize)
library(patchwork)

df1 <- read_csv('/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba_boxscore_1819.csv')
df2 <- read_csv('/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba_boxscore_1920.csv')

df3 <- filter(df1, VENUE == "R")

df3 <- select(df3, DATASET, TEAM, VENUE, FT, FTA, PF)

df3 %>% rename(dataset = DATASET, teamR = TEAM, venueR = VENUE, ftR = FT, ftaR = FTA,
               pfR = PF) -> df3

df3$dataset <- factor(df3$dataset)
df3$teamR <- factor(df3$teamR)
df3$venueR <- factor(df3$venueR)

df4 <- filter(df1, VENUE == "H")

df4 <- select(df4, TEAM, VENUE, FT, FTA, PF)

df4 %>% rename(teamH = TEAM, venueH = VENUE, ftH = FT, ftaH = FTA,
               pfH = PF) -> df4

df4$teamH <- factor(df4$teamH)
df4$venueH <- factor(df4$venueH)

dim(df3) 
dim(df4)

fouls1819 <- cbind(df3, df4)
dim(fouls1819) 

fouls1819 %>% 
  filter(dataset == "NBA 2018-2019 Regular Season") -> fouls1819reg

sum(fouls1819reg$pfR) - sum(fouls1819reg$pfH)
mean(fouls1819reg$pfR) - mean(fouls1819reg$pfH)
mean(fouls1819reg$ftaR) - mean(fouls1819reg$ftaH)
sum(fouls1819reg$ftR) / sum(fouls1819reg$ftaR) 
sum(fouls1819reg$ftH) / sum(fouls1819reg$ftaH) 

options(scipen = 999)

t.test(fouls1819reg$pfR, fouls1819reg$pfH)

t.test(fouls1819reg$ftaR, fouls1819reg$ftaH)

temp1 <- select(fouls1819reg, c(pfR, pfH))
temp1 %>%
  pivot_longer(cols = c(pfR, pfH),
               names_to = "team",
               values_to = "fouls") -> temp1
head(temp1)

temp1.text <- c("Home Team", "Road Team")
p1 <- ggplot(temp1, aes(x = team, y = fouls, fill = team)) + 
  geom_boxplot() +
  labs(title = "Personal Foul Calls: Home vs. Road Teams", 
       subtitle = "2018-19 Regular Season",
       x = "", 
       y = "Personal Fouls per Game") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(legend.position = "none") +
  scale_x_discrete(labels = temp1.text) +
  theme(plot.title = element_text(face = "bold")) +
  stat_compare_means(method = "t.test", label.x = 1.4, label.y = 34)

temp2 <- select(fouls1819reg, c(5,10)) 
temp2 %>%
  pivot_longer(cols = c(ftaR, ftaH),
               names_to = "team",
               values_to = "ftattempts") -> temp2

temp2.text <- c("Home Team", "Road Team")
p2 <- ggplot(temp2, aes(x = team, y = ftattempts, fill = team)) + 
  geom_boxplot() +
  labs(title = "Free Throw Attempts: Home vs. Road Teams", 
       subtitle = "2018-19 Regular Season",
       x = "", 
       y = "Free Throw Attempts per Game") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(legend.position = "none") +
  scale_x_discrete(labels = temp2.text) +
  theme(plot.title = element_text(face = "bold")) +
  stat_compare_means(method = "t.test", label.x = 1.4, label.y = 48)

p1 + p2 + plot_layout(ncol = 2)

sqldf("select * from fouls1819reg WHERE pfH >= 34 OR pfR >= 34")

sqldf("select * from fouls1819reg WHERE ftaH > 50 OR ftaR > 50")

fouls1819 %>% 
  filter(dataset == "NBA 2019 Playoffs") -> fouls1819post

sum(fouls1819post$pfR) - sum(fouls1819post$pfH)
mean(fouls1819post$pfR) - mean(fouls1819post$pfH)
mean(fouls1819post$ftaR) - mean(fouls1819post$ftaH)
sum(fouls1819post$ftR) / sum(fouls1819post$ftaR) 
sum(fouls1819post$ftH) / sum(fouls1819post$ftaH) 

t.test(fouls1819post$pfR, fouls1819post$pfH)

t.test(fouls1819post$ftaR, fouls1819post$ftaH)

temp3 <- select(fouls1819post, c(6,11)) 
temp3 %>%
  pivot_longer(cols = c(pfR, pfH),
               names_to = "team",
               values_to = "fouls") -> temp3

temp3.text <- c("Home Team", "Road Team")
p3 <- ggplot(temp3, aes(x = team, y = fouls, fill = team)) + geom_boxplot() +
  labs(title = "Personal Foul Calls: Home vs. Road Teams", 
       subtitle = "2019 Playoffs",
       x = "", 
       y = "Personal Fouls per Game") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(legend.position = "none") +
  scale_x_discrete(labels = temp1.text) +
  theme(plot.title = element_text(face = "bold")) +
  stat_compare_means(method = "t.test", label.x = 1.4, label.y = 34)

temp4 <- select(fouls1819post, c(5,10)) 
temp4 %>%
  pivot_longer(cols = c(ftaR, ftaH),
               names_to = "team",
               values_to = "ftattempts") -> temp4
head(temp4)

temp4.text <- c("Home Team", "Road Team")
p4 <- ggplot(temp4, aes(x = team, y = ftattempts, fill = team)) + geom_boxplot() +
  labs(title = "Free Throw Attempts: Home vs. Road Teams", 
       subtitle = "2019 Playoffs",
       x = "", 
       y = "Free Throw Attempts per Game") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, color = "white", fill = "white") + 
  theme(legend.position = "none") +
  scale_x_discrete(labels = temp2.text) +
  theme(plot.title = element_text(face = "bold")) +
  stat_compare_means(method = "t.test", label.x = 1.4, label.y = 48)

p3 + p4 + plot_layout(ncol = 2)

cohen.d(fouls1819reg$pfR, fouls1819reg$pfH)
cohen.d(fouls1819post$pfR, fouls1819post$pfH)

cohen.d(fouls1819reg$ftaR, fouls1819reg$ftaH)
cohen.d(fouls1819post$ftaR, fouls1819post$ftaH)

df5 <- filter(df2, VENUE == "R")

df5 <- select(df5, DATASET, GAME_ID, TEAM, VENUE, FT, FTA, PF)

df5 %>% rename(dataset = DATASET, gameID = GAME_ID, teamR = TEAM, venueR = VENUE, ftR = FT, ftaR = FTA,
               pfR = PF) -> df5

df5$dataset <- factor(df5$dataset)
df5$teamR <- factor(df5$teamR)
df5$venueR <- factor(df5$venueR)

df6 <- filter(df2, VENUE == "H")

df6 <- select(df6, TEAM, VENUE, FT, FTA, PF)

df6 %>% rename(teamH = TEAM, venueH = VENUE, ftH = FT, ftaH = FTA,
               pfH = PF) -> df6

df6$teamH <- factor(df6$teamH)
df6$venueH <- factor(df6$venueH)

fouls1920 <- cbind(df5, df6)
dim(df5) 
dim(df6) 
dim(fouls1920) 

fouls1920 %>% 
  filter(gameID <= 21900973) -> fouls1920a

sum(fouls1920a$pfR) - sum(fouls1920a$pfH)
mean(fouls1920a$pfR) - mean(fouls1920a$pfH)
mean(fouls1920a$ftaR) - mean(fouls1920a$ftaH) 
sum(fouls1920a$ftR) / sum(fouls1920a$ftaR) 
sum(fouls1920a$ftH) / sum(fouls1920a$ftaH) 

t.test(fouls1920a$pfR, fouls1920a$pfH)

t.test(fouls1920a$ftaR, fouls1920a$ftaH)

temp5 <- select(fouls1920a, c(7,12)) 
temp5 %>%
  pivot_longer(cols = c(pfR, pfH),
               names_to = "team",
               values_to = "fouls") -> temp5
head(temp5)

temp5.text <- c("Home Team", "Road Team")
p5 <- ggplot(temp5, aes(x = team, y = fouls, fill = team)) + geom_boxplot() +
  labs(title = "Personal Foul Calls: Home vs. Road Teams", 
       subtitle = "2019-20 Regular Season (pre-COVID)",
       x = "", 
       y = "Personal Fouls per Game") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(legend.position = "none") +
  scale_x_discrete(labels = temp1.text) +
  theme(plot.title = element_text(face = "bold")) +
  stat_compare_means(method = "t.test", label.x = 1.4, label.y = 43)

temp6 <- select(fouls1920a, c(6,11)) 
temp6 %>%
  pivot_longer(cols = c(ftaR, ftaH),
               names_to = "team",
               values_to = "ftattempts") -> temp6
head(temp6)

temp6.text <- c("Home Team", "Road Team")
p6 <- ggplot(temp6, aes(x = team, y = ftattempts, fill = team)) + geom_boxplot() +
  labs(title = "Free Throw Attempts: Home vs. Road Teams", 
       subtitle = "2019-20 Regular Season (pre-COVID)",
       x = "", 
       y = "Free Throw Attempts per Game") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(legend.position = "none") +
  scale_x_discrete(labels = temp1.text) +
  theme(plot.title = element_text(face = "bold")) +
  stat_compare_means(method = "t.test", label.x = 1.4, label.y = 48)

p5 + p6 + plot_layout(ncol = 2)

fouls1920 %>% 
  filter(dataset == "NBA 2019-2020 Regular Season" & gameID >= 21901231) -> fouls1920b

sum(fouls1920b$pfR) - sum(fouls1920b$pfH)  
mean(fouls1920b$pfR) - mean(fouls1920b$pfH) 
mean(fouls1920b$ftaR) - mean(fouls1920b$ftaH) 
sum(fouls1920b$ftR) / sum(fouls1920b$ftaR) 
sum(fouls1920b$ftH) / sum(fouls1920b$ftaH) 

t.test(fouls1920b$pfR, fouls1920b$pfH)

t.test(fouls1920b$ftaR, fouls1920b$ftaH)

temp7 <- select(fouls1920b, c(7,12)) 
temp7 %>%
  pivot_longer(cols = c(pfR, pfH),
               names_to = "team",
               values_to = "fouls") -> temp7
head(temp7)

temp7.text <- c("Home Team", "Road Team")
p7 <- ggplot(temp7, aes(x = team, y = fouls, fill = team)) + geom_boxplot() +
  labs(title = "Personal Foul Calls: Home vs. Road Teams",
       subtitle = "2019-20 Regular Season (post-COVID)", 
       x = "", 
       y = "Personal Fouls per Game") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(legend.position = "none") +
  scale_x_discrete(labels = temp1.text) +
  theme(plot.title = element_text(face = "bold")) +
  stat_compare_means(method = "t.test", label.x = 1.4, label.y = 38)

temp8 <- select(fouls1920b, c(6,11)) 
temp8 %>%
  pivot_longer(cols = c(ftaR, ftaH),
               names_to = "team",
               values_to = "ftattempts") -> temp8
head(temp8)

temp8.text <- c("Home Team", "Road Team")
p8 <- ggplot(temp8, aes(x = team, y = ftattempts, fill = team)) + geom_boxplot() +
  labs(title = "Free Throw Attempts: Home vs. Road Teams",
       subtitle = "2019-20 Regular Season (post-COVID)", 
       x = "", 
       y = "Free Throw Attempts per Game") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, color = "white", fill = "white") + 
  theme(legend.position = "none") +
  scale_x_discrete(labels = temp1.text) +
  theme(plot.title = element_text(face = "bold")) +
  stat_compare_means(method = "t.test", label.x = 1.4, label.y = 43)

p7 + p8 + plot_layout(ncol = 2)

cohen.d(fouls1920a$pfR, fouls1920a$pfH)
cohen.d(fouls1920a$ftaR, fouls1920a$ftaH)

cohen.d(fouls1920b$pfR, fouls1920b$pfH)
cohen.d(fouls1920b$ftaR, fouls1920b$ftaH)
