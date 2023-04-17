library(tidyverse)
library(GGally)
library(car)
library(broom)
library(tree)
library(patchwork)

hustle <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/hustle.csv")

glimpse(hustle)

hustle$team <- as.factor(hustle$team)
hustle$season <- as.factor(hustle$season)
hustle$team_season <- as.factor(hustle$team_season)

summary(hustle)

hustle %>% 
  select(-c(off_loose_balls, def_loose_balls)) -> hustle

dim(hustle)

sp1 <- qplot(seq_along(hustle$deflections), hustle$deflections) +
  labs(title = "Deflections", 
       subtitle = "scatterplot", 
       x = "", 
       y = "Value") +
  theme(plot.title = element_text(face = "bold")) +
  annotate("text", x = 65, y = 18.5, 
           label = "Outlier?", color = "red",
           size = 3, fontface = "bold") +
  annotate("text", x = 85, y = 18.3, 
           label = "Outlier?", color = "red",
           size = 3, fontface = "bold") 

hist1 <- ggplot(hustle, aes(x = deflections)) + 
  geom_histogram(fill = "snow1", color = "dodgerblue4", bins = 8) + 
  labs(title  ="Deflections", 
       subtitle = "histogram",
       x = "",           
       y  = "Frequency") +
  theme(plot.title = element_text(face = "bold")) +
  annotate("text", x = 18.75, y = 3, 
           label = "  Outliers?", color = "red",
           size = 3, fontface = "bold") 

bp1 <- ggplot(hustle, aes(x = "", y = deflections)) + 
  labs(title = "Deflections", 
       subtitle = "boxplot", 
       x = "", 
       y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "snow1", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,                                                
               color = "dodgerblue4", fill = "dodgerblue4") + 
  annotate("text", x = "", y = 18.6, 
           label = "                  Outliers",
           color = "red", size = 3, fontface = "bold") +
  theme(plot.title = element_text(face = "bold"))

sp1 + hist1 + bp1 + plot_layout(ncol = 3)

hustle$deflections[hustle$deflections > 17.8] = 17.8

max(hustle$deflections)

bp2 <- ggplot(hustle, aes(x = "", 
                          y = deflections)) + 
  labs(title = "Deflections", 
       subtitle = "post-winsorization boxplot", 
       x = "", y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "grey65", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) 
print(bp2)

bp3 <- ggplot(hustle, aes(x = "", y = wins)) +
  labs(title = "Wins", 
       x = "", 
       y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "snow1", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) 

bp4 <- ggplot(hustle, aes(x = "", y = screen_assists)) +
  labs(title = "Screens", 
       x = "", 
       y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "snow1", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) 

bp5 <- ggplot(hustle, aes(x = "", y = screen_assists_pts)) + 
  labs(title = "Points off Screens", x = "", y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "snow1", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) 

bp6 <- ggplot(hustle, aes(x = "", y = loose_balls)) +
  labs(title = "Loose Balls Recovered", 
       x = "", 
       y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "snow1", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) 

bp7 <- ggplot(hustle, aes(x = "", y = charges)) + 
  labs(title = "Charges Drawn", 
       x = "", 
       y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "snow1", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) 

bp8 <- ggplot(hustle, aes(x = "", y = contested_2pt)) + 
  labs(title = "Contested 2pt Shots", 
       x = "", 
       y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "snow1", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.background = element_rect(color = "red", size = 2))

bp9 <- ggplot(hustle, aes(x = "", y = contested_3pt)) +
  labs(title = "Contested 3pt Shots", 
       x = "", 
       y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "snow1", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) 

bp10 <- ggplot(hustle, aes(x = "", y = contested_shots)) +
  labs(title ="Contested Shots", 
       x = "", 
       y ="") +
  geom_boxplot(color = "dodgerblue4", fill = "snow1", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face="bold")) +
  theme(panel.background = element_rect(color = "red", size = 2))

bp3 + bp4 + bp5 + bp6 + plot_layout(ncol = 2)

bp7 + bp8 + bp9 + bp10 + plot_layout(ncol = 2)

hustle$contested_2pt[hustle$contested_2pt > 48.5] = 48.5
hustle$contested_shots[hustle$contested_shots > 69.3] = 69.3
hustle$contested_shots[hustle$contested_shots < 57.4] = 57.4

max(hustle$contested_2pt)

bp11 <- ggplot(hustle, aes(x = "", y = contested_2pt)) + 
  labs(title = "Contested 2pt Shots",
       subtitle = "post-winsorization boxplot", 
       x = "", 
       y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "grey65", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) +
  print(bp11)

max(hustle$contested_shots)
min(hustle$contested_shots)

bp12 <- ggplot(hustle, aes(x = "", y = contested_shots)) + 
  labs(title = "Contested Shots",
       subtitle = "post-winsorization boxplot", 
       x = "", 
       y = "") +
  geom_boxplot(color = "dodgerblue4", fill = "grey65", width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8,                                                
               color = "dodgerblue4", fill = "dodgerblue4") + 
  theme(plot.title = element_text(face = "bold")) 
print(bp12)

options(scipen = 999)

shapiro.test(hustle$deflections)
dp1 <- ggplot(hustle, aes(x = deflections)) +
  geom_density(alpha = .3, fill = "dodgerblue4") +
  labs(title = "Deflections",
       subtitle = "Shapiro-Wilk test of normality: p-value = 0.42",
       x = "", 
       y = "Density") +
  theme(plot.title = element_text(face = "bold")) 
print(dp1)

shapiro.test(hustle$wins)
dp2 <- ggplot(hustle, aes(x = wins)) +
  geom_density(alpha = .3, fill = "dodgerblue4") +
  labs(title = "Wins",
       subtitle = "Shapiro-Wilk test of normality: p-value = 0.19",
       x = "", 
       y = "Density") +
  theme(plot.title = element_text(face = "bold")) 

shapiro.test(hustle$screen_assists)
dp3 <- ggplot(hustle, aes(x = screen_assists)) +
  geom_density(alpha = .3, fill = "dodgerblue4") +
  labs(title = "Screens",
       subtitle = "Shapiro-Wilk test of normality: p-value = 0.29",
       x = "", 
       y = "Density") +
  theme(plot.title = element_text(face = "bold")) 

shapiro.test(hustle$screen_assists_pts)
dp4 <- ggplot(hustle, aes(x = screen_assists_pts)) +
  geom_density(alpha = .3, fill = "dodgerblue4") +
  labs(title = "Points off Screens",
       subtitle = "Shapiro-Wilk test of normality: p-value = 0.06",
       x = "", 
       y = "Density") +
  theme(plot.title = element_text(face = "bold")) 

shapiro.test(hustle$loose_balls)
dp5 <- ggplot(hustle, aes(x = loose_balls)) +
  geom_density(alpha = .3, fill = "dodgerblue4") +
  labs(title = "Loose Balls Recovered",
       subtitle = "Shapiro-Wilk test of normality: p-value = 0.21",
       x = "", 
       y = "Density") +
  theme(plot.title = element_text(face = "bold")) 

shapiro.test(hustle$charges)
dp6 <- ggplot(hustle, aes(x = charges)) +
  geom_density(alpha = .3, fill = "dodgerblue4") +
  labs(title = "Charges Drawn",
       subtitle = "Shapiro-Wilk test of normality: p-value = 0.00",
       x = "", 
       y = "Density") +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.background = element_rect(color = "red", size = 2))

shapiro.test(hustle$contested_2pt)
dp7 <- ggplot(hustle, aes(x = contested_2pt)) +
  geom_density(alpha = .3, fill = "dodgerblue4") +
  labs(title = "Contested 2pt Shots",
       subtitle = "Shapiro-Wilk test of normality: p-value = 0.10",
       x = "", 
       y = "Density") +
  theme(plot.title = element_text(face = "bold")) 

shapiro.test(hustle$contested_3pt)
dp8 <- ggplot(hustle, aes(x = contested_3pt)) +
  geom_density(alpha = .3, fill = "dodgerblue4") +
  labs(title = "Contested 3pt Shots",
       subtitle = "Shapiro-Wilk test of normality: p-value = 0.29",
       x = "", 
       y = "Density") +
  theme(plot.title = element_text(face = "bold")) 

shapiro.test(hustle$contested_shots)
dp9 <- ggplot(hustle, aes(x = contested_shots)) +
  geom_density(alpha = .3, fill = "dodgerblue4") +
  labs(title = "Contested 2pt Shots",
       subtitle = "Shapiro-Wilk test of normality: p-value = 0.21",
       x = "", 
       y = "Density") +
  theme(plot.title = element_text(face = "bold")) 

dp2 + dp3 + dp4 + dp5 + plot_layout(ncol = 2)

dp6 + dp7 + dp8 + dp9 + plot_layout(ncol = 2)

cor(hustle$deflections, hustle$wins) 
cor1 <- ggplot(hustle, aes(x = deflections, y = wins)) + 
  geom_point(size = 3) +
  labs(title = "Deflections and Wins", 
       subtitle = "correlation coefficient = 0.24",
       x = "Deflections per Game", 
       y = "Regular Season Wins") + 
  geom_smooth(method = lm, se = FALSE) +
  theme(plot.title = element_text(face = "bold")) 
print(cor1)

hustle %>% 
  select(-c(1:3, 6, 8)) -> hustle2
ggpairs(hustle2)

cor(hustle2)

hustle %>%
  filter(row_number() %% 4 == 1) -> test
train <- anti_join(hustle, test)

dim(train)
dim(test)

fit1 <- lm(wins ~ screen_assists_pts + deflections + loose_balls + contested_2pt + contested_shots, data = train)
tidy(fit1)

hustle %>%
  filter(team_season == "MIA 17") %>%
  select(wins, screen_assists_pts, deflections, loose_balls, contested_2pt, contested_shots)

wins = -62.91 + (1.04 * 22.3) + (2.23 * 14.2) + (5.38 * 7.2) + (0.52 * 45.5) - (0.24 * 64.7)
print(round(wins))

wins = -62.91 + (1.04 * 22.3) + (2.23 * 14.2) + (5.38 * 8.2) + (0.52 * 45.5) - (0.24 * 64.7)
print(round(wins))

augment(fit1) -> fit1_tbl

head(fit1_tbl$wins)

head(fit1_tbl$.fitted)

fit1_tbl %>%
  mutate(wins_dif = abs(wins - .fitted)) -> fit1_tbl
mean(fit1_tbl$wins_dif)

glance(fit1)

vif(fit1)

par(mfrow = c(2, 2))
plot(fit1)

fit2 <- lm(wins ~ screen_assists_pts + deflections + loose_balls, data = train)

tidy(fit2)

augment(fit2) -> fit_tbl2
print(fit_tbl2)

fit_tbl2 %>%
  mutate(wins_dif = abs(wins - .fitted)) -> fit_tbl2
mean(fit_tbl2$wins_dif)

glance(fit2)

vif(fit2)

par(mfrow = c(2,2))
plot(fit2)

fit2_pred <- predict(fit2, data.frame(test), interval = 'confidence')
print(fit2_pred)

test %>%
  select(wins) -> test

cbind(fit2_pred, test) %>%
  mutate(wins_dif = abs(wins - fit)) -> fit_tbl_pred
mean(fit_tbl_pred$wins_dif)

p1 <- ggplot(fit_tbl_pred, aes(x = wins_dif)) +
  geom_histogram(fill = "snow1", color = "dodgerblue4", bins = 6) + 
  labs(title = "Frequency of Differences between Actual and Predicted Wins",
       subtitle = "Wins ~ Points Off Screens + Deflections + Loose Balls Recovered",
       x = "Difference between Actual and Predicted Wins", 
       y = "Frequency") +
  theme(plot.title = element_text(face = "bold"))
print(p1)

fit_tbl_pred %>%
  filter(wins_dif > 15)

fit_tbl_pred %>%
  filter(wins_dif < 5)

fit_tbl_pred %>%
  arrange(wins) -> fit_tbl_pred

fit_tbl_pred$row_num <- seq.int(nrow(fit_tbl_pred))

p2 <- ggplot(fit_tbl_pred, aes(x = row_num, y = wins, group = 1)) + 
  geom_line(aes(y = wins), color = "navy", size = 1.5) +
  geom_line(aes(y = fit), color = "gold3", size = 1.5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(title = "Actual Wins versus Predicted Wins",
       subtitle = "Results on test data set (23 observations)",
       x = "2016-17 through 2018-19\nSorted in Ascending Order by Actual Wins", 
       y = "Wins",             
       caption = "Actuals in dark\nPredictions in light") +
  theme(plot.title = element_text(face = "bold")) 
print(p2)

fit3 <- tree(formula = wins ~ screen_assists_pts + deflections + loose_balls + contested_2pt + contested_shots, data = train)
summary(fit3)

plot(fit3)
text(fit3)