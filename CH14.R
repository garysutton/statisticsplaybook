packages <- c("tidyverse", "patchwork", "ggpubr", "pscl", "SciViews", 
              "questionr", "caret", "pROC")

lapply(packages, library, character.only = TRUE)

salaries <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/salaries.csv")

salaries %>%
  select(Team, w2017:w2008) -> first_salaries
salaries %>%
  select(Team, pc2017:pc2008) -> second_salaries

first_salaries %>%
  pivot_longer(col = c(w2017:w2008),
               names_to = "year",
               values_to = "wins") -> first_stats

second_salaries %>%
  pivot_longer(col = c(pc2017:pc2008),
               names_to = "season",
               values_to = "playoffs") -> second_stats

dim(first_stats)
head(first_stats, n = 3)
tail(first_stats, n = 3)

dim(second_stats)
head(second_stats, n = 3)
tail(second_stats, n = 3)

first_stats %>%
  mutate(season = str_sub(year, -4, -1)) -> first_stats
first_stats %>%
  select(-year) -> first_stats
first_stats$season <- as.factor(first_stats$season)
head(first_stats, n = 3)

second_stats %>%
  select(playoffs) -> second_stats

stats <- cbind(first_stats, second_stats)
head(stats, n = 3)

nba2017 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2017.csv")
nba2016 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2016.csv")
nba2015 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2015.csv")
nba2014 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2014.csv")
nba2013 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2013.csv")
nba2012 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2012.csv")
nba2011 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2011.csv")
nba2010 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2010.csv")
nba2009 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2009.csv")
nba2008 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba2008.csv")

nba <- rbind(nba2017, nba2016, nba2015, nba2014, nba2013, nba2012, 
             nba2011, nba2010, nba2009, nba2008)

dim(nba)

nba %>%
  mutate(season = rep(c(2017:2008), each = 30)) -> nba
nba$season <- as.factor(nba$season)

head(nba$season, n = 3)
tail(nba$season, n = 3)
summary(nba$season)

nba %>%
  mutate(O_PTS = PTS - PTS_DIFF) -> nba

nba %>%
  select(Team, PTS, PTS_DIFF, season, O_PTS) -> nba

glimpse(nba)

nba %>%
  select(Team, season, PTS, O_PTS, PTS_DIFF) -> nba

glimpse(nba)

stats$playoffs <- as.factor(stats$playoffs)

nba_stats <- left_join(stats, nba, by = c("Team", "season"))

nba_stats_right <- right_join(stats, nba, by = c("Team", "season"))
nba_stats_full <- inner_join(stats, nba, by = c("Team", "season"))
nba_stats_inner <- full_join(stats, nba, by = c("Team", "season"))

head(nba_stats, n = 3)
head(nba_stats_right, n = 3)
head(nba_stats_full, n = 3)
head(nba_stats_inner, n = 3)

nba_stats %>%
  group_by(season) %>%
  mutate(z_wins = (wins - mean(wins)) / sd(wins)) -> nba_stats
nba_stats$z_wins <- round(nba_stats$z_wins, digits = 2)

nba_stats %>%
  group_by(season) %>%
  mutate(z_pts = (PTS - mean(PTS)) / sd(PTS)) -> nba_stats
nba_stats$z_pts <- round(nba_stats$z_pts, digits = 2)  

nba_stats %>%
  group_by(season) %>%
  mutate(z_o_pts = (O_PTS - mean(O_PTS)) / sd(O_PTS)) -> nba_stats
nba_stats$z_o_pts <- round(nba_stats$z_o_pts, digits = 2) 

head(nba_stats, n = 3)

nba_stats %>%
  group_by(season) %>%
  summarize(pts_avg = round(mean(PTS), digits = 2)) -> first_tibble
print(first_tibble)

ggplot(first_tibble, aes(x = season, y = pts_avg)) +
  geom_bar(stat = "identity", width = .5, 
           color = "steelblue4", fill = "steelblue1") +
  labs(title = "Average Points per Game per Team by Season", 
       subtitle = "2008-17",
       x = "Season", 
       y = "Average Points per Game") +
  geom_text(aes(label = (pts_avg), vjust = -0.3, fontface = "bold")) +
  ylim(0, 110) +
  theme(plot.title = element_text(face = "bold"))

ggplot(nba_stats, aes(x = PTS)) +
  geom_histogram(fill = "steelblue1", color = "steelblue4", bins = 15) + 
    geom_vline(aes(xintercept = mean(PTS)),
             color = "black", linetype = "longdash", size = .8) +
    geom_vline(aes(xintercept = median(PTS)),
             color = "black", size = .8) +
  labs(title = "Distribution of Points Scored per Game per Team",
       subtitle = "2008-17",
       caption = "dashed line represents the mean
          solid line represents the median",
       x = "Average Points Scored per Game",
       y = "Frequency") +
  theme(plot.title = element_text(face = "bold"))

shapiro.test(nba_stats$PTS)

cor(nba_stats$z_wins, nba_stats$z_o_pts)
cor(nba_stats$z_wins, nba_stats$z_pts)

p1 <- ggplot(nba_stats, aes(x = z_o_pts, y = z_wins)) + 
  geom_point() +
  labs(title = "Points Allowed vs. Wins (2008-17)",
       subtitle = "correlation coefficient = -0.58",
       x = "Points Allowed (standardized)", 
       y = "Wins (standardized)") + 
  geom_smooth(method = lm, se = FALSE) +
  xlim(-3.3, 3.3) +
  theme(plot.title = element_text(face = "bold"))

p2 <- ggplot(nba_stats, aes(x = z_pts, y = z_wins)) + 
  geom_point() +
  labs(title = "Points Scored vs. Wins (2008-17)",
       subtitle = "correlation coefficient = 0.53",
       x = "Points Scored (standardized)", 
       y = "Wins (standardized)") + 
  geom_smooth(method = lm, se = FALSE) +
  xlim(-3.3, 3.3) +
  theme(plot.title = element_text(face = "bold"))

p1 + p2 + plot_layout(ncol = 2)

nba_stats %>%
  group_by(season) %>%
  summarize(cor_dif = round(cor(PTS, wins) - abs(cor(O_PTS, z_wins)), 
                            digits = 2)) -> second_tibble
print(second_tibble)

ggplot(second_tibble, aes(x = season, y = cor_dif)) +
  geom_bar(stat = "identity", width = .7, color = "gold4", 
           fill = "gold1") +
  labs(title = "Annual Differences in Absolute Correlations",
       subtitle = "2008-17",
       caption = "when negative, points allowed mattered more;
          when positive, points scored mattered more",
       x = "Season", 
       y = "Absolute Correlation Difference") +
  geom_text(aes(label = cor_dif, y = cor_dif, fontface = "bold",
               vjust = ifelse(cor_dif >= 0, -0.5, 1.3),
               hjust = ifelse(cor_dif >= 0, 0.5, 0.5))) +
  ylim(-.4, .4) +
  theme(plot.title = element_text(face = "bold"))

options(scipen = 999)
cor.test(nba_stats$z_wins, nba_stats$z_o_pts)
cor.test(nba_stats$z_wins, nba_stats$z_pts)

nba_stats %>%
  mutate(o_pts_cat = case_when(z_o_pts < -1 ~ "A",
                               z_o_pts >= -1 & z_o_pts < 0 ~ "B",
                               z_o_pts >= 0 & z_o_pts <= 1 ~ "C",
                               z_o_pts > 1 ~ "D")) -> nba_stats
nba_stats$o_pts_cat <- as.factor(nba_stats$o_pts_cat)

nba_stats %>%
  mutate(pts_cat = case_when(z_pts < -1 ~ "A",
                             z_pts >= -1 & z_pts < 0 ~ "B",
                             z_pts >= 0 & z_pts <= 1 ~ "C",
                             z_pts > 1 ~ "D")) -> nba_stats
nba_stats$pts_cat <- as.factor(nba_stats$pts_cat)

summary(nba_stats$o_pts_cat)
summary(nba_stats$pts_cat)

p3 <- ggplot(nba_stats, aes(x = o_pts_cat, y = z_wins, fill = o_pts_cat)) +
  geom_boxplot(notch = TRUE, alpha = 0.5) +
  labs(title = "Points Allowed vs. Wins", 
       subtitle = "2008-17",
       x = "Standardized Points Allowed Category", 
       y = "Standardized Regular Season Wins") +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 8, color = "white", fill = "white") + 
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Points\nAllowed\nZ-Score", 
                      labels = c("< -1", "-1 to 0", "0 to 1","> 1")) +
  theme(plot.title = element_text(face = "bold")) 

p4 <- ggplot(nba_stats, aes(x = pts_cat, y = z_wins, fill = pts_cat)) +
  geom_boxplot(notch = TRUE, alpha = 0.5) +
  labs(title = "Points Scored vs. Wins", 
       subtitle = "2008-17",
       x = "Standardized Points Scored Category", 
       y = "Standardized Regular Season Wins") +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 8, color = "white", fill = "white") + 
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Points\nScored\nZ-Score", 
                      labels = c("< -1", "-1 to 0", "0 to 1", "> 1")) +
  theme(plot.title = element_text(face = "bold"))

p3 + p4 + plot_layout(ncol = 2)

fit1 <- aov(z_wins ~ o_pts_cat, data = nba_stats)

summary(fit1)

plot(fit1, which = c(2, 1))

fit2 <- aov(z_wins ~ pts_cat, data = nba_stats)
summary(fit2)

plot(fit2, which = c(2, 1))

AIC(fit1, fit2)

nba_stats %>%
  mutate(playoffs2 = ifelse(playoffs == 0, 0, 1)) -> nba_stats
head(nba_stats[,c(1, 3, 4, 13)], 10)
tail(nba_stats[,c(1, 3, 4, 13)], 10)

nba_stats %>%
  filter(row_number() %% 4 == 1) -> test
train <- anti_join(nba_stats, test)

dim(train)
dim(test)

fit3 <- glm(playoffs2 ~ z_o_pts, family = "binomial", data = train)
summary(fit3)

pR2(fit3)["McFadden"]

varImp(fit3)

ln(2.33)

exp(0.85)

2.34 / 3.34

plogis(-1.72)

predicted_fit3 <- predict(fit3, test, type = "response")

test %>%
  ungroup(season) %>%
  select(playoffs2) -> actuals
  
actuals %>%
  rename(actual_values = playoffs2) -> actuals

predictions <- as.data.frame(predicted_fit3)

predictions %>%
  rename(predicted_values = predicted_fit3) -> predictions

predictions %>%
  mutate(predicted_values2 = 
           ifelse(predicted_values >= 0.50, 1, 0)) -> predictions

confusion_matrix <- table(actuals$actual_values, predictions$predicted_values2)
print(confusion_matrix)

sensitivity(actuals$actual_values, predictions$predicted_values2)
specificity(actuals$actual_values, predictions$predicted_values2)
print(misclassification_error <- (11 + 10) / 80)
print(accuracy <- 1 - misclassification_error)

roc_curve <- roc(actuals$actual_values, predictions$predicted_values2)
print(roc_curve)

plot(roc_curve, 
     col = "red", 
     main = " ROC Curve: AUC = 0.72",
     xlab = "Specificity: TN / (TN + FP)",
     ylab = "Sensitivity: TP / (TP + FN)")

fit4 <- glm(playoffs2 ~ z_pts, family = "binomial", data = train)
summary(fit4)

plogis(0.63)

pR2(fit4)["McFadden"]

varImp(fit4)

predicted_fit4 <-predict(fit4, test, type = "response")

predictions <- as.data.frame(predicted_fit4)

predictions %>%
  rename(predicted_values = predicted_fit4) -> predictions

predictions %>%
  mutate(predicted_values2 = 
           ifelse(predicted_values >= 0.50, 1, 0)) -> predictions

confusion_matrix <- table(actuals$actual_values, 
                          predictions$predicted_values2)
print(confusion_matrix)

sensitivity(actuals$actual_values, predictions$predicted_values2)
specificity(actuals$actual_values, predictions$predicted_values2)
print(misclassification_error <- (3 + 22) / 80)
print(accuracy <- 1 - misclassification_error)

roc_curve <- roc(actuals$actual_values, predictions$predicted_values2)
print(roc_curve)

plot(roc_curve, 
     col = "blue", 
     main = " ROC Curve: AUC = 0.73",
     xlab = "Specificity: TN / (TN + FP)",
     ylab = "Sensitivity: TP / (TP + FN)")

nba_stats %>%
  filter(playoffs == 11) %>%
  select(Team, season, PTS, O_PTS) %>%
  arrange(season) -> df1

df1 %>%
  select(-Team) -> df1

df2 <- data.frame(season = as.factor(c(2008:2017)),
                     PTS = c(94.0, 102.4, 101.1, 98.2, 97.3, 97.1, 
                             106.3, 103.3, 104.8, 119.3),
                     O_PTS = c(88.8, 95.2, 97.3, 92.4, 90.2, 90.7, 
                               97.0, 95.5, 96.2, 105.8))

df3 <- rbind(df1, df2)

df3$Season <- rep(c("Regular Season", "Playoffs"), each = 10)
print(df3)

ggpaired(df3, x = "Season", y = "O_PTS",
         color = "Season", line.color = "gray", line.size = 0.4,
         palette = "aaas",
         main = "Points Allowed Comparison: Regular Season versus Playoffs
            NBA Champions Only (2008-17)", 
         xlab = "",
         ylab = "Points Allowed per Game")

ggpaired(df3, x = "Season", y = "PTS",
         color = "Season", line.color = "gray", line.size = 0.4,
         palette = "aaas",
         main = "Points Scored Comparison: Regular Season versus Playoffs
            NBA Champions Only (2008-17)",
         xlab = "",
         ylab = "Points Scored per Game")



