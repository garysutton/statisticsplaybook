library(tidyverse)
library(sqldf)
library(scales)
library(ineq)
library(gglorenz)
library(effsize)

install.packages('ineq')

gini <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/salaries_1985to2018.csv")

glimpse(gini) 

gini %>%
  group_by(player_id, season_end) %>%
  mutate(duplicate = n() > 1) -> gini2

gini2 %>%
  group_by(duplicate) %>%
  tally()

head(sqldf("SELECT * FROM gini2 WHERE duplicate = TRUE"))

gini %>%
  select(salary, season_end, team) -> gini

gini$season_end <- as.factor(gini$season_end )
gini$team <- as.factor(gini$team)

summary(gini, maxsum = 40)

sqldf("SELECT * FROM gini WHERE team == ''")

sqldf("SELECT * FROM gini WHERE team == 'Boston Celtics' AND season_end == 1987")
sqldf("SELECT * FROM gini WHERE team == 'Los Angeles Lakers' AND season_end == 1990")

gini %>%
  filter(team == "Boston Celtics" & season_end == 1987)
gini %>%
  filter(team == "Los Angeles Lakers" & season_end == 1990)

gini[!(gini$season_end %in% c(1985, 1986, 1987, 1988, 1989, 1990)),] -> gini3

dim(gini3)

gini3 %>%
  arrange(season_end, team, salary) %>%
  group_by(season_end, team) %>%
  mutate(rank = rank(-salary, ties.method = "first")) %>%
  filter(rank <= 14) -> gini4

sqldf("SELECT * FROM gini3 WHERE season_end = 2012 AND team = 'Denver Nuggets'")
sqldf("SELECT * FROM gini4 WHERE season_end = 2012 AND team = 'Denver Nuggets'")

sqldf("SELECT * FROM gini3 WHERE season_end = 2018 AND team = 'Chicago Bulls'")
sqldf("SELECT * FROM gini4 WHERE season_end = 2018 AND team = 'Chicago Bulls'")

sqldf("SELECT COUNT (*) FROM gini4 WHERE team = 'Denver Nuggets'") 
sqldf("SELECT COUNT (*) FROM gini4 WHERE team = 'Chicago Bulls'")

a <- rep(c(50), each = 10)
print(a)
ineq(a)

b <- rep(c(50, 100), each = 5)
print(b)
ineq(b)

c <- rep(c(150, 300), each = 5)
print(c)
ineq(c)

d <- rep(c(60, 70, 80, 90, 100), each = 2)
print(d)
ineq(d)

e <- rep(c(50, 75, 100, 125, 150), each = 2)
print(e)
ineq(e)

f <- rep(c(10, 100), times = c(9, 1))
print(f)
ineq(f)

gini4 %>%
  group_by(season_end) %>%
  summarize(gc = round(ineq(salary), digits = 2)) -> gini_summary
print(gini_summary)

ggplot(gini_summary, aes(x = season_end, y = gc, group = 1)) + 
  geom_line(aes(y = gc), color = "coral3", size = 1.5) + 
  geom_point(size = 3, color = "coral3") +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Gini Coefficient of NBA Player Salaries by Season", 
       subtitle = "1991-2018",
       x = "Season", 
       y = "Gini Coeffiicient",
       caption = "includes a maximum top 14 salaries for each team") +
  annotate("text", x = "2014", y = .38, label = "min = 0.39", 
           fontface = 'bold') +
  annotate("text", x = "2014", y = .40, label = "max = 0.52", 
           fontface = 'bold') +
  annotate("text", x = "2014", y = .39, label = "mean = 0.47", 
           fontface = 'bold') +
  annotate("text", x = "2014", y = .37, 
           label = "standard deviation = 0.03", fontface = 'bold') +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gini1994 <- filter(gini4, season_end == 1994)

ggplot(gini1994, aes(salary)) +
  stat_lorenz(desc = FALSE, color = "red", lwd = 2) +
  coord_fixed() +
  geom_abline(linetype = "dashed", lwd = 1.5) +
  labs(title = "Lorenz Curve\n1993-94 Season", 
       subtitle = "Gini coefficient = 0.39",
       x = "Salary Distribution",
       y = "Percentage of NBA Players",
       caption = "includes a maximum top 14 salaries for each team") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(face = "bold")) 

gini1997 <- filter(gini4, season_end == 1997)

ggplot(gini1997, aes(salary)) +
  stat_lorenz(desc = FALSE, color = "red", lwd = 2) +
  coord_fixed() +
  geom_abline(linetype = "dashed", lwd = 1.5) +
  labs(title = "Lorenz Curve\n1996-97 Season", 
       subtitle = "Gini coefficient = 0.52",
       x = "Salary Distribution", 
       y = "Percentage of NBA Players",
       caption = "includes a maximum top 14 salaries for each team") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(face = "bold")) 

gini4 <- select(gini4, -c(rank))

gini4 %>%
  group_by(team, season_end) %>%
  mutate(id = row_number(salary)) %>%
  pivot_wider(names_from = id, values_from = salary) -> gini5
head(gini5)

names(gini5) = c("season_end", "team", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10",
                 "s11", "s12", "s13", "s14")

options(scipen = 999)

head(gini5)

gini5 %>%
  mutate(gini_index = round(ineq(c(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
                                   s12, s13, s14, na.rm = TRUE)), digits = 2)) -> gini6
head(gini6)

records <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/records.csv")

glimpse(records)

records$season_end <- as.factor(records$season_end)
records$team <- as.factor(records$team)
records$champ <- as.factor(records$champ)

gini_records <- left_join(gini6, records, by = c("season_end", "team"))

dim(gini_records)

head(gini_records, n = 3)

gini_records %>%
  group_by(champ) %>%
  summarize(mean = round(mean(gini_index), digits = 2)) -> gini_summary2
print(gini_summary2)

gini_records %>%
  filter(champ == 0) -> giniX
gini_records %>%
  filter(champ == 1) -> giniY

t.test(giniX$gini_index, giniY$gini_index)

giniXY <- rbind(giniX, giniY)
ggplot(giniXY, aes(x = champ, y = gini_index)) +
  geom_boxplot() +
  labs(title = "Comparison of Gini Coefficients based on Season-End Disposition ",
       subtitle = "1991-2018",
       x = "", 
       y = "Gini Coefficients") +
  geom_boxplot(color = "skyblue4", fill = "skyblue1") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) +
  scale_x_discrete(breaks = c("1", "0"),
                   labels = c("League Champions", "All Other Teams")) 

cohen.d(giniX$gini_index, giniY$gini_index)

gini_records %>%
  group_by(pct >= 0.50) %>%
  summarize(mean = round(mean(gini_index), digits = 2)) -> gini_summary3
print(gini_summary3)

gini_records %>%
  filter(pct >= 0.50) -> giniA
gini_records %>%
  filter(pct < 0.50) -> giniB

t.test(giniA$gini_index, giniB$gini_index)

giniAB <- rbind(giniA, giniB)
mutate(giniAB, win_pct = ifelse(pct >= 0.50, "y", "n")) -> giniAB
ggplot(giniAB, aes(x = win_pct, y = gini_index)) + 
  geom_boxplot() +
  labs(title = "Comparison of Gini Coefficients based on Regular Season Winning Percentage",
       subtitle = "1991-2018",
       x = "", 
       y = "Gini Coefficients") +
  geom_boxplot(color = "skyblue4", fill = "skyblue1") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) +
  scale_x_discrete(breaks = c("y", "n"),
                   labels = c("Winning Teams", "Losing Teams")) 

cohen.d(giniA$gini_index, giniB$gini_index)

gini_records %>%
  mutate(gini_band = case_when(gini_index >= .60 ~ ">0.60",
                               gini_index >= .50 & gini_index < .60 ~ ">0.50",
                               gini_index >= .40 & gini_index < .50 ~ ">0.40",
                               gini_index >= .30 & gini_index < .40 ~ ">0.30",
                               gini_index < .30 ~ ">0.20")) -> gini_records
gini_records$gini_band <- as.factor(gini_records$gini_band)
head(gini_records, n = 3)

gini_records %>%
  group_by(gini_band) %>%
  summarize(mean_pct = round(mean(pct), digits = 2)) -> gini_summary3
print(gini_summary3)

ggplot(gini_summary3, aes(x = gini_band, y = mean_pct)) + 
  geom_bar(stat = "identity", width = .6, fill = "sienna1") + 
  labs(title = "Gini Coefficients and Winning Percentages",
       subtitle = "1991-2018", 
       x = "Gini Coefficient Bands", 
       y = "Average Regular Season Winning Percentage") + 
  ylim(0, 0.65) +
  geom_text(aes(x = gini_band, y = mean_pct, label = mean_pct,
                vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold"))


























