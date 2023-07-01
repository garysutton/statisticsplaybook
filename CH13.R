library(tidyverse)
library(sqldf)
library(scales)
library(ineq)
library(gglorenz)
library(effsize)
library(effectsize)
library(car)

ws_gini <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/seasons_stats.csv")

dim(ws_gini)

ws_gini %>%
  select(Year, Tm, WS) -> ws_gini

ws_gini %>%
  filter(Year >= 1991) -> ws_gini

ws_gini %>%
  filter(Tm != "TOT") -> ws_gini

dim(ws_gini)

ws_gini$Year <- as.factor(ws_gini$Year)
ws_gini$Tm <- as.factor(ws_gini$Tm)

head(ws_gini)
tail(ws_gini)

summary(ws_gini, maxsum = 40)

ws_gini %>%
  arrange(Year, Tm, WS) %>%
  group_by(Year, Tm) %>%
  mutate(rank = rank(-WS, ties.method = "first")) %>%
  filter(rank <= 14) -> ws_gini2

head(ws_gini2, n = 14)

sqldf("SELECT * FROM ws_gini WHERE Year = 2012 AND Tm = 'GSW'")
sqldf("SELECT * FROM ws_gini2 WHERE Year = 2012 AND Tm = 'GSW'")

ws_gini %>%
  filter(Year == 2017 & Tm == "BOS")
ws_gini2 %>%
  filter(Year == 2017 & Tm == "BOS")

sqldf("SELECT COUNT(*) FROM ws_gini2 WHERE Tm = 'GSW'") 
sqldf("SELECT COUNT(*) FROM ws_gini2 WHERE Tm = 'BOS'")

ws_gini2 %>%
  group_by(Year) %>%
  summarize(gc = round(ineq(WS), digits = 2)) -> ws_gini_summary
print(ws_gini_summary)

ggplot(ws_gini_summary, aes(x = Year, y = gc, group = 1)) + 
  geom_line(aes(y = gc), color = "black", size = .5) + 
  geom_point(size = 5, color = "seagreen3") +
  geom_text(aes(label = gc),
            nudge_x = 0.01, nudge_y = 0.01,
            check_overlap = TRUE, size = 2.5) +
  labs(title = "Gini Coefficient for Win Shares", 
       subtitle = "1991-2017",
       x = "Season", 
       y = "Gini Coeffiicient",
       caption = "includes a maximum top 14 win shares for each team") +
  ylim(0.42, 0.60) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("rect", xmin = "1991", xmax = "2009", 
           ymin = 0.42, ymax = 0.60, alpha = 0.1, fill = "orange") +
  annotate("rect", xmin = "2010", xmax = "2017", 
           ymin = 0.42, ymax = 0.60, alpha = 0.1, fill = "blue")

sqldf("SELECT COUNT(*) FROM ws_gini2 WHERE WS < 0") 

ws_gini2$WS[ws_gini2$WS < 0] = 0.0
min(ws_gini2$WS)

ws_gini2 %>%
  filter(Year == 1991) -> gini91

ggplot(gini91, aes(WS)) +
  stat_lorenz(desc = FALSE, color = "red", lwd = 2) +
  coord_fixed() +
  geom_abline(linetype = "dashed", lwd = 1.5) +
  labs(title = "Lorenz Curve\n1990-91 Season", 
       subtitle = "Gini coefficient = 0.56",
       x = "Win Share Distribution",
       y = "Percentage of NBA Players",
       caption = "includes a maximum top 14 salaries for each team") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(face = "bold")) 

ws_gini2 %>%
  filter(Year == 1991 | Year == 2017) -> gini9117

head(gini9117, n = 3)
tail(gini9117, n = 3)

ggplot(gini9117, aes(WS, fill = Year)) +
  stat_lorenz(geom = "polygon", desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  labs(title = "Lorenz Curve\n1990-91 versus 2016-17 Seasons", 
       subtitle = "Gini coefficients = 0.56 and 0.49",
       x = "Win Share Distribution", 
       y = "Percentage of NBA Players",
       caption = "includes a maximum top 14 salaries for each team") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(face = "bold")) 

for (i in 1:5)  {
  print(i * 2)
}

ws_gini2 %>%
  ungroup(Tm, Year) -> ws_gini2

ws_gini2 %>%
  filter(Year == 1991 | Year == 1992 | Year == 1993 | Year == 1994) -> ws9194
head(ws9194)
tail(ws9194)

ws9194 %>%
  pivot_wider(names_from = Year, values_from = WS) -> ws9194
head(ws9194)

ws9194 %>%
  select(-c(Tm, rank)) -> ws9194

ws9194 <- as.data.frame(ws9194)
class(ws9194)

names(ws9194) <- c("a", "b", "c", "d")
head(ws9194)

par(mfrow = c(2, 2)) 

loop.vector <- 1:4

for (i in loop.vector) {
x <- ws9194[,i]

plot(Lc(x), col = "red", lwd = 2,
      main = paste0("Lorenz Curve\n", "199", i),
      xlab = "Win Share Distribution",
      ylab = "Percentage of NBA Players")
}

r_to_rsquared <- function(r) {
   rsquared <- r^2
   return(rsquared)
 }

r_to_rsquared(.42)

rsquared_to_r <- function(rsquared) {
  r <- sqrt(rsquared)
  return(r)
}

rsquared_to_r(.1764)

gini.est <- function(a, b, c) {
  gini <- 1 - 2 * ((0.4 - 0.0) * (a + 0) * 0.5 +
                   (0.6 - 0.4) * (b + a) * 0.5 +
                   (0.8 - 0.6) * (c + b) * 0.5 +
                   (1.0 - 0.8) * (1 + c) * 0.5)
  return(gini)
}

gini.est(.05, .18)

gini.est(.05, .18, .44)

gini.est(.05, .19, .44)
gini.est(.06, .20, .44)
gini.est(.06, .20, .44)

ws_gini2 %>%
  select(-c(rank)) -> ws_gini2

ws_gini2 %>%
  group_by(Tm, Year) %>%
  mutate(id = row_number(WS)) %>%
  pivot_wider(names_from = id, values_from = WS) -> ws_gini3
head(ws_gini3)

names(ws_gini3) = c("season_end", "team", "ws1", "ws2", "ws3", "ws4", "ws5", "ws6", "ws7", "ws8", "ws9", "ws10", "ws11", "ws12", "ws13", "ws14")

head(ws_gini3)

ws_gini3 %>%
  mutate(gini_index = round(ineq(c(ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws10, ws11,
                             ws12, ws13, ws14, na.rm = TRUE)), digits = 2)) -> ws_gini4
head(ws_gini4)

records <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/records.csv")

records %>%
  filter(season_end < 2018) -> records

records$season_end <- as.factor(records$season_end )
records$team <- as.factor(records$team)
records$champ <- as.factor(records$champ)

levels(ws_gini4$team)
levels(records$team)

ws_gini4$team <- recode(ws_gini4$team, "'ATL' = 'Atlanta Hawks';
                        'BOS' = 'Boston Celtics';
                        'BRK' = 'Brooklyn Nets';
                        'CHA' = 'Charlotte Bobcats';
                        'CHH' = 'Charlotte Hornets';
                        'CHI' = 'Chicago Bulls';
                        'CHO' = 'Charlotte Hornets';
                        'CLE' = 'Cleveland Cavaliers';
                        'DAL' = 'Dallas Mavericks';
                        'DEN' = 'Denver Nuggets';
                        'DET' = 'Detroit Pistons';
                        'GSW' = 'Golden State Warriors';
                        'HOU' = 'Houston Rockets';
                        'IND' = 'Indiana Pacers';
                        'LAC' = 'Los Angeles Clippers';
                        'LAL' = 'Los Angeles Lakers';
                        'MEM' = 'Memphis Grizzlies';
                        'MIA' = 'Miami Heat';
                        'MIL' = 'Milwaukee Bucks';
                        'MIN' = 'Minnesota Timberwolves';
                        'NJN' = 'New Jersey Nets';
                        'NOH' = 'New Orleans Hornets';
                        'NOK' = 'New Orleans/Oklahoma City Hornets';
                        'NOP' = 'New Orleans Pelicans';
                        'NYK' = 'New York Knicks';
                        'OKC' = 'Oklahoma City Thunder';
                        'ORL' = 'Orlando Magic';
                        'PHI' = 'Philadelphia 76ers';
                        'PHO' = 'Phoenix Suns';
                        'POR' = 'Portland Trail Blazers';
                        'SAC' = 'Sacramento Kings';
                        'SAS' = 'San Antonio Spurs';
                        'SEA' = 'Seattle SuperSonics';
                        'TOR' = 'Toronto Raptors';
                        'UTA' = 'Utah Jazz';
                        'VAN' = 'Vancouver Grizzlies';
                        'WAS' = 'Washington Wizards';
                        'WSB' = 'Washington Bullets'")

left_join(ws_gini4, records, by = c("season_end", "team")) -> ws_gini_records

glimpse(ws_gini_records) 

ws_gini_records %>%
  select(season_end, team, gini_index:champ) -> ws_gini_records

ws_gini_records %>%
  group_by(champ) %>%
  summarize(mean = round(mean(gini_index), digits = 2)) -> ws_gini_summary2
print(ws_gini_summary2)

ws_gini_records %>%
  filter(champ == 0) -> ws_giniX
ws_gini_records %>%
  filter(champ == 1) -> ws_giniY

t.test(ws_giniX$gini_index, ws_giniY$gini_index)

ws_giniXY <- rbind(ws_giniX, ws_giniY)
ggplot(ws_giniXY, aes(x = champ, y = gini_index)) +
  geom_boxplot() +
  labs(title = "Comparison of Gini Coefficients based on Season-End Disposition ",
       x = "", 
       y = "Gini Coefficients", subtitle = "1991-2017") +
  geom_boxplot(color = "darkorange4", fill = "darkorange1") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) +
  scale_x_discrete(breaks = c("1", "0"),
                   labels = c("League Champions", "All Other Teams")) 

cohen.d(ws_giniX$gini_index, ws_giniY$gini_index)

cohens_d(ws_giniX$gini_index, ws_giniY$gini_index)
hedges_g(ws_giniX$gini_index, ws_giniY$gini_index)
glass_delta(ws_giniX$gini_index, ws_giniY$gini_index)

round(sd(ws_giniX$gini_index), digits = 2)
round(sd(ws_giniY$gini_index), digits = 2)

round(sqrt(var(ws_giniX$gini_index)), digits = 2)
round(sqrt(var(ws_giniY$gini_index)), digits = 2)

var.test(ws_giniX$gini_index, ws_giniY$gini_index)

cohens_d(ws_giniY$gini_index, ws_giniX$gini_index)
hedges_g(ws_giniY$gini_index, ws_giniX$gini_index)
glass_delta(ws_giniY$gini_index, ws_giniX$gini_index)

ws_gini_records %>%
  group_by(pct >= 0.50) %>%
  summarize(mean = round(mean(gini_index), digits = 2)) -> ws_gini_summary3
print(ws_gini_summary3)

ws_gini_records %>%
  filter(pct >= 0.50) -> ws_giniA
ws_gini_records %>%
  filter(pct < 0.50) -> ws_giniB

t.test(ws_giniA$gini_index, ws_giniB$gini_index)

ws_giniAB <- rbind(ws_giniA, ws_giniB)
mutate(ws_giniAB, win_pct = ifelse(pct >= 0.50, "y", "n")) -> ws_giniAB
ggplot(ws_giniAB, aes(x = win_pct, y = gini_index)) + 
  geom_boxplot() +
  labs(title = "Comparison of Gini Coefficients based on Regular Season Winning Percentage",
       subtitle = "1991-2017",
       x = "", 
       y = "Gini Coefficients") +
  geom_boxplot(color = "darkorange4", fill = "darkorange1") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) +
  scale_x_discrete(breaks = c("y", "n"),
                   labels = c("Winning Teams", "Losing Teams")) 

cohens_d(ws_giniA$gini_index, ws_giniB$gini_index)
hedges_g(ws_giniA$gini_index, ws_giniB$gini_index)
glass_delta(ws_giniA$gini_index, ws_giniB$gini_index)

ws_gini_records %>%
  mutate(ws_gini_band = case_when(gini_index >= .50 ~ ">0.50",
                                  gini_index >= .45 & gini_index < .50 ~ ">0.45",
                                  gini_index >= .40 & gini_index < .45 ~ ">0.40",
                                  gini_index >= .35 & gini_index < .40 ~ ">0.35",
                                  gini_index >= .30 & gini_index < .35 ~ ">0.30",
                                  gini_index >= .25 & gini_index < .30 ~ ">0.25",
                                  gini_index < .25 ~ "<0.25")) -> ws_gini_records
ws_gini_records$ws_gini_band <- as.factor(ws_gini_records$ws_gini_band)
head(ws_gini_records, n = 3)

ws_gini_records %>%
  group_by(ws_gini_band) %>%
  summarize(mean_pct = round(mean(pct), digits = 2)) -> ws_gini_summary4
print(ws_gini_summary4)

ggplot(ws_gini_summary4, aes(x = ws_gini_band, y = mean_pct)) + 
  geom_bar(stat = "identity", width = .6, fill = "steelblue1") + 
  labs(title = "Gini Coefficients and Winning Percentages",
       subtitle = "1991-2017", 
       x = "Gini Coefficient Bands", 
       y = "Average Regular Season Winning Percentage") + 
  ylim(0, 0.7) +
  geom_text(aes(x = ws_gini_band, y = mean_pct, 
                label = mean_pct, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold"))


