library(tidyverse)

cap <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/salary_cap2.csv")

records <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/team_records.csv")

cap$real <- cap$real * -1
head(cap)

cap %>%
  pivot_longer(cols = c("real", "adjusted"),
               names_to = "type",
               values_to = "cap") -> new_cap
head(new_cap)

breaks <- seq(-120000000, 120000000, 10000000)
labels <- paste0(as.character(c(seq(120, 0, -10), seq(10, 120, 10))), "m")

p1 <- ggplot(new_cap, aes(x = season, y = cap, fill = type)) +   
  geom_col(width = .6) +
  scale_y_continuous(breaks = breaks,   
                     labels = labels) + 
  coord_flip() +  
  labs(title = "NBA Salary Cap History: 1984-85 to 2020-21",
       x = "Season", 
       y = "Real or Adjusted Cap",
       caption = "salary cap was introduced prior to the 1984-85 season") +
  scale_fill_discrete(name = "", labels = c("Adjusted Dollars", "Real Dollars")) +
  theme(plot.title = element_text(face = "bold"))
print(p1)

records %>%
  select(Season, Lg, Team, W, L) -> records

records %>%
  filter(Lg == "NBA" & Season > "1969-70" & Season < "1998-99") -> records

glimpse(records)

records %>%
  group_by(Season) %>%
  summarize(v_wins = var(W)) -> var_records

head(var_records, n = 3)
tail(var_records, n = 3)

p2 <- ggplot(var_records, aes(x = Season, y = v_wins, group = 1)) + 
  geom_line(aes(y = v_wins), color = "black", size = .5) + 
  geom_point(size = 5, color = "dodgerblue") +
  labs(title = "Year-over-Year Variance in Regular Season Wins",
       subtitle = "1970-98",
       x = "Season", 
       y = "Variance",
       caption = "salary cap was introduced prior to the 1984-85 season") +
  ylim(0, 300) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = var_records[as.character(var_records$Season) < "1984-85",]) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = var_records[as.character(var_records$Season) > "1983-84",]) +
  annotate("rect", xmin = "1970-71", xmax = "1983-84",
           ymin = 0, ymax = 300, alpha = 0.1, fill = "orange") +
  annotate("rect", xmin = "1984-85", xmax = "1997-98",
           ymin = 0, ymax = 300, alpha = 0.1, fill = "green")
print(p2)

records %>%
  group_by(Season) %>%
  summarize(sd_wins = sd(W)) -> sd_records

head(sd_records, n = 3)
tail(sd_records, n = 3)

p3 <- ggplot(sd_records, aes(x = Season, y = sd_wins, group = 1)) + 
  geom_line(aes(y = sd_wins), color = "black", size = .5) + 
  geom_point(size = 5, color = "dodgerblue") +
  labs(title = "Year-over-Year Standard Deviation in Regular Season Wins",
       subtitle = "1970-98",
       x = "Season", 
       y = "Standard Deviation",
       caption = "salary cap was introduced prior to the 1984-85 season") +
  ylim(0, 20) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = sd_records[as.character(sd_records$Season) < "1984-85",]) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = sd_records[as.character(sd_records$Season) > "1983-84",]) +
  annotate("rect", xmin = "1970-71", xmax = "1983-84",
           ymin = 0, ymax = 20, alpha = 0.1, fill = "orange") +
  annotate("rect", xmin = "1984-85", xmax = "1997-98",
           ymin = 0, ymax = 20, alpha = 0.1, fill = "green")
print(p3)

records %>%
  group_by(Season) %>%
  summarize(r_wins = max(W) - min(W)) -> r_records

head(r_records, n = 3)
tail(r_records, n = 3)

p4 <- ggplot(r_records, aes(x = Season, y = r_wins, group = 1)) + 
  geom_line(aes(y = r_wins), color = "black", size = .5) + 
  geom_point(size = 5, color = "dodgerblue") +
  labs(title = "Year-over-Year Range in Regular Season Wins",
       subtitle = "1970-98",
       x = "Season", 
       y = "Range",
       caption = "salary cap was introduced prior to the 1984-85 season") +
  ylim(0, 60) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = r_records[as.character(r_records$Season) < "1984-85",]) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = r_records[as.character(r_records$Season) > "1983-84",]) +
  annotate("rect", xmin = "1970-71", xmax = "1983-84",
    ymin = 0, ymax = 60, alpha = 0.1, fill = "orange") +
  annotate("rect", xmin = "1984-85", xmax = "1997-98",
    ymin = 0, ymax = 60, alpha = 0.1, fill = "green")
print(p4)

records %>%
  group_by(Season) %>%
  mutate(mad_wins = abs(W - mean(W))) -> mad_records

head(mad_records, n = 3)
tail(mad_records, n = 3)

mad_records %>%
  group_by(Season) %>%
  summarize(mad_wins2 = sum(mad_wins) / sum(W > 0)) -> mad_records2

head(mad_records2, n = 3)
tail(mad_records2, n = 3)

p5 <- ggplot(mad_records2, aes(x = Season, y = mad_wins2, group = 1)) + 
  geom_line(aes(y = mad_wins2), color = "black", size = .5) + 
  geom_point(size = 5, color = "dodgerblue") +
  labs(title = "Year-over-Year Mean Absolute Deviation in Regular Season Wins",
       subtitle = "1970-98",
       x = "Season", 
       y = "Mean Absolute Deviation",
       caption = "salary cap was introduced prior to the 1984-85 season") +
  ylim(0, 14) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = mad_records2[as.character(mad_records2$Season) < "1984-85",]) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = mad_records2[as.character(mad_records2$Season) > "1983-84",]) +
  annotate("rect", xmin = "1970-71", xmax = "1983-84",
           ymin = 0, ymax = 14, alpha = 0.1, fill = "orange") +
  annotate("rect", xmin = "1984-85", xmax = "1997-98",
           ymin = 0, ymax = 14, alpha = 0.1, fill = "green")
print(p5)

records %>%
  group_by(Season) %>%
  mutate(mdad_wins = abs(W - median(W))) -> mdad_records

head(mdad_records, n = 3)
tail(mdad_records, n = 3)

mdad_records %>%
  group_by(Season) %>%
  summarize(mdad_wins2 = sum(mdad_wins) / sum(W > 0)) -> mdad_records2

head(mdad_records2, n = 3)
tail(mdad_records2, n = 3)

p6 <- ggplot(mdad_records2, aes(x = Season, y = mdad_wins2, group = 1)) + 
  geom_line(aes(y = mdad_wins2), color = "black", size = .5) + 
  geom_point(size = 5, color = "dodgerblue") +
  labs(title = "Year-over-Year Median Absolute Deviation in Regular Season Wins",
       subtitle = "1970-98",
       x = "Season", 
       y = "Median Absolute Deviation",
       caption = "salary cap was introduced prior to the 1984-85 season") +
  ylim(0, 14) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = mdad_records2[as.character(mdad_records2$Season) < "1984-85",]) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = mdad_records2[as.character(mdad_records2$Season) > "1983-84",]) +
  annotate("rect", xmin = "1970-71", xmax = "1983-84",
    ymin = 0, ymax = 14, alpha = 0.1, fill = "orange") +
  annotate("rect", xmin = "1984-85", xmax = "1997-98",
    ymin = 0, ymax = 14, alpha = 0.1, fill = "green")
print(p6)

records -> churn

churn$Team <- gsub('[^[:alnum:] ]', '', churn$Team) 

churn %>%
  group_by(Season) %>%
  mutate(rank = rank(-W, ties.method = 'average')) -> churn

churn %>%
  mutate(topTeam = ifelse(rank > 8, 0, 1)) -> churn

churn %>%
  arrange(Team, desc(Season)) %>%
  group_by(Team) %>%
  mutate(topTeam2 = lead(topTeam, n = 1)) -> churn

head(churn, n = 10)

na.omit(churn) -> churn

churn %>%
  group_by(Season) %>%
  count(topTeam > topTeam2) -> churn_tbl
head(churn_tbl)

row_odd <- seq_len(nrow(churn_tbl)) %% 2 
churn_tbl[row_odd == 0, ] -> churn_tbl
churn_tbl %>%
  select(Season, n) -> churn_tbl
print(churn_tbl)

p7 <- ggplot(churn_tbl, aes(x = Season, y = n, group = 1)) + 
  geom_line(aes(y = n), color = "black", size = .5) + 
  geom_point(size = 5, color = "orange") +
  labs(title = "Year-over-Year Churn in the NBA's Top 8",
       subtitle = "1971-98",
       x = "Season", 
       y = "Number of New Top 8 Teams from Prior Season",
       caption = "salary cap was introduced prior to the 1984-85 season") +
  ylim(0, 6) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = churn_tbl[as.character(churn_tbl$Season) < "1984-85",]) +
  geom_smooth(method = lm, color = "red", se = FALSE, 
              data = churn_tbl[as.character(churn_tbl$Season) > "1983-84",]) +
  annotate("rect", xmin = "1971-72", xmax = "1983-84",
    ymin = 0, ymax = 6, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = "1984-85", xmax = "1997-98",
    ymin = 0, ymax = 6, alpha = 0.1, fill = "green") 
print(p7)


