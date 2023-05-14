library(tidyverse)
library(runner)

ft <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/pbp.csv")

ft %>%
  select(date, event_type, player, points, result, type) -> ft

ft %>%
  filter(event_type == "free throw", type != "Free Throw Technical") -> ft

ft$date <- as.Date(ft$date, "%Y-%m-%d")
class(ft$date)
                         
ft$event_type <- as.factor(ft$event_type)
ft$result <- as.factor(ft$result)
ft$type <- as.factor(ft$type)
                         
glimpse(ft)

sample(x = c(0, 1), size = 10, prob = c(0.5, 0.5), replace = TRUE)

prior_successes <-seq(1, 20)
independent_trials <- seq(1, 20)

df <- data.frame(prior_successes, independent_trials)
print(df)

df %>%
  mutate(laplace = (prior_successes + 1) / (independent_trials + 2) * 100) -> df
  print(df)

p1 <- ggplot(df, aes(x = independent_trials, y = laplace, group = 1)) + 
      geom_line(aes(y = laplace), color = "purple", size = 1.5) +
      geom_point(size = 3, color = "purple") +
      labs(title = "Laplace's Rule of Succession", 
           subtitle = "success leads to more success", 
           x = "Independent Trials",
           y = "Probability of Success") +
     scale_x_continuous(breaks = seq(0, 20)) +
     theme(plot.title = element_text(face = "bold")) 
print(p1)

ft %>%
  filter(player == "Giannis Antetokounmpo") -> giannis

dim(giannis)
sum(giannis$points)

giannis %>%
  group_by(date) %>%
  mutate(streak = streak_run(lag(points))) -> giannis_final

head(giannis_final, n = 10)

giannis_final %>%
  group_by(date) %>%
  slice(-1) -> giannis_final

dim(giannis_final)

giannis_final %>%
  filter(lag(points) == 1) %>%
  group_by(streak) %>%
  summarize(makes = sum(points == 1), misses = sum(points == 0)) -> giannis_tbl1
print(giannis_tbl1)

giannis_tbl1 %>%
  mutate(pct = makes / (makes + misses) * 100) -> giannis_tbl2
print(giannis_tbl2)

p2 <- ggplot(giannis_tbl2, aes(x = streak, y = pct, group = 1)) + 
  geom_line(aes(y = pct), color = "steelblue", size = 1.5) +
  geom_point(size = 3, color = "steelblue") +
  labs(title = "Giannis Antetokounmpo", 
       subtitle = "Free Throw Percentage Following Consecutive Makes", 
       x = "Consecutive Makes (Streak)", 
       y = "Free Throw Shooting Percentage", 
       caption = "2019-20 regular season and postseason\n
              Antetokounmpo shot 62.5% from the line during the regular season and postseason combined") +
  scale_x_continuous(breaks = seq(0, 12, 1)) +
  theme(plot.title = element_text(face = "bold")) 
print(p2)

coin_flips <- sample(x = c(0, 1), size = 700, prob = c(0.5, 0.5), replace = TRUE)
coin_flips <- as.data.frame(coin_flips)
colnames(coin_flips) <- c("heads_tails")

coin_flips %>%
  mutate(streak = streak_run(heads_tails)) %>%
  filter(heads_tails == 1) -> coin_flips

max(coin_flips$streak)

ft %>%
  filter(player == "Julius Randle") -> randle

dim(randle)
sum(randle$points)

randle %>%
  group_by(date) %>%
  mutate(streak = streak_run(lag(points))) -> randle_final

head(randle_final, n = 10)

randle_final %>%
  group_by(date) %>%
  slice(-1) -> randle_final

dim(randle_final)

randle_final %>%
  filter(lag(points) == 1) %>%
  group_by(streak) %>%
  summarize(makes = sum(points == 1), misses = sum(points == 0)) -> randle_tbl1
print(randle_tbl1)

randle_tbl1 %>%
  mutate(pct = makes / (makes + misses) * 100) -> randle_tbl2
print(randle_tbl2)

p3 <- ggplot(randle_tbl2, aes(x = streak, y = pct, group = 1)) + 
  geom_line(aes(y = pct), color = "steelblue", size = 1.5) +
  geom_point(size = 3, color = "steelblue") +
  labs(title = "Julius Randle", 
       subtitle = "Free Throw Percentage Following Consecutive Makes", 
       x = "Consecutive Makes (Streak)", 
       y = "Free Throw Shooting Percentage", 
       caption = "2019-20 regular season and postseason\n
                  Randle shot 73.4% from the line during the regular season") +
  scale_x_continuous(breaks = seq(0, 8, 1)) +
  theme(plot.title = element_text(face = "bold")) 
print(p3)

ft %>%
  filter(player == "James Harden") -> harden
dim(harden)
sum(harden$points)

harden %>%
  group_by(date) %>%
  mutate(streak = streak_run(lag(points))) -> harden_final

head(harden_final, n = 10)

harden_final %>%
  group_by(date) %>%
  slice(-1) -> harden_final

dim(harden_final)

harden_final %>%
  filter(lag(points) == 1) %>%
  group_by(streak) %>%
  summarize(makes = sum(points == 1), misses = sum(points == 0)) -> harden_tbl1
print(harden_tbl1)

harden_tbl1 %>%
  mutate(pct = makes / (makes + misses) * 100) -> harden_tbl2
print(harden_tbl2)

p4 <- ggplot(harden_tbl2, aes(x = streak, y = pct, group = 1)) + 
  geom_line(aes(y = pct), color = "steelblue", size = 1.5) +
  geom_point(size = 3, color = "steelblue") +
  labs(title = "James Harden", subtitle = "Free Throw Percentage Following Consecutive Makes", 
       x = "Consecutive Makes (Streak)", 
       y = "Free Throw Shooting Percentage", 
       caption = "2019-20 regular season and postseason\n
                  Harden shot 86.4% from the line during the regular season and postseason combined") +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  theme(plot.title = element_text(face = "bold")) 
print(p4)

ft %>%
  group_by(date, player) %>%
  mutate(streak = streak_run(lag(points))) -> ft_final

head(ft_final, n = 10)

ft_final %>%
  group_by(date, player) %>%
  slice(-1) -> ft_final

ft_final %>% 
  filter(lag(points) == 1) %>%
  group_by(streak) %>%
  summarize(makes = sum(points == 1), misses = sum(points == 0)) -> ft_tbl1
print(ft_tbl1)

ft_tbl1 %>%
  mutate(pct = makes / (makes + misses) * 100) -> ft_tbl2
print(ft_tbl2)

p5 <- ggplot(ft_tbl2, aes(x = streak, y = pct, group = 1)) + 
  geom_line(aes(y = pct), color = "steelblue", size = 1.5) +
  geom_point(size = 3, color = "steelblue") +
  labs(title = "Entire NBA", 
       subtitle = "Free Throw Percentage Following Consecutive Makes", 
       x = "Consecutive Makes (Streak)",
       y = "Free Throw Shooting Percentage", 
       caption = "2019-20 regular season and postseason") +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  theme(plot.title = element_text(face = "bold")) +
  annotation_custom(ggplotGrob(p1), xmin = 1, xmax = 11, ymin = 89, ymax = 101)
print(p5)



       