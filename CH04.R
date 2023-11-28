# single line comment example: lpSolve is for constrained optimization
library(lpSolve)

# multi-line example: tidyverse includes dplyr and tidyr
# mult-line example continued: tidyverse also includes ggplot2
# mult-line example continued: tidyverse also includes readr
library(tidyverse)

# scales package is part of tidyverse, but it’s best to load
# this as a standalone package
library(scales)

# we use the patchwork package to bundle multiple plots into one object
library(patchwork)

free_agents <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/free_agents.csv") # saved in default directory

dim(free_agents)

glimpse(free_agents)

str(free_agents)

free_agents$position1 <- as.factor(free_agents$position1)
free_agents$position2 <- as.factor(free_agents$position2)

levels(free_agents$position1)
levels(free_agents$position2)

head(free_agents, n = 3)
tail(free_agents, n = 3)

summary(free_agents)

options(scipen = 999)

p1 <- ggplot(free_agents, aes(x = annual_salary)) +
  geom_density(alpha = .3, fill = "salmon") +
  geom_vline(aes(xintercept = mean(annual_salary, na.rm = TRUE)),
             color = "red", linetype = "longdash", size = .8) +
  geom_vline(aes(xintercept = median(annual_salary, na.rm = TRUE)),
             color = "blue", linetype = "longdash", size = .8) +
  labs(title = "Annual Salary Distribution",
       subtitle = "Shortlisted Free Agents",
       x = "Annual Salary", 
       y = "Density",
       caption = "Salary data is illustrative only") +
  scale_x_continuous(labels = comma) +
  theme(plot.title = element_text(face = "bold")) +
  annotate("text", x = 18000000, 
           y = .000000025, label = "Mean", color = "black",
           size = 4, fontface = "bold", angle = 90) +
  annotate("text", x = 14000000, 
           y = .000000025, label = "Median", color = "black",
           size = 4, fontface = "bold", angle = 90)

p2 <- ggplot(free_agents, aes(x = win_shares)) +
  geom_density(alpha = .3, fill = "salmon") +
  geom_vline(aes(xintercept = mean(win_shares, na.rm = TRUE)),
             color = "red", linetype = "longdash", size = .8) +
  geom_vline(aes(xintercept = median(win_shares, na.rm = TRUE)),
             color = "blue", linetype = "longdash", size = .8) +
  labs(title = "Projected Annual Win Shares Distribution",
       subtitle = "Shortlisted Free Agents",
       x = "Win Shares", y = "Density",
       caption = "Win Shares data is illustrative only") +
  theme(plot.title = element_text(face = "bold")) +
  annotate("text", x = 4.1, 
           y = .1, label = "Mean", color = "black",
           size = 4, fontface = "bold", angle = 90) +
  annotate("text", x = 3.7, 
           y = .1, label = "Median", color = "black",
           size = 4, fontface = "bold", angle = 90)

p3 <- ggplot(free_agents, aes(x = age)) +
  geom_density(alpha = .3, fill = "salmon") +
  geom_vline(aes(xintercept = mean(age, na.rm = TRUE)),
             color = "red", linetype = "longdash", size = .8) +
  geom_vline(aes(xintercept = median(age, na.rm = TRUE)),
             color = "blue", linetype = "longdash", size = .8) +
  labs(title = "Age Distribution", subtitle = "Shortlisted Free Agents",
       x = "Age", y = "Density",
       caption = "Player ages are real; source: Spotrac") +
  theme(plot.title = element_text(face = "bold")) +
  annotate("text", x = 30.2, 
           y = .04, label = "Mean", color = "black",
           size = 4, fontface = "bold", angle = 90) +
  annotate("text", x = 30.7,
           y = .04, label = "Median", color = "black",
           size = 4, fontface = "bold", angle = 90)

p1 + p2 - p3 + plot_layout(ncol = 1)

p4 <- ggplot(free_agents, aes(x = position1, y = annual_salary)) + 
  geom_boxplot(color = "sienna4", fill = "sienna1" ) +
  labs(title = "Annual Salary Distribution by Position",
       subtitle = "Shortlisted Free Agents",
       x = "Position", 
       y = "Annual Salary", 
       caption = "Salary data is illustrative only") + 
  scale_y_continuous(labels = comma) +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 8, color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) 

p5 <- ggplot(free_agents, aes(x = position1, y = win_shares)) + 
  geom_boxplot(color = "steelblue4", fill = "steelblue1" ) +
  labs(title = "Annual Win Shares Distribution by Position",
       subtitle = "Shortlisted Free Agents",
       x = "Position", 
       y = "Annual Win Shares", 
       caption = "Win Share data is illustrative") + 
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 8, color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) 

p6 <- ggplot(free_agents, aes(x = position1, y = age)) + 
  geom_boxplot(color = "gold4", fill = "gold1" ) +
  labs(title = "Age Distribution by Position",
       subtitle = "Shortlisted Free Agents",
       x = "Position", 
       y = "Age", 
       caption = "Player ages are real; source: Spotrac") + 
  stat_summary(fun = mean, geom = "point",
               shape = 20, size = 8, color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) 

p4 + p5 + p6 + plot_layout(ncol = 1)

cor(free_agents$annual_salary, free_agents$win_shares)

p7 <- ggplot(free_agents, aes(x = annual_salary, y = win_shares)) + 
  geom_point(size = 3) +
  labs(title = "Annual Salaries vs. Win Shares",
       subtitle = "correlation coefficient = 0.76",
       x = "Annual Salaries", 
       y = "Win Shares") + 
  geom_smooth(method = lm, se = FALSE) +
  scale_x_continuous(label = scales::comma) +
  theme(plot.title = element_text(face = "bold"))
print(p7)

free_agents %>%
  group_by(position1) %>%
  tally() -> tibble1
print(tibble1)

free_agents %>% 
  group_by(position1) %>%
  summarize(meanSalary = mean(annual_salary),
            meanWinShares = mean(win_shares),
            meanAge = mean(age)) -> tibble2
print(tibble2)

left_join(tibble1, tibble2, by = "position1") -> tibble3
print(tibble3)

p8 <- ggplot(tibble3, aes(x = position1, y = n)) + 
  geom_bar(stat = "identity", width = .8, 
           fill = "darkorchid1", color = "darkorchid4") + 
  labs(title = "Position Counts",
       subtitle = "Shortlisted Free Agents", 
       x = "Position", 
       y = "Counts",
       caption = "Salary mean ($M)\nWin Shares mean\nAge mean") + 
  geom_text(aes(label = n, vjust = -0.3)) +
  geom_label(aes(label = trunc(meanSalary*.000001), vjust = 1.2)) +
  geom_label(aes(label = trunc(meanWinShares), vjust = 2.4)) +
  geom_label(aes(label = trunc(meanAge), vjust = 3.6)) +
  ylim(0, 8) +
  theme(plot.title = element_text(face = "bold"))
print(p8)

head(free_agents_sort <- arrange(free_agents, position2))

head(free_agents_sort <- select(free_agents_sort, player, age, 
                                position2, annual_salary, win_shares))

free_agents_sort$centers = c(1,1,1,1,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0) 
free_agents_sort$power_forwards = c(0,0,0,0,1,1,1,1,1,1,
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0)  
free_agents_sort$point_guards = c(0,0,0,0,0,0,0,0,0,0,
                                  1,1,1,1,1,1,0,0,0,0,0,0,0,0) 
free_agents_sort$small_forwards = c(0,0,0,0,0,0,0,0,0,0,
                                    0,0,0,0,0,0,1,1,1,1,1,0,0,0) 
free_agents_sort$shooting_guards = c(0,0,0,0,0,0,0,0,0,0,
                                     0,0,0,0,0,0,0,0,0,0,0,1,1,1) 
print(free_agents_sort)

constraint_matrix <- as.matrix(rbind(free_agents_sort$centers,
                                     free_agents_sort$centers,
                                     free_agents_sort$power_forwards,
                                     free_agents_sort$power_forwards,
                                     free_agents_sort$point_guards,
                                     free_agents_sort$point_guards,
                                     free_agents_sort$small_forwards,
                                     free_agents_sort$small_forwards,
                                     free_agents_sort$shooting_guards,
                                     free_agents_sort$shooting_guards,
                                     t(rep(1, length = 24)),
                                     free_agents_sort$annual_salary,
                                     free_agents_sort$age))

constraint_matrix <- as.matrix(rbind(free_agents_sort$centers,
                                     free_agents_sort$centers,
                                     free_agents_sort$power_forwards,
                                     free_agents_sort$power_forwards,
                                     free_agents_sort$point_guards,
                                     free_agents_sort$point_guards,
                                     free_agents_sort$small_forwards,
                                     free_agents_sort$small_forwards,
                                     free_agents_sort$shooting_guards,
                                     free_agents_sort$shooting_guards,
                                     t(rep(1, length = 24)),
                                     free_agents_sort$annual_salary,
                                     free_agents_sort$age))
dimnames(constraint_matrix) <- 
  list(c("OneCenterMax",
         "OneCenterMin",
         "OnePowerForwardMax",
         "OnePowerForwardMin",
         "OnePointGuardMax",
         "OnePointGuardMin",
         "OneSmallForwardMax",
         "OneSmallForwardMin",
         "OneShootingGuardMax",
         "OneShootingGuardMin",
         "FivePlayerMax",
         "SalaryMax",
         "AgeMax"),
       free_agents_sort$position2)
print(constraint_matrix)

co_object <-
  lp(const.mat = constraint_matrix,
     objective = free_agents_sort$win_shares,
     direction = "max",
     const.rhs = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 90000000, 150), 
     const.dir = c("<=", ">=","<=", ">=", "<=", ">=", 
                   "<=", ">=", "<=", ">=", "<=", "<=", "<="), 
     int.vec = 1:24, all.bin = TRUE)
print(df <- select(free_agents_sort[as.logical(co_object$solution),], 1:5))

sum(df$win_shares)

sum(df$annual_salary) 

mean(df$age) 
