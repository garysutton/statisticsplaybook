library(tidyverse)
library(scales)
library(patchwork)

cap <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/salary_cap.csv")

salaries <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/salaries.csv")

options(scipen = 999)

salaries %>%
  select(Team, s2017, sa2017, w2017, pc2017) %>%
  glimpse()

dim(salaries)

p1 <- ggplot(cap, aes(x = year, y = real, group = 1)) + 
  geom_line(aes(y = real), color = "steelblue", size = 1.5, 
            linetype = "dashed") +
  geom_line(aes(y = adjusted), color = "black", size = 1.5) +
  labs(title = "NBA Salary Cap in USD and 2021 USD", 
       subtitle = "2000-2017", 
       x = "Season", 
       y = "Dollars") +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  annotate("text", x = 2005, y = 68000000, label = "2021 USD", 
           fontface = "bold", color = c("black")) +
  annotate("text", x = 2008, y = 50000000, label = "USD", 
           fontface = "bold", color = c("steelblue")) 
print(p1)

salaries %>%
  summarize(across(s2017:s2000, mean, na.rm = TRUE)) -> mean_salaries_real
print(as.data.frame(mean_salaries_real))

mean_salaries_real %>%
  pivot_longer(col = c(s2017:s2000),
               names_to = "year",
               values_to = "real") -> new_mean_salaries_real
print(new_mean_salaries_real)

salaries %>%
  summarize(across(sa2017:sa2000, mean, 
                   na.rm = TRUE)) -> mean_salaries_adjusted

mean_salaries_adjusted %>%
  pivot_longer(col = c(sa2017:sa2000),
               names_to = "year_temp",
               values_to = "adjusted") -> new_mean_salaries_adjusted
print(new_mean_salaries_adjusted)

salaries_temp <- cbind(new_mean_salaries_real, new_mean_salaries_adjusted)
print(salaries_temp)

salaries_temp %>% 
  select(-c(year_temp)) -> salaries_temp

salaries_temp$year <- as.factor(2017:2000)
print(salaries_temp)

p2 <- ggplot(salaries_temp, aes(x = year, y = real, group = 1)) + 
  geom_line(aes(y = real), color = "steelblue", size = 1.5, linetype = "dashed") +
  geom_line(aes(y = adjusted), color = "black", size = 1.5) +
  labs(title = "Average Payroll per NBA Team in USD and 2021 USD", 
       subtitle = "2000-2017", 
       x = "Season", 
       y = "Dollars") +
  theme(plot.title = element_text(face = "bold")) + 
  scale_y_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  annotate("text", x = "2003", y = 85000000, 
           label = "2021 USD", fontface = "bold", color = c("black")) +
  annotate("text", x = "2007", y = 61000000, 
           label = "USD", fontface = "bold", color = c("steelblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)

cor(salaries$s2000, salaries$w2000, use = "complete.obs")

cor(salaries$s2000, salaries$w2000, method = 'spearman',
    use = "complete.obs")

salaries %>%
  select(s2017:s2000, w2017:w2000) -> salaries_cor
dim(salaries_cor)

options("max.print" = 100)
round(cor(salaries_cor, use = "complete.obs"), digits = 2)

salaries_cor %>%
  select(s2017:s2000) %>%
  pivot_longer(col = c(s2017:s2000),
               names_to = "year1",
               values_to = "salary") -> salaries_sals
dim(salaries_sals)
head(salaries_sals, n = 3)
tail(salaries_sals, n = 3)

salaries_cor %>%
  select(w2017:w2000) %>%
  pivot_longer(col = c(w2017:w2000),
               names_to = "year2",
               values_to = "wins") -> salaries_wins
dim(salaries_wins)
head(salaries_wins, n = 3)
tail(salaries_wins, n = 3)

salaries_cor2 <- cbind(salaries_sals, salaries_wins)
dim(salaries_cor2)
head(salaries_cor2, n = 3)
tail(salaries_cor2, n = 3)

salaries_cor2 %>%
  group_by(year1) %>%
  summarize(cor = round(cor(salary, wins, use = "complete.obs"), digits = 2)) -> tbl1
print(tbl1)

cor.test(salaries_cor2$salary, salaries_cor2$wins)

salaries2000 <- select(salaries, Team, s2000, w2000, pc2000)
salaries2000 <- na.omit(salaries2000)
cor1 <- ggplot(salaries2000, aes(x = s2000, y = w2000, 
                                 color = factor(pc2000))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (1999-2000)",
       subtitle = "correlation coefficient = 0.57",
       x = "Team Payrolls", 
       y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2000[salaries2000$s2000 > 20000000,], 
              se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2001 <- select(salaries, Team, s2001, w2001, pc2001)
salaries2001 <- na.omit(salaries2001)
cor2 <- ggplot(salaries2001, aes(x = s2001, y = w2001, 
                                 color = factor(pc2001))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2000-01)",
       subtitle = "correlation coefficient = 0.37",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2001[salaries2001$s2001 > 20000000,], 
              se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cor1 + cor2 + plot_layout(ncol = 2)

tbl1 %>%
  mutate(year1 = replace(year1, 1:18, sprintf("%02.0f", 00:17))) -> tbl1
tbl1$year1 <- as.factor(tbl1$year1)
print(tbl1)

p3 <- ggplot(tbl1, aes(x = year1, y = cor, group = 1)) + 
  geom_line(aes(y = cor), color = "orange2", size = 2.5) +
  labs(title ="YoY Correlation between Payrolls and Wins", 
       subtitle = "2000-17", 
       x = "Season", 
       y = "Correlation Coefficient") +
  geom_smooth(method = lm, color = "blue", se = FALSE) +
  geom_smooth(method = lm, color = "purple", se = FALSE,
            data = tbl1[as.numeric(tbl1$year1) < 07,]) +
  geom_smooth(method = lm, color = "red", se = FALSE,
            data = tbl1[as.numeric(tbl1$year1) > 05,]) +
  theme(plot.title = element_text(face = "bold")) +
  geom_segment(aes(x = 10,
                   y = 0.5,
                   xend = 11,
                   yend = 0.5),
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("text", x = 8.7, y = 0.5, 
           label = "YoY Correlations", size = 3) +
  geom_segment(aes(x = 4,
                   y = 0.34,
                   xend = 3,
                   yend = 0.34),
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("text", x = 6.3, y = 0.34, 
           label = "Trend between 2000 and 2006", size = 3) +
  geom_segment(aes(x = 11.5,
                   y = 0.24,
                   xend = 11.5,
                   yend = 0.29),
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("text", x = 11.5, y = 0.22, 
           label = "Trend between 2006 and 2017", size = 3) +
  geom_segment(aes(x = 17.5,
                   y = 0.31,
                   xend = 17.5,
                   yend = 0.36),
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("text", x = 17.5, y = 0.27, 
           label = "Trend\nbetween\n2000 and 2017", size = 3)
print(p3)

salaries2000 <- select(salaries, Team, s2000, pc2000)
salaries2000 <- na.omit(salaries2000)
dot1 <- ggplot(salaries2000) +
  geom_point(aes(x = s2000, y = reorder(Team, s2000), 
                 color = factor(pc2000)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", 
       subtitle = "1999-2000", 
       x = "Team Payroll", 
       y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2001 <- select(salaries, Team, s2001, pc2001)
salaries2001 <- na.omit(salaries2001)
dot2 <- ggplot(salaries2001) +
  geom_point(aes(x = s2001, y = reorder(Team, s2001), 
                 color = factor(pc2001)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", 
       subtitle = "2000-01", 
       x = "Team Payroll", 
       y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dot1 + dot2 + plot_layout(ncol = 2)

salaries2000 <- na.omit(salaries2000)
salaries2000 %>%
  group_by(pc2000) %>%
  summarize(mean = mean(s2000)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),
                       'M', sep = "")) -> tbl2
  tbl2$pc2000 <- c("No playoffs", "Made playoffs", "Won title")
lol1 <- ggplot(tbl2, aes(x = pc2000, y = mean)) +
  geom_segment(aes(x = pc2000, xend = pc2000, 
                   y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), 
             fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", 
         subtitle = "1999-2000",
         x = "", 
         y = "Averqge Team Payroll") +
  scale_y_continuous(labels = 
                       label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", 
                              "Won title")) +
  geom_text(aes(label = mean2), color = "white", 
            fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2001 <- na.omit(salaries2001)
salaries2001 %>%
  group_by(pc2001) %>%
  summarize(mean = mean(s2001)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),
                       'M', sep = "")) -> tbl2
  tbl2$pc2001 <- c("No playoffs", "Made playoffs", "Won title")
lol2 <- ggplot(tbl2, aes(x = pc2001, y = mean)) +
  geom_segment(aes(x = pc2001, xend = pc2001, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), 
             fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)",
         subtitle = "2000-01",
         x = "", 
         y = "Averqge Team Payroll") +
  scale_y_continuous(labels = 
                       label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", 
                              "Won title")) +
  geom_text(aes(label = mean2), color = "white", 
            fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lol1 + lol2 + plot_layout(ncol = 2)

salaries %>%
  select(sa2017:sa2000) %>%
  pivot_longer(col = c(sa2017:sa2000),
               names_to = "year1",
               values_to = "salary") -> salaries_mean
dim(salaries_mean)
head(salaries_mean, n = 3)
tail(salaries_mean, n = 3)

salaries %>%
  select(pc2017:pc2000) %>%
  pivot_longer(col = c(pc2017:pc2000),
               names_to = "year2",
               values_to = "flag") -> salaries_flag
dim(salaries_flag)
head(salaries_flag, n = 3)
tail(salaries_flag, n = 3)

salaries2 <- cbind(salaries_mean, salaries_flag)
dim(salaries2)
head(salaries2, n = 3)
tail(salaries2, n = 3)

salaries2 <- na.omit(salaries2)
salaries2 %>%
  group_by(flag) %>%
  summarize(mean = mean(salary, na.rm = TRUE)) %>%
  mutate(mean2 = paste("$", round(mean/1000000),"M", sep = "")) -> tbl3
print(tbl3)

tbl3$flag <- c("No playoffs", "Made playoffs", "Won title")
p4 <- ggplot(tbl3, aes(x = flag, y = mean)) +
  geom_segment(aes(x = flag, xend = flag, 
                   y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), 
             fill = c("navy", "gold3", "red")) +
  labs(title = "Adjusted Team Payroll Comparisons (2021 USD)", 
       subtitle = "2000-2017 Seasons",
       x = "", 
       y = "Averqge Team Payroll\nAdjusted for Inflation") +
  scale_y_continuous(labels = 
                       label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", 
                              "Won title")) +
  geom_text(aes(label = mean2), color = "white", 
            fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(panel.border = element_rect(fill = "transparent", 
                                    color = "lightskyblue", size = 2))
print(p4)


# not in manuscript
salaries2002 <- select(salaries, Team, s2002, w2002, pc2002)
salaries2002 <- na.omit(salaries2002)
cor3 <- ggplot(salaries2002, aes(x = s2002, y = w2002, color = factor(pc2002))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2001-02)",
       subtitle = "correlation coefficient = 0.13",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2002[salaries2002$s2002 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2003 <- select(salaries, Team, s2003, w2003, pc2003)
salaries2003 <- na.omit(salaries2003)
cor4 <- ggplot(salaries2003, aes(x = s2003, y = w2003, color = factor(pc2003))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2002-03)",
       subtitle = "correlation coefficient = 0.31",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2003[salaries2003$s2003 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2004 <- select(salaries, Team, s2004, w2004, pc2004)
salaries2004 <- na.omit(salaries2004)
cor5 <- ggplot(salaries2004, aes(x = s2004, y = w2004, color = factor(pc2004))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2003-04)",
       subtitle = "correlation coefficient = 0.21",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2004[salaries2004$s2004 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2005 <- select(salaries, Team, s2005, w2005, pc2005)
salaries2005 <- na.omit(salaries2005)
cor6 <- ggplot(salaries2005, aes(x = s2005, y = w2005, color = factor(pc2005))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2004-05)",
       subtitle = "correlation coefficient = 0.15",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2005[salaries2005$s2005 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2006 <- select(salaries, Team, s2006, w2006, pc2006)
salaries2006 <- na.omit(salaries2006)
cor7 <- ggplot(salaries2006, aes(x = s2006, y = w2006, color = factor(pc2006))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2005-06)",
       subtitle = "correlation coefficient = 0.02",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2006[salaries2006$s2006 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2007 <- select(salaries, Team, s2007, w2007, pc2007)
salaries2007 <- na.omit(salaries2007)
cor8 <- ggplot(salaries2007, aes(x = s2007, y = w2007, color = factor(pc2007))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2006-07)",
       subtitle = "correlation coefficient = 0.10",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2007[salaries2007$s2007 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2008 <- select(salaries, Team, s2008, w2008, pc2008)
salaries2008 <- na.omit(salaries2008)
cor9 <- ggplot(salaries2008, aes(x = s2008, y = w2008, color = factor(pc2008))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2007-08)",
       subtitle = "correlation coefficient = 0.16",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2008[salaries2008$s2008 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2009 <- select(salaries, Team, s2009, w2009, pc2009)
salaries2009 <- na.omit(salaries2009)
cor10 <- ggplot(salaries2009, aes(x = s2009, y = w2009, color = factor(pc2009))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2008-09)",
       subtitle = "correlation coefficient = 0.43",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2009[salaries2009$s2009 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2010 <- select(salaries, Team, s2010, w2010, pc2010)
salaries2010 <- na.omit(salaries2010)
cor11 <- ggplot(salaries2010, aes(x = s2010, y = w2010, color = factor(pc2010))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2009-10)",
       subtitle = "correlation coefficient = 0.48",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2010[salaries2010$s2010 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2011 <- select(salaries, Team, s2011, w2011, pc2011)
salaries2011 <- na.omit(salaries2011)
cor12 <- ggplot(salaries2011, aes(x = s2011, y = w2011, color = factor(pc2011))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2010-11)",
       subtitle = "correlation coefficient = 0.54",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2011[salaries2011$s2011 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2012 <- select(salaries, Team, s2012, w2012, pc2012)
salaries2012 <- na.omit(salaries2012)
cor13 <- ggplot(salaries2012, aes(x = s2012, y = w2012, color = factor(pc2012))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2011-12)",
       subtitle = "correlation coefficient = 0.39",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2012[salaries2012$s2012 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2013 <- select(salaries, Team, s2013, w2013, pc2013)
salaries2013 <- na.omit(salaries2013)
cor14 <- ggplot(salaries2013, aes(x = s2013, y = w2013, color = factor(pc2013))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2012-13)",
       subtitle = "correlation coefficient = 0.25",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2013[salaries2013$s2013 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2014 <- select(salaries, Team, s2014, w2014, pc2014)
salaries2014 <- na.omit(salaries2014)
cor15 <- ggplot(salaries2014, aes(x = s2014, y = w2014, color = factor(pc2014))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2013-14)",
       subtitle = "correlation coefficient = 0.26",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2014[salaries2014$s2014 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2015 <- select(salaries, Team, s2015, w2015, pc2015)
salaries2015 <- na.omit(salaries2015)
cor16 <- ggplot(salaries2015, aes(x = s2015, y = w2015, color = factor(pc2015))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2014-15)",
       subtitle = "correlation coefficient = 0.30",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2015[salaries2015$s2015 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2016 <- select(salaries, Team, s2016, w2016, pc2016)
salaries2016 <- na.omit(salaries2016)
cor17 <- ggplot(salaries2016, aes(x = s2016, y = w2016, color = factor(pc2016))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2015-16)",
       subtitle = "correlation coefficient = 0.54",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2016[salaries2016$s2016 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2017 <- select(salaries, Team, s2017, w2017, pc2017)
salaries2017 <- na.omit(salaries2017)
cor18 <- ggplot(salaries2017, aes(x = s2017, y = w2017, color = factor(pc2017))) + 
  geom_point(size = 3) +
  labs(title = "Team Payrolls vs. Wins (2016-17)",
       subtitle = "correlation coefficient = 0.39",
       x = "Team Payrolls", y = "Wins") + 
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = dollar) +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, color = "green4",
              data = salaries2017[salaries2017$s2017 > 20000000,], se = FALSE) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cor3 + cor4 + plot_layout(ncol = 2)
cor5 + cor6 + plot_layout(ncol = 2)
cor7 + cor8 + plot_layout(ncol = 2)
cor9 + cor10 + plot_layout(ncol = 2)
cor11 + cor12 + plot_layout(ncol = 2)
cor13 + cor14 + plot_layout(ncol = 2)
cor15 + cor16 + plot_layout(ncol = 2)
cor17 + cor18 + plot_layout(ncol = 2)

salaries2002 <- select(salaries, Team, s2002, pc2002)
salaries2002 <- na.omit(salaries2002)
dot3 <- ggplot(salaries2002) +
  geom_point(aes(x = s2002, y = reorder(Team, s2002), color = factor(pc2002)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2001-02", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2003 <- select(salaries, Team, s2003, pc2003)
salaries2003 <- na.omit(salaries2003)
dot4 <- ggplot(salaries2003) +
  geom_point(aes(x = s2003, y = reorder(Team, s2003), color = factor(pc2003)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2002-03", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2004 <- select(salaries, Team, s2004s, pc2004)
salaries2004 <- na.omit(salaries2004)
dot5 <- ggplot(salaries2004) +
  geom_point(aes(x = s2004, y = reorder(Team, s2004), color = factor(pc2004)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2003-04", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2005 <- select(salaries, Team, s2005, pc2005)
salaries2005 <- na.omit(salaries2005)
dot6 <- ggplot(salaries2005) +
  geom_point(aes(x = s2005, y = reorder(Team, s2005), color = factor(pc2005)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2004-05", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2006 <- select(salaries, Team, s2006, pc2006)
salaries2006 <- na.omit(salaries2006)
dot7 <- ggplot(salaries2006) +
  geom_point(aes(x = s2006, y = reorder(Team, s2006), color = factor(pc2006)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2005-06", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2007 <- select(salaries, Team, s2007, pc2007)
salaries2007 <- na.omit(salaries2007)
dot8 <- ggplot(salaries2007) +
  geom_point(aes(x = s2007, y = reorder(Team, s2007), color = factor(pc2007)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2006-07", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2008 <- select(salaries, Team, X2008s, X2008pc)
salaries2008 <- na.omit(salaries2008)
dot9 <- ggplot(salaries2008) +
  geom_point(aes(x = s2008, y = reorder(Team, s2008), color = factor(pc2008)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2007-08", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2009 <- select(salaries, Team, s2009, pc2009)
salaries2009 <- na.omit(salaries2009)
dot10 <- ggplot(salaries2009) +
  geom_point(aes(x = s2009, y = reorder(Team, s2009), color = factor(pc2009)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2008-09", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2010 <- select(salaries, Team, s2010, pc2010)
salaries2010 <- na.omit(salaries2010)
dot11 <- ggplot(salaries2010) +
  geom_point(aes(x = s2010, y = reorder(Team, s2010), color = factor(pc2010)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2009-10", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2011 <- select(salaries, Team, s2011, pc2011)
salaries2011 <- na.omit(salaries2011)
dot12 <- ggplot(salaries2011) +
  geom_point(aes(x = s2011, y = reorder(Team, s2011), color = factor(pc2011)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2010-11", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2012 <- select(salaries, Team, s2012, pc2012)
salaries2012 <- na.omit(salaries2012)
dot13 <- ggplot(salaries2012) +
  geom_point(aes(x = s2012, y = reorder(Team, s2012), color = factor(pc2012)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2011-12", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2013 <- select(salaries, Team, s2013, pc2013)
salaries2013 <- na.omit(salaries2013)
dot14 <- ggplot(salaries2013) +
  geom_point(aes(x = s2013, y = reorder(Team, s2013), color = factor(pc2013)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2012-13", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2014 <- select(salaries, Team, s2014, pc2014)
salaries2014 <- na.omit(salaries2014)
dot15 <- ggplot(salaries2014) +
  geom_point(aes(x = s2014, y = reorder(Team, s2014), color = factor(pc2014)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2013-14", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2015 <- select(salaries, Team, s2015, pc2015)
salaries2015 <- na.omit(salaries2015)
dot16 <- ggplot(salaries2015) +
  geom_point(aes(x = s2015, y = reorder(Team, s2015), color = factor(pc2015)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2014-15", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2016 <- select(salaries, Team, s2016, pc2016)
salaries2016 <- na.omit(salaries2016)
dot17 <- ggplot(salaries2016) +
  geom_point(aes(x = s2016, y = reorder(Team, s2016), color = factor(pc2016)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2015-16", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2017 <- select(salaries, Team, s2017, pc2017)
salaries2017 <- na.omit(salaries2017)
dot18 <- ggplot(salaries2017) +
  geom_point(aes(x = s2017, y = reorder(Team, s2017), color = factor(pc2017)), size = 3) +
  labs(title= "NBA Team Payrolls (USD)", subtitle = "2016-17", 
       x = "Team Payroll", y = "") +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme(plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", 
                     labels = c("No playoffs", 
                                "Made playoffs", 
                                "Won title"), 
                     values = c("0" = "navy", 
                                "10" = "gold3", 
                                "11" = "red")) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dot3 + dot4 + plot_layout(ncol = 2)
dot5 + dot6 + plot_layout(ncol = 2)
dot7 + dot8 + plot_layout(ncol = 2)
dot9 + dot10 + plot_layout(ncol = 2)
dot11 + dot12 + plot_layout(ncol = 2)
dot13 + dot14 + plot_layout(ncol = 2)
dot15 + dot16 + plot_layout(ncol = 2)
dot17 + dot18 + plot_layout(ncol = 2)

salaries2002 <- na.omit(salaries2002)
salaries2002 %>%
  group_by(pc2002) %>%
  summarize(mean = mean(s2002)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2002 <- c("No playoffs", "Made playoffs", "Won title")
lol3 <- ggplot(tbl2, aes(x = pc2002, y = mean)) +
  geom_segment(aes(x = pc2002, xend = pc2002, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2001-02",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2003 <- na.omit(salaries2003)
salaries2003 %>%
  group_by(pc2003) %>%
  summarize(mean = mean(s2003)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2003 <- c("No playoffs", "Made playoffs", "Won title")
lol4 <- ggplot(tbl2, aes(x = pc2003, y = mean)) +
  geom_segment(aes(x = pc2003, xend = pc2003, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2002-03",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2004 <- na.omit(salaries2004)
salaries2004 %>%
  group_by(pc2004) %>%
  summarize(mean = mean(s2004)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2004 <- c("No playoffs", "Made playoffs", "Won title")
lol5 <- ggplot(tbl2, aes(x = pc2004, y = mean)) +
  geom_segment(aes(x = pc2004, xend = pc2004, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2003-04",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2005 <- na.omit(salaries2005)
salaries2005 %>%
  group_by(pc2005) %>%
  summarize(mean = mean(s2005)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2005 <- c("No playoffs", "Made playoffs", "Won title")
lol6 <- ggplot(tbl2, aes(x = pc2005, y = mean)) +
  geom_segment(aes(x = pc2005, xend = pc2005, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2004-05",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2006 <- na.omit(salaries2006)
salaries2006 %>%
  group_by(pc2006) %>%
  summarize(mean = mean(s2006)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2006 <- c("No playoffs", "Made playoffs", "Won title")
lol7 <- ggplot(tbl2, aes(x = pc2006, y = mean)) +
  geom_segment(aes(x = pc2006, xend = pc2006, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2005-06",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2007 <- na.omit(salaries2007)
salaries2007 %>%
  group_by(pc2007) %>%
  summarize(mean = mean(s2007)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2007 <- c("No playoffs", "Made playoffs", "Won title")
lol8 <- ggplot(tbl2, aes(x = pc2007, y = mean)) +
  geom_segment(aes(x = pc2007, xend = pc2007, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2006-07",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2008 <- na.omit(salaries2008)
salaries2008 %>%
  group_by(pc2008) %>%
  summarize(mean = mean(s2008)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2008 <- c("No playoffs", "Made playoffs", "Won title")
lol9 <- ggplot(tbl2, aes(x = pc2008, y = mean)) +
  geom_segment(aes(x = pc2008, xend = pc2008, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2007-08",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2009 <- na.omit(salaries2009)
salaries2009 %>%
  group_by(pc2009) %>%
  summarize(mean = mean(s2009)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2009 <- c("No playoffs", "Made playoffs", "Won title")
lol10 <- ggplot(tbl2, aes(x = pc2009, y = mean)) +
  geom_segment(aes(x = pc2009, xend = pc2009, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2008-09",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2010 <- na.omit(salaries2010)
salaries2010 %>%
  group_by(pc2010) %>%
  summarize(mean = mean(s2010)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2010 <- c("No playoffs", "Made playoffs", "Won title")
lol11 <- ggplot(tbl2, aes(x = pc2010, y = mean)) +
  geom_segment(aes(x = pc2010, xend = pc2010, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2009-10",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2011 <- na.omit(salaries2011)
salaries2011 %>%
  group_by(pc2011) %>%
  summarize(mean = mean(s2011)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2011 <- c("No playoffs", "Made playoffs", "Won title")
lol12 <- ggplot(tbl2, aes(x = pc2011, y = mean)) +
  geom_segment(aes(x = pc2011, xend = pc2011, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2010-11",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2012 <- na.omit(salaries2012)
salaries2012 %>%
  group_by(pc2012) %>%
  summarize(mean = mean(s2012)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2012 <- c("No playoffs", "Made playoffs", "Won title")
lol13 <- ggplot(tbl2, aes(x = pc2012, y = mean)) +
  geom_segment(aes(x = pc2012, xend = pc2012, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2011-12",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2013 <- na.omit(salaries2013)
salaries2013 %>%
  group_by(pc2013) %>%
  summarize(mean = mean(s2013)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2013 <- c("No playoffs", "Made playoffs", "Won title")
lol14 <- ggplot(tbl2, aes(x = pc2013, y = mean)) +
  geom_segment(aes(x = pc2013, xend = pc2013, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2012-13",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2014 <- na.omit(salaries2014)
salaries2014 %>%
  group_by(pc2014) %>%
  summarize(mean = mean(s2014)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2014 <- c("No playoffs", "Made playoffs", "Won title")
lol15 <- ggplot(tbl2, aes(x = pc2014, y = mean)) +
  geom_segment(aes(x = pc2014, xend = pc2014, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2013-14",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2015 <- na.omit(salaries2015)
salaries2015 %>%
  group_by(pc2015) %>%
  summarize(mean = mean(s2015)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2015 <- c("No playoffs", "Made playoffs", "Won title")
lol16 <- ggplot(tbl2, aes(x = pc2015, y = mean)) +
  geom_segment(aes(x = pc2015, xend = pc2015, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2014-15",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2016 <- na.omit(salaries2016)
salaries2016 %>%
  group_by(pc2016) %>%
  summarize(mean = mean(s2016)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2016 <- c("No playoffs", "Made playoffs", "Won title")
lol17 <- ggplot(tbl2, aes(x = pc2016, y = mean)) +
  geom_segment(aes(x = pc2016, xend = pc2016, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2015-16",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

salaries2017 <- na.omit(salaries2017)
salaries2017 %>%
  group_by(pc2017) %>%
  summarize(mean = mean(s2017)) %>%
  mutate(mean2 = paste('$', round(mean/1000000),'M', sep = "")) -> tbl2
  tbl2$pc2017 <- c("No playoffs", "Made playoffs", "Won title")
lol18 <- ggplot(tbl2, aes(x = pc2017, y = mean)) +
  geom_segment(aes(x = pc2017, xend = pc2017, y = 0, yend = mean)) +
  geom_point(size = 15, color = c("navy", "gold3", "red"), fill = c("navy", "gold3", "red")) +
    labs(title = "Team Payroll Comparisons (USD)", subtitle = "2016-17",
       x = "", y = "Averqge Team Payroll") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(limits = c("No playoffs", "Made playoffs", "Won title")) +
  geom_text(aes(label = mean2), color = "white", fontface = "bold", size = 3) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lol3 + lol4 + plot_layout(ncol = 2)
lol5 + lol6 + plot_layout(ncol = 2)
lol7 + lol8 + plot_layout(ncol = 2)
lol9 + lol10 + plot_layout(ncol = 2)
lol11 + lol12 + plot_layout(ncol = 2)
lol13 + lol14 + plot_layout(ncol = 2)
lol15 + lol16 + plot_layout(ncol = 2)
lol17 + lol18 + plot_layout(ncol = 2)

























