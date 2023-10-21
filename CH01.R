
library(tidyverse)
library(patchwork)

attach(mtcars)

packages <- read.csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/packages2.csv",
                     header = TRUE, stringsAsFactors = FALSE)

cor(mtcars$mpg, mtcars$wt) 
p1 <- ggplot(mtcars, aes(x = mpg, y = wt)) + 
  geom_point() +
  labs(title = "MPG vs. Weight in 1973-74 Automobiles",
       subtitle = "correlation coefficient = -0.87",
       x = "Miles per Gallon", 
       y = "Weight (x 1,000 lbs.)") + 
  geom_smooth(method = lm) +
  theme(plot.title = element_text(face = 'bold'))

mtcars$cyl <- factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am)
p2 <- ggplot(mtcars, aes(x = am, y = mpg)) +
  geom_boxplot(color = "palegreen4", fill = "palegreen1") +
  labs(title = "Average MPG by Cylinders and Transmission",
       x = "", 
       y = "Miles per Gallon (mpg)", subtitle = "Select 1973-74 Automobiles") + 
  stat_summary(fun = mean, geom = "point", shape = 20,
               size = 4, color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) +
  facet_wrap(~cyl) +
  scale_x_discrete(breaks = c(0, 1),
                   labels = c("Automatic", "Manual"))

mtcars$gear <- factor(mtcars$gear)
mtcars %>% 
  group_by(gear) %>%
  summarize(mean = mean(mpg)) -> tibble1
print(tibble1)
p3 <- ggplot(tibble1, aes(x = reorder(gear, -mean), y = mean)) + 
  geom_bar(stat = "identity", width = .5, fill  = "orange1") + 
  labs(title = "Average MPG by Number of Forward Gears",
       subtitle = "Select 1973-74 Automobiles", 
       x = "Number of Forward Gears", 
       y = "Average Miles per Gallon (mpg)") + 
  geom_text(aes(label = (trunc(mean)), vjust = -0.3)) +
  theme(plot.title = element_text(face = "bold")) +
  ylim(0, 26) 

p4 <- ggplot(mtcars, aes(x = mpg)) + 
  geom_histogram(fill = "indianred1", color = "indianred4", 
                 bins = 6) + 
  labs(title = "Distribution of MPG in 1973-74 Automobiles", 
       subtitle = "n = 32",
       x = "Miles per Gallon (mpg)", y = "Frequency") +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(plot.title = element_text(face = 'bold')) +
  annotate("text", x = 28.6, y = 8.5, 
           label = "Highest MPG: Toyota Corolla", size = 3) + 
  annotate("text", x = 28.6, y = 8, 
           label = "Lowest MPG: Cadillac Fleetwood and Lincoln Continental",
           size = 3)

p1 + p2 + p3 + p4 + plot_layout(ncol = 2)

mutate(packages, date = substr(date, 1, 4)) -> packages
packages %>%
  group_by(date) %>%
  tally() -> tibble2
tibble2 %>%
  filter(date >= 2015 & date <= 2022) -> tibble2
tibble2 %>%
  mutate(pct = (n/19305)*100) -> tibble2
print(tibble2)

p5 <- ggplot(tibble2, aes(x = date, y = n)) + 
  geom_bar(stat = 'identity', width = .5, fill  = "dodgerblue1") + 
  labs(title = "Number of Available CRAN Packages by Publication Year",
       subtitle = "2015-22", 
       x = "Year", 
       y = "Count", 
       caption = "March 2023 snapshot") + 
  geom_text(aes(label = n, vjust = -0.3)) +
  geom_label(aes(label = (trunc(pct)), vjust = 1.2)) +
  theme(plot.title = element_text(face = "bold")) 
print(p5)

df1 <- data.frame(
  platform = c("R","Matlab","SAS","Minitab","SPSS", "JMP") ,  
  jobs = c(18750, 12870, 9197, 2882, 2185, 1770)
)
df1$platform <- factor(df1$platform, levels = c("R","Matlab","SAS","Minitab","SPSS", "JMP"))
p6 <- ggplot(df1, aes(x = platform, y = jobs)) + 
  geom_bar(stat = "identity", width = .5, 
           color = c("dodgerblue1", "grey40", "grey40", "grey40", "grey40", "grey40"),
           fill = c("dodgerblue1", "grey40", "grey40", "grey40", "grey40", "grey40")) +
  labs(title = "U.S. Job Opportunity Counts",
       subtitle = "March 2023", 
       x = "Programming Language", 
       y = "Count") + 
  geom_text(aes(label = jobs, vjust = -0.3)) +
  theme(plot.title = element_text(face = "bold")) 
print(p6)





