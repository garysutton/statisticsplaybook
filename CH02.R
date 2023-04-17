library(tidyverse)
library(reshape2)
library(sqldf)
library(patchwork)

draft <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/draft.csv")

dim(draft)

draft <- select(draft,-c(3,4,16:24))

draft <- draft[-c(90, 131),]

glimpse(draft)

head(draft, 3) 
tail(draft, 3)

draft$Year <- as.factor(draft$Year)
draft$Tm <- as.factor(draft$Tm)
draft$Pos <- as.factor(draft$Pos)
draft$Born <- as.factor(draft$Born)
draft$From <- as.factor(draft$From)
draft$To <- as.factor(draft$To)

class(draft$Year)

mutate(draft, Born2 = ifelse(Born == "us", "USA", "World")) -> draft
draft$Born2 <- as.factor(draft$Born2)

draft$College[is.na(draft$College)] <- 0 
mutate(draft, College2 = ifelse(College == 0, 0, 1)) -> draft
draft$College2 <- as.factor(draft$College2)

levels(draft$Pos)

draft$Pos2 <- draft$Pos
recode(draft$Pos2, "C" = "Center", "C-F" = "Big", "F" = "Forward", "F-C" = "Big", "F-G" = "Swingman", "G" = "Guard", "G-F" = "Swingman") -> levels(draft$Pos2)
draft$College2 <- factor(draft$College2)

head(draft)

summary(draft)
sd(draft$G)
sd(draft$MP)
sd(draft$WS)

sqldf("SELECT min(WS), Player, Tm, Pk, Year FROM draft")

sqldf("SELECT max(WS), Player, Tm, Pk, Year FROM draft")

sqldf("SELECT min(G), Player, Tm, Pk, Year FROM draft")

sqldf("SELECT max(G), Player, Tm, Pk, Year FROM draft")

sqldf("SELECT min(MP), Player, Tm, Pk, Year FROM draft")

sqldf("SELECT max(MP), Player, Tm, Pk, Year FROM draft")

sqldf("SELECT min(Age), Player, Tm, Pk, Year FROM draft")

sqldf("SELECT max(Age), Player, Tm, Pk, Year FROM draft")

p1 <- ggplot(draft, aes(x = WS)) + 
  geom_histogram(fill = "royalblue3", color = "royalblue3", 
                 bins = 8) + 
  labs(title = "Career Win Shares Distribution of NBA First-Round Selections",
       subtitle = "2000-09 NBA Drafts",
       x = "Career Win Shares",
       y = "Frequency") +
  theme(plot.title = element_text(face = "bold"))
print(p1)

sqldf("SELECT COUNT (*) FROM draft WHERE WS >= 75")

sqldf("select COUNT (*) FROM draft WHERE WS < 75")

sqldf("select COUNT (*) FROM draft WHERE WS <= 25")

p2 <- ggplot(draft, aes(x = College2, y = WS)) + 
  geom_boxplot(color = "orange4", fill = "orange1") +  
  labs(title = "Career Win Shares Distribution of NBA First-Round Selections",
       x = "", 
       y = "Career Win Shares", 
       subtitle = "2000-09 NBA Drafts") + 
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 8, color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) +
  facet_wrap(~Born2) + 
  scale_x_discrete(breaks = c(0, 1),
                   labels = c("No College", "College"))
print(p2)

p3 <- ggplot(draft, aes(x = Year, y = WS)) + 
  geom_boxplot(color = "dodgerblue4", fill = "dodgerblue" ) +
  labs(title = "Year-over_Year Win Shares Distribution of NBA First-Round Selections",
       x = "", 
       y = "Win Shares", 
       subtitle = "2000-09 NBA Drafts") + 
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 8, color = "white", fill = "white") + 
  theme(plot.title = element_text(face = "bold")) 
print(p3)

draft %>%
  summarize(MIN = min(WS),
            LQ = quantile(WS, .25),
            UQ = quantile(WS, .75),
            AVG = mean(WS),
            M = median(WS),
            MAX = max(WS)) -> first_tibble
print(first_tibble)

draft %>%
  group_by(Year) %>%
  summarize(avg = mean(WS)) -> second_tibble
print(second_tibble)

draft %>%
  group_by(Year) %>%
  tally(WS >= 75) -> third_tibble
print(third_tibble) 

sqldf("SELECT Player, Pk FROM draft WHERE WS >= 75 AND Year == 2001")

sqldf("SELECT Player, Pk, WS FROM draft WHERE WS >= 75 AND Year == 2003 ORDER BY WS DESC")

draft %>%
  filter(WS >= 75) %>%
  group_by(Year) %>%
  summarize(avg = mean(Pk)) -> fourth_tibble
print(fourth_tibble)

sqldf("SELECT AVG(Pk) FROM draft WHERE WS >= 75")

draft %>%
  filter(WS >= 75) %>%
  group_by(Year) %>%
  summarize(med = median(Pk)) -> fifth_tibble
print(fifth_tibble)

sqldf("SELECT MEDIAN(Pk) FROM draft WHERE WS >= 75")

draft %>%
  filter(WS >= 100) %>%
  group_by(Year) %>%
  summarize(avg = mean(Pk)) -> sixth_tibble
print(sixth_tibble)

sqldf("SELECT AVG(Pk) FROM draft WHERE WS >= 100")

draft %>%
  filter(WS >= 100) %>%
  group_by(Year) %>%
  summarize(med = median(Pk)) -> seventh_tibble
print(seventh_tibble)

sqldf("select MEDIAN(Pk) FROM draft WHERE WS >= 100")

draft %>%
  filter(WS <= 25) %>%
  group_by(Year) %>%
  summarize(avg = mean(Pk),
            med = median(Pk)) -> eighth_tibble
print(eighth_tibble)

sqldf("select AVG(Pk), MEDIAN(Pk) FROM draft WHERE WS <= 25")

draft %>%
  filter(WS <= 5) %>%
  group_by(Year) %>%
  summarize(avg = mean(Pk),
            med = median(Pk)) -> ninth_tibble
print(ninth_tibble)

sqldf("select AVG(Pk), MEDIAN(Pk) FROM draft WHERE WS <= 5")

sqldf("SELECT COUNT (*) FROM draft WHERE WS < 5")

sqldf("SELECT COUNT (*) FROM draft WHERE WS < 0")

draft %>%
  		select(c(Age, G:WS)) -> cor_draft
cor_matrix <- cor(cor_draft)
print (cor_matrix)

cor_table <- melt(cor_matrix)
head(cor_table, n = 3)
tail(cor_table, n = 3)

p4 <- ggplot(data = draft_cor, aes(x = Var1, 
                                   y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, mid = "grey84", 
                       limits = c(-1, 1)) +
  labs(title = "Correlation Matrix", 
    subtitle = "Correlation Coefficients between Win Shares and Other Continuous Variables",
    x = "", 
    y = "", 
    fill = "Correlation\nCoefficient", 
    caption = "Source: draft data set") +
  theme(plot.title = element_text(face = "bold"), 
  legend.title = element_text(face = "bold", color = "brown", 
                              size = 10)) +
  geom_text(aes(x = Var1, y = Var2, 
                label = round(value, 2)), color = "black", 
  fontface = "bold", size = 5)
print(p4)
 
draft %>% 
  group_by(Born2) %>%
  summarize(meanWS = mean(WS),
            medianWS = median(WS)) -> tenth_tibble
print(tenth_tibble)

p5 <- ggplot(tenth_tibble, aes(x = Born2, y = meanWS)) + 
  geom_bar(stat = "identity", width = .5, fill  = "darkorchid4") + 
  labs(title = "Average Career Win Shares by Place of Birth",
       subtitle = "2000-09 NBA Drafts", 
       x = "Where Born", 
       y = "Average Career Win Shares") + 
  geom_text(aes(label = trunc(meanWS), vjust = -0.3)) +
  ylim(0, 35) +
  theme(plot.title = element_text(face = "bold"))

p6 <- ggplot(tenth_tibble, aes(x = Born2, y = medianWS)) + 
  geom_bar(stat = "identity", width = .5, fill  = "sienna1") + 
  labs(title = "Median Career Win Shares by Place of Birth",
       subtitle = "2000-09 NBA Drafts", 
       x = "Where Born", 
       y = "Median Career Win Shares") + 
  geom_text(aes(label = trunc(medianWS), vjust = -0.3)) +
  ylim(0, 35) +
  theme(plot.title = element_text(face = "bold"))

p5 + p6 + plot_layout(ncol = 2)
 
draft %>% 
  group_by(College2) %>%
  summarize(meanWS = mean(WS),
            medianWS = median(WS)) -> eleventh_tibble
print(eleventh_tibble)

p7 <- ggplot(eleventh_tibble, aes(x = College2, y = meanWS)) + 
  geom_bar(stat = "identity", width = .5, fill  = "darkorchid4") + 
  labs(title = "Average Career Win Shares: College vs. No College",
       subtitle = "2000-09 NBA Drafts", 
       x = "College or No College",
       y = "Average Career Win Shares") + 
  scale_x_discrete(breaks = c(0, 1),
                        labels = c("No College", "College")) +
  geom_text(aes(label = trunc(meanWS), vjust = -0.3)) +
  ylim(0, 35) +
  theme(plot.title = element_text(face = "bold"))

p8 <- ggplot(eleventh_tibble, aes(x = College2, y = medianWS)) + 
  geom_bar(stat = "identity", width = .5, fill  = "sienna1") + 
  labs(title = "Median Career Win Shares: College vs. No College",
       subtitle = "2000-09 NBA Drafts", 
       x = "College or No College",
       y = "Median Career Win Shares") + 
  scale_x_discrete(breaks = c(0, 1),
                        labels = c("No College", "College")) +
  geom_text(aes(label = trunc(medianWS), vjust = -0.3)) +
  ylim(0, 35) +
  theme(plot.title = element_text(face = "bold"))

p7 + p8 + plot_layout(ncol = 2)
 
draft %>% 
  group_by(Pos2, Born2, College2) %>%
  summarize(mean = mean(WS),
            median = median(WS)) -> twelfth_tibble
head(twelfth_tibble, n = 3)
tail(twelfth_tibble, n = 3)

new_labels <- c("0" = "No College", "1" = "College")
p9 <- ggplot(twelfth_tibble, aes(x = Pos2, y = mean)) + 
  geom_bar(stat = "identity", width = .5, fill  = "slateblue4") +  
  labs(title = "Average Win Shares by Place of Birth", 
       x = "", 
       y = "Win Shares",
       subtitle = "2000-09 NBA Drafts") + 
  theme(plot.title = element_text(face = "bold")) +
  facet_grid(Born2 ~ College2, labeller = labeller(College2 = new_labels))

new_labels <- c("0" = "No College", "1" = "College")
p10 <- ggplot(twelfth_tibble, aes(x = Pos2, y = median)) + 
  geom_bar(stat = "identity", width = .5, fill  = "indianred3") +  
  labs(title = "Median Win Shares by Place of Birth",
       x = "", 
       y = "Win Shares",
       subtitle = "2000-09 NBA drafts") + 
  theme(plot.title = element_text(face = "bold")) +
  facet_grid(Born2 ~ College2, labeller = labeller(College2 = new_labels))

p9 + p10 + plot_layout(ncol = 2)
 
draft -> draft2
write.csv(draft2,"/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/draft2.csv", row.names = FALSE)
