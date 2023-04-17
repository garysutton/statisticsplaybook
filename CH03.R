library(tidyverse)
library(networkD3)
library(patchwork)

packages <- c("tidyverse", "networkD3", "patchwork")
  lapply(packages, library, character.only = TRUE)

draft2 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/draft2.csv")

glimpse(draft2)

dim(draft2)

mutate(draft2, Pk2 = ifelse(Pk %in% 1:5, "1-5",
                            ifelse(Pk %in% 6:10, "6-10",
                                   ifelse(Pk %in% 11:15, "11-15",
                                          ifelse(Pk %in% 16:20, "16-20",
                                                 ifelse(Pk %in% 21:25, "21-25",
                                                        ifelse(Pk %in% 26:30, "26-30", "NA"))))))) -> draft2
draft2$Pk2 <- as.factor(draft2$Pk2)

sumG <- sum(draft2$G)
draft2 %>% 
  group_by(Pk2) %>%
  summarize(mean = mean(G),
            median = median(G),
            pct = sum(G)/sumG) -> tibble1
print(tibble1)

g1 <- ggplot(tibble1, aes(x = Pk2, y = mean)) + 
  geom_bar(stat = "identity", width = .8, 
           fill = "coral", color = "coral4") + 
  labs(title = "Average Career Games Played",
       subtitle = "First-Round Selections between 2000 and 2009 NBA Drafts", 
       x = "Segment", 
       y = "Average Career Games Played",
       caption = "regular season games only") + 
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(mean), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 800) +
  theme(plot.title = element_text(face = "bold"))

g2 <- ggplot(tibble1, aes(x = Pk2, y = median)) + 
  geom_bar(stat = "identity", width = .8, 
           fill = "coral3", color = "coral4") + 
  labs(title = "Median Career Games Played",
       subtitle = "First-Round Selections between 2000 and 2009 NBA Drafts", 
       x = "Segment", 
       y = "Median Career Games Played",
       caption = "regular season games only") + 
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(median), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 800) +
  theme(plot.title = element_text(face = "bold"))

g1 + g2 + plot_layout(ncol = 1)

sumMP <- sum(draft2$MP)
draft2 %>% 
  group_by(Pk2) %>%
  summarize(mean = mean(MP),
            median = median(MP),
            pct = sum(MP)/sumMP) -> tibble2

mp1 <- ggplot(tibble2, aes(x = Pk2, y = mean)) + 
  geom_bar(stat = "identity", width = .8, 
           fill = "deepskyblue", color = "deepskyblue4") + 
  labs(title = "Average Minutes Played per Game",
       subtitle = "First-Round Selections between 2000 and 2009 NBA Drafts", 
       x = "Segment", 
       y = "Average Minutes Played per Game",
       caption = "regular season games only") + 
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(mean), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 30) +
  theme(plot.title = element_text(face = "bold"))

mp2 <- ggplot(tibble2, aes(x = Pk2, y = median)) + 
  geom_bar(stat = "identity", width = .8, 
           fill = "deepskyblue3", color = "deepskyblue4") + 
  labs(title = "Median Minutes Played per Game",
       subtitle = "First-Round Selections between 2000 and 2009 NBA Drafts", 
       x = "Segment", 
       y = "Median Minutes Played per Game",
       caption = "regular season games only") + 
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(median), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 30) +
  theme(plot.title = element_text(face = "bold"))

mp1 + mp2 + plot_layout(ncol = 1)

sumWS <- sum(draft2$WS)
draft2 %>% 
  group_by(Pk2) %>%
  summarize(mean = mean(WS),
            median = median(WS),
            pct = sum(WS)/sumWS) -> tibble3

ws1 <- ggplot(tibble3, aes(x = Pk2, y = mean)) + 
  geom_bar(stat = "identity", width = .8, 
           fill = "springgreen", color = "springgreen4") + 
  labs(title = "Average Career Win Shares",
       subtitle = "First-Round Selections between 2000 and 2009 NBA Drafts", 
       x = "Segment", 
       y = "Average Career Win Shares",
       caption = "regular season games only") + 
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(mean), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 60) +
  theme(plot.title = element_text(face = "bold"))

ws2 <- ggplot(tibble3, aes(x = Pk2, y = median)) + 
  geom_bar(stat = "identity", width = .8, fill = "springgreen3", color = "springgreen4") + 
  labs(title = "Median Career Win Shares",
       subtitle = "First-Round Selections between 2000 and 2009 NBA Drafts", 
       x = "Segment", y = "Median Career Win Shares",
       caption = "regular season games only") + 
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30")) +
  geom_text(aes(label = trunc(median), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 70) +
  theme(plot.title = element_text(face = "bold"))

ws1 + ws2 + plot_layout(ncol = 1)

sumWS48 <- sum(draft2$WS48)
draft2 %>% 
  group_by(Pk2) %>%
  summarize(mean = mean(WS48),
            median = median(WS48),
            pct = sum(WS48)/sumWS48) -> tibble4

ws3 <- ggplot(tibble4, aes(x = Pk2, y = mean)) + 
  geom_bar(stat = "identity", width = .8, 
           fill = "gold", color = "gold4") + 
  labs(title = "Average Win Shares per 48 Minutes",
       subtitle = "First-Round Selections between 2000 and 2009 NBA Drafts", 
       x = "Segment", 
       y = "Average Win Shares per 48 Minutes",
       caption = "regular season games only") + 
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15",
                              "16-20", "21-25", "26-30")) +
  geom_text(aes(label = round(mean, 2), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 0.13) +
  theme(plot.title = element_text(face = "bold"))

ws4 <- ggplot(tibble4, aes(x = Pk2, y = median)) + 
  geom_bar(stat = "identity", width = .8, fill = "gold3", color = "gold4") + 
  labs(title = "Median Win Shares per 48 Minutes",
       subtitle = "First-Round Selections between 2000 and 2009 NBA Drafts", 
       x = "Segment", y = "Median Win Shares per 48 Minutes",
       caption = "regular season games only") + 
  scale_x_discrete(limits = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30"),
                   labels = c("1-5", "6-10", "11-15", 
                              "16-20", "21-25", "26-30")) +
  geom_text(aes(label = round(median, 2), vjust = -0.3)) +
  geom_label(aes(label = trunc(pct*100), vjust = 1.2)) +
  ylim(0, 0.13) +
  theme(plot.title = element_text(face = "bold"))

ws3 + ws4 + plot_layout(ncol = 1)

draft2 %>%
  mutate(Age2 = trunc(Age)) -> draft2

draft2 %>%
  mutate(draft2, WS2 = trunc(WS)) %>%
  mutate(WS3 = case_when(WS2 <= 19 ~ "<20",
                         WS2 >= 20 & WS2 <= 39 ~ "20-39",
                         WS2 >= 40 & WS2 <= 59 ~ "40-59",
                         WS2 >= 60 & WS2 <= 79 ~ "60-79",
                         WS2 >= 80 & WS2 <= 99 ~ "80-99",
                         WS2 >= 100 ~ "100+")) -> draft2
nodes <- data.frame(
  "name" = c("USA", "World",
             "0", "1",
             "17", "18", "19", "20", "21", "22", "23", "24", "25",
             "Big",  "Center", "Forward", "Guard",  "Swingman",
             "1-5",  "6-10", "11-15", "16-20", "21-25", "26-30",
             "<20", "20-39", "40-59", "60-79", "80-99", "100+"))
links <- as.data.frame(matrix(c(
  0,2,21, 0,3,203,
  1,2,51, 1,3,16,
  2,4,1, 2,5,20, 2,6,19, 2,7,15, 2,8,12, 2,9,5, 2,10,0, 2,11,0, 2,12,0,
  3,4,0, 3,5,3, 3,6,32, 3,7,50, 3,8,58, 3,9,58, 3,10,14, 3,11,3, 3,12,1,
  4,13,0, 4,14,0, 4,15,1, 4,16,0, 4,17,0,
  5,13,2, 5,14,8, 5,15,6, 5,16,2, 5,17,5,
  6,13,11, 6,14,6, 6,15,15, 6,16,14, 6,17,5,
  7,13,7, 7,14,12, 7,15,19, 7,16,24, 7,17,3,
  8,13,9, 8,14,7, 8,15,19, 8,16,25, 8,17,10,
  9,13,5, 9,14,5, 9,15,23, 9,16,24, 9,17,6,
  10,13,0, 10,14,1, 10,15,4, 10,16,6, 10,17,3,
  11,13,0, 11,14,1, 11,15,2, 11,16,0, 11,17,0,
  12,13,0, 12,14,1, 12,15,0, 12,16,0, 12,17,0, 
  13,18,7, 13,19,6, 13,20,8, 13,21,3, 13,22,2, 13,23,8,
  14,18,7, 14,19,6, 14,20,7, 14,21,7, 14,22,6, 14,23,9,
  15,18,16, 15,19,18, 15,20,13, 15,21,13, 15,22,13, 15,23,15,
  16,18,15, 16,19,13, 16,20,15, 16,21,22, 16,22,18, 16,23,12,
  17,18,5, 17,19,6, 17,20,7, 17,21,5, 17,22,3, 17,23,6,
  18,24,12, 18,25,9, 18,26,9, 18,27,6, 18,28,2, 18,29,12,
  19,24,19, 19,25,15, 19,26,5, 19,27,7, 19,28,3, 19,29,1,
  20,24,33, 20,25,9, 20,26,3, 20,27,3, 20,28,1, 20,29,0,
  21,24,27, 21,25,12, 21,26,8, 21,27,1, 21,28,2, 21,29,0,
  22,24,30, 22,25,10, 22,26,7, 22,27,2, 22,28,1, 22,29,0,
  23,24,26, 23,25,10, 23,26,2, 23,27,3, 23,28,0, 23,29,1),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

draft2 %>%
  count(Born2, College2)
draft2 %>%
  count(College2, Age2)
draft2 %>%
  count(Age2, Pos2)
draft2 %>%
  count(Pos2, Pk2)
draft2 %>%
  count(Pk2, WS3)

c(draft2 %>%
    filter(Pk < 6 & WS > 100) %>%
    tally() / 50,
  draft2 %>%
    filter(Pk < 6 & WS > 75 & WS < 100) %>%
    tally() / 50,
  draft2 %>%
    filter(Pk < 6 & WS > 50 & WS < 75) %>%
    tally() / 50,
  draft2 %>%
    filter(Pk < 6 & WS > 25 & WS < 50) %>%
    tally() / 50,
  draft2 %>%
    filter(Pk < 6 & WS < 25) %>%
    tally() / 50) -> probs1
print(probs1)

c(draft2 %>%
    filter(Pk < 6 & WS > 100) %>%
    summarize(med = median(WS)),
  draft2 %>%
    filter(Pk < 6 & WS > 75 & WS < 100) %>%
    summarize(med = median(WS)),
  draft2 %>%
    filter(Pk < 6 & WS > 50 & WS < 75) %>%
    summarize(med = median(WS)),
  draft2 %>%
    filter(Pk < 6 & WS > 25 & WS < 50) %>%
    summarize(med = median(WS)),
  draft2 %>%
    filter(Pk < 6 & WS < 25) %>%
    summarize(med = median(WS))) -> vals1
print(vals1)

sum(as.numeric(probs1) * as.numeric(vals1))

c(draft2 %>%
    filter(Pk > 5 & WS > 100) %>%
    tally() / 241,
  draft2 %>%
    filter(Pk > 5 & WS > 75 & WS < 100) %>%
    tally() / 241,
  draft2 %>%
    filter(Pk > 5 & WS > 50 & WS < 75) %>%
    tally() / 241,
  draft2 %>%
    filter(Pk > 5 & WS > 25 & WS < 50) %>%
    tally() / 241,
  draft2 %>%
    filter(Pk > 5 & WS < 25) %>%
    tally() / 241) -> probs2
print(probs2)

c(draft2 %>%
    filter(Pk > 5 & WS > 100) %>%
    summarize(med = median(WS)),
  draft2 %>%
    filter(Pk > 5 & WS > 75 & WS < 100) %>%
    summarize(med = median(WS)),
  draft2 %>%
    filter(Pk > 5 & WS > 50 & WS < 75) %>%
    summarize(med = median(WS)),
  draft2 %>%
    filter(Pk > 5 & WS > 25 & WS < 50) %>%
    summarize(med = median(WS)),
  draft2 %>%
    filter(Pk > 5 & WS < 25) %>%
    summarize(med = median(WS))) -> vals2
print(vals2)

sum(as.numeric(probs2) * as.numeric(vals2))

draft_clust <- select(draft2, Pk, WS)
draft_clust %>%
  group_by(Pk) %>%
  summarize(ws = mean(WS)) -> draft_clust_final
head(draft_clust_final, n = 3)
tail(draft_clust_final, n = 3)

d <- dist(draft_clust_final, method = "euclidean")
print(d)

hc <- hclust(d, method = "complete")
bg = par(bg = "darkseagreen1")
plot(as.dendrogram(hc, cex = 0.6, hang = -1),
     main = "Cluster Dendrogram: Win Shares by First-Round Selection",
     xlab = "First-Round Selection Number\n2000-2009 NBA Drafts",
     ylab = "Height (aka Euclidian Distance")
rect.hclust(hc, k = 2)

