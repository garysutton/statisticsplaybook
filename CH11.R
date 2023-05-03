library(tidyverse)
library(scales)
library(patchwork)
library(factoextra)

salaries <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/salaries.csv")

salaries %>%
  select(Team, sa2017, w2017) %>%
  glimpse 

var1 <- c(2, 4, 6, 8, 10)
var2 <- c(1, 2, 3, 10, 14)
df <- data.frame(var1, var2)
print(df)

sd(df$var1)
sd(df$var2)

df %>%
  mutate(zvar1 = (var1 - mean(var1)) / sd(var1),
         zvar2 = (var2 - mean(var2)) / sd(var2)) -> df
print(df)

options(scipen = 999)

salaries %>%
  filter(Team == "Charlotte Hornets") -> cha

cha %>%
  select(Team, sa2017:sa2005, sa2002:sa2000, w2017:w2005, w2002:w2000) -> cha

cha %>%
  mutate(sumSalaries = rowSums(.[2:17]),
         sumWins = rowSums(.[18:33]),
         efficiency = rowSums(.[2:17]) / rowSums(.[18:33]),
         meanWins = round(rowSums(.[18:33]) / 16)) -> cha

cha %>%
  select(Team, sumSalaries, sumWins, efficiency, meanWins) -> cha_final
print(cha_final)

salaries %>%
  filter(Team == "New Orleans Pelicans") -> nop

nop %>%
  select(Team, sa2017:sa2003, w2017:w2003) -> nop

nop %>%
  mutate(sumSalaries = rowSums(.[2:16]),
         sumWins = rowSums(.[17:31]),
         efficiency = rowSums(.[2:16]) / rowSums(.[17:31]),
         meanWins = round(rowSums(.[17:31]) / 15)) -> nop

nop %>%
  select(Team, sumSalaries, sumWins, efficiency, meanWins) -> nop_final
print(nop_final)

salaries %>%
  filter(Team != "Charlotte Hornets" & Team != "New Orleans Pelicans") -> league

league %>%
  select(Team, sa2017:sa2000, w2017:w2000) -> league

league %>%
  mutate(sumSalaries = rowSums(.[2:19]),
         sumWins = rowSums(.[20:37]),
         efficiency = rowSums(.[2:19]) / rowSums(.[20:37]),
         meanWins = round(rowSums(.[20:37]) / 18)) -> league

league %>%
  select(Team, sumSalaries, sumWins, efficiency, meanWins) -> league_final
print(league_final)

final <- rbind(cha_final, nop_final, league_final)
head(final)

final %>%
  mutate(zSalaries = (sumSalaries - mean(sumSalaries)) / sd(sumSalaries),
         zWins = (sumWins - mean(sumWins)) / sd(sumWins)) -> final
head(final, n = 3)
tail(final, n = 3)

p1 <- ggplot(final) +
  geom_segment(aes(x = Team, xend = Team, 
                   y = zSalaries, yend = zWins), color = "grey50") +
  geom_point(aes(x = Team, y = zSalaries), color = "springgreen3", size = 3) +
  geom_point(aes(x = Team, y = zWins), color = "darkred", size = 3) +
  labs(title = "Comparison of Inflation-Adjusted Payrolls versus Regular Season Wins",
       subtitle = "2000-17", 
       x = "", 
       y = "Standard Deviations", 
       caption = "green/light = salaries\nred/dark = wins") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)

final %>%
  tally(zWins > zSalaries)
final %>%
  tally(zWins < zSalaries)  

final %>%
  filter(zWins > zSalaries) %>%
  summarize(mean = mean(zWins - zSalaries),
            median = median(zWins - zSalaries))

final %>%
  filter(zSalaries > zWins) %>%
  summarize(mean = mean(zSalaries - zWins),
            median = median(zSalaries - zWins))

p2 <- ggplot(final, aes(x = reorder(Team, -efficiency), y = efficiency)) + 
  geom_bar(stat = "identity", width = .5, fill = "darkorange1") + 
  coord_flip() +
  labs(title = "NBA Team Efficiency: Salary Spend per Win (2000-17)", 
       subtitle = "2021 USD", 
       x = "", 
       y = "Salary Spend per Regular Season Win", 
       caption = "Average number of regular season wins affixed atop bars") + 
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  geom_text(aes(label = meanWins, fontface = "bold", 
                vjust = 0.3, hjust = -0.4)) +
  theme(plot.title = element_text(face = "bold"))
print(p2)

cha %>%
  mutate(salarytotal = rowSums(.[2:17]),
         wintotal = rowSums(.[18:33])) -> cha_kmeans

cha_kmeans$salarytotal <- cha_kmeans$salarytotal * 1.11 
cha_kmeans$wintotal <- round(cha_kmeans$wintotal * 1.11)

cha_kmeans %>%
  select(Team, salarytotal, wintotal) -> cha_kmeans

nop %>%
  mutate(salarytotal = rowSums(.[2:16]),
         wintotal = rowSums(.[17:31])) -> nop_kmeans

nop_kmeans$salarytotal <- nop_kmeans$salarytotal * 1.17
nop_kmeans$wintotal <- round(nop_kmeans$wintotal) * 1.17

nop_kmeans %>%
  select(Team, salarytotal, wintotal) -> nop_kmeans

league %>%
  mutate(salarytotal = rowSums(.[2:19]),
         wintotal = rowSums(.[20:37])) -> league_kmeans

league_kmeans %>%
  select(Team, salarytotal, wintotal) -> league_kmeans

final_kmeans <- rbind(cha_kmeans, nop_kmeans, league_kmeans)

final_kmeans %>%
  select(salarytotal, wintotal) %>%
  trunc(final_kmeans$wintotal) -> final_kmeans
print(final_kmeans)

rownames(final_kmeans) <- c("CHA", "NOP", "ATL", "BOS", "BKN", "CHI",
                            "CLE", "DAL", "DEN", "DET", "GSW", "HOU",
                            "IND", "LAC", "LAL", "MEM", "MIA", "MIL",
                            "MIN", "NYK", "OKC", "ORL", "PHI", "PHO",
                            "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

options(scipen = 000)
p3 <- fviz_nbclust(final_kmeans, kmeans, method = "wss") 
p4 <- fviz_nbclust(final_kmeans, kmeans, method = "silhouette") 

p3 + p4 + plot_layout(ncol = 1)

k <- kmeans(final_kmeans, 4, iter.max = 25, nstart = 25) 
print(k)

p5 <- fviz_cluster(k, data = final_kmeans,
             main = "K-means Cluster of Payrolls and Wins (2000-17)",
             subtitle = "K = 4",
             xlab = "Team Payrolls",
             ylab = "Regular Season Wins",
             font.main = "bold") + 
      theme(legend.position = "none")
print(p5)

shapiro.test(final_kmeans$salarytotal)
shapiro.test(final_kmeans$wintotal)

# following code not included in manuscript
k <- kmeans(final_kmeans, 2, iter.max = 25, nstart = 25) 
print(k)

p6 <- fviz_cluster(k, data = final_kmeans,
                   main = "K = 2",
                   xlab = "Team Payrolls",
                   ylab = "Regular Season Wins",
                   font.main = "bold") + 
  theme(legend.position = "none")

k <- kmeans(final_kmeans, 3, iter.max = 25, nstart = 25) 
print(k)

p7 <- fviz_cluster(k, data = final_kmeans,
                   main = "K = 3",
                   xlab = "Team Payrolls",
                   ylab = "Regular Season Wins",
                   font.main = "bold") + 
  theme(legend.position = "none")

k <- kmeans(final_kmeans, 4, iter.max = 25, nstart = 25) 
print(k)

p8 <- fviz_cluster(k, data = final_kmeans,
                   main = "K = 4",
                   xlab = "Team Payrolls",
                   ylab = "Regular Season Wins",
                   font.main = "bold") + 
  theme(legend.position = "none")

k <- kmeans(final_kmeans, 5, iter.max = 25, nstart = 25) 
print(k)

p9 <- fviz_cluster(k, data = final_kmeans,
                   main = "K = 5",
                   xlab = "Team Payrolls",
                   ylab = "Regular Season Wins",
                   font.main = "bold") + 
  theme(legend.position = "none")

k <- kmeans(final_kmeans, 6, iter.max = 25, nstart = 25) 
print(k)

p10 <- fviz_cluster(k, data = final_kmeans,
                   main = "K = 6",
                   xlab = "Team Payrolls",
                   ylab = "Regular Season Wins",
                   font.main = "bold") + 
  theme(legend.position = "none")

k <- kmeans(final_kmeans, 7, iter.max = 25, nstart = 25) 
print(k)

p11 <- fviz_cluster(k, data = final_kmeans,
                    main = "K = 7",
                    xlab = "Team Payrolls",
                    ylab = "Regular Season Wins",
                    font.main = "bold") + 
  theme(legend.position = "none")

#####
p6 + p7 + p8 + p9 + p10 + p11 + plot_layout(ncol = 3)















