library(tidyverse)

df <- read_csv('/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba_lindy.csv')

df %>%
  filter(parent_child != "parent")

ggplot(df, aes(x = years)) +
  geom_density(alpha = .3, fill = "blue") +
  labs(title = "Distribution of NBA Franchises by Number of Years in League",
       subtitle = "1946-2020",
       x = "Years in NBA", y = "Density",
       caption = "one-to-one relationship between franchises and team names") +
  theme(plot.title = element_text(face = "bold")) 

###########################################################

library(tidyverse) 
library(patchwork)
library(car)
library(ggpubr)
library(ggQC)
library(qcc)

df1 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba_lindy.csv") 

glimpse(df1)

df1$franchise <- as.factor(df1$franchise)
df1$parent_child <- as.factor(df1$parent_child)
df1$active_inactive <- as.factor(df1$active_inactive)

summary(df1)

df1 %>%
  filter(parent_child != "parent") -> df2
dim(df2)

p2 <- ggviolin(df2, y = "games",
               color = "darkslategray1", fill = "salmon1",
               add = "boxplot", add.params = list(fill = "white"),
               main = "Distribution of Games Played:\nAll NBA Franchises",
               font.main = "bold",
               subtitle = "1946-2020", 
               xlab = "", 
               ylab = "Games Played") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "darkslategray1", fill = "darkslategray1") + 
  theme(axis.text.x = element_blank())

p3 <- ggviolin(df2, y = "wins",
               color = "darkslategray1", fill = "salmon1",
               add = "boxplot", add.params = list(fill = "white"),
               main = "Distribution of Games Won:\nAll NBA Franchises",
               font.main = "bold",
               subtitle = "1946-2020", 
               xlab = "", 
               ylab = "Games Won") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, 
               color = "darkslategray1", fill = "darkslategray1") + 
  theme(axis.text.x = element_blank())

df2 %>%
  pivot_longer(cols = c("games", "wins"),
               names_to = "games",
               values_to = "counts") -> df3

df3$games <- as.factor(df3$games)

df3$games <- recode(df3$games, "games" = "Played",
                        "wins" = "Won")
head(df3[,c(1, 7, 8)], 10)

p4 <- gghistogram(df3, x = "counts", bins = 15,
   add = "mean", rug = TRUE,
   main = "Distributions of Games Played and Won",
   font.main = "bold", 
   subtitle = "1946-2020", 
   xlab = "Games Played / Games Won", 
   ylab = "Frequency",
   legend.title = "", font.legend = "bold",
   legend = "top",
   color = "games", fill = "games") 

p2 + p3 - p4 + plot_layout(ncol = 1)

df1 %>%
  filter(parent_child != "child") -> df4
dim(df4)

df4 %>%
  group_by(active_inactive) %>%
  tally()

df4 %>%
  group_by(franchise) %>%
  summarize(games) %>%
ggplot(aes(x = franchise, y = games)) +
        theme(axis.text.x = element_text(angle = 90, 
                                         hjust = 1, vjust = 0.5)) +
        stat_pareto() +
  ggtitle("Pareto Chart: Games Played (1946-2020)") +
  xlab("") +
  ylab("Games Played") +
  theme(plot.title = element_text(face = "bold"))   

pareto.chart(df4$wins,  
             main = "Pareto Chart: Games Won (1946-2020)", 
             xlab = "NBA Franchises",
             col = heat.colors(length(df4$wins)),
             xaxt = "n")












