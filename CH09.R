library(tidyverse)
library(sqldf)
library(gtools)
library(gplots)
library(vcd)
library(questionr)
library(rcompanion)

NBAboxscores <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/2012_18_officialBoxScore.csv")

dim(NBAboxscores) 

NBAboxscores %>%
  select(-starts_with("off")) -> NBAboxscores
dim(NBAboxscores)

NBAboxscores <- unique(NBAboxscores)
dim(NBAboxscores) 

NBAboxscores %>% 
  count(teamPTS > opptPTS) 

NBAboxscores %>% 
  count(teamPTS < opptPTS) 

NBAboxscores %>%
  filter(teamPTS > opptPTS) -> NBAboxscores
dim(NBAboxscores) 

mydata <- NBAboxscores
dim(mydata)

mydata %>%
  select(teamLoc, teamRslt, teamDayOff, opptLoc, opptRslt, 
         opptDayOff) -> mydata
head(mydata) 
tail(mydata)

min(mydata$teamDayOff)
max(mydata$teamDayOff)
min(mydata$opptDayOff)
max(mydata$opptDayOff)

mydata %>%
  filter(teamDayOff <= 4,
         opptDayOff <= 4) -> mydata
dim(mydata) 

map_df(mydata, class)

mydata$teamLoc <- as.factor(mydata$teamLoc)
mydata$teamRslt <- as.factor(mydata$teamRslt)
mydata$teamDayOff <- as.factor(mydata$teamDayOff)
mydata$opptLoc <- as.factor(mydata$opptLoc)
mydata$opptRslt <- as.factor(mydata$opptRslt)
mydata$opptDayOff <- as.factor(mydata$opptDayOff)

mydata %>%
  group_by(teamLoc, teamRslt) %>%
  tally()

n = 5
r = 2
permutationsCount = n^r
paste0("The permuation count equals: ", permutationsCount)

nrow(permutations(n = 5, r = 2, repeats.allowed = TRUE))

permutations(n = 5, r = 2, repeats.allowed = TRUE) - 1

mydata %>%
  group_by(teamLoc, teamDayOff, opptLoc, opptDayOff) %>%
  tally() -> finaldf
print(finaldf, n = 50) 

sum(finaldf$n) 

ggplot(data = finaldf, aes(x = n, y = teamLoc, fill = teamLoc)) +
  geom_bar(stat = "identity") +
  facet_wrap(teamDayOff~opptDayOff) +
  labs(title = 
         "Home and Away Win Totals Broken Down by Days Off Permutations", 
       subtitle = "2012-13 to 2017-18 Regular Seasons", 
       caption = "Top Numbers: Home team prior days off
       Bottom Numbers: Away team prior days off",
       x = "Win Totals",
       y = "") +
  xlim(0,1500) +
  geom_text(aes(label = n, vjust = 0.1, hjust = -0.1)) +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = "none")

finaldf %>%
  filter(teamDayOff == opptDayOff) %>%
  group_by(teamLoc) %>%
  summarize(wins = sum(n))

sqldf("select SUM(n) FROM finaldf WHERE teamLoc ='Home' and 
      teamDayOff = opptDayOff")
sqldf("select SUM(n) FROM finaldf WHERE teamLoc ='Away' 
      and teamDayOff = opptDayOff")

finaldf %>%
  filter(as.numeric(teamDayOff) > as.numeric(opptDayOff)) %>%
  group_by(teamLoc) %>%
  summarize(wins = sum(n))

sqldf("select SUM(n) FROM finaldf WHERE teamLoc ='Home' 
      and teamDayOff > opptDayOff")
sqldf("select SUM(n) FROM finaldf WHERE teamLoc ='Away' 
      and teamDayOff > opptDayOff")

finaldf %>%
  filter(as.numeric(teamDayOff) < as.numeric(opptDayOff)) %>%
  group_by(teamLoc) %>%
  summarize(wins = sum(n))

sqldf("select SUM(n) FROM finaldf WHERE teamLoc ='Home' 
      and teamDayOff < opptDayOff")
sqldf("select SUM(n) FROM finaldf WHERE teamLoc ='Away' 
      and teamDayOff < opptDayOff")

chisq_table <- matrix(c(1552, 545, 1955, 1432, 717, 990), 
                      ncol = 2, byrow = TRUE)
rownames(chisq_table) <- c("More Rest","Same Rest","Less Rest")
colnames(chisq_table) <- c("Home Wins","Home Losses")
print(chisq_table)

chisq_table <- as.table(as.matrix(chisq_table))
balloonplot(t(chisq_table), main = "Home Wins and Home Losses", 
            xlab = "", ylab = "",
            label = TRUE, show.margins = TRUE)

options(scipen = 999)

test <- chisq.test(chisq_table)
test

mosaic(chisq_table, shade = TRUE, legend = TRUE,
       main = "Home Wins and Home Losses")

cramer.v(chisq_table)

cramerV(chisq_table)


