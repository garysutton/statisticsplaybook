library(tidyverse)
library(sqldf)
library(patchwork)

nba1819 <- read.csv('/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba_boxscore_1819.csv')
nba1920 <- read.csv('/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/nba_boxscore_1920.csv')

dim(nba1819) 
dim(nba1920) 

nba1819 <- filter(nba1819, DATASET == "NBA 2018-2019 Regular Season" & MIN == 240)
dim(nba1819) 

nba1920 <- filter(nba1920, GAME_ID <= 21900973 & MIN == 240)
dim(nba1920) 

nbadf1 <- rbind(nba1819, nba1920)
dim(nbadf1)

nbadf1 <- select(nbadf1, DATASET:MIN)
dim(nbadf1) 

road <- filter(nbadf1, VENUE == "R")
dim(road) 

home <- filter(nbadf1, VENUE == "H")
dim(home) 

road %>%
  rename(dataset = DATASET, ID = GAME_ID, date = DATE, teamR = TEAM, 
         venueR = VENUE, Q1R = Q1, Q2R = Q2, Q3R = Q3, Q4R = Q4, OT1R = OT1,       
         OT2R = OT2, OT3R = OT3, OT4R = OT4, OT5R = OT5, FR = F, MINR = MIN) -> road
home %>%
  rename(dataset = DATASET, ID = GAME_ID, date = DATE, teamH = TEAM, 
         venueH = VENUE, Q1H = Q1, Q2H = Q2, Q3H = Q3, Q4H = Q4, OT1H = OT1,       
         OT2H = OT2, OT3H = OT3, OT4H = OT4, OT5H = OT5, FH = F, MINH = MIN) -> home 

left_join(road, home, by = c("dataset", "ID", "date")) -> nbadf2 
dim(nbadf2)

nbadf2 <- select(nbadf2,-c(OT1R:OT5R, MINR, OT1H:OT5H, MINH))
dim(nbadf2) 

nbadf2$dataset <- as.factor(nbadf2$dataset)
nbadf2$teamR <- as.factor(nbadf2$teamR)
nbadf2$venueR <- as.factor(nbadf2$venueR)
nbadf2$teamH <- as.factor(nbadf2$teamH)
nbadf2$venueH <- as.factor(nbadf2$venueH)

glimpse(nbadf2)

nbadf3 <- filter(nbadf2, Q1R != Q1H) 
dim(nbadf3)

nbadf3 %>% mutate(Q1vF = case_when(Q1H > Q1R & FH > FR ~ "HH",
                                   Q1R > Q1H & FR > FH ~ "RR",
                                   Q1H > Q1R & FR > FH ~ "HR",
                                   Q1R > Q1H & FH > FR ~ "RH")) -> nbadf3

nbadf3$Q1vF <- as.factor(nbadf3$Q1vF)

count(nbadf3, Q1vF) %>% arrange(desc(n)) -> tbl1
tbl1 %>%
  mutate(pct_total = n/nrow(nbadf3)*100) %>%
  mutate(cum_n = cumsum(n)) %>%
  mutate(cum_pct_total = cumsum(pct_total)) -> tbl1
tbl1$pct_total <- round(tbl1$pct_total, digits = 2)
tbl1$cum_pct_total <- round(tbl1$cum_pct_total, digits = 2)
print(tbl1)

sqldf("SELECT COUNT(*) FROM nbadf3 WHERE Q1H > Q1R AND FH > FR") 

sqldf("SELECT COUNT(*) FROM nbadf3 WHERE Q1R > Q1H AND FR > FH") 

sqldf("SELECT COUNT(*) FROM nbadf3 WHERE Q1R > Q1H AND FR < FH") 

sqldf("SELECT COUNT(*) FROM nbadf3 WHERE Q1H > Q1R AND FH < FR") 

nbadf3 %>% 
  count(Q1H > Q1R & FH > FR)

nbadf3 %>% 
  count(Q1R > Q1H & FR > FH)

nbadf3 %>% 
  count(Q1R > Q1H & FR < FH)

nbadf3 %>% 
  count(Q1H > Q1R & FH < FR)

plot1a <- ggplot(tbl1, aes(x = reorder(Q1vF, -n), y = n)) + 
  geom_bar(color = c("orange1", "orange1", "gray74", "gray74"),
           fill = c("orange1", "orange1", "gray74", "gray74"), 
           stat = "identity") + 
  labs(title = "Teams Winning the First Quarter", 
       subtitle = "Win-Loss Record = 1,333-657",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Win Combinations between First Quarter and End of Game", 
       y = "Wins") + 
  geom_text(aes(x = Q1vF, y = n, label = n, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot1b <- ggplot(tbl1, aes(x = reorder(Q1vF, -pct_total), y = pct_total)) + 
  geom_bar(color = c("orange1", "orange1", "gray74", "gray74"),
           fill = c("orange1", "orange1", "gray74", "gray74"), 
           stat = "identity") + 
  labs(title = "Teams Winning the First Quarter", 
       subtitle = "Winning Percentage = 66.98%",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Win Combinations between First Quarter and End of Game", 
       y = "Winning Percentage") + 
  geom_text(aes(x = Q1vF, y = pct_total, label = pct_total, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot1a + plot1b + plot_layout(ncol = 2)

nbadf4 <- filter(nbadf3, Q2R != Q2H)

nbadf4 %>% mutate(Q2vF = case_when(Q2H > Q2R & FH > FR ~ "HH",
                                   Q2R > Q2H & FR > FH ~ "RR",
                                   Q2H > Q2R & FR > FH ~ "HR",
                                   Q2R > Q2H & FH > FR ~ "RH")) -> nbadf4

nbadf4$Q2vF <- factor(nbadf4$Q2vF)
count(nbadf4, Q2vF) %>% arrange(desc(n)) -> tbl2
tbl2 %>% 
  mutate(pct_total = n/nrow(nbadf4)*100) %>%
  mutate(cum_n = cumsum(n)) %>%
  mutate(cum_pct_total = cumsum(pct_total)) -> tbl2
tbl2$pct_total <- round(tbl2$pct_total, digits = 2)
tbl2$cum_pct_total <- round(tbl2$cum_pct_total, digits = 2)
print(tbl2)

plot2a <- ggplot(tbl2, aes(x = reorder(Q2vF, -n), y = n)) + 
  geom_bar(color = c("skyblue3", "skyblue3", "gray74", "gray74"),
           fill = c("skyblue3", "skyblue3", "gray74", "gray74"), 
           stat = "identity") + 
  labs(title = "Teams Winning the Second Quarter", 
       subtitle = "Win-Loss Record = 1,266-692",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Win Combinations between Second Quarter and End of Game", 
       y = "Wins") + 
  geom_text(aes(x = Q2vF, y = n, label = n, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot2b <- ggplot(tbl2, aes(x = reorder(Q2vF, -pct_total), y = pct_total)) + 
  geom_bar(color = c("skyblue3", "skyblue3", "gray74", "gray74"),
           fill = c("skyblue3", "skyblue3", "gray74", "gray74"), 
           stat = "identity") + 
  labs(title = "Teams Winning the Second Quarter", 
       subtitle = "Winning Percentage = 64.66%",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Win Combinations between Second Quarter and End of Game", 
       y = "Winning Percentage") + 
  geom_text(aes(x = Q2vF, y = pct_total, label = pct_total, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot2a + plot2b + plot_layout(ncol = 2)

nbadf5 <- filter(nbadf2, Q3R != Q3H)

nbadf5 %>% mutate(Q3vF = case_when(Q3H > Q3R & FH > FR ~ "HH",
                                   Q3R > Q3H & FR > FH ~ "RR",
                                   Q3H > Q3R & FR > FH ~ "HR",
                                   Q3R > Q3H & FH > FR ~ "RH")) -> nbadf5

nbadf5$Q3vF <- factor(nbadf5$Q3vF)

count(nbadf5, Q3vF) %>% arrange(desc(n)) -> tbl3
tbl3 %>% 
  mutate(pct_total = n/nrow(nbadf5)*100) %>%
  mutate(cum_n = cumsum(n)) %>%
  mutate(cum_pct_total = cumsum(pct_total)) -> tbl3
tbl3$pct_total <- round(tbl3$pct_total, digits = 2)
tbl3$cum_pct_total <- round(tbl3$cum_pct_total, digits = 2)
print(tbl3)

plot3a <- ggplot(tbl3, aes(x = reorder(Q3vF, -n), y = n)) + 
  geom_bar(color = c("springgreen3", "springgreen3", "gray74", "gray74"),
           fill = c("springgreen3", "springgreen3", "gray74", "gray74"), 
           stat = "identity") + 
  labs(title = "Teams Winning the Third Quarter", 
       subtitle = "Win-Loss Record = 1,322-645",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Win Combinations between Third Quarter and End of Game", 
       y = "Wins") + 
  geom_text(aes(x = Q3vF, y = n, label = n, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot3b <- ggplot(tbl3, aes(x = reorder(Q3vF, -pct_total), y = pct_total)) + 
  geom_bar(color = c("springgreen3", "springgreen3", "gray74", "gray74"),
           fill = c("springgreen3", "springgreen3", "gray74", "gray74"), 
           stat = "identity") + 
  labs(title = "Teams Winning the Second Quarter", 
       subtitle = "Winning Percentage = 67.21%",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Win Combinations between Third Quarter and End of Game", 
       y = "Winning Percentage") + 
  geom_text(aes(x = Q3vF, y = pct_total, label = pct_total, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot3a + plot3b + plot_layout(ncol = 2)

nbadf6 <- filter(nbadf2, Q4R != Q4H)

nbadf6 %>% mutate(Q4vF = case_when(Q4H > Q4R & FH > FR ~ "HH",
                                   Q4R > Q4H & FR > FH ~ "RR",
                                   Q4H > Q4R & FR > FH ~ "HR",
                                   Q4R > Q4H & FH > FR ~ "RH")) -> nbadf6

nbadf6$Q4vF <- factor(nbadf6$Q4vF)

count(nbadf6, Q4vF) %>% arrange(desc(n)) -> tbl4
tbl4 %>% 
  mutate(pct_total = n/nrow(nbadf6)*100) %>%
  mutate(cum_n = cumsum(n)) %>%
  mutate(cum_pct_total = cumsum(pct_total)) -> tbl4
tbl4$pct_total <- round(tbl4$pct_total, digits = 2)
tbl4$cum_pct_total <- round(tbl4$cum_pct_total, digits = 2)
print(tbl4)

plot4a <- ggplot(tbl4, aes(x = reorder(Q4vF, -n), y = n)) + 
  geom_bar(color = c("darkorchid3", "darkorchid3", "gray74", "gray74"),
           fill = c("darkorchid3", "darkorchid3", "gray74", "gray74"), 
           stat = "identity") + 
  labs(title = "Teams Winning the Fourth Quarter", 
       subtitle = "Win-Loss Record = 1,291-673",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Win Combinations between Fourth Quarter and End of Game", 
       y = "Wins") + 
  geom_text(aes(x = Q4vF, y = n, label = n, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot4b <- ggplot(tbl4, aes(x = reorder(Q4vF, -pct_total), y = pct_total)) + 
  geom_bar(color = c("darkorchid3", "darkorchid3", "gray74", "gray74"),
           fill = c("darkorchid3", "darkorchid3", "gray74", "gray74"), 
           stat = "identity") + 
  labs(title = "Teams Winning the Fourth Quarter", 
       subtitle = "Winning Percentage = 65.73%",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Win Combinations between Fourth Quarter and End of Game", 
       y = "Winning Percentage") + 
  geom_text(aes(x = Q4vF, y = pct_total, label = pct_total, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot4a + plot4b + plot_layout(ncol = 2)

nbadf7 <- filter(nbadf3, teamR == "Milwaukee" | teamR == "Toronto" | teamR == "Boston" | teamR == "Denver" | teamR == "Houston" | teamR == "LA Clippers")

nbadf8 <- filter(nbadf7, Q1vF == "RR" | Q1vF == "RH")

count(nbadf8, Q1vF) %>% arrange(desc(Q1vF)) -> tbl5
tbl5 %>% 
  mutate(pct_total = n/nrow(nbadf8)*100) -> tbl5
tbl5$pct_total <- round(tbl5$pct_total, digits = 2)
print(tbl5)

nbadf9 <- filter(nbadf4, teamR == "Milwaukee" | teamR == "Toronto" | teamR == "Boston" | teamR == "Denver" | teamR == "Houston" | teamR == "LA Clippers") 

nbadf10 <- filter(nbadf9, Q2vF == "RR" | Q2vF == "RH")

count(nbadf10, Q2vF) %>% arrange(desc(Q2vF)) -> tbl6
tbl6 %>% 
  mutate(pct_total = n/nrow(nbadf10)*100) -> tbl6
tbl6$pct_total <- round(tbl6$pct_total, digits = 2)
print(tbl6)
nbadf11 <- filter(nbadf5, teamR == "Milwaukee" | teamR == "Toronto" | teamR == "Boston" | teamR == "Denver" | teamR == "Houston" | teamR == "LA Clippers")

nbadf12 <- filter(nbadf11, Q3vF == "RR" | Q3vF == "RH")

count(nbadf12, Q3vF) %>% arrange(desc(Q3vF)) -> tbl7
tbl7 %>% 
  mutate(pct_total = n/nrow(nbadf12)*100) -> tbl7
tbl7$pct_total <- round(tbl7$pct_total, digits = 2)
print(tbl7)

nbadf13 <- filter(nbadf6, teamR == "Milwaukee" | teamR == "Toronto" | teamR == "Boston" | teamR == "Denver" | teamR == "Houston" | teamR == "LA Clippers")

nbadf14 <- filter(nbadf13, Q4vF == "RR" | Q4vF == "RH")

count(nbadf14, Q4vF) %>% arrange(desc(Q4vF)) -> tbl8
tbl8 %>% 
  mutate(pct_total = n/nrow(nbadf14)*100) -> tbl8
tbl8$pct_total <- round(tbl8$pct_total, digits = 2)
print(tbl8)

df1 <- data.frame(quarter = c("1Q", "2Q", "3Q", "4Q"),
                  win_pct = c(70.89, 72.81, 75.12, 71.01))
print(df1)

plot5 <- ggplot(df1, aes(x = quarter, y = win_pct)) + 
  geom_bar(color = c("gray74", "gray74", "skyblue", "gray74"),
           fill = c("gray74", "gray74", "skyblue", "gray74"), 
           stat = "identity") + 
  labs(title = "Top 6 Teams on the Road", subtitle = "Winning Percentages when Winning each Quarter",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Quarter", 
       y = "Winning Percentage") + 
  geom_text(aes(x = quarter, y = win_pct, label = win_pct, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

nbadf15 <- filter(nbadf3, teamH == "Milwaukee" | teamH == "Toronto" | teamH == "Boston" | teamH == "Denver" | teamH == "Houston" | teamH == "LA Clippers")

nbadf16 <- filter(nbadf15, Q1vF == "HR" | Q1vF == "HH")

count(nbadf16, Q1vF) %>% arrange(Q1vF) -> tbl9
tbl9 %>% 
  mutate(pct_total = n/nrow(nbadf16)*100) -> tbl9
tbl9$pct_total <- round(tbl9$pct_total, digits = 2)
print(tbl9)

nbadf17 <- filter(nbadf4, teamH == "Milwaukee" | teamH == "Toronto" | teamH == "Boston" | teamH == "Denver" | teamH == "Houston" | teamH == "LA Clippers")

nbadf18 <- filter(nbadf17, Q2vF == "HR" | Q2vF == "HH")

count(nbadf18, Q2vF) %>% arrange(Q2vF) -> tbl10
tbl10 %>% 
  mutate(pct_total = n/nrow(nbadf18)*100) -> tbl10
tbl10$pct_total <- round(tbl10$pct_total, digits = 2)
print(tbl10)

nbadf19 <- filter(nbadf5, teamH == "Milwaukee" | teamH == "Toronto" | teamH == "Boston" | teamH == "Denver" | teamH == "Houston" | teamH == "LA Clippers")

nbadf20 <- filter(nbadf19, Q3vF == "HR" | Q3vF == "HH")

count(nbadf20, Q3vF) %>% arrange(Q3vF) -> tbl11
tbl11 %>% 
  mutate(pct_total = n/nrow(nbadf20)*100) -> tbl11
tbl11$pct_total <- round(tbl11$pct_total, digits = 2)
print(tbl11)

nbadf21 <- filter(nbadf6, teamH == "Milwaukee" | teamH == "Toronto" | teamH == "Boston" | teamH == "Denver" | teamH == "Houston" | teamH == "LA Clippers")

nbadf22 <- filter(nbadf21, Q4vF == "HR" | Q4vF == "HH")

count(nbadf22, Q4vF) %>% arrange(Q4vF) -> tbl12
tbl12 %>% 
  mutate(pct_total = n/nrow(nbadf22)*100) -> tbl12
tbl12$pct_total <- round(tbl12$pct_total, digits = 2)
print(tbl12)

df2 <- data.frame(quarter = c("1Q", "2Q", "3Q", "4Q"),
                  win_pct = c(84.88, 84.15, 87.76, 82.99))
print(df2)

plot6 <- ggplot(df2, aes(x = quarter, y = win_pct)) + 
  geom_bar(color = c("gray74", "gray74", "brown3", "gray74"),
           fill = c("gray74", "gray74", "brown3", "gray74"), 
           stat = "identity") + 
  labs(title = "Top 6 Teams at Home", 
       subtitle = "Winning Percentages when Winning each Quarter",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Quarter", 
       y = "Winning Percentage") + 
  geom_text(aes(x = quarter, y = win_pct, label = win_pct, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot5 + plot6 + plot_layout(ncol = 2)

nbadf23 <- filter(nbadf3, teamR == "Minnesota" | teamR == "Phoenix" | teamR == "Atlanta" | teamR == "Chicago" | teamR == "Cleveland" | teamR == "New York")

nbadf24 <- filter(nbadf23, Q1vF == "RR" | Q1vF == "RH")

count(nbadf24, Q1vF) %>% arrange(desc(Q1vF)) -> tbl13
tbl13 %>% 
  mutate(pct_total = n/nrow(nbadf24)*100) -> tbl13
tbl13$pct_total <- round(tbl13$pct_total, digits = 2)
print(tbl13)

nbadf25 <- filter(nbadf4, teamR == "Minnesota" | teamR == "Phoenix" | teamR == "Atlanta" | teamR == "Chicago" | teamR == "Cleveland" | teamR == "New York")

nbadf26 <- filter(nbadf25, Q2vF == "RR" | Q2vF == "RH")

count(nbadf26, Q2vF) %>% arrange(desc(Q2vF)) -> tbl14
tbl14 %>% 
  mutate(pct_total = n/nrow(nbadf26)*100) -> tbl14
tbl14$pct_total <- round(tbl14$pct_total, digits = 2)
print(tbl14)

nbadf27 <- filter(nbadf5, teamR == "Minnesota" | teamR == "Phoenix" | teamR == "Atlanta" | teamR == "Chicago" | teamR == "Cleveland" | teamR == "New York")

nbadf28 <- filter(nbadf27, Q3vF == "RR" | Q3vF == "RH")

count(nbadf28, Q3vF) %>% arrange(desc(Q3vF)) -> tbl15
tbl15 %>% 
  mutate(pct_total = n/nrow(nbadf28)*100) -> tbl15
tbl15$pct_total <- round(tbl15$pct_total, digits = 2)
print(tbl15)

nbadf29 <- filter(nbadf6, teamR == "Minnesota" | teamR == "Phoenix" | teamR == "Atlanta" | teamR == "Chicago" | teamR == "Cleveland" | teamR == "New York")

nbadf30 <- filter(nbadf29, Q4vF == "RR" | Q4vF == "RH")

count(nbadf30, Q4vF) %>% arrange(desc(Q4vF)) -> tbl16
tbl16 %>% 
  mutate(pct_total = n/nrow(nbadf30)*100) -> tbl16
tbl16$pct_total <- round(tbl16$pct_total, digits = 2)
print(tbl16)

df3 <- data.frame(quarter = c("1Q", "2Q", "3Q", "4Q"),
                  win_pct = c(42.03, 33.33, 38.11, 37.76))
print(df3)

plot7 <- ggplot(df3, aes(x = quarter, y = win_pct)) + 
  geom_bar(color = c("orange", "gray74", "gray74", "gray74"),
           fill = c("orange", "gray74", "gray74", "gray74"), 
           stat = "identity") + 
  labs(title = "Bottom 6 Teams on the Road", 
       subtitle = "Winning Percentages when Winning each Quarter",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Quarter", 
       y = "Winning Percentage") + 
  geom_text(aes(x = quarter, y = win_pct, label = win_pct, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

nbadf31 <- subset(nbadf3, teamH == "Minnesota" | teamH == "Phoenix" | teamH == "Atlanta" | teamH == "Chicago" | teamH == "Cleveland" | teamH == "New York")

nbadf32 <- filter(nbadf31, Q1vF == "HR" | Q1vF == "HH")

count(nbadf32, Q1vF) %>% arrange(Q1vF) -> tbl17
tbl17 %>% 
  mutate(pct_total = n/nrow(nbadf32)*100) -> tbl17
tbl17$pct_total <- round(tbl17$pct_total, digits = 2)
print(tbl17)

nbadf33 <- filter(nbadf4, teamH == "Minnesota" | teamH == "Phoenix" | teamH == "Atlanta" | teamH == "Chicago" | teamH == "Cleveland" | teamH == "New York")

nbadf34 <- filter(nbadf33, Q2vF == "HR" | Q2vF == "HH")

count(nbadf34, Q2vF) %>% arrange(Q2vF) -> tbl18
tbl18 %>% 
  mutate(pct_total = n/nrow(nbadf34)*100) -> tbl18
tbl18$pct_total <- round(tbl18$pct_total, digits = 2)
print(tbl18)

nbadf35 <- filter(nbadf5, teamH == "Minnesota" | teamH == "Phoenix" | teamH == "Atlanta" | teamH == "Chicago" | teamH == "Cleveland" | teamH == "New York")

nbadf36 <- filter(nbadf35, Q3vF == "HR" | Q3vF == "HH")

count(nbadf36, Q3vF) %>% arrange(Q3vF) -> tbl19
tbl19 %>% 
  mutate(pct_total = n/nrow(nbadf36)*100) -> tbl19
tbl19$pct_total <- round(tbl19$pct_total, digits = 2)
print(tbl19)

nbadf37 <- filter(nbadf6, teamH == "Minnesota" | teamH == "Phoenix" | teamH == "Atlanta" | teamH == "Chicago" | teamH == "Cleveland" | teamH == "New York")

nbadf38 <- filter(nbadf37, Q4vF == "HR" | Q4vF == "HH")

count(nbadf38, Q4vF) %>% arrange(Q4vF) -> tbl20
tbl20 %>% 
  mutate(pct_total = n/nrow(nbadf38)*100) -> tbl20
tbl20$pct_total <- round(tbl20$pct_total, digits = 2)
print(tbl20)

df4 <- data.frame(quarter = c("1Q", "2Q", "3Q", "4Q"),
                  win_pct = c(45.35, 52.02, 49.37, 52.72))
print(df4)

plot8 <- ggplot(df4, aes(x = quarter, y = win_pct)) + 
  geom_bar(color = c("gray74", "gray74", "gray74", "orchid"),
           fill = c("gray74", "gray74", "gray74", "orchid"), 
           stat = "identity") + 
  labs(title = "Bottom 6 Teams at Home", 
       subtitle = "Winning Percentages when Winning each Quarter",
       caption = "\n2018-19 and 2019-20 regular season games that ended in regulation\n2020 'bubble' games excluded",
       x = "Quarter", 
       y = "Winning Percentage") + 
  geom_text(aes(x = quarter, y = win_pct, label = win_pct, vjust = -0.3, fontface = "bold")) +
  theme(plot.title = element_text(face = "bold")) 

plot7 + plot8 + plot_layout(ncol = 2)

nbadf39 <- filter(nbadf2, Q1R + Q2R == Q1H + Q2H)
nbadf39 <- filter(nbadf39, Q3R != Q3H & Q4R != Q4H)
dim(nbadf39)

nbadf39 %>%
  group_by(Q3H > Q3R & Q4H > Q4R & FH > FR,
           Q3H > Q3R & Q4H < Q4R & FH > FR,
           Q3H < Q3R & Q4H > Q4R & FH > FR,
           Q3R > Q3H & Q4R > Q4H & FR > FH,
           Q3R > Q3H & Q4R < Q4H & FR > FH,
           Q3R < Q3H & Q4R > Q4H & FR > FH) %>%
  tally() -> tbl21
print(tbl21)

colnames(tbl21) <- c('HHH', 'HRH', 'RHH', 'RRR', 'RHR', 'HRR', 'count')
head(tbl21, n = 1)

tbl21$HHH <- as.numeric(tbl21$HHH)
tbl21$HRH <- as.numeric(tbl21$HRH)
tbl21$RHH <- as.numeric(tbl21$RHH)
tbl21$RRR <- as.numeric(tbl21$RRR)
tbl21$RHR <- as.numeric(tbl21$RHR)
tbl21$HRR <- as.numeric(tbl21$HRR)

tbl21[6, 1] = 18
tbl21[5, 2] = 8
tbl21[4, 3] = 8
tbl21[3, 4] = 13
tbl21[2, 5] = 8
tbl21[1, 6] = 6

tbl21 %>% 
  pivot_longer(cols = c('HHH', 'HRH', 'RHH', 'RRR', 'RHR', 'HRR'),
               names_to = 'result',
               values_to = 'count') -> tbl21
head(tbl21)

tbl21 <- filter(tbl21, count > 0)
print(tbl21)
