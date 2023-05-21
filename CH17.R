library(tidyverse)
library(tableone)
library(DataExplorer)
library(SmartEDA)

oddsdf1 <- read_csv("/Users/garysutton/Library/Mobile Documents/com~apple~CloudDocs/2018-2019_NBA_Box_Score_Team-Stats.csv")

dim(oddsdf1)

oddsdf1 %>%
  filter(DATASET != "NBA 2019 Playoffs") -> oddsdf1

oddsdf1 %>%
  select(GAME_ID, DATE, TEAM, VENUE, PTS, OPENING_SPREAD, OPENING_TOTAL,
         CLOSING_SPREAD, CLOSING_TOTAL) -> oddsdf1
    
oddsdf1 %>%
  filter(VENUE == "R") -> roadodds

roadodds %>%
  rename(ID = GAME_ID, date = DATE, teamR = TEAM, venueR = VENUE,        
         ptsR = PTS, openspreadR = OPENING_SPREAD, opentotal = OPENING_TOTAL,   
         closespreadR = CLOSING_SPREAD, closetotal = CLOSING_TOTAL) -> roadodds

oddsdf1 %>%
  filter(VENUE == "H") -> homeodds

homeodds %>%
  select(TEAM, VENUE, PTS, OPENING_SPREAD, CLOSING_SPREAD) -> homeodds

homeodds %>%
  rename(teamH = TEAM, venueH = VENUE, ptsH = PTS,                
         openspreadH = OPENING_SPREAD, closespreadH = CLOSING_SPREAD) -> homeodds

oddsdf2 <- cbind(roadodds, homeodds)

dim(oddsdf2)

oddsdf2$teamR <- as.factor(oddsdf2$teamR)
oddsdf2$teamH <- as.factor(oddsdf2$teamH)
oddsdf2$venueR <- as.factor(oddsdf2$venueR)
oddsdf2$venueH <- as.factor(oddsdf2$venueH)

oddsdf2$date <- as.Date(oddsdf2$date, "%m/%d/%Y")

oddsdf2 %>%
  mutate(month = format(date, "%B")) -> oddsdf2

oddsdf2 %>%
  mutate(month2 = case_when(month == "October" ~ 1,
                            month == "November" ~ 2,
                            month == "December" ~ 3,
                            month == "January" ~ 4,
                            month == "February" ~ 5,
                            month == "March" ~ 6,
                            month == "April" ~ 7)) -> oddsdf2

oddsdf2$month2 <- as.factor(oddsdf2$month2)

oddsdf2 %>%
  select(-ID, -date, -month) -> oddsdf2

tableOne <- CreateTableOne(data = oddsdf2)
print(tableOne)

options(scipen = 999)
summary(tableOne)

oddsdf2 %>%
  select(teamR, venueR, ptsR, opentotal, closetotal, teamH, venueH, ptsH, month2) -> oddsdf3

oddsdf3 %>%
  mutate(ptsT = ptsR + ptsH) -> oddsdf3

oddsdf3 %>%
  mutate(diff_ptsT_opentotal = abs(ptsT - opentotal)) -> oddsdf3

oddsdf3 %>%
  mutate(diff_ptsT_closetotal = abs(ptsT - closetotal)) -> oddsdf3

oddsdf3 %>%
  mutate(totalmove = case_when(closetotal > opentotal ~ "up",
                             closetotal < opentotal ~ "down",
                             closetotal == opentotal ~ "same")) -> oddsdf3

oddsdf3$totalmove <- as.factor(oddsdf3$totalmove)

oddsdf3 %>%
  mutate(versusPTS = case_when(diff_ptsT_opentotal > diff_ptsT_closetotal ~ "closetotal",
                               diff_ptsT_closetotal > diff_ptsT_opentotal ~ "opentotal",
                               diff_ptsT_opentotal == diff_ptsT_closetotal ~ "same")) -> oddsdf3

oddsdf3$versusPTS <- factor(oddsdf3$versusPTS)

oddsdf3 %>%
  select(9, 1, 2, 3, 6, 7, 8, 10, 4, 5, 11, 12, 13, 14) -> oddsdf3
head(oddsdf3)

create_report(oddsdf3)

plot_intro(oddsdf3)

introduce(oddsdf3)

plot_str(oddsdf3)

plot_str(oddsdf3, type = "r")

plot_missing(oddsdf3)

plot_histogram(oddsdf3)

plot_qq(oddsdf3)

plot_density(oddsdf3)

plot_correlation(oddsdf3, type = "c")

plot_bar(oddsdf3)

plot_bar(oddsdf3, by = "month2") 

oddsdf2 %>%
  select(teamR, venueR, ptsR, openspreadR, closespreadR, teamH, 
         venueH, ptsH, openspreadH, closespreadH, month2) -> oddsdf4

oddsdf4 %>%
  mutate(margin = ptsR - ptsH) -> oddsdf4

oddsdf4 %>%
  mutate(diff_margin_openspreadH = abs(margin - openspreadH)) -> oddsdf4

oddsdf4 %>%
  mutate(diff_margin_closespreadH = abs(margin - closespreadH)) -> oddsdf4

oddsdf4 %>%
  mutate(spreadmove = case_when(abs(closespreadH) > abs(openspreadH) ~ "up",
                                abs(closespreadH) < abs(openspreadH) ~ "down",
                                abs(closespreadH) == abs(openspreadH) ~ "same")) -> oddsdf4

oddsdf4$spreadmove <- factor(oddsdf4$spreadmove)

oddsdf4 %>%
  select(11, 1, 2, 3, 6, 7, 8, 4, 5, 9, 10, 12, 13, 14, 15) -> oddsdf4

head(oddsdf4)

ExpReport(oddsdf4, op_file = "oddsdf4.html")

ExpData(oddsdf4, type = 1)

ExpData(oddsdf4, type = 2)

ExpNumStat(oddsdf4, Outlier = TRUE, round = 2)

ExpOutQQ(oddsdf4, Page = c(2, 2), sample = 4)

ExpNumViz(oddsdf4, Page = c(2,2), sample = 4)

ExpNumViz(oddsdf4, Page = c(2,2), scatter = TRUE, sample = 4)

ExpCTable(oddsdf4)

oddsdf4 %>%
  select(month2) -> temp1
ExpCatViz(temp1, Page = c(1, 1))

oddsdf4 %>%
  select(spreadmove) -> temp2
ExpCatViz(temp2, Page = c(1, 1))

oddsdf3 %>%
  summarize(SUM1 = sum(diff_ptsT_opentotal > diff_ptsT_closetotal),
            SUM2 = sum(diff_ptsT_closetotal > diff_ptsT_opentotal),
            SUM3 = sum(diff_ptsT_opentotal == diff_ptsT_closetotal)) %>%
  pivot_longer(cols = c("SUM1", "SUM2", "SUM3"),
               names_to = "sum",
               values_to = "total") -> tblA

p1 <- ggplot(tblA, aes(x = sum, y = total, fill = sum)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = total), vjust = -0.2, fontface = "bold") +
  labs(title = "Opening Total and Closing Total Performance versus Combined Points", 
       subtitle = "Closing Total performed ~10% better than Opening Total",
       caption = "2018-19 Regular Season",
       x = "",
       y = "Counts") +
  ylim(0, 625) +
  theme(plot.title = element_text(face = "bold")) +
  scale_x_discrete(labels = c("SUM1" = "closetotal\nBEAT\nopentotal",
                              "SUM2" = "opentotal\nBEAT\nclosetotal",
                              "SUM3" = "closetotal\nEQUALED\nopentotal")) +
  theme(legend.position = "none") 
print(p1)

oddsdf3 %>%
  group_by(totalmove) %>%
  summarize(SUM1 = sum(diff_ptsT_closetotal > diff_ptsT_opentotal),
            SUM2 = sum(diff_ptsT_opentotal > diff_ptsT_closetotal),
            SUM3 = sum(diff_ptsT_opentotal == diff_ptsT_closetotal)) %>%
  pivot_longer(cols = c("SUM1", "SUM2", "SUM3"),
               names_to = "sum",
               values_to = "total") %>%
  filter(total > 100) -> tblB

p2 <- ggplot(tblB, aes(x = sum, y = total, fill = sum))+
  geom_bar(stat = "identity") +
  facet_wrap(~totalmove) +
  geom_text(aes(label = total), vjust = -0.2, fontface = "bold") +
  labs(title = "Opening Total and Closing Total Performance by O/U Movement", 
       subtitle = "Closing Total performed ~10% better than Opening Total...regardless of movement",
       caption = "2018-19 Regular Season",
       x = "",
       y = "Counts") +
  ylim(0, 325) +
  theme(plot.title = element_text(face = "bold")) +
  scale_x_discrete(labels = c("SUM1" = "opentotal\nBEAT\nclosetotal",
                              "SUM2" = "closetotal\nBEAT\nopentotal")) +
  theme(legend.position = "none") 
print(p2)

oddsdf3 %>%
  group_by(month2) %>%
  summarize(SUM1 = sum(diff_ptsT_opentotal < diff_ptsT_closetotal),
            SUM2 = sum(diff_ptsT_closetotal < diff_ptsT_opentotal)) %>%
  pivot_longer(cols = c("SUM1", "SUM2"),
               names_to = "sum",
               values_to = "total") -> tblC

p3 <-ggplot(tblC, aes(x = month2, y = total, 
                      fill = factor(sum, levels = c("SUM1", "SUM2")))) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = total), position = position_dodge(width = 0.9), 
            vjust = -0.2, fontface = "bold") +
  labs(title = "Month-over-Month Opening Total and Closing Total Performance", 
       subtitle = "Closing Total beat Opening Total in 4 of 7 Months",
       caption = "2018-19 Regular Season",
       x = "Month",
       y = "Counts") +
  ylim(0, 120) +
  scale_fill_discrete(name = "", labels = c("Opening Total", "Closing Total")) +
  scale_x_discrete(labels = c("1" = "October", "2" = "November",            "3" = "December", "4" = "January", "5" = "February", "6" = "March",               "7" = "April")) +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(face = "bold")) 
print(p3)

oddsdf3 %>%
  summarize(AVG1 = mean(diff_ptsT_opentotal),
            AVG2 = mean(diff_ptsT_closetotal)) %>%
  pivot_longer(cols = c('AVG1', 'AVG2'),
               names_to = 'avg',
               values_to = 'value') -> tblD

p4 <- ggplot(tblD, aes(x = avg, y = value, fill = avg)) + 
  geom_col() +
  geom_text(aes(label = round(value, 2)), vjust = -0.2,
            fontface = "bold") +
  labs(title = "Average Variances between Opening Total and Closing Total versus Combined Points", 
       subtitle = "Closing Total performed ~2% better than Opening Total",
       caption = "2018-19 Regular Season",
       x = "",
       y = "Average Variance from Combined Points") +
  ylim(0, 16) +
  theme(plot.title = element_text(face = "bold")) +
  scale_x_discrete(labels = c("AVG1" = "opentotal\nversus\nptsT",
                              "AVG2" = "closetotal\nversus\nptsT")) +
  theme(legend.position = "none") 
print(p4)

oddsdf3 %>%
  group_by(totalmove) %>%
  summarize(AVG1 = mean(diff_ptsT_opentotal),
            AVG2 = mean(diff_ptsT_closetotal)) %>%
  pivot_longer(cols = c("AVG1", "AVG2"),
               names_to = "avg",
               values_to = "value") %>%
  filter(totalmove != "same") -> tblE

p5 <- ggplot(tblE, aes(x = avg, y = value, fill = avg)) +
  geom_col() +
  facet_wrap(~totalmove) +
  geom_text(aes(label = round(value, 2)), vjust = 1.5,
            fontface = "bold") +
  labs(title = "Opening Total and Closing Total Performance by O/U Movement", 
       subtitle = "Closing Total performed ~2% better than Opening Total...regardless of movement",
       caption = "2018-19 Regular Season",
       x = "",
       y = "Average Variance from Combined Points") +
  theme(plot.title = element_text(face = "bold")) +
  scale_x_discrete(labels = c("AVG1" = "opentotal",
                              "AVG2" = "closetotal")) +
  theme(legend.position = "none") 
print(p5)

oddsdf3 %>%
  group_by(month2) %>%
  summarize(AVG1 = mean(diff_ptsT_opentotal),
            AVG2 = mean(diff_ptsT_closetotal)) %>%
  pivot_longer(cols = c("AVG1", "AVG2"),
               names_to = "avg",
               values_to = "value") -> tblF

p6 <-ggplot(tblF, aes(x = month2, y = value, 
                      fill = factor(avg, levels = c("AVG1", "AVG2")))) + 
  geom_bar(position = position_dodge(width = 0.5), stat = "identity") +
  geom_text(aes(label = round(value, 0)), 
            position = position_dodge(width = 0.5), vjust = -0.2, fontface = "bold") +
  labs(title = "Month-over-Month Opening Total and Closing Total Performance", 
       subtitle = "Closing Total beat or equaled Opening Total in 7 of 7 Months",
       caption = "2018-19 Regular Season",
       x = "Month",
       y = "Average Variance from Combined Points") +
  ylim(0,18) +
  scale_fill_discrete(name = "", labels = c("Opening Total", "Closing Total")) +
  scale_x_discrete(labels = c("1" = "October", "2" = "November",           "3" = "December", "4" = "January", "5" = "February", "6" = "March",          "7" = "April")) +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(face = "bold")) 
print(p6)

oddsdf4 %>%
  summarize(SUM1 = sum(diff_margin_openspreadH > diff_margin_closespreadH),
            SUM2 = sum(diff_margin_closespreadH > diff_margin_openspreadH),
            SUM3 = sum(diff_margin_openspreadH == diff_margin_closespreadH)) %>%
  pivot_longer(cols = c("SUM1", "SUM2", "SUM3"),
               names_to = "sum",
               values_to = "total") -> tblG
print(tblG)

oddsdf4 %>%
  group_by(spreadmove) %>%
  summarize(SUM1 = sum(diff_margin_closespreadH > diff_margin_openspreadH),
            SUM2 = sum(diff_margin_openspreadH > diff_margin_closespreadH),
            SUM3 = sum(diff_margin_openspreadH == diff_margin_closespreadH)) %>%
  pivot_longer(cols = c("SUM1", "SUM2", "SUM3"),
               names_to = "sum",
               values_to = "total") %>%
  filter(spreadmove != "same", sum != "SUM3") -> tblH
print(tblH)

oddsdf4 %>%
  group_by(month2) %>%
  summarize(SUM1 = sum(diff_margin_openspreadH < diff_margin_closespreadH),
            SUM2 = sum(diff_margin_closespreadH < diff_margin_openspreadH)) %>%
  pivot_longer(cols = c("SUM1", "SUM2"),
               names_to = "sum",
               values_to = "total") -> tblI
print(tblI)

oddsdf4 %>%
  summarize(AVG1 = mean(diff_margin_openspreadH),
            AVG2 = mean(diff_margin_closespreadH)) %>%
  pivot_longer(cols = c("AVG1", "AVG2"),
               names_to = "avg",
               values_to = "value") -> tblJ
print(tblJ)

oddsdf4 %>%
  group_by(spreadmove) %>%
  summarize(AVG1 = mean(diff_margin_openspreadH),
            AVG2 = mean(diff_margin_closespreadH)) %>%
  pivot_longer(cols = c("AVG1", "AVG2"),
               names_to = "avg",
               values_to = "value") %>%
  filter(spreadmove != "same") -> tblK
print(tblK)

oddsdf4 %>%
  group_by(month2) %>%
  summarize(AVG1 = mean(diff_margin_openspreadH),
            AVG2 = mean(diff_margin_closespreadH)) %>%
  pivot_longer(cols = c("AVG1", "AVG2"),
               names_to = "avg",
               values_to = "value") -> tblL
print(tblL)