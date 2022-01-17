# Loading libraries
library(tidyverse)
library(gridExtra)

# library(flextable)
# library(scales)
# library(cowplot)
# library(grid)

# Setting working directory
setwd("~/Documents/Basketball/NBA/Switch5")

# Importing data from Second Spectrum csv's
Switch_Pick_Data <- read.csv("./Data/Switch_All_Games.csv") %>% 
  left_join(., read.csv("./Data/Picks_All_Games.csv"), by = c("Screener.Defender", "Season")) %>%
  mutate(Switches = Picks.x, TotalScreens = Picks.y) %>%
  select(Screener.Defender, Season, Switches, TotalScreens)

# Calculating Switch Rate by Player Season
Switch_Pick_Data %>%
  group_by(Screener.Defender) %>%
  filter(TotalScreens > 1000) %>%
  summarize(SwitchRate = 100 * (sum(Switches) / sum(TotalScreens))) %>%
  select(Screener.Defender, SwitchRate) %>%
  arrange(desc(SwitchRate)) %>%
  print(n = 100)

# Adding in over screen 3 data
Over_Screen_3 <- read.csv("./Data/Over_Screen_3.csv")

# Visualizing Switch Rate at the season level
Switch_Pick_Data %>%
  group_by(Season) %>%
  summarize(SwitchRate = 100 * (sum(Switches) / sum(TotalScreens))) %>%
  left_join(., Over_Screen_3, by = "Season") %>%
  ggplot() +
  geom_col(aes(x = Season, y = SwitchRate), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = Season, y = Shots.Attempted.Per.100.Possessions), size = 1.5, color = "red", group = 1)

# Importing Regular Season data
Regular_Switch_Pick_Data <- read.csv("./Data/Switch_Regular_Games.csv") %>% 
  left_join(., read.csv("./Data/Picks_Regular_Games.csv"), by = c("Screener.Defender", "Season")) %>%
  mutate(Switches = Picks.x, TotalScreens = Picks.y) %>%
  select(Screener.Defender, Season, Switches, TotalScreens)

# Importing Playoff data
Playoff_Switch_Pick_Data <- read.csv("./Data/Switch_Playoff_Games.csv") %>% 
  left_join(., read.csv("./Data/Picks_Playoff_Games.csv"), by = c("Screener.Defender", "Season")) %>%
  mutate(Switches = Picks.x, TotalScreens = Picks.y) %>%
  select(Screener.Defender, Season, Switches, TotalScreens)

plot_data <- Regular_Switch_Pick_Data %>%
  group_by(Season) %>%
  summarize(SwitchRate = 100 * (sum(Switches) / sum(TotalScreens))) %>%
  mutate(Type = "Regular Season")

Playoff_Switch_Pick_Data %>%
  group_by(Season) %>%
  summarize(SwitchRate = 100 * (sum(Switches) / sum(TotalScreens))) %>%
  mutate(Type = "Playoffs") %>%
  rbind(., plot_data) %>%
  ggplot(aes(x = Season, y = SwitchRate, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge")

# Data on non-switching possessions 
No_Switch <- read.csv("./Data/No_Switch_Poss.csv")

# Non-switch trends
p1 <- No_Switch %>%
  ggplot() +
  geom_line(aes(x = Season, y = BHR.FGA.Per.Direct.Pick), color = "darkblue", group = 1) +
  ylab("Ball-Handler FGA Per Pick")

p2 <- No_Switch %>%
  ggplot() + 
  geom_line(aes(x = Season, y = BHR.PASS.Per.Direct.Pick), color = "red", group = 2) +
  ylab("Ball-Handler Pass Per Pick")

p3 <- No_Switch %>%
  ggplot() + 
  geom_line(aes(x = Season, y = Screen.Die.On.Percentage), color = "darkgreen", group = 3) +
  ylab("Screen Die-On %")

p4 <- No_Switch %>%
  ggplot() +
  geom_line(aes(x = Season, y = Points.Per.Chance), color = "black", group = 4) +
  ylab("Ball Screen DER")

# Combining the plots
grid.arrange(p1, p2, p3, p4, nrow = 2,
             top = "Ball Screen Trends When Defense Doesn't Switch")

# Data on switching possessions 
Switch <- read.csv("./Data/Switch_Poss.csv")

# Switch trends
p5 <- Switch %>%
  ggplot() +
  geom_line(aes(x = Season, y = BHR.FGA.Per.Direct.Pick), color = "darkblue", group = 1) +
  ylab("Ball-Handler FGA Per Pick")

p6 <- Switch %>%
  ggplot() + 
  geom_line(aes(x = Season, y = BHR.PASS.Per.Direct.Pick), color = "red", group = 2) +
  ylab("Ball-Handler Pass Per Pick")

p7 <- Switch %>%
  ggplot() + 
  geom_line(aes(x = Season, y = Screen.Die.On.Percentage), color = "darkgreen", group = 3) +
  ylab("Screen Die-On %")

p8 <- Switch %>%
  ggplot() +
  geom_line(aes(x = Season, y = Points.Per.Chance), color = "black", group = 4) +
  ylab("Ball Screen DER")

# Combining the plots
grid.arrange(p5, p6, p7, p8, nrow = 2,
             top = "Ball Screen Trends When Defense Switches")

# Lineup composition
Big <- read.csv("./Data/Big_SQ.csv") %>%
  mutate(LineupType = "Big")
Small <- read.csv("./Data/Small_SQ.csv") %>%
  mutate(LineupType = "Small")

LineupComp <- rbind(Big, Small)

LineupComp %>%
  ggplot(aes(x = Season, color = LineupType, group = LineupType)) +
  geom_line(aes(y = qSP)) + 
  ylab("Shot Quality Surrendered by Defense") +
  ggtitle("NBA Playoff Shot Quality") 

LineupComp %>%
  ggplot(aes(x = Season, y = qSM, fill = LineupType)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ylab("Shot Making Surrendered by Defense") +
  ggtitle("NBA Playoff Shot Making") 




# Looking at specific lineups in 2017-18 playoffs
Lineups1718 <- read_csv("Documents/Basketball/NBA/Switch5/lineups_four_factors.csv") %>%
  filter(Team != "League Averages") %>%
  filter(C %in% c("Kevin Love", "Draymond Green", "Anthony Davis", "PJ Tucker")) %>%
  select(Team, `Center in Lineup` = C, Poss, DER = `DEFENSE: Pts/Poss`)

colourer <- col_numeric(
  palette = c("red", "white", "dodgerblue2"),
  domain = c(80, 120))

myft <- flextable(Lineups1718) %>%
  autofit() %>%
  add_header_lines("Most Used Small Lineups (17-18 Playoffs)") %>%
  theme_zebra() %>%
  align(align = "center", part = "header") %>%
  border_outer() %>%
  border_inner() %>%
  bg(bg = colourer, j = "DER", part = "body")

myft

OffenseScoring <- read_csv("./OffenseScoring.csv")
OffenseRebounding <- read_csv("./OffenseREB.csv")
OffenseShotProfile <- read_csv("./OffenseSP.csv")
OffenseAssists <- read_csv("./OffenseAST.csv")
OffenseFT <- read_csv("./OffenseFT.csv")
OffenseTurnovers <- read_csv("./OffenseTOV.csv")
DefenseScoring <- read_csv("./DefenseScoring.csv")
DefenseRebounding <- read_csv("./DefenseREB.csv")
DefenseShotProfile <- read_csv("./DefenseSP.csv")
DefenseAssists <- read_csv("./DefenseAST.csv")
DefenseFT <- read_csv("./DefenseFT.csv")
DefenseTurnovers <- read_csv("./DefenseTOV.csv")

offense <- merge(merge(merge(merge(merge(
  OffenseScoring,
  OffenseRebounding, by = c("ShortName", "TeamAbbreviation", "Minutes")),
  OffenseShotProfile, by = c("ShortName", "TeamAbbreviation", "Minutes")),
  OffenseAssists, by = c("ShortName", "TeamAbbreviation", "Minutes")),
  OffenseFT, by = c("ShortName", "TeamAbbreviation", "Minutes")),
  OffenseTurnovers, by = c("ShortName", "TeamAbbreviation", "Minutes"))

defense <- merge(merge(merge(merge(merge(
  DefenseScoring,
  DefenseRebounding, by = c("ShortName", "TeamAbbreviation", "Minutes")),
  DefenseShotProfile, by = c("ShortName", "TeamAbbreviation", "Minutes")),
  DefenseAssists, by = c("ShortName", "TeamAbbreviation", "Minutes")),
  DefenseFT, by = c("ShortName", "TeamAbbreviation", "Minutes")),
  DefenseTurnovers, by = c("ShortName", "TeamAbbreviation", "Minutes"))

smallones <- c("Kuzma, Green, Davis, Caldwell-Pope, James", "Green, Morris, Davis, Caldwell-Pope, James","Caruso, Kuzma, Rondo, Davis, James","Caruso, Kuzma, Rondo, Davis, Caldwell-Pope", "Rondo, Holiday, Mirotic, Moore, Davis","Durant, Curry, Thompson, Green, Iguodala", "Curry, Thompson, Barnes, Green, Iguodala", "Batum, George, Morris Sr., Leonard, Jackson", "Lee, Curry, Thompson, Green, Iguodala", "Tucker, Westbrook, Gordon, Harden, Covington", "Connaughton, Tucker, Holiday, Middleton, Antetokounmpo", "House Jr., Tucker, Gordon, Harden, Covington", "Adebayo, Herro, Dragic, Butler, Crowder", "Durant, Curry, Thompson, Green, Livingston", "Durant, Griffin, Harden, Irving, Harris", "Brown, Durant, Griffin, Irving, Harris", "Paul, Tucker, Gordon, Harden, Ariza", "Mann, Batum, George, Morris Sr., Jackson", "Adebayo, Nunn, Herro, Dragic, Iguodala", "Adebayo, Herro, Dragic, Butler, Iguodala", "Thompson, Barnes, Green, Livingston, Iguodala", "Curry, Thompson, Barnes, Green, Livingston", "Adebayo, Robinson, Dragic, Butler, Ariza", "Mann, Beverley, George, Morris Sr., Jackson", "Powell, Siakam, VanVleet, Anunoby, Lowry", "Durant, Green, Griffin, Harden, Harris", "Paul, Tucker, Gordon, Harden, Rivers", "Robinson, Herro, Butler, Crowder, Iguodala", "Durant, Young, Thompson, Green, Iguodala", "House Jr., Tucker, Green, Gordon, Harden", "Curry, Thompson, Green, Livingston, Iguodala", "Adebayo, Nunn, Herro, Butler, Iguodala", "Curry, Barnes, Green, Livingston, Iguodala", "Adebayo, Robinson, Herro, Dragic, Iguodala", "Tucker, Green, Harden, Rivers, McLemore", "Adebayo, Robinson, Nunn, Butler, Ariza", "McCaw, Durant, Curry, Thompson, Green", "Finney-Smith, Kleber, Doncic, Hardaway Jr., Curry", "Powell, Lillard, McCollum, Covington, Anthony", "House Jr., Green, Gordon, Harden, Covington")
  
SmallD <- defense %>%
  filter(ShortName %in% smallones) %>%
  summarize(DER = round(sum(Points) / sum(OffPoss), 2),
            `DREB%` = paste(round(100*(sum(Rebounds) / (sum(DefRebounds) + sum(Rebounds))), 2), "%", sep = ""),
            RimRate = paste(round(100*(sum(AtRimFGA) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            ThreeRate = paste(round(100*(sum(`FG3A`) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            MidRate = paste(round(100*(1 - (sum(AtRimFGA) / (sum(`FG2A`) + sum(`FG3A`))) - (sum(`FG3A`) / (sum(`FG2A`) + sum(`FG3A`)))),2), "%", sep = ""),
            FTRate = paste(round(100*(sum(FTA) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            TOVRate = paste(round(100*(sum(Turnovers) / sum(OffPoss)), 2), "%", sep = ""),
            ASTRate = paste(round(100*(sum(Assists) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""))

BigD <- defense %>% 
  filter(!ShortName %in% smallones) %>%
  summarize(DER = round(sum(Points) / sum(OffPoss), 2),
            `DREB%` = paste(round(100*(sum(Rebounds) / (sum(DefRebounds) + sum(Rebounds))), 2), "%", sep = ""),
            RimRate = paste(round(100*(sum(AtRimFGA) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            ThreeRate = paste(round(100*(sum(`FG3A`) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            MidRate = paste(round(100*(1 - (sum(AtRimFGA) / (sum(`FG2A`) + sum(`FG3A`))) - (sum(`FG3A`) / (sum(`FG2A`) + sum(`FG3A`)))),2), "%", sep = ""),
            FTRate = paste(round(100*(sum(FTA) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            TOVRate = paste(round(100*(sum(Turnovers) / sum(OffPoss)), 2), "%", sep = ""),
            ASTRate = paste(round(100*(sum(Assists) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""))

SmallO <- offense %>%
  filter(ShortName %in% smallones) %>%
  summarize(OER = round(sum(Points) / sum(OffPoss), 2),
            `OREB%` = paste(round(100*(sum(DefRebounds) / (sum(DefRebounds) + sum(Rebounds))), 2), "%", sep = ""),
            RimRate = paste(round(100*(sum(AtRimFGA) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            ThreeRate = paste(round(100*(sum(`FG3A`) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            MidRate = paste(round(100*(1 - (sum(AtRimFGA) / (sum(`FG2A`) + sum(`FG3A`))) - (sum(`FG3A`) / (sum(`FG2A`) + sum(`FG3A`)))),2), "%", sep = ""),
            FTRate = paste(round(100*(sum(FTA) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            TOVRate = paste(round(100*(sum(Turnovers) / sum(OffPoss)), 2), "%", sep = ""),
            ASTRate = paste(round(100*(sum(Assists) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""))

BigO <- offense %>% 
  filter(!ShortName %in% smallones) %>%
  summarize(OER = round(sum(Points) / sum(OffPoss), 2),
            `OREB%` = paste(round(100*(sum(DefRebounds) / (sum(Rebounds) + sum(Rebounds))), 2), "%", sep = ""),
            RimRate = paste(round(100*(sum(AtRimFGA) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            ThreeRate = paste(round(100*(sum(`FG3A`) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            MidRate = paste(round(100*(1 - (sum(AtRimFGA) / (sum(`FG2A`) + sum(`FG3A`))) - (sum(`FG3A`) / (sum(`FG2A`) + sum(`FG3A`)))),2), "%", sep = ""),
            FTRate = paste(round(100*(sum(FTA) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""),
            TOVRate = paste(round(100*(sum(Turnovers) / sum(OffPoss)), 2), "%", sep = ""),
            ASTRate = paste(round(100*(sum(Assists) / (sum(`FG2A`) + sum(`FG3A`))), 2), "%", sep = ""))

table <- rbind(BigD, SmallD) %>% t() %>% as.data.frame() %>%
  mutate(Stat = row.names(.),
         Traditional = V1,
         `Switch5` = V2,
         Change = as.numeric(as.character(str_remove(`Switch5`, "%"))) - as.numeric(as.character(str_remove(Traditional, "%")))) %>%
  select(Stat, Traditional, `Switch5`, Change)

row.names(table) <- c()

colourer2 <- col_numeric(
  palette = c("purple", "white", "purple"),
  domain = c(-5, 5))

myft2 <- flextable(table) %>%
  autofit() %>%
  add_header_lines("Defensive Traits of Switch 5 Lineups") %>%
  theme_zebra() %>%
  align(align = "center", part = "header") %>%
  border_outer() %>%
  border_inner() %>%
  bg(bg = colourer2, j = "Change", part = "body") %>%
  as_raster

myft2 <- ggplot() +
  theme_void() +
  annotation_custom(rasterGrob(myft2))

myft2

table2 <- rbind(BigO, SmallO) %>% t() %>% as.data.frame() %>%
  mutate(Stat = row.names(.),
         Traditional = V1,
         `Switch5` = V2,
         Change = as.numeric(as.character(str_remove(`Switch5`, "%"))) - as.numeric(as.character(str_remove(Traditional, "%")))) %>%
  select(Stat, Traditional, `Switch5`, Change)

row.names(table2) <- c()

myft3 <- flextable(table2) %>%
  autofit() %>%
  add_header_lines("Offensive Traits of Switch 5 Lineups") %>%
  theme_zebra() %>%
  align(align = "center", part = "header") %>%
  border_outer() %>%
  border_inner() %>%
  bg(bg = colourer2, j = "Change", part = "body") %>%
  as_raster()

myft3 <- ggplot() +
  theme_void() +
  annotation_custom(rasterGrob(myft3))

plot_grid(myft3, myft2, nrow = 1, ncol = 2)


