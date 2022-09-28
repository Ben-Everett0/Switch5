# Switch 5 ----- Part 1

# Setup
setup <- function() {
  # Loading libraries
  library(tidyverse)
  library(ggpubr)
  library(flextable)
  library(scales)
  
  # Setting working directory
  setwd("~/Documents/Basketball/NBA/Switch5")
  
  # Clean theme for graphs
  theme_clean <- function() {
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size = 18, face = "bold")) 
  }
}

# Data Import
data_import <- function() {
  # Importing data from Second Spectrum csv's
  Switch_Pick_Data <- read.csv("./Data/Switch_All_Games.csv") %>% 
    left_join(., read.csv("./Data/Picks_All_Games.csv"), by = c("Screener.Defender", "Season")) %>%
    mutate(Switches = Picks.x, TotalScreens = Picks.y) %>%
    select(Screener.Defender, Season, Switches, TotalScreens)
  
  # Adding in over screen 3 data
  Over_Screen_3 <- read.csv("./Data/Over_Screen_3.csv")
  
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
  
  # Data on non-switching possessions 
  No_Switch <- read.csv("./Data/No_Switch_Poss.csv") %>% mutate(Type = "NoSwitch")
  
  # Data on switching possessions 
  Switch <- read.csv("./Data/Switch_Poss.csv") %>% mutate(Type = "Switch")
  
  # Big and small lineup Shot Quality data
  Big <- read.csv("./Data/Big_SQ.csv") %>%
    mutate(LineupType = "Big")
  Small <- read.csv("./Data/Small_SQ.csv") %>%
    mutate(LineupType = "Small")
  
  # 2017-18 Playoff Lineups data from Cleaning the Glass
  Lineups1718 <- read_csv("./Data/Lineups_1718_FF.csv") %>%
    filter(Team != "League Averages") %>%
    filter(C %in% c("Kevin Love", "Draymond Green", "Anthony Davis", "PJ Tucker")) %>%
    select(Team, `Center in Lineup` = C, Poss, DER = `DEFENSE: Pts/Poss`)
}

# Switch Rate by Season Trends
switch_rate_by_season <- function() {
  
  # Visualizing Switch Rate at the season level
  annotation <- data.frame(
    x = c(2,2,2),
    y = c(7.3,7.0,6.7),
    label = c("Red line represents year-to-year", "trend of ball screen 3's", "attempted per 100 possesions")
  )
  
  Switch_Pick_Data %>%
    group_by(Season) %>%
    summarize(SwitchRate = 100 * (sum(Switches) / sum(TotalScreens))) %>%
    left_join(., Over_Screen_3, by = "Season") %>%
    ggplot() +
    ggtitle("NBA teams expect a little more mobility from centers these days") +
    geom_col(aes(x = Season, y = SwitchRate), size = 1, color = "darkblue", fill = "white") +
    geom_line(aes(x = Season, y = Shots.Attempted.Per.100.Possessions), size = 1.5, color = "red", group = 1) +
    theme_clean() +
    theme(axis.title =  element_text(size = 15)) +
    geom_text(data = annotation, aes(x = x, y = y, label = label),
              color = "black",
              size = 4, angle = 0, fontface = "bold")
  
  # Visualizing Switch Rate in regular season vs. playoffs
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
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle("PSA for Bigs: Get that cardio in before the playoffs") +
    theme_clean() +
    theme(axis.title =  element_text(size = 15)) +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(size = 12))
  
}

# Switch vs. No Switch trends
switch_v_no_switch <- function() {
  p_data <- rbind(Switch, No_Switch) %>% mutate(Type = as.factor(Type))

  p1 <- p_data %>%
    ggplot(aes(x = Season, y = BHR.FGA.Per.Direct.Pick)) +
    geom_line(aes(color = Type, linetype = Type, group = Type)) + 
    scale_color_manual(values = c("darkred", "steelblue")) +
    theme_clean() +
    ylab("Ball-Handler FGA Per Pick") + 
    ylim(0, 1)
  
  p2 <- p_data %>%
    ggplot(aes(x = Season, y = BHR.PASS.Per.Direct.Pick)) +
    geom_line(aes(color = Type, linetype = Type, group = Type)) + 
    scale_color_manual(values = c("darkred", "steelblue")) +
    theme_clean() +
    ylab("Ball-Handler Pass Per Pick") +
    ylim(0, 1)
  
  p3 <- p_data %>%
    ggplot(aes(x = Season, y = Screen.Die.On.Percentage)) +
    geom_line(aes(color = Type, linetype = Type, group = Type)) + 
    scale_color_manual(values = c("darkred", "steelblue")) +
    theme_clean() +
    ylab("Screen Die-On %") +
    ylim(0, 5)
  
  p4 <- p_data %>%
    ggplot(aes(x = Season, y = Points.Per.Chance)) +
    geom_line(aes(color = Type, linetype = Type, group = Type)) + 
    scale_color_manual(values = c("darkred", "steelblue")) +
    theme_clean() +
    ylab("Points Per Chance") +
    ylim(0, 1)
  
  # Combining the plots
  plot <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2,common.legend = TRUE, legend = "bottom")
  annotate_figure(plot, top = "NBA Players Got Good at Using Screens")
}

# Lineup Composition
lineup_comp <- function() {
  LineupComp <- rbind(Big, Small)
  
  LineupComp %>%
    ggplot(aes(x = Season, color = LineupType, group = LineupType)) +
    geom_line(aes(y = qSP)) + 
    theme_clean() +
    ylab("Shot Quality Surrendered by Defense") +
    ggtitle("Big or Small, Lineups are Giving Up Better Looks")
  
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
}