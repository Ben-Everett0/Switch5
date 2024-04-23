# Switch 5 ----- Part 3

# Setup
setup = {
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
data_import = {
  
  # Importing Playoff Switch data
  Playoff_Switch_Pick_Data <- read.csv("./Data/Switch_Playoff_Games.csv") %>% 
    left_join(., read.csv("./Data/Picks_Playoff_Games.csv"), by = c("Screener.Defender", "Season")) %>%
    mutate(Switches = Picks.x, TotalScreens = Picks.y,
           SwitchRate = Picks.x / Picks.y) %>%
    select(Screener.Defender, Season, Switches, TotalScreens, SwitchRate)
  
  # Importing fitness stats
  Fitness_Data <- read.csv("./Data/Fitness.csv")
  
  # Importing team switch stats by season
  Team_Switches <- read.csv("./Data/Team_Switches.csv") %>%
    mutate(Team = Defensive.Team)
  
  # Importing league rankings data
  League_Rankings <- read.csv("./Data/League_Rankings.csv")
  
  # Importing team SQ stats by season
  Team_SQ <- read.csv("./Data/Team_SQ.csv") %>%
    mutate(Team = Defensive.Team)
  
}

# Fitness Stats
fitness_stats = {
  
  fit_plot <- Fitness_Data %>%
    left_join(., Playoff_Switch_Pick_Data, by = c("Player" = "Screener.Defender" ,"Season")) %>%
    filter(TotalScreens > 100) %>%
    mutate(Switch_Freq = ifelse(SwitchRate > 0.15, "High", "Low"))
  
  p1 <- fit_plot %>%
    ggplot(aes(x = Switch_Freq, y = Total.Distance..mi..Per.36.Minutes, fill = Switch_Freq)) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_clean() +
    ylab("Distance Traveled Per 36 Minutes") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  p2 <- fit_plot %>%
    ggplot(aes(x = Switch_Freq, y = Load.Slow.Per.36.Minutes, fill = Switch_Freq)) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_clean() +
    ylab("Slow Load Per 36 Minutes") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  p3 <- fit_plot %>%
    ggplot(aes(x = Switch_Freq, y = Load.Medium.Per.36.Minutes, fill = Switch_Freq)) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_clean() +
    ylab("Medium Load Per 36 Minutes") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  p4 <- fit_plot %>%
    ggplot(aes(x = Switch_Freq, y = Load.Fast.Per.36.Minutes, fill = Switch_Freq)) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_clean() +
    ylab("Fast Load Per 36 Minutes") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  plot <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
  annotate_figure(plot, top = "Switching is Hard")
  
}

# Regular Season Defensive Rating
def_rating_analysis = {
  
  def_plot <- Team_Switches %>%
    filter(Season != "2022-23") %>%
    left_join(., League_Rankings, by = c("Season", "Team"))
  
  def_plot %>%
    ggplot(aes(x = Defense, y = Picks.Per.100.Possessions)) +
    geom_point() +
    xlab("Defensive Rating") +
    ylab("Switches Per 100 Possessions") +
    ggtitle("Great Regular Season Defenses Don't Need to Switch") +
    theme(plot.title = element_text(size = 15, face = "bold"))
  
  cor(def_plot$Picks.Per.100.Possessions, def_plot$Defense)
  
  def_table <- League_Rankings %>%
    arrange(Defense) %>%
    select(Season, Team, Defense) %>%
    slice(1:10)
    
  flextable(def_table) %>%
    autofit() %>%
    add_header_lines("Best Defensive Teams Since 2017") %>%
    theme_zebra() %>%
    color(part = "header", i = 1, color = "white") %>%
    italic(part = "header", i = 1)

  def_plot2 <- Team_SQ %>%
    left_join(., League_Rankings, by = c("Season", "Team"))
  
  def_plot2 %>%
    ggplot(aes(x = Defense, y = qSQ)) +
    geom_point() +
    xlab("Defensive Rating") +
    ylab("Shot Quality Surrendered") +
    ggtitle("Shot Quality Defense Seems to Matter in the Regular Season") +
    theme(plot.title = element_text(size = 15, face = "bold"))

  cor(def_plot2$qSQ, def_plot2$Defense)
}
