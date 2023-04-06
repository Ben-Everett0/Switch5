# Switch 5 ----- Part 3

# Setup
setup = {
  # Loading libraries
  library(tidyverse)
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
}

# Fitness Stats
fitness_stats = {
  
  # Extracting the top 5 (6) of switched screens
  Playoff_Switch_Pick_Data %>%
    arrange(desc(Switches)) %>%
    slice(1:6)
  
  # Extracting the bottom 5 of switch rate
  Playoff_Switch_Pick_Data %>%
    arrange(SwitchRate) %>%
    slice(1:5)
  
  # Comparing the fitness of the two groups
  Fitness_Data %>%
    mutate(Distance_Per_Min = (Total.Distance..mi..Per.Game / Minutes.Per.Game) * 36,
           Load_Per_Min = (Total.Load.Per.Game / Minutes.Per.Game) * 36) %>%
    select(1:3, Distance_Per_Min, Load_Per_Min)
  
  fit_plot <- Fitness_Data %>%
    mutate(Distance_Per_36 = (Total.Distance..mi..Per.Game / Minutes.Per.Game) * 36,
           Load_Per_36 = (Total.Load.Per.Game / Minutes.Per.Game) * 36) %>%
    group_by(Switch.Freq) %>%
    summarize(Distance_Avg = mean(Distance_Per_36),
              Load_Avg = mean(Load_Per_36))
  
  fit_plot %>%
    ggplot(aes(x = Switch.Freq, y = Distance_Avg)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, fill = "lightblue") +
    theme_clean() +
    theme(axis.title =  element_text(size = 15)) +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(size = 12)) +
    xlab("Player Switch Frequency") +
    ylab("Average Distance Traveled Per 36 Minutes")
  
  fit_plot %>%
    ggplot(aes(x = Switch.Freq, y = Load_Avg)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, fill = "pink") +
    theme_clean() +
    theme(axis.title =  element_text(size = 15)) +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(size = 12)) +
    xlab("Player Switch Frequency") +
    ylab("Average Load Per 36 Minutes")
  
  
}

# Regular Season Defensive Rating
def_rating_analysis = {
  
  def_plot <- Team_Switches %>%
    filter(Season != "2022-23") %>%
    left_join(., League_Rankings, by = c("Season", "Team"))
  
  def_plot %>%
    ggplot(aes(x = Defense, y = Picks)) +
    geom_point() +
    xlab("Defensive Rating") +
    ylab("Switches")
  
  cor(def_plot$Picks, def_plot$Defense)
  
  League_Rankings %>%
    arrange(Defense) %>%
    select(Season, Team, Defense)
  
}
