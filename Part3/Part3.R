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
}

# Fitness Stats
season_analysis = {
  
  
  
}
