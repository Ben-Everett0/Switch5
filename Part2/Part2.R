# Switch 5 ----- Part 2

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
  opp_data <- read.csv("./Data/pbp_opp_data.csv")
  lineup_data <- read.csv("./Data/pbp_lineup_data.csv")
}

# Defensive Shot Profile Comparison
shot_profile = {
  switch_5_sp <- opp_data %>%
    filter(Type != "Traditional") %>%
    summarize(RimFreq = sum(Rim.FGA) / (sum(FG2A) + sum(FG3A)),
              RimEff = sum(Rim.FGM) / sum(Rim.FGA),
              SMRFreq = sum(SMR.FGA) / (sum(FG2A) + sum(FG3A)),
              SMREff = sum(SMR.FGM) / sum(SMR.FGA),
              LMRFreq = sum(LMR.FGA) / (sum(FG2A) + sum(FG3A)),
              LMREff = sum(LMR.FGM) / sum(LMR.FGA),
              C3Freq = sum(C3.FGA) / (sum(FG2A) + sum(FG3A)),
              C3Eff = sum(C3.FGM) / sum(C3.FGA),
              AB3Freq =  sum(AB3.FGA) / (sum(FG2A) + sum(FG3A)),
              AB3Eff = sum(AB3.FGM) / sum(AB3.FGA))
  
  traditional_sp <- opp_data %>%
    filter(Type == "Traditional") %>%
    summarize(RimFreq = sum(Rim.FGA) / (sum(FG2A) + sum(FG3A)),
              RimEff = sum(Rim.FGM) / sum(Rim.FGA),
              SMRFreq = sum(SMR.FGA) / (sum(FG2A) + sum(FG3A)),
              SMREff = sum(SMR.FGM) / sum(SMR.FGA),
              LMRFreq = sum(LMR.FGA) / (sum(FG2A) + sum(FG3A)),
              LMREff = sum(LMR.FGM) / sum(LMR.FGA),
              C3Freq = sum(C3.FGA) / (sum(FG2A) + sum(FG3A)),
              C3Eff = sum(C3.FGM) / sum(C3.FGA),
              AB3Freq =  sum(AB3.FGA) / (sum(FG2A) + sum(FG3A)),
              AB3Eff = sum(AB3.FGM) / sum(AB3.FGA))
  
  shot_profile <- rbind(switch_5_sp, traditional_sp) %>% t() %>% as.data.frame() %>%
    mutate(Stat = row.names(.),
           `Switch 5 / Hybrid` = round(V1, 2),
           Traditional = round(V2, 2),
           Difference = `Switch 5 / Hybrid` - Traditional) %>% 
    select(Stat, `Switch 5 / Hybrid`, Traditional, Difference)
  row.names(shot_profile) <- c()
  
  flextable(shot_profile) %>%
    autofit() %>%
    add_header_lines("Defensive Shot Profile Comparison") %>%
    theme_zebra()
    
  # Spruce up this table - stats are done
  
}

# More Stats Comparison - Rebounding, Assists, Turnovers
stat_comp = {
  switch_5_stats <- opp_data %>%
    filter(Type != "Traditional") %>%
    summarize(DER = sum(Pts) / sum(Possessions),
              ASTRate = (sum(AST2.Pts) + sum(AST3.Pts)) / (sum(AST2.Pts) + sum(AST3.Pts) + sum(UAST2.2s.Pts) + sum(UAST3.Pts)),
              TwoASTRate = sum(AST2.Pts) / (sum(UAST2.2s.Pts) + sum(AST2.Pts)),
              ThreeASTRate = sum(AST3.Pts) / (sum(UAST3.Pts) + sum(AST3.Pts)))
  
  traditional_stats <- opp_data %>%
    filter(Type == "Traditional") %>%
    summarize(DER = sum(Pts) / sum(Possessions),
              ASTRate = (sum(AST2.Pts) + sum(AST3.Pts)) / (sum(AST2.Pts) + sum(AST3.Pts) + sum(UAST2.2s.Pts) + sum(UAST3.Pts)),
              TwoASTRate = sum(AST2.Pts) / (sum(UAST2.2s.Pts) + sum(AST2.Pts)),
              ThreeASTRate = sum(AST3.Pts) / (sum(UAST3.Pts) + sum(AST3.Pts)))
  
  # Offensive & rebounding stats of traditional center vs. switch 5 lineups
  switch_5_stats_2 <- lineup_data %>%
    filter(Type != "Traditional") %>%
    mutate(DREB_Opps = DReb / (as.numeric(str_remove(FG.DReb., "%")) / 100),
           OREB_Opps = OReb / (as.numeric(str_remove(FG.OReb., "%")) / 100)) %>%
    summarize(OER = sum(Pts) / sum(Possessions),
              DREBRate = sum(DReb) / sum(DREB_Opps),
              OREBRate = sum(OReb) / sum(OREB_Opps))
  
  traditional_stats_2 <- lineup_data %>%
    filter(Type == "Traditional") %>%
    mutate(DREB_Opps = DReb / (as.numeric(str_remove(FG.DReb., "%")) / 100),
           OREB_Opps = OReb / (as.numeric(str_remove(FG.OReb., "%")) / 100)) %>%
    summarize(OER = sum(Pts) / sum(Possessions),
              DREBRate = sum(DReb) / sum(DREB_Opps),
              OREBRate = sum(OReb) / sum(OREB_Opps))
  
  
}

