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
  playoff_post_up_data <- read.csv("./Data/Playoff_Post_Up.csv")
  trad_post_up_data <- read_csv("./Data/Traditional_Post_Up_Def.csv")
  switch_post_up_data <- read_csv("./Data/Switch_Hybrid_Post_Up_Def.csv")
  big_post_up_data <- read_csv("./Data/Big_Post_Up_Def.csv")
  small_post_up_data <- read_csv("./Data/Small_Post_Up_Def.csv")
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
    mutate(`Switch 5 / Hybrid` = paste0((`Switch 5 / Hybrid` * 100), "%"),
           Traditional = paste0((Traditional * 100), "%"),
           Difference = paste0((Difference * 100), "%")) %>%
    select(Stat, Traditional, `Switch 5 / Hybrid`, Difference)
  row.names(shot_profile) <- c()

  flextable(shot_profile) %>%
    autofit() %>%
    add_header_lines("Defensive Shot Profile Comparison") %>%
    theme_zebra() %>%
    color(part = "header", i = 1, color = "white") %>%
    italic(part = "header", i = 1) %>%
    align(align = "center", part = "body", j = 2:4) %>%
    bg(i = c(2,4,8), j = 4, bg = "pink", part = "body") %>%
    bg(i = c(6,10), j = 4, bg = "lightgreen", part = "body") %>%
    border_outer(part = "body", border = fp_border_default(color = "black", width = 1.5)) %>%
    footnote(part = "header", i = 2, j = 1, value = as_paragraph("SMR = Short Mid-Range, LMR = Long Mid-Range, C3 = Corner 3, AB3 = Above the Break 3"), ref_symbols = "a") %>%
    footnote(part = "header", i = 2, j = 1, value = as_paragraph("Freq = Frequency, Eff = Efficiency"), ref_symbols = "b")
}

# More Stats Comparison - Rebounding & Turnovers
stat_comp = {
  
  # Offensive & rebounding stats of traditional center vs. switch 5 lineups
  switch_5_reb <- lineup_data %>%
    filter(Type != "Traditional") %>%
    mutate(DREB_Opps = DReb / (as.numeric(str_remove(FG.DReb., "%")) / 100),
           OREB_Opps = OReb / (as.numeric(str_remove(FG.OReb., "%")) / 100)) %>%
    summarize(DREBRate = sum(DReb) / sum(DREB_Opps),
              OREBRate = sum(OReb) / sum(OREB_Opps))
  
  traditional_reb <- lineup_data %>%
    filter(Type == "Traditional") %>%
    mutate(DREB_Opps = DReb / (as.numeric(str_remove(FG.DReb., "%")) / 100),
           OREB_Opps = OReb / (as.numeric(str_remove(FG.OReb., "%")) / 100)) %>%
    summarize(DREBRate = sum(DReb) / sum(DREB_Opps),
              OREBRate = sum(OReb) / sum(OREB_Opps))
  
  # Turnover stats of traditional center vs. switch 5 lineups 
  switch_5_tov <- opp_data %>%
    filter(Type != "Traditional") %>%
    summarize(TOV_Perc = sum(TOs) / sum(Possessions))
  
  traditional_tov <- opp_data %>%
    filter(Type == "Traditional") %>%
    summarize(TOV_Perc = sum(TOs) / sum(Possessions))
  
  # Combining rebounding and turnovers comparison into a table
  reb <- rbind(switch_5_reb, traditional_reb) %>% t() %>% as.data.frame()
  tov <- rbind(switch_5_tov, traditional_tov) %>% t() %>% as.data.frame()
  
  reb_tov <- rbind(reb, tov) %>%
    mutate(Stat = row.names(.),
           `Switch 5 / Hybrid` = round(V1, 2),
           Traditional = round(V2, 2),
           Difference = `Switch 5 / Hybrid` - Traditional) %>% 
    mutate(`Switch 5 / Hybrid` = paste0((`Switch 5 / Hybrid` * 100), "%"),
           Traditional = paste0((Traditional * 100), "%"),
           Difference = paste0((Difference * 100), "%")) %>%
    select(Stat, Traditional, `Switch 5 / Hybrid`, Difference)
  row.names(reb_tov) <- c()
  rm(reb, tov)
  
  flextable(reb_tov) %>%
    autofit() %>%
    add_header_lines("Rebounding and Turnovers Comparison") %>%
    theme_zebra() %>%
    color(part = "header", i = 1, color = "white") %>%
    italic(part = "header", i = 1) %>%
    align(align = "center", part = "body", j = 2:4) %>%
    bg(i = c(1,2), j = 4, bg = "pink", part = "body") %>%
    bg(i = 3, j = 4, bg = "lightgreen", part = "body") %>%
    border_outer(part = "body", border = fp_border_default(color = "black", width = 1.5)) %>%
    footnote(part = "header", i = 2, j = 1, value = as_paragraph("TOV_Perc represents TOV% forced while on defense"), ref_symbols = "1")
  
}

# Post-Up Stats
mis_match = {
  
  playoff_post_up_data %>%
    ggplot() +
    geom_col(aes(x = Season, y = Direct.Posts), size = 1, color = "darkblue", fill = "white") +
    theme_clean() +
    theme(axis.title =  element_text(size = 15)) +
    ylab("Direct Posts") +
    ggtitle("Playoff Post-Ups Are Going Extinct")
  
  trad_post_up_data$X1 <- "Traditional"
  switch_post_up_data$X1 <- "Switch/Hybrid"
  p_data <- rbind(trad_post_up_data, switch_post_up_data) %>%
    mutate(Type = X1) %>%
    select(Type, 2:8)
  
  flextable(p_data) %>%
    autofit() %>%
    add_header_lines("Playoff Post-Up Defense Based on Post-Up Defender Type") %>%
    theme_zebra() %>%
    color(part = "header", i = 1, color = "white") %>%
    italic(part = "header", i = 1) %>%
    align(align = "center", part = "body", j = 2:4)
  
  big_post_up_data$X1 <- "1 or more Centers"
  small_post_up_data$X1 <- "0 Centers"
  p2_data <- rbind(big_post_up_data, small_post_up_data) %>%
    mutate(Type = X1) %>%
    select(Type, 2:8)
  
  flextable(p2_data) %>%
    autofit() %>%
    add_header_lines("Playoff Post-Up Defense Based on # of Centers in Lineup") %>%
    theme_zebra() %>%
    color(part = "header", i = 1, color = "white") %>%
    italic(part = "header", i = 1) %>%
    align(align = "center", part = "body", j = 2:4)
  
}