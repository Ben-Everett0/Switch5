# Switch 5 ----- Part 2

# Setup
setup <- function() {
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
data_import <- function() {
  pbp_data <- read.csv("./Data/pbp_data.csv")
}

baseline <- pbp_data %>%
  filter(Center == "Overall") %>%
  select(X3PAr, TS., Rim.or.3.Freq, Shot.Quality, Rim.Freq, C3.Freq, AST2.Pts, UAST2.2s.Pts, AST3.Pts, UAST3.Pts)
  
pbp_data %>%
  filter(Center != "Overall") %>%
  summarize(ThreePointRate = sum(FG3A) / (sum(FG2A) + sum(FG3A)),
            RimFreq = sum(Rim.FGA) / (sum(FG2A) + sum(FG3A)),
            ASTRate = (sum(AST2.Pts) + sum(AST3.Pts)) / (sum(AST2.Pts) + sum(AST3.Pts) + sum(UAST2.2s.Pts) + sum(UAST3.Pts)),
            TwoASTRate = sum(AST2.Pts) / (sum(UAST2.2s.Pts) + sum(AST2.Pts)),
            ThreeASTRate = sum(AST3.Pts) / (sum(UAST3.Pts) + sum(AST3.Pts)))

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


