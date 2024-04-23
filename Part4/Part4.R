# Switch 5 ----- Part 4

# Setup
setup = {
  # Loading libraries
  library(tidyverse)
  library(flextable)
  library(scales)
  library(zoo)
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
  
  # Importing NBA salary data
  Salary <- read.csv("./Data/nba-salaries.csv")
  
  # Importing Free Agency data
  FA <- read.csv("./Data/FA.csv")
  
  # Importing NBA draft history
  Draft <- read.csv("./Data/Draft.csv")

}

# Salary Analysis
salary_analysis = {
  
  # Salary$position <- trimws(Salary$position)
  # 
  # Salary %>%
  #   mutate(type = ifelse(position == "C", "Center", "Not a Center")) %>%
  #   group_by(season, type) %>%
  #   summarize(tot_salary = sum(salary))
  # 
  # Salary %>%
  #   group_by(season, type) %>%
  #   summarize(avg_salary = median(salary)) %>%
  #   ggplot(aes(x = season, y = avg_salary, fill = type)) +
  #   geom_bar(stat = "identity", position = "dodge") +
  #   xlab("Season") +
  #   ylab("Salary") +
  #   scale_y_continuous(labels = dollar)
  
  FA$Year <- as.factor(FA$Year)
  
  SalaryCap <- data.frame(Year = c(2016:2022), 
                          Figure = c(94143000, 99093000, 101869000, 109140000, 109140000, 112414000, 123655000))
  
  SalaryCap$Year <- as.factor(SalaryCap$Year)
  
  FA %>%
    filter(AAV > 0) %>%
    group_by(Year) %>%
    summarize(AAV = mean(AAV)) %>%
    left_join(., SalaryCap, by = "Year") %>%
    ggplot(aes(x = Year, y = AAV)) +
    geom_col(size = 1, color = "darkred", fill = "white") +
    theme_clean() + 
    xlab("Year") +
    ylab("Average Annual Value of Contract (in millions) of Centers") +
    ggtitle("The Cap Grows, But Demand For Centers Does Not") +
    theme(axis.title =  element_text(size = 15)) +
    scale_y_continuous(labels = dollar)
    #geom_line(aes(x = Year, y = Figure), size = 1.5, color = "blue", group = 1)



  
  
}

# Draft Analysis
draft_analysis = {
  
  plot_data <- Draft %>%
    mutate(Height_Inches = as.numeric((str_extract(Height, ".(?=\\-)")))*12 + as.numeric(str_extract(Height, "(?<=\\-).")),
           Center = ifelse(str_detect(Pos, "C") == TRUE, "Yes", "No"),
           Year = as.factor(Year),
           Height_to_Pick = Height_Inches / Pick)
    
  plot_data %>%
    filter(Center == "Yes") %>%
    group_by(Year) %>%
    summarize(Count = n()) %>%
    ggplot(aes(x = Year, y = Count)) +
    geom_col(size = 1, color = "darkblue", fill = "gray") +
    theme_clean() +
    xlab("Draft Year") +
    ylab("Number of Centers Taken") +
    ggtitle("Teams Are More Hesitant to Draft Centers") +
    theme(axis.title =  element_text(size = 15))

}




