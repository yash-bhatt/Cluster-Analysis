#Libraries:
library(dplyr)
library(ggplot2)

#loading the datasets:
salaries <- read.csv("Salaries1.csv", header = TRUE, sep = ",")
season_stats <- read.csv("Seasons_Stats.csv", header = TRUE, sep = ",")
