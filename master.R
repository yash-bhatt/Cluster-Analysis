#Libraries:
library(dplyr)
library(ggplot2)

#loading the datasets:
salaries <- read.csv("Salaries1.csv", header = TRUE, sep = ",")
season_stats <- read.csv("Seasons_Stats.csv", header = TRUE, sep = ",")

#Exploring the data:
str(salaries)
str(season_stats)

# Data cleaning:
season_stats <- season_stats[,-1]
names(season_stats) <- gsub("X","",names(season_stats))

#Filtering out only 2017 games
nba <- season_stats %>% 
  filter(Year == 2017) %>% 
  select(c("Year", "Player", "Tm", "Pos", "Age", "G","MP","FG", "FGA", "3P",
           "3PA", "2P", "2PA","FT", "FTA", "TRB", "AST", "STL", "BLK", "PTS", "VORP", "PER"))

#Normalize on a per game basis for better comparions 
nba_per_game <- data.frame(sapply(nba[,c("MP", "FG", "FGA", "3P", "3PA", "2P",
                                         "2PA", "FT", "FTA", "TRB", "AST", "STL","BLK", "PTS")],
                                  function(x){x/nba$G}))

colnames(nba_per_game) = paste0(c("MP", "FG", "FGA", "3P", "3PA", "2P","2PA", "FT", "FTA", "TRB",
                                  "AST", "STL","BLK", "PTS"), "_pg")
nba <- cbind(nba, nba_per_game)

#Players who switched teams (duplicate observations issue)
number_teams <- data.frame(nba %>% count(Player) %>% arrange(desc(n)) %>% rename("number_teams" = "n"))
nba <- left_join(nba, number_teams, by = "Player")
players_multiple_teams <- nba %>% filter(number_teams >1)
players_minutes_max <- data.frame(players_multiple_teams %>% filter(Tm != "TOT") %>% select(Player, MP) %>% group_by(Player) %>% summarize(max_min = max(MP)))
team_minutes_max <- data.frame(left_join(players_minutes_max, players_multiple_teams[,c("Player", "MP", "Tm")], by = c("max_min" = "MP", "Player")))
player_team_assign <- data.frame(left_join(players_multiple_teams %>% select(Player, Tm) %>% filter(Tm == "TOT"), team_minutes_max %>% select(Player, Tm), by = "Player"))
players_one_team <- data.frame(nba %>% filter(number_teams == 1) %>% select(Player, Tm))
players_one_team$copy <- players_one_team$Tm

library(data.table)

setnames(players_one_team, old = names(players_one_team), new = names(player_team_assign))
final_teams <- rbind(player_team_assign, players_one_team)
nba <- left_join(nba, final_teams, by = c("Player", "Tm" = "Tm.x"))
nba <- nba[, -which(names(nba) %in% c("Tm", "number_teams"))]

library(tibble)

nba <- add_column(nba, nba$Tm.y, .after = "Player")
colnames(nba)[3] <- "Tm"

sapply(nba, function(x) sum(is.na(x)))
nba <- nba[!is.na(nba$Tm), -which(names(nba) %in% "Tm.y")]

#Check for missing values 
sapply(nba, function(x) sum(is.na(x)))





