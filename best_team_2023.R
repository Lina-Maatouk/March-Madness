
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(data.table)
library(lubridate)

rm(list = ls())

setwd("C:/Users/lukec/OneDrive/Documents/data332/March-Madness/data")

#reads in all the .csv files
team_data <- readRDS("GameData.rds")
tournament_game_data <- read.csv("Tournament Game Data.csv")

top_performers_2023 <- team_data %>%
  select(Seed, Team, OffenseKP, DefenseKP, Barthag, SOS, EFG.., WIN..) %>%
  mutate(Offensive_Rating = OffenseKP/124.0870,
         Defensive_Rating = 87.3259/DefenseKP,
         National_Rank = Barthag/.959,
         Strength_of_Schedule = SOS/38.469,
         Effective_Field_Goal_Percent = EFG../58.6,
         Win_Percentage = WIN../91.17647,
         Current_Year_Strength = Offensive_Rating + Defensive_Rating + National_Rank + Strength_of_Schedule + Effective_Field_Goal_Percent + Win_Percentage)
         
best_team_previous_years <- tournament_game_data %>%
  dplyr::filter(TEAM %in%  c("Alabama", "Houston", "Kansas", "Purdue", "Arizona", "Marquette", "Texas", "UCLA",
                             "Baylor", "Gonzaga", "Kansas St.", "Xavier", "Connecticut", "Indiana", "Tennessee",
                             "Virginia", "Duke", "Miami FL", "Saint Mary's", "San Diego St.", "Creighton",
                             "Iowa St.", "Kentucky", "TCU", "Michigan St.", "Missouri", "Northwestern",
                             "Texas A&M", "Arkansas", "Iowa", "Maryland", "Memphis", "Auburn", "Florida Atlantic",
                             "Illinois", "West Virginia", "Boise St.", "Penn St.", "USC", "Utah St.",
                             "Arizona St.", "Mississippi St.", "Nevada", "North Carolina St.", "Pittsburgh",
                             "Providence", "College of Charleston", "Drake", "Oral Roberts", "VCU", "Furman",
                             "Iona", "Kent St.", "Louisiana Lafayette", "Grand Canyon", "Kennesaw St.",
                             "Montana St.", "UC Santa Barbara", "Colgate", "Princeton", "UNC Asheville", "Vermont",
                             "Fairleigh Dickinson", "Howard", "Northern Kentucky", "Southeast Missouri St.",
                             "Texas A&M Corpus Chris", "Texas Southern")) %>%
  #filtering for only distinct team and year values (not including 2023)--don't need values for each round
  head(n=361) 
  
tournament_appearances <- best_team_previous_years %>%
  group_by(TEAM) %>%
  summarise(Tournament_Appearances = n())
  
offense <-  best_team_previous_years %>%
  group_by(TEAM) %>%
  summarise(Overall_Offense = sum(KENPOM.ADJUSTED.OFFENSE)) %>%
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  mutate(Avg_Offense = Overall_Offense/Tournament_Appearances) %>%
  mutate(Offensive_Rating = Avg_Offense/120.36154)

defense <- best_team_previous_years %>%
  group_by(TEAM) %>%
  summarise(Overall_Defense = sum(KENPOM.ADJUSTED.DEFENSE)) %>%
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  mutate(Avg_Defense = Overall_Defense/Tournament_Appearances) %>%
  mutate(Defensive_Rating = 88.01429/Avg_Defense)

barthag <- best_team_previous_years %>%
  group_by(TEAM) %>%
  summarise(Overall_Barthag = sum(BARTHAG)) %>%
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  mutate(Avg_Barthag = Overall_Barthag/Tournament_Appearances) %>%
  mutate(National_Rank = Avg_Barthag/0.9520769)

strength_of_schedule <- best_team_previous_years %>%
  group_by(TEAM) %>%
  summarise(Overall_Schedule_Strength = sum(ELITE.SOS)) %>%
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  mutate(Avg_SOS = Overall_Schedule_Strength/Tournament_Appearances) %>%
  mutate(Strength_of_Schedule = Avg_SOS/38.84)

field_goal_percent <- best_team_previous_years %>%
  group_by(TEAM) %>%
  summarise(Overall_Field_Goal_Percent = sum(EFG..)) %>%
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  mutate(Avg_EFG = Overall_Field_Goal_Percent/Tournament_Appearances) %>%
  mutate(Effective_Field_Goal_Percent = Avg_EFG/57.042856)

win_percentage <- best_team_previous_years %>%
  group_by(TEAM) %>%
  summarise(Overall_Win_Percentage = sum(WIN..)) %>%
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  mutate(Avg_WIN = Overall_Win_Percentage/Tournament_Appearances) %>%
  mutate(Win_Percentage = Avg_WIN/86.29)

historical_season_strength <- best_team_previous_years %>%
  group_by(TEAM) %>%
  summarise(Tournament_Appearances = n()) %>%
  cbind(offense$Offensive_Rating, 
        defense$Defensive_Rating, 
        barthag$National_Rank,
        strength_of_schedule$Strength_of_Schedule,
        field_goal_percent$Effective_Field_Goal_Percent,
        win_percentage$Win_Percentage) %>%
  dplyr::rename("Offensive_Rating" = "offense$Offensive_Rating",
                "Defensive_Rating" = "defense$Defensive_Rating",
                "National_Rank" = "barthag$National_Rank",
                "Strength_of_Schedule" = "strength_of_schedule$Strength_of_Schedule",
                "Effective_Field_Goal_Percent" = "field_goal_percent$Effective_Field_Goal_Percent",
                "Win_Percentage" = "win_percentage$Win_Percentage") %>%
  mutate(Historical_Strength = Offensive_Rating + Defensive_Rating + National_Rank + Strength_of_Schedule + Effective_Field_Goal_Percent + Win_Percentage)


TEAM.ROUND <- c(64, 32, 16, 8, 4, 2, 1)
rating <- c(0, 1, 2, 3, 4, 5, 6)

round_scoring <- data.table(TEAM.ROUND, rating) 


tournament_performance <- best_team_previous_years %>%
  group_by(TEAM, TEAM.ROUND) %>%
  summarise(Tournament_Appearances = n()) %>%
  left_join(round_scoring, by = c("TEAM.ROUND")) %>%
  mutate(True_Score = Tournament_Appearances * rating) %>%
  summarise(Total_Score = sum(True_Score)) %>%
  mutate(Historical_Tournament_Strength = Total_Score/6.5)

setDT(tournament_performance)

setDT(historical_season_strength)

setDT(top_performers_2023)

#try to reformat
top_performers_2023 <- top_performers_2023[tournament_performance, Historical_Tournament_Strength:= i.Historical_Tournament_Strength,on =.(Team = TEAM)] %>%
  replace(is.na(.), 0)

#try to reformat
top_performers_2023 <- top_performers_2023[historical_season_strength, Historical_Strength:= i.Historical_Strength,on =.(Team = TEAM)]

power_ranking_metric <- top_performers_2023 %>%
  select(Seed, Team, Current_Year_Strength, Historical_Strength, Historical_Tournament_Strength)

    
#power_ranking_metric <-ifelse(power_ranking_metric$Historical_Strength == "NA", 1, 2) mutate(Overall_Team_Rating = (power_ranking_metric$Current_Year_Strength * 0.75) + (power_ranking_metric$Historical_Tournament_Strength * 0.25)),
         #mutate(Overall_Team_Rating = (power_ranking_metric$Current_Year_Strength * 0.7) + (power_ranking_metric$Historical_Strength * 0.1) + (power_ranking_metric$Historical_Tournament_Strength * 0.2)))
  

  

  
  
  

  



  
                
                
                
                
                
                
                
                
                
                
                
                
                
               
  
  
  





