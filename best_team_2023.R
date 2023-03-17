
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(lubridate)

rm(list = ls())

setwd("C:/Users/lukec/OneDrive/Documents/data332/March-Madness/data")

#reads in all the .csv files
team_data <- readRDS("GameData.rds")
tournament_game_data <- read.csv("Tournament Game Data.csv")

top_performers_2023 <- team_data %>%
  select(Team, OffenseKP, DefenseKP, Barthag, SOS, EFG.., WIN..) %>%
  mutate(Offensive_rating = OffenseKP/124.0870,
         Defensive_rating = 87.3259/DefenseKP,
         National_rank = Barthag/.959,
         Strength_of_schedule = SOS/38.469,
         Effective_field_goal_percent = EFG../58.6,
         Win_percentage = WIN../91.17647,
         Overall_strength = Offensive_rating + Defensive_rating + National_rank + Strength_of_schedule + Effective_field_goal_percent + Win_percentage)
         





