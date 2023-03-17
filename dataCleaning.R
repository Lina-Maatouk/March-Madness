library(tidytext)
library(textdata)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(sentimentr)
library(reshape2)
library(readr)
library(shiny)
library(DT)
library(lubridate)

rm(list = ls())

##########    IMPORTANT   ################
# before running the data cleaning program, make sure you set your directory to
  # folder with the .csv files

#  NOTES   #
#This piece of data is the same as Tournamnet data just less team. 
  #GameData <- read.csv("2023 Game Data.csv")

#This piece of data may be difficult to use right now but we may use it later in analyzation
  #Lina will analyze this in a different RScript possibly
  #TournamentConferenceData <- read.csv("Tournament Conference Data.csv")

#reads in all the .csv files
GameData <- read.csv("2023 Tournament Data.csv")
AllYearsGameData <- read.csv("Tournament Game Data.csv")


#1. cleaning for Game Data, filtering out teams that are not in the tournamnet
cleanGameData <- GameData %>%
  select(SEED, TEAM, KENPOM.ADJUSTED.DEFENSE, KENPOM.ADJUSTED.OFFENSE, ELITE.SOS, EFG.., WIN.., BARTHAG) %>%
  rename(Seed = SEED, Team = TEAM, DefenseKP = KENPOM.ADJUSTED.DEFENSE, OffenseKP = KENPOM.ADJUSTED.OFFENSE, SOS = ELITE.SOS, Barthag = BARTHAG)

saveRDS(cleanGameData, "GameData.rds")
#2. adding teams that made the playoffs into clean data.




#TournamentData has all the same data as Game Data just that it is in years past,
  #we will run our analysis on this years game data (finding the best team and predicitng 
  #rounds possibily). Then make an RScript using the old data as well and merging that into 
  #this years predictions. 






  




