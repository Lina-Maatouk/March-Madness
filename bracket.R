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
library(tidyverse)

rm(list = ls())


#reads in all the .csv files
team_data <- readRDS("top_performers_2023.rds")

#making simple bracket with just Seed, Team, and Strength
bracket <- team_data %>%
  select(Seed, Team, Overall_Team_Rating)%>%
  rename(Rating = Overall_Team_Rating)

teams <- c("Alabama", "Houston", "Kansas", "Purdue", "Arizona", "Marquette", "Texas", "UCLA", "Baylor", "Gonzaga", "Kansas St.", "Xavier", "Connecticut", "Indiana", "Tennessee", "Virginia", "Duke", "Miami FL", "Saint Mary's", "San Diego St.", "Creighton", "Iowa St.", "Kentucky", "TCU", "Michigan St.", "Missouri", "Northwestern", "Texas A&M", "Arkansas", "Iowa", "Maryland", "Memphis", "Auburn", "Florida Atlantic", "Illinois", "West Virginia", "Boise St.", "Penn St.", "USC", "Utah St.", "Arizona St.", "North Carolina St.", "Pittsburgh", "Providence", "College of Charleston", "Drake", "Oral Roberts", "VCU", "Furman", "Iona", "Kent St.", "Louisiana Lafayette", "Grand Canyon", "Kennesaw St.", "Montana St.", "UC Santa Barbara", "Colgate", "Princeton", "UNC Asheville", "Vermont", "Fairleigh Dickinson", "Howard", "Northern Kentucky", "Texas A&M Corpus Chris")
regions <- c("South", "Midwest", "West", "East", "South", "East",             "Midwest", "West", "South", "West", "East",         "Midwest", "West",       "Midwest", "East",      "South", "East",     "Midwest", "West",         "South",           "South", "Midwest",    "East", "West",      "East",            "South", "West",         "Midwest", "West", "Midwest", "South", "East",         "Midwest", "East", "West", "South",                            "West", "Midwest", "East", "South",        "West", "South",                "Midwest", "East",               "South",          "Midwest", "East",      "West", "South", "West", "Midwest", "East",                     "West", "Midwest",           "East",        "South",          "Midwest", "South", "West", "East",                   "East",                      "West", "Midwest",         "South")
teamsRegions <- data.frame(Team = teams, Region = regions)

bracket <- left_join(bracket, teamsRegions, by = "Team") %>%
  arrange(desc(Region))
bracket <- na.omit(bracket) 


bracket$Round1 <- 0
bracket$Round2 <- 0
bracket$Sweet16 <- 0
bracket$Elite8 <- 0
bracket$Final4 <- 0
bracket$Championship <- 0

#### variability added ###
lowerOffset <- .75
higherOffset <- 1.25

############ EAST BRACKET #########
bracketEast <- bracket$Region == "East"
eastBracket <- bracket[bracketEast,]

#Round 1
for (i in 1:8) {
  countUp <- i
  countDown <- 16 - i + 1
  winners <- ifelse(eastBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > eastBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  eastBracket$Round1[winners] <- 1
}
eastBracket <- eastBracket %>%
  arrange(Seed) %>%
  arrange(desc(Round1)) 

#Round 2
for (i in 1:4) {
  countUp <- i
  countDown <- 8 - i + 1
  winners <- ifelse(eastBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > eastBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  eastBracket$Round2[winners] <- 1
}
eastBracket <- eastBracket %>%
  arrange(Seed) %>%
  arrange(desc(Round2))

#Sweet 16
for (i in 1:2) {
  countUp <- i
  countDown <- 4 - i + 1
  winners <- ifelse(eastBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > eastBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  eastBracket$Sweet16[winners] <- 1
}

eastBracket <- eastBracket %>%
  arrange(Seed) %>%
  arrange(desc(Sweet16))

#Elite 8
winners <- ifelse(eastBracket$Rating[1] + runif(1, -lowerOffset, lowerOffset) > eastBracket$Rating[2] + runif(1, -higherOffset, higherOffset), 1, 2)
eastBracket$Elite8[winners] <- 1



############ WEST ###############
bracketWest <- bracket$Region == "West"
westBracket <- bracket[bracketWest,]

#Round 1
for (i in 1:8) {
  countUp <- i
  countDown <- 16 - i + 1
  winners <- ifelse(westBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > westBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  westBracket$Round1[winners] <- 1
}
westBracket <- westBracket %>%
  arrange(Seed) %>%
  arrange(desc(Round1)) 

#Round 2
for (i in 1:4) {
  countUp <- i
  countDown <- 8 - i + 1
  winners <- ifelse(westBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > westBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  westBracket$Round2[winners] <- 1
}
westBracket <- westBracket %>%
  arrange(Seed) %>%
  arrange(desc(Round2))

#Sweet 16
for (i in 1:2) {
  countUp <- i
  countDown <- 4 - i + 1
  winners <- ifelse(westBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > westBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  westBracket$Sweet16[winners] <- 1
}

westBracket <- westBracket %>%
  arrange(Seed) %>%
  arrange(desc(Sweet16))

#Elite 8
winners <- ifelse(westBracket$Rating[1] + runif(1, -lowerOffset, lowerOffset) > westBracket$Rating[2] + runif(1, -higherOffset, higherOffset), 1, 2)
westBracket$Elite8[winners] <- 1



############ SOUTH ###############
bracketSouth <- bracket$Region == "South"
southBracket <- bracket[bracketSouth,]

#Round 1
for (i in 1:8) {
  countUp <- i
  countDown <- 16 - i + 1
  winners <- ifelse(southBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > southBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  southBracket$Round1[winners] <- 1
}
southBracket <- southBracket %>%
  arrange(Seed) %>%
  arrange(desc(Round1)) 

#Round 2
for (i in 1:4) {
  countUp <- i
  countDown <- 8 - i + 1
  winners <- ifelse(southBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > southBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  southBracket$Round2[winners] <- 1
}
southBracket <- southBracket %>%
  arrange(Seed) %>%
  arrange(desc(Round2))

#Sweet 16
for (i in 1:2) {
  countUp <- i
  countDown <- 4 - i + 1
  winners <- ifelse(southBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > southBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  southBracket$Sweet16[winners] <- 1
}

southBracket <- southBracket %>%
  arrange(Seed) %>%
  arrange(desc(Sweet16))

#Elite 8
winners <- ifelse(southBracket$Rating[1] + runif(1, -lowerOffset, lowerOffset) > southBracket$Rating[2] + runif(1, -higherOffset, higherOffset), 1, 2)
southBracket$Elite8[winners] <- 1


############ MIDWEST ###############
bracketMidwest <- bracket$Region == "Midwest"
midwestBracket <- bracket[bracketMidwest,]

#Round 1
for (i in 1:8) {
  countUp <- i
  countDown <- 16 - i + 1
  winners <- ifelse(midwestBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > midwestBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  midwestBracket$Round1[winners] <- 1
}
midwestBracket <- midwestBracket %>%
  arrange(Seed) %>%
  arrange(desc(Round1)) 

#Round 2
for (i in 1:4) {
  countUp <- i
  countDown <- 8 - i + 1
  winners <- ifelse(midwestBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > midwestBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  midwestBracket$Round2[winners] <- 1
}
midwestBracket <- midwestBracket %>%
  arrange(Seed) %>%
  arrange(desc(Round2))

#Sweet 16
for (i in 1:2) {
  countUp <- i
  countDown <- 4 - i + 1
  winners <- ifelse(midwestBracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > midwestBracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  midwestBracket$Sweet16[winners] <- 1
}

midwestBracket <- midwestBracket %>%
  arrange(Seed) %>%
  arrange(desc(Sweet16))

#Elite 8
winners <- ifelse(midwestBracket$Rating[1] + runif(1, -lowerOffset, lowerOffset) > midwestBracket$Rating[2] + runif(1, -higherOffset, higherOffset), 1, 2)
midwestBracket$Elite8[winners] <- 1

#### combining brackets ####
bracket <- rbind(eastBracket, midwestBracket, southBracket, westBracket)

bracket <- bracket %>%
  arrange(Seed) %>%
  arrange(desc(Elite8))


for (i in 1:2) {
  countUp <- i
  countDown <- 4 - i + 1
  winners <- ifelse(bracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > bracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
  bracket$Final4[winners] <- 1
}

bracket <- bracket %>%
  arrange(Seed) %>%
  arrange(desc(Final4))


winners <- ifelse(bracket$Rating[1] + runif(1, -lowerOffset, lowerOffset) > bracket$Rating[2] + runif(1, -higherOffset, higherOffset), 1, 2)
bracket$Championship[winners] <- 1



