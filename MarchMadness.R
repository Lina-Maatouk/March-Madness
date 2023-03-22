library(tidytext)
library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(wordcloud)
library(sentimentr)
library(reshape2)
library(stringr)
library(plotly)


rm(list=ls())

setwd("C:/Users/linamaatouk21/Documents/MarchMadness")

#read as an rds file
ConferenceData <- read.csv("Tournament Conference Data.csv")
saveRDS(ConferenceData, file = "Tournament Conference Data.rds")
ConferenceData <- readRDS("Tournament Conference Data.rds")

#For sweet 16 round
round3 <- ConferenceData %>%
  dplyr::select(YEAR, CONFERENCE, SWEET.16.COUNT)

#For Elite 8 round
round4 <- ConferenceData %>%
  dplyr::select(YEAR, CONFERENCE, ELITE.8.COUNT)

#For Final 4 round
round5 <- ConferenceData %>%
  dplyr::select(YEAR, CONFERENCE, FINAL.4.COUNT)

#For Finals round
round6 <- ConferenceData %>%
  dplyr::select(YEAR, CONFERENCE, FINALS.COUNT)

#Final results
FinalResults <- ConferenceData %>%
  dplyr::select(YEAR, CONFERENCE, CHAMP.COUNT)


# Combine the Sweet 16, Elite 8, Final 4, and Finals data frames into one
tournament_results <- rbind(round3, round4, round5, round6)

# Aggregate the results by conference and year
conference_results <- aggregate(tournament_results[, 3:5], by = list(CONFERENCE = tournament_results$CONFERENCE, YEAR = tournament_results$YEAR), FUN = sum)

# Plot the data using ggplot2
# ggplot(conference_results, aes(x = YEAR, y = CHAMP.COUNT, color = CONFERENCE)) + 
#   geom_line() + 
#   xlab("Year") + 
#   ylab("Number of Championships") +
#   ggtitle("March Madness Championships by Conference") +
#   theme_minimal()

RatingData <- read.csv("538ratingsMen.csv")
saveRDS(RatingData, file = "538ratingsMen.rds")
RatingData <- readRDS("538ratingsMen.rds")

# team_name <- "TeamName" # replace with desired team name
data_filtered <- RatingData[RatingData$team == team_name, ]

data_filtered$date <- as.Date(data_filtered$date)


ggplot(data_filtered, aes(x = date, y = rating, color = season)) +
  geom_line() +
  labs(x = "Season", y = "Rating", title = paste0("Rating for ", team_name)) +
  theme_minimal()

ggplotly()
plot_team_rating <- function(team_name) {
  data_filtered <- RatingData[RatingData$team == team_name, ]
  data_filtered$date <- as.Date(data_filtered$date)
  
  ggplot(data_filtered, aes(x = date, y = rating, color = season)) +
    geom_line() +
    labs(x = "Season", y = "Rating", title = paste0("Rating for ", team_name)) +
    theme_minimal() %>%
    ggplotly()
}



