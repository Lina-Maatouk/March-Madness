library(tidytext)
library(textdata)
library(dplyr)
library(tidyr)
library(readr)
library(shiny)
library(rpart)
library(rpart.plot)

# rm(list = ls())
# setwd("C:/Users/linamaatouk21/Documents/MM")


# Read the data and save as rds 
GameData <- read.csv("2023 Tournament Data.csv")
saveRDS(GameData, file = "2023TournamentData.rds")
GameData <- readRDS("2023TournamentData.rds")
# Remove missing values
GameData <- na.omit(GameData)
#decision tree model
tree <- rpart(TEAM ~ KENPOM.ADJUSTED.EFFICIENCY + KENPOM.ADJUSTED.OFFENSE + KENPOM.ADJUSTED.DEFENSE + 
                KENPOM.ADJUSTED.TEMPO + BARTHAG + ELITE.SOS + BARTTORVIK.ADJUSTED.TEMPO + X2PT.. + X3PT.. + 
                FREE.THROW.. + EFG.. + FREE.THROW.RATE + X3PT.RATE + ASSIST.. + OFFENSIVE.REBOUND.. + 
                DEFENSIVE.REBOUND.. + BLOCK.. + TURNOVER.. + X2PT...DEFENSE + X3PT...DEFENSE + 
                FREE.THROW...DEFENSE + EFG...DEFENSE + FREE.THROW.RATE.DEFENSE + X3PT.RATE.DEFENSE + 
                OP.ASSIST.. + OP.O.REB.. + OP.D.REB.. + BLOCKED.. + TURNOVER...DEFENSE, data = GameData)

rpart.plot(tree, type = 3, clip.right.labs = FALSE, varlen = 0, box.col = "lightblue")

# Make predictions using the decision tree
predictions <- predict(tree, GameData, type = "class")
predictions <- data.frame(predictions)

winner <- names(table(predictions))[which.max(table(predictions))]

winnerdf <- data.frame(winner)
cat("The predicted winner is", winner)


# shiny app that displays the decision tree
ui <- fluidPage(
  tags$style(".navbar {display: none;}"),
  titlePanel("March Madness 2023 Decision Tree"),
  mainPanel(
    plotOutput("treeplot", width = "100%", height = "800px")
  )
)


server <- function(input, output) {
  output$treeplot <- renderPlot({
    rpart.plot(tree, type = 3, clip.right.labs = FALSE, varlen = 0, box.col = "lightblue")
  }, height = 800) 
}

shinyApp(ui = ui, server = server)
