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

#shiny app that runs the model for you for any csv file
ui <- fluidPage(
  titlePanel("2023 NCAA Tournament Predictor"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload your data file (CSV format)"),
      
      br(),
      actionButton("submit", "Predict Winner")
      
    ),
    
    mainPanel(
      
      verbatimTextOutput("winner")
      
    )
    
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  output$winner <- renderText({
    req(input$submit)
    GameData <- data()
    GameData <- na.omit(GameData)
    tree <- rpart(TEAM ~ KENPOM.ADJUSTED.EFFICIENCY + KENPOM.ADJUSTED.OFFENSE + KENPOM.ADJUSTED.DEFENSE + 
                    KENPOM.ADJUSTED.TEMPO + BARTHAG + ELITE.SOS + BARTTORVIK.ADJUSTED.TEMPO + X2PT.. + X3PT.. + 
                    FREE.THROW.. + EFG.. + FREE.THROW.RATE + X3PT.RATE + ASSIST.. + OFFENSIVE.REBOUND.. + 
                    DEFENSIVE.REBOUND.. + BLOCK.. + TURNOVER.. + X2PT...DEFENSE + X3PT...DEFENSE + 
                    FREE.THROW...DEFENSE + EFG...DEFENSE + FREE.THROW.RATE.DEFENSE + X3PT.RATE.DEFENSE + 
                    OP.ASSIST.. + OP.O.REB.. + OP.D.REB.. + BLOCKED.. + TURNOVER...DEFENSE, data = GameData)
    predictions <- predict(tree, GameData, type = "class")
    winner <- names(table(predictions))[which.max(table(predictions))]
    paste0("The predicted winner is ", winner)
  })
  
}

shinyApp(ui = ui, server = server)

