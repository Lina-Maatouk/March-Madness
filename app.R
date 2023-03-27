library(readr)
library(shiny)
library(rpart)
library(rpart.plot)

# rm(list = ls())
setwd("C:/Users/linamaatouk21/Documents/MM")

# Read the data and save as rds 
GameData <- read.csv("2023 Tournament Data.csv")
saveRDS(GameData, file = "2023TournamentData.rds")
GameData <- readRDS("2023TournamentData.rds")
# Remove missing values
GameData <- na.omit(GameData)

# Decision tree model
tree <- rpart(TEAM ~ KENPOM.ADJUSTED.EFFICIENCY + KENPOM.ADJUSTED.OFFENSE + KENPOM.ADJUSTED.DEFENSE + 
                KENPOM.ADJUSTED.TEMPO + BARTHAG + ELITE.SOS + BARTTORVIK.ADJUSTED.TEMPO + X2PT.. + X3PT.. + 
                FREE.THROW.. + EFG.. + FREE.THROW.RATE + X3PT.RATE + ASSIST.. + OFFENSIVE.REBOUND.. + 
                DEFENSIVE.REBOUND.. + BLOCK.. + TURNOVER.. + X2PT...DEFENSE + X3PT...DEFENSE + 
                FREE.THROW...DEFENSE + EFG...DEFENSE + FREE.THROW.RATE.DEFENSE + X3PT.RATE.DEFENSE + 
                OP.ASSIST.. + OP.O.REB.. + OP.D.REB.. + BLOCKED.. + TURNOVER...DEFENSE, data = GameData)

# Function to get top four predicted teams
get_top_four <- function(data) {
  predictions <- predict(tree, data, type = "class")
  top4 <- names(sort(table(predictions), decreasing = TRUE)[1:4])
  return(paste("•", paste(top4, collapse = "\n•")))
}

ui <- fluidPage(
  titlePanel("2023 NCAA Tournament Predictor"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload your data file (CSV format)"),
      br(),
      actionButton("submit", "Predict Top Four Teams")
    ),
    mainPanel(
      h3("Predicted Top Four Teams:"),
      verbatimTextOutput("top_four"),
      br(),
      h3("Predicted Winner:"),
      verbatimTextOutput("winner")
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file1)
    read.csv(file.choose())
  })
  
  output$top_four <- renderText({
    req(input$submit)
    GameData <- data()
    GameData <- na.omit(GameData)
    get_top_four(GameData)
  })
  
  output$winner <- renderText({
    req(input$submit)
    GameData <- data()
    GameData <- na.omit(GameData)
    predictions <- predict(tree, GameData, type = "class")
    winner <- names(table(predictions))[which.max(table(predictions))]
    paste(winner)
  })
  
}

shinyApp(ui = ui, server = server)
