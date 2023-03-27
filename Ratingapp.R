library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)


rm(list=ls())

setwd("C:/Users/linamaatouk21/Documents/MarchMadnessapp")

#read as an rds file
RatingData <- read.csv("538ratingsMen.csv")
saveRDS(RatingData, file = "538ratingsMen.rds")
RatingData <- readRDS("538ratingsMen.rds")

ui <- fluidPage(
  titlePanel("Team Ratings Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "teams",
        label = "Select teams:",
        choices = sort(unique(RatingData$TeamName)),
        multiple = TRUE
      )
    ),
    mainPanel(
      plotOutput("ratings_plot")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    RatingData %>%
      filter(TeamName %in% input$teams)
  })
  
  output$ratings_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Season, y = X538rating, color = TeamName)) +
      geom_line() +
      labs(x = "Season", y = "Rating", title = "Ratings Over Time") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)

