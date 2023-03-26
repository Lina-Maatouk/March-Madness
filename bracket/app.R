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
lowerOffset <- .25
higherOffset <- 1.50

### Define groups ###
groupA <- c(1, 16, 8, 9)
groupB <- c(2, 15, 7, 10)
groupC <- c(3, 14, 6, 11)
groupD <- c(4, 13, 5, 12)



################ shiny app ################

generate_data <- function(){
  
  ############# EAST BRACKET #############
  bracketEast <- bracket$Region == "East"
  eastBracket <- bracket[bracketEast,]
  
  # Add group column to eastBracket dataframe
  eastBracket$Group <- ifelse(eastBracket$Seed %in% groupA, "A",
                              ifelse(eastBracket$Seed %in% groupB, "B",
                                     ifelse(eastBracket$Seed %in% groupC, "C",
                                            ifelse(eastBracket$Seed %in% groupD, "D", NA))))
  
  # Modify the for loop to only iterate over each group
  for (group in c("A", "B", "C", "D")) {
    groupTeams <- eastBracket %>% filter(Group == group)
    
    # Iterate over each team within the group
    for (i in 1:2) {
      countUp <- i
      countDown <- 4 - i
      winners <- ifelse(groupTeams$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > groupTeams$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
      groupTeams$Round1[winners] <- 1
    }
    
    # Update eastBracket dataframe with modified groupTeams
    eastBracket[eastBracket$Group == group, ] <- groupTeams
  }
  
  # Arrange by seed and round 1
  eastBracket <- eastBracket %>%
    arrange(Seed) %>%
    arrange(Group)
  
  # Round 2
  for (group in c("A", "B", "C", "D")) {
    groupTeams <- eastBracket %>% filter(Group == group)
    round1Winners <- groupTeams %>% filter(Round1 == 1)
    
    # Iterate over each team within the group
    for (i in 1:2) {
      countUp <- i
      countDown <- 2 - i
      winners <- ifelse(round1Winners$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > round1Winners$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
      round1Winners$Round2[winners] <- 1
    }
    
    # Update eastBracket dataframe with modified round1Winners
    eastBracket[eastBracket$Group == group & eastBracket$Round1 == 1, ] <- round1Winners
  }
  
  # Arrange by seed and round 2
  eastBracket <- eastBracket %>%
    arrange(Seed) %>%
    arrange(Group) %>%
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
  
  
  
  ############### WEST ##################
  bracketWest <- bracket$Region == "West"
  westBracket <- bracket[bracketWest,]
  
  # Add group column to southBracket dataframe
  westBracket$Group <- ifelse(westBracket$Seed %in% groupA, "A",
                              ifelse(westBracket$Seed %in% groupB, "B",
                                     ifelse(westBracket$Seed %in% groupC, "C",
                                            ifelse(westBracket$Seed %in% groupD, "D", NA))))
  
  # Modify the for loop to only iterate over each group
  for (group in c("A", "B", "C", "D")) {
    groupTeams <- westBracket %>% filter(Group == group)
    
    # Iterate over each team within the group
    for (i in 1:2) {
      countUp <- i
      countDown <- 4 - i
      winners <- ifelse(groupTeams$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > groupTeams$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
      groupTeams$Round1[winners] <- 1
    }
    
    # Update westBracket dataframe with modified groupTeams
    westBracket[westBracket$Group == group, ] <- groupTeams
  }
  
  # Arrange by seed and round 1
  westBracket <- westBracket %>%
    arrange(Seed) %>%
    arrange(Group)
  
  # Round 2
  for (group in c("A", "B", "C", "D")) {
    groupTeams <- westBracket %>% filter(Group == group)
    round1Winners <- groupTeams %>% filter(Round1 == 1)
    
    # Iterate over each team within the group
    for (i in 1:2) {
      countUp <- i
      countDown <- 2 - i
      winners <- ifelse(round1Winners$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > round1Winners$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
      round1Winners$Round2[winners] <- 1
    }
    
    # Update westBracket dataframe with modified round1Winners
    westBracket[westBracket$Group == group & westBracket$Round1 == 1, ] <- round1Winners
  }
  
  # Arrange by seed and round 2
  westBracket <- westBracket %>%
    arrange(Seed) %>%
    arrange(Group) %>%
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
  
  ############### SOUTH #################
  bracketSouth <- bracket$Region == "South"
  southBracket <- bracket[bracketSouth,]
  
  # Add group column to southBracket dataframe
  southBracket$Group <- ifelse(southBracket$Seed %in% groupA, "A",
                               ifelse(southBracket$Seed %in% groupB, "B",
                                      ifelse(southBracket$Seed %in% groupC, "C",
                                             ifelse(southBracket$Seed %in% groupD, "D", NA))))
  
  # Modify the for loop to only iterate over each group
  for (group in c("A", "B", "C", "D")) {
    groupTeams <- southBracket %>% filter(Group == group)
    
    # Iterate over each team within the group
    for (i in 1:2) {
      countUp <- i
      countDown <- 4 - i
      winners <- ifelse(groupTeams$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > groupTeams$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
      groupTeams$Round1[winners] <- 1
    }
    
    # Update southBracket dataframe with modified groupTeams
    southBracket[southBracket$Group == group, ] <- groupTeams
  }
  
  # Arrange by seed and round 1
  southBracket <- southBracket %>%
    arrange(Seed) %>%
    arrange(Group)
  
  # Round 2
  for (group in c("A", "B", "C", "D")) {
    groupTeams <- southBracket %>% filter(Group == group)
    round1Winners <- groupTeams %>% filter(Round1 == 1)
    
    # Iterate over each team within the group
    for (i in 1:2) {
      countUp <- i
      countDown <- 2 - i
      winners <- ifelse(round1Winners$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > round1Winners$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
      round1Winners$Round2[winners] <- 1
    }
    
    # Update southBracket dataframe with modified round1Winners
    southBracket[southBracket$Group == group & southBracket$Round1 == 1, ] <- round1Winners
  }
  
  # Arrange by seed and round 2
  southBracket <- southBracket %>%
    arrange(Seed) %>%
    arrange(Group) %>%
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
  
  
  ############## MIDWEST #################
  bracketMidwest <- bracket$Region == "Midwest"
  midwestBracket <- bracket[bracketMidwest,]
  
  
  # Add group column to midwestBracket dataframe
  midwestBracket$Group <- ifelse(midwestBracket$Seed %in% groupA, "A",
                                 ifelse(midwestBracket$Seed %in% groupB, "B",
                                        ifelse(midwestBracket$Seed %in% groupC, "C",
                                               ifelse(midwestBracket$Seed %in% groupD, "D", NA))))
  
  # Modify the for loop to only iterate over each group
  for (group in c("A", "B", "C", "D")) {
    groupTeams <- midwestBracket %>% filter(Group == group)
    
    # Iterate over each team within the group
    for (i in 1:2) {
      countUp <- i
      countDown <- 4 - i
      winners <- ifelse(groupTeams$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > groupTeams$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
      groupTeams$Round1[winners] <- 1
    }
    
    # Update midwestBracket dataframe with modified groupTeams
    midwestBracket[midwestBracket$Group == group, ] <- groupTeams
  }
  
  # Arrange by seed and round 1
  midwestBracket <- midwestBracket %>%
    arrange(Seed) %>%
    arrange(Group)
  
  # Round 2
  for (group in c("A", "B", "C", "D")) {
    groupTeams <- midwestBracket %>% filter(Group == group)
    round1Winners <- groupTeams %>% filter(Round1 == 1)
    
    # Iterate over each team within the group
    for (i in 1:2) {
      countUp <- i
      countDown <- 2 - i
      winners <- ifelse(round1Winners$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > round1Winners$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
      round1Winners$Round2[winners] <- 1
    }
    
    # Update midwestBracket dataframe with modified round1Winners
    midwestBracket[midwestBracket$Group == group & midwestBracket$Round1 == 1, ] <- round1Winners
  }
  
  # Arrange by seed and round 2
  midwestBracket <- midwestBracket %>%
    arrange(Seed) %>%
    arrange(Group) %>%
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
  bracket <- rbind(westBracket, midwestBracket, southBracket, eastBracket)
  
  bracket <- bracket %>%
    arrange(Seed) %>%
    arrange(Region) %>%
    arrange(desc(Elite8)) 
  
  
  #Final4
  for (i in 1:2) {
    countUp <- i
    countDown <- i + 2
    winners <- ifelse(bracket$Rating[countUp] + runif(1, -lowerOffset, lowerOffset) > bracket$Rating[countDown] + runif(1, -higherOffset, higherOffset), countUp, countDown)
    bracket$Final4[winners] <- 1
  }
  
  bracket <- bracket %>%
    arrange(desc(Final4))
  
  
  #Championship 
  winners <- ifelse(bracket$Rating[1] + runif(1, -lowerOffset, lowerOffset) > bracket$Rating[2] + runif(1, -higherOffset, higherOffset), 1, 2)
  bracket$Championship[winners] <- 1
  
  #Orgnaization
  bracket <- bracket %>%
    arrange(desc(Round1)) %>%
    arrange(desc(Round2)) %>%
    arrange(desc(Sweet16)) %>%
    arrange(desc(Elite8)) %>%
    arrange(desc(Final4)) %>%
    arrange(desc(Championship)) %>%
    arrange(Seed) %>%
    arrange(Group) %>%
    arrange(Region)
}

######### end of generate data #########3

dataset <- generate_data()
colnames1 = c("Round1", "Round2", "Sweet16", "Elite8", "Final4", "Championship")
colnames2 = c("West", "East", "Midwest", "South")

ui <- fluidPage (
  
  titlePanel(title = "March Madness Bracket"),
  h4('Evaluate Each Regions Round Winners and Losers'),
  
  fluidRow(
    column(2,
           selectInput('round', 'Choose a Round', choices = colnames1),
           selectInput("region", "Choose regions", choices = colnames2, multiple = TRUE),
           actionButton("generate_data_button", "Generate New Bracket")
    ),
    column(4,plotOutput('plot_01')),
    column(6,DT::dataTableOutput("table_01", width = "100%"))
  )
)

server <- function(input, output) {
  
  dataset <- reactiveVal(generate_data())
  
  filtered_data <- reactive ({
    dataset() %>%
      filter(Region %in% input$region) 
  })
  
  observeEvent(input$generate_data_button, {
    dataset(generate_data())
  })
  
  output$plot_01 <- renderPlot({
    if(input$round != "Round1") {
      current_round_index <- which(colnames1 == input$round)
      previous_round <- colnames1[current_round_index - 1]
      filtered_data <- filtered_data() %>%
        filter(!!sym(previous_round) == 1)
    } else {
      filtered_data <- filtered_data()
    }
    
    filtered_data %>%
      ggplot(aes(x = reorder(Team, -Seed), fill = !!sym(input$round))) +
      geom_bar() +
      scale_y_continuous(breaks = c(0, 1)) +
      coord_flip() +
      labs(x = "Teams", y = "Loss/darkblue  -  Win/lightblue", title = "") +
      theme(axis.text.x = element_text(size = 10, face = "bold"), axis.text =  element_text(size = 10, face = "bold"))
  })
  
  output$table_01 <- DT::renderDataTable(filtered_data() %>% 
                                           arrange(Seed) %>%
                                           select(Seed, Team, Rating, Region, !!input$round, Group), 
                                         options = list(pageLength = 16,  lengthMenu = list(c(2, 4, 8, 16), c('2', '4', '8', '16'))))
}

shinyApp(ui=ui, server=server)



