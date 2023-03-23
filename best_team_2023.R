library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(data.table)
library(lubridate)
library(RColorBrewer)

rm(list = ls())

#setwd("C:/Users/lukec/OneDrive/Documents/data332/March-Madness/data")

#reading in relevant data files
team_data <- readRDS("GameData.rds")
tournament_game_data <- read.csv("Tournament Game Data.csv")

#creating a data frame to determine the strongest team in the 2023 regular season based on 6 different stats
top_performers_2023 <- team_data %>%
  select(Seed, Team, OffenseKP, DefenseKP, Barthag, SOS, EFG.., WIN..) %>%
  #making new variables where teams with the highest value are assigned 1, and teams with lesser values are assigned a number relative to the highest ranking team
  mutate(Offensive_Rating = OffenseKP/124.0870,
         Defensive_Rating = 87.3259/DefenseKP,
         National_Rank = Barthag/.959,
         Strength_of_Schedule = SOS/38.469,
         Effective_Field_Goal_Percent = EFG../58.6,
         Win_Percentage = WIN../91.17647,
         #combining the six stats above to determine the strongest teams based on the 2023 regular season
         Current_Year_Strength = Offensive_Rating + Defensive_Rating + National_Rank + Strength_of_Schedule + Effective_Field_Goal_Percent + Win_Percentage)

team_history_data <- tournament_game_data %>%
  #removing miscellaneous teams that are not in the 2023 NCAA March Madness Tournament
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

#calculating how many times each team qualified for the tournament for later use  
tournament_appearances <- team_history_data %>%
  group_by(TEAM) %>%
  summarise(Tournament_Appearances = n())

offense <-  team_history_data %>%
  group_by(TEAM) %>%
  #calculating the sum of each team's regular season offensive ratings in the years that they made the tournament
  summarise(Overall_Offense = sum(KENPOM.ADJUSTED.OFFENSE)) %>%
  #merging the tournament appearances value for each team and renaming it
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  #calculating the average annual offensive rating of each team
  mutate(Avg_Offense = Overall_Offense/Tournament_Appearances) %>%
  #assigning offensive ranking values to each team (out of 1)
  mutate(Offensive_Rating = Avg_Offense/120.36154)

defense <- team_history_data %>%
  group_by(TEAM) %>%
  #calculating the sum of each team's regular season defensive ratings in the years that they made the tournament
  summarise(Overall_Defense = sum(KENPOM.ADJUSTED.DEFENSE)) %>%
  #merging the tournament appearances value for each team and renaming it
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  #calculating the average annual defensive rating of each team
  mutate(Avg_Defense = Overall_Defense/Tournament_Appearances) %>%
  #assigning defensive ranking values to each team (out of 1)
  mutate(Defensive_Rating = 88.01429/Avg_Defense)

barthag <- team_history_data %>%
  group_by(TEAM) %>%
  #calculating the sum of each team's regular season barthag ratings in the years that they made the tournament
  summarise(Overall_Barthag = sum(BARTHAG)) %>%
  #merging the tournament appearances value for each team and renaming it
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  #calculating the average annual barthag rating of each team
  mutate(Avg_Barthag = Overall_Barthag/Tournament_Appearances) %>%
  #assigning barthag ranking values to each team (out of 1)
  mutate(National_Rank = Avg_Barthag/0.9513333)

strength_of_schedule <- team_history_data %>%
  group_by(TEAM) %>%
  #calculating the sum of each team's regular season schedule strength ratings in the years that they made the tournament
  summarise(Overall_Schedule_Strength = sum(ELITE.SOS)) %>%
  #merging the tournament appearances value for each team and renaming it
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  #calculating the average annual schedule strength rating of each team
  mutate(Avg_SOS = Overall_Schedule_Strength/Tournament_Appearances) %>%
  #assigning schedule strength ranking values to each team (out of 1)
  mutate(Strength_of_Schedule = Avg_SOS/38.84)

field_goal_percent <- team_history_data %>%
  group_by(TEAM) %>%
  #calculating the sum of each team's regular season effective field goal percentage in the years that they made the tournament
  summarise(Overall_Field_Goal_Percent = sum(EFG..)) %>%
  #merging the tournament appearances value for each team and renaming it
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  #calculating the average annual effective field goal percentage of each team
  mutate(Avg_EFG = Overall_Field_Goal_Percent/Tournament_Appearances) %>%
  #assigning schedule effective field goal percentage ranking values to each team (out of 1)
  mutate(Effective_Field_Goal_Percent = Avg_EFG/57.042856)

win_percentage <- team_history_data %>%
  group_by(TEAM) %>%
  #calculating the sum of each team's regular season win percentage in the years that they made the tournament
  summarise(Overall_Win_Percentage = sum(WIN..)) %>%
  #merging the tournament appearances value for each team and renaming it
  cbind(tournament_appearances$Tournament_Appearances) %>%
  dplyr::rename("Tournament_Appearances" = "tournament_appearances$Tournament_Appearances") %>%
  #calculating the average annual win percentage of each team
  mutate(Avg_WIN = Overall_Win_Percentage/Tournament_Appearances) %>%
  #assigning schedule win percentage ranking values to each team (out of 1)
  mutate(Win_Percentage = Avg_WIN/86.29)

#creating a data frame to combine the stat values calculated in the previous six data frames
historical_season_strength <- team_history_data %>%
  group_by(TEAM) %>%
  summarise(Tournament_Appearances = n()) %>%
  #joining in rating values for six calculated statistical rankings
  cbind(offense$Offensive_Rating, 
        defense$Defensive_Rating, 
        barthag$National_Rank,
        strength_of_schedule$Strength_of_Schedule,
        field_goal_percent$Effective_Field_Goal_Percent,
        win_percentage$Win_Percentage) %>%
  #renaming the joined columns
  dplyr::rename("Offensive_Rating" = "offense$Offensive_Rating",
                "Defensive_Rating" = "defense$Defensive_Rating",
                "National_Rank" = "barthag$National_Rank",
                "Strength_of_Schedule" = "strength_of_schedule$Strength_of_Schedule",
                "Effective_Field_Goal_Percent" = "field_goal_percent$Effective_Field_Goal_Percent",
                "Win_Percentage" = "win_percentage$Win_Percentage") %>%
  #combining the six stats above to determine the strongest teams based on the last 15 regular seasons
  mutate(Historical_Strength = Offensive_Rating + Defensive_Rating + National_Rank + Strength_of_Schedule + Effective_Field_Goal_Percent + Win_Percentage)

#creating objects to be merged with the following data frame--assigning point values to specific rounds in the tournament
TEAM.ROUND <- c(64, 32, 16, 8, 4, 2, 1)
rating <- c(0, 1, 2, 3, 4, 5, 6)

#organizing the objects above in data table format
round_scoring <- data.table(TEAM.ROUND, rating) 

#creating a data frame to determine the best tournament performers in the last 15 years
tournament_performance <- team_history_data %>%
  group_by(TEAM, TEAM.ROUND) %>%
  summarise(Tournament_Appearances = n()) %>%
  #merging the data table created above
  left_join(round_scoring, by = c("TEAM.ROUND")) %>%
  #calculating the value associated with each round for each team
  mutate(True_Score = Tournament_Appearances * rating) %>%
  #summing the individual true scores for each team
  summarise(Total_Score = sum(True_Score)) %>%
  #calculating each team's torunament performance relative to Kansas, the best performer (out of 6 like the two previous metrics)
  mutate(Historical_Tournament_Strength = Total_Score/6.5)

#making these data frames into data tables for the purpose of merging the newly calculated metrics
setDT(tournament_performance)
setDT(historical_season_strength)
setDT(top_performers_2023)

#merging Historical_Tournament_Strength metric with top_performers_2023
top_performers_2023 <- top_performers_2023[tournament_performance, Historical_Tournament_Strength:= i.Historical_Tournament_Strength,on =.(Team = TEAM)] %>%
  #assigning 0 values to the 6 teams that have never qualified for the tournament before this year
  replace(is.na(.), 0)

#merging Historical_Strength metric with top_performers_2023
#6 teams with no data are not assigned 0 values as with previous metric--they certainly would have had a higher rating had their regular season stats been included
top_performers_2023 <- top_performers_2023[historical_season_strength, Historical_Strength:= i.Historical_Strength,on =.(Team = TEAM)]

#creating data frame to calculate the strongest teams based on our final power ranking metric
power_ranking_metric <- top_performers_2023 %>%
  select(Seed, Team, Current_Year_Strength, Historical_Strength, Historical_Tournament_Strength) %>%
  #calculating final power ranking metric--number values are weights associated with variables and the top line is for the six teams without Historical_Strength values
  mutate(Overall_Team_Rating = ifelse(is.na(Historical_Strength), (Current_Year_Strength * 0.85 + Historical_Tournament_Strength * 0.15),
                                      (Current_Year_Strength * 0.75 + Historical_Strength * 0.1 + Historical_Tournament_Strength * 0.15)))

#making this data frame into a data table for the purpose of merging the newly calculated metric
setDT(power_ranking_metric)

#merging Overall_Team_Rating metric with top_performers_2023
top_performers_2023 <- top_performers_2023[power_ranking_metric, Overall_Team_Rating:= i.Overall_Team_Rating,on =.(Team = Team)]


#dynamic shiny app visualizations
ui<-fluidPage( 
  
  titlePanel(title = "Explore NCAA College Basketball March Madness Datasets"),
  h4("NCAA 2023 Tournament Teams Power Ranking"),
  
  #setting up data entry/selection areas for user
  fluidRow(
    column(2,
           selectizeInput("Teams", label = "Choose 16 teams.", choices = top_performers_2023$Team, multiple = TRUE, options = list(maxItems = 16)),
           actionButton("Button", "Go")
    ),
    #structuring charts so that they are displayed in a two-by-two grid format
    verticalLayout(splitLayout(cellWidths = c("50%", "50%"), 
                               plotOutput("plot_01"), 
                               plotOutput("plot_02")),
                   splitLayout(cellWidths = c("50%", "50%"),
                               plotOutput("plot_03"),
                               plotOutput("plot_04"))
                   
    )
  )
)



server<-function(input,output){
  #passing on inputs from the ui to the server to enable graphing
  observeEvent(input$Button, {  
    top_performers_2023 <- top_performers_2023[Team %in% input$Teams]
    
    #visualization for team strength based on 2023 regular season stats
    current_year_strength <- 
      ggplot(top_performers_2023, aes(x = Team, y = Current_Year_Strength, fill = as.factor(Seed))) +
      geom_col(show.legend = TRUE) +
      coord_flip() + 
      #titling and labeling the plot
      labs(title = "Current Year Strength by Team", subtitle = "Best 2023 Regular Season Teams",y = "Current Year Strength",fill = "Tournament Seed") 
    
    #visualization for team strength based on their performance(s) in the March Madness tournament over the last 15 years
    tournament_strength <- 
      ggplot(top_performers_2023, aes(x = Team, y = Historical_Tournament_Strength, fill = as.factor(Seed))) +
      scale_fill_brewer(palette="Paired") +
      geom_col(show.legend = TRUE) +
      coord_flip() + 
      #titling and labeling the plot
      labs(title = "Historical Tournament Strength by Team", subtitle = "Best Tournament Teams Last 15 Years", 
           y = "Historical Tournament Strength", fill = "Tournament Seed") 
    
    #visualization for team strength based on their regular season stats in the years they made the tournament over the last 15 years
    past_seasons_strength <- 
      ggplot(top_performers_2023, aes(x = Team, y = Historical_Strength, fill = as.factor(Seed))) +
      scale_fill_brewer(palette="Dark2") +
      geom_col(show.legend = TRUE) +
      coord_flip() + 
      #titling and labeling the plot
      labs(title = "Historical Regular Season Strength by Team", subtitle = "Best Regular Season Teams Last 15 Years", 
           y = "Historical Regular Season Strength", fill = "Tournament Seed")  
    
    #visualization showing how strong a team is based on a weighted metric derived from the three previous visualizations' statistics
    overall_strength <- 
      ggplot(top_performers_2023, aes(x = Team, y = Overall_Team_Rating, size = Seed)) +
      geom_point(color = "blue", alpha=0.8) +
      scale_size(range = c(10, 2), name="Seed") +
      coord_flip() +
      #titling and labeling the plot
      labs(title = "Overall Strength by Team", subtitle = "Power Ranking 2023 NCAA Tournament Teams", y = "Overall Team Rating")
    
    output$plot_01 = renderPlot({current_year_strength})
    output$plot_02 = renderPlot({tournament_strength})
    output$plot_03 = renderPlot({past_seasons_strength})
    output$plot_04 = renderPlot({overall_strength})
    
  })
  
}

shinyApp(ui=ui, server=server) 







































