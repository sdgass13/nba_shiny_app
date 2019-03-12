library(shiny)
library(dplyr)
source('scripts/functions.R')
source('global.R')
source('scripts/individ_player_server.R')

allplayers <- read.csv('data/players.csv', stringsAsFactors = F)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  callModule(nbaIndividPage, id = "nba_individual", data = allplayers) #see meta_server.R
  
})
