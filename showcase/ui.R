library(shiny)
library(shinydashboard)
library(plotly)

# SOURCE SCRIPTS
source('scripts/ui_functions.R')

shinyUI(
  navbarPage(
    tags$head(tags$style(HTML('.navbar-default {
                                  background-color: #99AAB8 !important;
                                  height: 180px
                              }
                               .navbar-default:hover {
                                  background-color: #99AAB8!important;
                                  color: #C0D6EA;
                              }
                               .navbar-nav {
                                  position: absolute;
                                  top: 85%;
                                  left: 50%;
                                  margin-right: -50%;
                                  transform: translate(-50%, -50%);
                              }
                              .navbar-nav :first-child{
                                  background-color: #99AAB8!important;
                              }
                              .small-box {
                                  width: 240px
                              }
                              '
    ))),
    
    tabPanel("Player Stats", 
               
               includeCSS(path = "www/AdminLTE.css"),
               includeCSS(path = "www/shinydashboard.css"),
               
               nbaIndividPageUI(id = "nba_individual")
      )
  )
)
