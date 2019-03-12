# UI Modular Functions for Shiny APP #
######################################

nbaIndividPageUI <- function(id){
  
  ns <- NS(id)
  
  tagList(

    fluidRow(
      
      tags$style(HTML('.selectize-input{background-color: #B5B5B5 !important}')), #Selectinput color
    
      absolutePanel(
        draggable = T, right = 370, width = 150, top = 700,
        selectInput(ns("select_stat"), "Stat", 
                    NULL, NULL)
      ),
      
      absolutePanel(
        draggable = T, right = 370, width = 150, top = 1200,
        selectizeInput(ns("ranking_stat"), "Stat", 
                    NULL, NULL)
      ),
      
      absolutePanel(
        draggable = F, fixed = T, left = 15, width = 300, top = 190,
        htmlOutput(ns("headshot")), 
        br(),
        h3(textOutput(ns("position")))
      ),
      # 
      # absolutePanel(
      #   draggable = F, fixed = T, left = 17, width = 100, top = 480,
      #   valueBoxOutput(ns("height"), width = 1), 
      #   style = "z-index: 10;"
      # ),
      # 
      # absolutePanel(
      #   draggable = F, fixed = T, left = 17, width = 100, top = 560,
      #   valueBoxOutput(ns("weight"), width = 1), 
      #   style = "z-index: 10;"
      # ),
      # 
      absolutePanel(
        draggable = F, fixed = T, left = 35, width = 200, top = 450,
        selectInput(ns("active"), "Active/Inactive Players", 
                    c("", "All", "Active", "Inactive"),
                    selected = 'Active')
      ),
      
      absolutePanel(
        draggable = F, fixed = T, left = 35, width = 200, top = 530,
        selectInput(ns("select_team"), NULL, 
                    teams, 
                    selected = 'Los Angeles Lakers')
      ),
      
      absolutePanel(
        draggable = F, fixed = T, left = 35, width = 200, top = 585,
        selectInput(ns("select_player"), NULL, 
                    players, 
                    selected = "LeBron James")
      ),
      
      
      
      # column(2, 
      #        absolutePanel(
      #          draggable = T, fixed = T, 
      #          htmlOutput(ns("headshot")), 
      #          valueBoxOutput(ns("position"), width = 6)
      #        ) 
      #        
      # 
      # )#, 
      # column(3, 
      #        
      #        valueBoxOutput(ns("seasons"), width = 6)
      # ),
      # 
      # column(3, 
      #        valueBoxOutput(ns("allnba"), width = 6)
      # ),
      # 
      # column(3, 
      #        valueBoxOutput(ns("mvp"), width = 6)
      # )#,
             #plotlyOutput(ns("perseason"), height = 600)), 
      
      # column(2, 
      #        
      #        absolutePanel(
      #          draggable = F, right = 10, width = 250, fixed = T,
      #          selectInput(ns("active"), "Active/Inactive Players", 
      #                      c("", "All", "Active", "Inactive"),
      #                      selected = 'Active'), 
      #          
      #          selectInput(ns("select_team"), "Team", 
      #                      teams, 
      #                      selected = 'Los Angeles Lakers'),
      #          
      #          selectInput(ns("select_player"), "Player", 
      #                      players, 
      #                      selected = "LeBron James")
      #        )
      #  )

    # fluidRow(
    #   column(2
    #   ), 
    #   
    #   column(6, 
    #          plotlyOutput(ns('mvp_dial'), width = 500), 
    #          style = "z-index: 1;")
    # ),

      column(width = 2
      ), 
      
      column(width = 4,
             plotlyOutput(ns('all_nba_plot'), width = 400)
      ), 
      
      column(width = 4, style = 'padding-right:200px;',
             plotlyOutput(ns('mvp_dial'), width = 400, height = 300)
             ), 
      
      column(width = 2, style='padding-right:150px;',
             valueBoxOutput(ns('career_pts'),  width = NULL), 
             valueBoxOutput(ns('career_asts'), width = NULL), 
             valueBoxOutput(ns('career_rbs'),  width = NULL)
      )
    ),
    
    fluidRow(
      column(10),
      column(2
             )
    ),
    
    br(), 
    
    fluidRow(
      column(2,
             br()
      ), 
      column(10,
             plotlyOutput(ns("perseason"), height = 550)
             )
    )
    
  )
}