library(RColorBrewer)

nbaIndividPage <- function(input, output, session, data){
  
  # NBA Player Tab =================================================================
  players <- reactiveValues()
  players$data <- data
  
  ## Select Player-------------------------------------------------------------------
  observeEvent(input$active, ignoreInit = T, {
    
    players$activelist <- players$data
    
    if(input$active == 'Active'){
      
      players$activelist <- filter(players$activelist, isActive == TRUE)
      
      updateSelectInput(session, "select_team", choices = c("", 'All', unique(players$activelist$teamName)), 
                        selected = "")
    }
    
    if(input$active == 'Inactive'){
      players$activelist <- filter(players$activelist, isActive == FALSE)
      
      updateSelectInput(session, "select_team", choices = c('Active Teams Only'))
    }
    
  })
  
  observeEvent(input$select_team, ignoreInit = T, {
    
    players$teamlist <- players$data
    
    if(input$active == 'Active'){
      if(input$select_team != 'All'){
        players$teamlist <- filter(players$teamlist, nameTeam == input$select_team)
      }
    }
    
    updateSelectInput(session, "select_player", choices = c(players$teamlist$namePlayer))
    
  })
  
  ## Player Information------------------------------------------------------------------
  
  player_info <- reactiveValues()
  
  observeEvent(input$select_player,{
    
    ### Info from local data
    if(!is.null(players$namelist)){
      
      player_info$data <- filter(players$namelist, namePlayer == input$select_player)
      
      player_info$num_seasons <- player_info$data$countSeasons
      
      output$seasons <- renderValueBox({
        valueBox(
          value = player_info$num_seasons,
          subtitle = 'Seasons',
          color = "blue"
        )
        
      })
      
      ### Pull Info from Internet
      
      #### Headshot
      
      player_info$hs <- select(player_info$data, urlPlayerHeadshot)[[1]]
      
      src <- player_info$hs
      
      output$headshot<-renderText({
        c('<img src="',src,'">')
      })
      
      #### Career Stats
      
      id <- player_info$data$idPlayer
      
      if(!identical(id, integer(0))){
        
        player_info$career <- get_career(id)
        
      }
      
    }
    
    else{

      player_info$data <- filter(data, namePlayer == input$select_player)
      
      player_info$num_seasons <- player_info$data$countSeasons
      
      output$seasons <- renderValueBox({
        
        valueBox(
          value = player_info$num_seasons,
          subtitle = 'Seasons',
          color = "blue"
        )
        
      })
      
      ### Pull Info from Internet
      
      #### Profile
      id <- player_info$data$idPlayer
      
      prof <- get_profile(id)
      
      ##### Value Boxes 
      print(prof$POSITION[[1]])
      
      output$position <- renderText({
        sprintf("%s / %s / %s", as.character(prof$POSITION[[1]]), as.character(prof$HEIGHT[[1]]), 
                sprintf("%s LBS", as.character(prof$WEIGHT[[1]])))
      })
      
      output$height <- renderText({
          as.character(prof$HEIGHT[[1]])
      })
      
      output$weight <- renderText({
          sprintf("%s Pounds", as.character(prof$WEIGHT[[1]]))
      })

      #### Headshot
      
      player_info$hs <- select(player_info$data, urlPlayerHeadshot)[[1]]
      src <- player_info$hs
      
      output$headshot<-renderText({
        c('<img src="',src,'">')
      })
      
      #### Career Stats
      
      
      if(!identical(id, integer(0))){
        
        player_info$career <- get_career(id)
        
      }
      
    }
    
    # Get Stats from downloaded Data
    
    ## By Season Avgs
    stats <- names(filter(player_info$career, nameTable == "SeasonTotalsRegularSeason")$dataTable[[1]])[-1:-5]
    
    updateSelectInput(session, "select_stat", choices = stats, 
                      selected = 'pts')
    
    ## By Season Ranks
    stats_rank <- names(filter(player_info$career, nameTable == "SeasonRankingsRegularSeason")$dataTable[[1]])[-1:-5]
    
    updateSelectInput(session, "ranking_stat", choices = stats_rank, 
                      selected = 'ptsRankPerGame')
    
    ## Career Avgs
    career_avg <- filter(player_info$career, nameTable == 'CareerTotalsRegularSeason')$dataTable[[1]]
    
    career_pts  <- select(career_avg, pts)[[1]]
    career_asts <- select(career_avg, ast)[[1]]
    career_rbs  <- select(career_avg, ast)[[1]]
    
    output$career_asts <- renderValueBox({
      valueBox(
        value = career_asts,
        subtitle = 'Assists Per Game',
        color = "red", 
        width = 1
      )
    })
    
    output$career_rbs <- renderValueBox({
      valueBox(
        value = career_rbs,
        subtitle = 'Rebounds Per Game',
        color = "blue", 
        width = 1
      )
    })
    
    output$career_pts <- renderValueBox({
      valueBox(
        value = career_pts,
        subtitle = 'Points Per Game',
        color = "orange", 
        width = 1
      )
    })
    
    
    # Awards 
    
    award_df <- tryCatch({
      players_awards(player_ids = id) %>%
        group_by(nameAward) %>%
        summarise(count = n())
    }, error = function(error_condition) {
      NULL
    }, finally = {
      NULL
    })
    
    ## All NBA
    
    if(!is.null(award_df)){
      
      all_nbadf <- filter(award_df, nameAward == 'All-NBA')
      
      if(nrow(all_nbadf) == 0){
        all_nba <- 0
        df_allnba <- data.frame(stat = c(0, player_info$num_seasons), label = c('All NBA Seasons', 'Non All NBA Seasons'))
        
      } else {
        all_nba <- filter(award_df, nameAward == 'All-NBA')$count[[1]]
        df_allnba <- data.frame(stat = c(all_nba, player_info$num_seasons - all_nba), label = c('All NBA Seasons', 'Non All NBA Seasons'))
      }
      
      mvpdf <- filter(award_df, nameAward == 'NBA Most Valuable Player')
      
      if(nrow(mvpdf) == 0){
        mvp <- 0
      } else {
        mvp <- filter(award_df, nameAward == 'NBA Most Valuable Player')$count[[1]]
      }
      
    } else {
      mvp     <- 0
      all_nba <- 0
      
      df_allnba <- data.frame(stat = c(0, player_info$num_seasons), label = c('All NBA Seasons', 'Non All NBA Seasons'))
    }
    
    output$all_nba_plot <- renderPlotly({
      plot_ly(data = df_allnba, 
              labels = ~label, 
              values = ~stat, 
              domain = list(x = c(.1, 1), y = c(0, 1)), 
              textfont = list(size = 14)) %>%
        add_pie(hole = 0.65) %>%
        layout(annotations = list(text = sprintf('%s Seasons \n %s All NBA Selections', 
                               player_info$num_seasons, all_nba),  x = .56, y = .5, showarrow=FALSE, font = list(size = 16)),
               plot_bgcolor='rgb(181, 181, 181)', 
               paper_bgcolor='rgb(181, 181, 181)') %>%
        config(displayModeBar = F) 
    })
    
    ## MVP's
    
    
    output$mvp_dial <- renderPlotly({
      
      h = 0.24
      k = 0.5
      r = 0.15
      # Map my_raw_value to degrees. my_raw_value is between 0 and 300
      theta = mvp * 180 / 7
      # and then into radians
      theta = theta * pi / 180
      x = h + r*cos(theta)
      y = k + r*sin(theta)
      path = paste('M 0.5 0.5 L ', x, ' ', y, ' L 0.5 0.5 Z', sep = '')
      
      print(path)
      print(x)
      print(y)
      print(mvp)
      
      base_plot <- plot_ly(
        type = "pie",
        values = c(40, 10, 10, 10, 10, 10, 10),
        labels = c(" ", "0", "2", "4", "6", "8", "10"),
        rotation = 108,
        direction = "clockwise",
        hole = 0.4,
        textinfo = "label",
        textposition = "outside",
        hoverinfo = "none",
        domain = list(x = c(0, 1), y = c(0, 1)),
        marker = list(colors = c('rgb(181, 181, 181)', 'rgb(181, 181, 181)', 'rgb(181, 181, 181)', 'rgb(181, 181, 181)', 'rgb(181, 181, 181)', 'rgb(181, 181, 181)', 'rgb(181, 181, 181)')),
        showlegend = FALSE
      )
      
      base_plot <- add_trace(
        base_plot,
        type = "pie",
        values = c(50, 10, 10, 10, 10, 10),
        labels = c(" ", "Starting Out", "Legend", "Jordan", "Kareem", "GOAT"),
        rotation = 90,
        direction = "clockwise",
        hole = 0.3,
        textinfo = "label",
        textposition = "inside",
        hoverinfo = "none",
        domain = list(x = c(0, 1), y = c(0, 1)),
        marker = list(colors = c('rgb(181, 181, 181)', 'rgb(232,226,202)', 'rgb(226,210,172)', 'rgb(223,189,139)', 'rgb(223,162,103)', 'rgb(226,126,64)')),
        showlegend= FALSE
      )
      
      a <- list(
        showticklabels = FALSE,
        autotick = FALSE,
        showgrid = FALSE,
        zeroline = FALSE)
      
      base_chart <- layout(
        base_plot,
        shapes = list(
          list(
            type = 'path',
            path = path,
            xref = 'paper',
            yref = 'paper',
            fillcolor = 'rgba(44, 160, 101, 0.5)'
          )
        ),
        xaxis = a,
        yaxis = a,
        annotations = list(text = "MVP Meter", x = .5, y = .1, showarrow = FALSE),
        plot_bgcolor='rgb(181, 181, 181)', 
        paper_bgcolor='rgb(181, 181, 181)'
        
      )
      
      
    })
    
  })
  
  ## Plots ----------------------------------------------------------
  output$perseason <- renderPlotly({
    
    plot1data <- filter(player_info$career, nameTable == "SeasonTotalsRegularSeason")$dataTable[[1]]
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 17,
      color = "gray50"
    )
    
    a <- list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1
    )
    
    pal <- c("dodgerblue3", "seagreen2", "lightslategrey", "orange2")
    
    plot_ly(data = plot1data,
            type = 'scatter', 
            mode = 'line',
            x = ~slugSeason, 
            y = ~get(input$select_stat), 
            name = NULL, 
            colors = pal) %>%
      layout(title = 'Per Game Stats',
             plot_bgcolor='rgb(181, 181, 181)', 
             paper_bgcolor='rgb(181, 181, 181)', 
             xaxis = a, 
             yaxis = a) %>%
      config(displayModeBar = F) %>%
      add_trace(x = ~slugSeason, y = ~get(input$select_stat),
                mode = 'markers', color = ~slugTeam,
                marker = list(size = 14))
  })
  
  output$rankingsseason <- renderPlotly({
    
    plot2data <- filter(player_info$career, nameTable == "SeasonRankingsRegularSeason")$dataTable[[1]]
    
    plot_ly(data = plot2data,
            type = 'scatter', 
            mode = 'line',
            x = ~slugSeason, 
            y = ~get(input$ranking_stat), 
            marker = list(size = 12), 
            color = 'orange') %>%
      layout(title = 'Season Rankings',
             plot_bgcolor = 'rgb(181, 181, 181)', 
             paper_bgcolor ='rgb(181, 181, 181)') %>%
      config(displayModeBar = F) 
  })
  
  
}