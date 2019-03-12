get_career <- function(id){
  return(nbastatR::players_careers(player_ids = id, modes = c("PerGame", "Totals"), 
                                   assign_to_environment = F, add_mode_names = F))
}

get_profile <- function(id){
  url <- sprintf('https://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=%s', id)
  data <- rjson::fromJSON(file = url)
  
  df <- data.frame(data$resultSets[[1]]$rowSet[[1]])
  names(df) <- data$resultSets[[1]]$headers
  
  return(df)
}