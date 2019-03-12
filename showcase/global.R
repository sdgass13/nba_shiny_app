library(dplyr)

df <- read.csv('data/players.csv', stringsAsFactors = F)

df <- filter(df, isActive == T)

teams   <- sort(unique(df$nameTeam))
players <- sort(unique(filter(df, nameTeam == "Los Angeles Lakers")$namePlayer))
