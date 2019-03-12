df <- read.csv('data/players.csv', stringsAsFactors = F)

df <- filter(df, isActive == "Active")

teams   <- sort(unique(df$nameTeam))
players <- sort(unique(filter(df, nameTeam == "Lakers")$namePlayers))