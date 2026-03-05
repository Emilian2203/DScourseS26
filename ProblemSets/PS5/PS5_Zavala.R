library(rvest)

# Problem 3: Web Scraping - MLB 2024 Standings
url <- "https://www.baseball-reference.com/leagues/majors/2024-standings.shtml"
page <- read_html(url)
tables <- page %>% html_nodes("table") %>% html_table()
al_east <- tables[[1]]
print(al_east)
write.csv(al_east, "mlb_standings_2024.csv", row.names = FALSE)

# Problem 4: API Data - MLB 2024 Home Run Leaders
library(rjson)
url <- "https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=homeRuns&season=2024&limit=20"
data <- fromJSON(file = url)
leaders <- data$leagueLeaders[[1]]$leaders
hr_table <- data.frame(
  Rank = sapply(leaders, function(x) x$rank),
  Player = sapply(leaders, function(x) x$person$fullName),
  Team = sapply(leaders, function(x) x$team$name),
  Home_Runs = sapply(leaders, function(x) x$value)
)
print(hr_table)
write.csv(hr_table, "mlb_hr_leaders_2024.csv", row.names = FALSE)
