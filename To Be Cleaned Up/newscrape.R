library(baseballr)
library(tidyverse)
library(RMySQL)
require(DBI)

# This is a preliminary script for obtaining player IDs to be uploaded to both the
# statcast and staging tables in the MySQL server. In the future it will be merged
# to SQL Database Generator for future projects.  

conn <- dbConnect(MySQL(), 
                  dbname = "figmentLeague",
                  user = "redacted", 
                  password = "redacted",
                  host = "redacted",
                  port = 3306)

startDate <- as.Date("2017-03-01")
endDate   <- as.Date("2017-11-1")
datelist <- as.Date(startDate:endDate, origin="1970-01-01")
  
for (i in 1:(length(datelist) - 1)) {
  data <- scrape_statcast_savant(
    start_date = datelist[[i]],
    end_date = datelist[[i+1]],
    playerid = NULL,
    player_type = "pitcher",
  )
  
  dbWriteTable(conn, name = "pitching", value = data, append = TRUE, row.names = FALSE)
}

pull(dbGetQuery(conn, n=-1, "select distinct pitcher from statcast.pitching")) -> players
bind_rows(map(players, playername_lookup)) %>% select(key_mlbam, key_retro, key_bbref, key_fangraphs) -> playerIDs
dbWriteTable(conn, name = "mlbIDScrape", value = playerIDs, append = TRUE, row.names = FALSE)

dbDisconnect(conn)

dates <- pull(dbGetQuery(conn, n = -1, 'select distinct substring_index(date, " ", 1) from test'))

get_game_pks_mlb("2018-10-01", level_ids = c(1)) %>% select(game_pk, teams.home.team.id, teams.away.team.id)