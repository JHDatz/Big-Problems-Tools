library(tidyverse)
library(RMySQL)
require(DBI)

load_gamelog <- function(season) {
  glheaders <- read.csv("game_log_header.csv")
  remote <- paste0("http://www.retrosheet.org/gamelogs/gl", season, ".zip")
  local <- paste0("gl", season, ".zip")
  download.file(url = remote, destfile = local)
  unzip(local)
  local_txt <- gsub(".zip", ".txt", local) %>% toupper()
  gamelog <- read_csv(local_txt, col_names = names(glheaders), na = character())
  file.remove(local)
  file.remove(local_txt)
  return(gamelog)
}

append_game_logs <- function(conn, season) {
  message(paste("Working on", season, "season..."))
  one_season <- load_gamelog(season)
  dbWriteTable(conn, name = "gamelogs", value = one_season, append = TRUE, row.names = FALSE)
}