library(tidyverse)

download_retrosheet <- function(season) {
  # get zip file from retrosheet website
  download.file(
    url = paste0("http://www.retrosheet.org/events/", season, "eve.zip"),
    destfile = file.path("retrosheet", "zipped", paste0(season, "eve.zip"))
  )
}

unzip_retrosheet <- function(season) {
  # unzip retrosheet files
  unzip(file.path("retrosheet", "zipped", paste0(season, "eve.zip")),
        exdir = file.path("retrosheet", "unzipped"))
}

create_csv_file <- function(season) {
  # http://chadwick.sourceforge.net/doc/cwevent.html
  # shell("cwevent -y 2000 2000TOR.EVA > 2000TOR.bev")
  wd <- getwd()
  setwd("retrosheet/unzipped")
  cmd <- paste0("cwevent -y ", season, " -f 0-96 ", season, "*.EV*", " > all", season, ".csv")
  message(cmd)
  if (.Platform$OS == "unix") {
    system(cmd)
  } else {
    shell(cmd)
  }
  setwd(wd)
}

create_csv_roster <- function(season) {
  # creates a CSV file of the rosters
  rosters <- list.files(
    path = file.path("retrosheet", "unzipped"),
    pattern = paste0(season, ".ROS"),
    full.names = TRUE)
  
  rosters %>%
    map_df(read_csv,
           col_names = c("PlayerID", "LastName", "FirstName", "Bats", "Pitches", "Team")) %>%
    write_csv(path = file.path("retrosheet", "unzipped", paste0("roster", season, ".csv")))
}

cleanup <- function() {
  # removes retrosheet files not needed
  files <- list.files(
    path = file.path("retrosheet", "unzipped"),
    pattern = "(*.EV|*.ROS|TEAM)",
    full.names = TRUE
  )
  unlink(files)
  
  zips <- list.files(
    path = file.path("retrosheet", "zipped"),
    pattern = "*.zip",
    full.names = TRUE
  )
  unlink(zips)
}

parse_retrosheet_pbp <- function(season) {
  download_retrosheet(season)
  unzip_retrosheet(season)
  create_csv_file(season)
  create_csv_roster(season)
  cleanup()
}

append_pbp <- function(single_file, table, conn) {
  if (grepl('all', single_file)) {
    datafile <- read_csv(single_file, col_names = pull(read_csv('retrosheet/unzipped/fields.csv'), Header))
    cols <- sapply(datafile, is.logical)                   # These two lines added 3/1/2021
    datafile[,cols] <- lapply(datafile[,cols], as.integer) # To fix bug where logical cols. entered SQL as all 0s.
  } else {
    datafile <- read_csv(single_file, col_names = c('PlayerID', 'LastName', 'FirstName', 'Bats', 'Pitches', 'Team'))
  }
  dbWriteTable(conn, name = table, value = datafile, append = TRUE, row.names = FALSE)
}

pbp_to_sql <- function(season, conn) {
  parse_retrosheet_pbp(season)
  pbp_filename <- paste0('retrosheet/unzipped/all', season, '.csv')
  roster_filename <- paste0('retrosheet/unzipped/roster', season, '.csv')
  append_pbp(pbp_filename, 'playByPlay', conn)
  append_pbp(roster_filename, 'players', conn)
}