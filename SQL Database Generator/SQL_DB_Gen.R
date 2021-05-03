# SQL Database Generator
#
# Written by: Joe Datz
# Date of being written originally unknown; it was made
# sometime in March of 2020.
# Last edited: 3/27/21
#
# This file is the master file for uploading retrosheet and lahman data to 
# a MySQL server. It is made from a heavily modified version of R code that 
# originates from ABDwR; modified mainly to deal with deprecations in
# Retrosheet data uploads.

library(tidyverse)
library(doParallel)
library(RMySQL)
library(mlbgameday)
library(Lahman)
library(retro)
require(DBI)
#source('installing_packages.R')
source('parse_retrosheet_pbp.R')
source('parse_retrosheet_gamelogs.R')
source('parse_retrosheet_extra.R')
source('lahman_upload.R')
source('gameday_upload.R')

# Upload the "gamelogs", "playByPlay", and "players" table from ABDwR.

setwd("C:\\Users\\joseph\\Desktop\\SQL Database Generator")

dbname <- 'redacted'
user <- 'redacted'
password <- 'redacted'
host <- 'redacted'

conn <- dbConnect(MySQL(), dbname = dbname, user = user, password = password, host = host, port = 3306)

map(1978:2020, pbp_to_sql, conn = conn)
map(1871:2020, append_game_logs, conn = conn)

# Updated 3/27/21 to include the tables "events", "games," and "subs" from
# chapter 11 of ABDwR. This involved the use of the retro and etl libraries
# in windows.
#
# In both Windows and Linux the libraries failed to run properly. For linux,
# something was wrong in the C++ code of the Chadwick library that led to a
# segmentation fault error. In Windows, it was commands being sent to the etl
# library to shell failing to run properly.
#
# A workaround was made in Windows. It required using XAMPP software to set
# a dummy localhost MySQL server whose information was then upload to an
# Amazon RDS MySQL server.

db <- src_mysql("retrosheet", user = 'root')
retro <- etl("retro", db = db, dir = 'C:\\Users\\joseph\\Desktop\\SQL Database Generator')

retro %>%
  etl_init() %>% 
  etl_extract(season = 1980:2020) %>% 
  etl_transform(season = 1980:2020)

# etl_transform failed to properly create CSV files, so we make them by
# manually sending commands to powershell.

setwd('C:\\Users\\joseph\\Desktop\\SQL Database Generator\\load')
files <- as.character(list.files(pattern = "EVN"))

commandEvents <- paste0(".\\cwevent -n -f 0-96 -x 0-62 -y ", substring(files, 1, 4), ' ', files, ' > ', 
                        substring(files, 1, 4), substring(files, 5, 7), '_events.csv')

commandGames <- paste0(".\\cwgame -n -f 0-83 -y ", substring(files, 1, 4), ' ', files, ' > ', 
                       substring(files, 1, 4), substring(files, 5, 7), '_games.csv')

commandSubs <- paste0(".\\cwsub -n -y ", substring(files, 1, 4), ' ', files, ' > ', 
                      substring(files, 1, 4), substring(files, 5, 7), '_subs.csv')

map(commands, shell)

# Lastly, we submit these files to the MySQL server.

conn <- dbConnect(MySQL(), dbname = dbname, user = user, password = password, host = host, port = 3306)
files <- as.character(list.files(pattern = 'events.csv|games.csv|subs.csv'))
map(files, upload_retrosheet_files, conn=conn)
dbDisconnect(conn)

# Upload the Lahman database.

conn <- dbConnect(MySQL(), dbname = dbname, user = user, password = password, host = host, port = 3306)

upload_lahman(conn)

dbDisconnect(conn)

# Upload various tables from MLB Game Day. The clusters are used to make the
# scraping of data more efficient.  

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)

year_list <- paste0("201", 2:9, "-1-1")
gameday_upload(year_list)

stopImplicitCluster()
rm(cl)
