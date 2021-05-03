upload_retrosheet_files <- function(file, conn) {
  df <- read.csv(file, encoding = "UTF-8")
  
  if (length(grep('event', file)) == 1) {
    
    dbWriteTable(conn, name = "events", value = df, append = TRUE, row.names = FALSE)
    
  } else if (length(grep('game', file)) == 1) {
    
    dbWriteTable(conn, name = "games", value = df, append = TRUE, row.names = FALSE)
    
  } else {
    
    dbWriteTable(conn, name = "subs", value = df, append = TRUE, row.names = FALSE)
    
  }
}