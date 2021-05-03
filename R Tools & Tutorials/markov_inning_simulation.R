library(tidyverse)
library(RMySQL)
require(DBI)

# construct_markov_matrix is a function which takes the state changes
# from the MySQL view merged.vGenerateStates and constructs a Markov
# transition matrix, where the entries are probability of moving from
# one state to the next.

construct_markov_matrix <- function(conn, startYear, endYear, team = "ANY") {
  
  # Pick up data
  
  dbGetQuery(conn, n = -1, paste0('call computeStates(', startYear, ', ', endYear, ')'))
  data <- dbGetQuery(conn, n = -1, 'select * from stateTracker')
  
  # Remove innings that don't go to 3 outs, innings where no scores
  # are made, and stick to ABs. Lastly, coalesce all 3 out transitions
  # to just a single 3 out state, regardless of runners on base. This
  # drops the 32 states in the end_state column to 25 states.
  
  data %>% filter(start_state != end_state | runs_scored > 0) %>%
    filter(outs_inning == 3, BAT_EVENT_FL == TRUE) %>%
    mutate(end_state = gsub("[0-1]{3} 3", "3", end_state)) -> dataClean
  
  # Create the transition matrix T and the prob. matrix P. Add a row 
  # to P so that it's clear that the 3 out state is an absorbing state.
  
  dataClean %>% select(start_state, end_state) %>% table() -> T_matrix
  T_matrix %>% prop.table(1) -> P_matrix
  
  # Create team-specific Markov Chains if requested. When getting to 
  # the team level, we might not have enough data to adequately 
  # represent the team's true probability distribution, so we introduce 
  # a smoothing curve from all team data to fill in the gaps a bit.
  
  if (team != "ANY") {
    
    dataClean %>%
      mutate(HOME_TEAM_ID = str_sub(GAME_ID, 1, 3),
             BATTING.TEAM = ifelse(BAT_HOME_ID == 0, AWAY_TEAM_ID, HOME_TEAM_ID)) %>%
      filter(BATTING.TEAM == team) %>%
      select(start_state, end_state) %>% table() -> team_T_matrix
    
    team_T_matrix %>% prop.table(1) -> team_P_matrix
    
    P_matrix <- (team_T_matrix / (1274 + team_T_matrix)) * team_P_matrix + (1274 / (1274 + team_T_matrix))* P_matrix
    
  }
  
  P_matrix <- rbind(P_matrix, c(rep(0, 24), 1))
  dimnames(P_matrix)[[1]][[25]] <- "3"
  return(P_matrix)
  
}

# count_runners_out is a function which sums up the number of runners and 
# outs. This is to aid us in creating the equation on page 206 of ABDwR 
# for every permutation of state changes.

count_runners_out <- function(s) {
  
  s %>% str_split("") %>%
    pluck(1) %>%
    as.numeric() %>%
    sum(na.rm = TRUE)
  
}

# simulate_half_inning allows us to use our Markov Matrix
# to simulate a half-inning of baseball. P is our state
# transition matrix and start defines the starting state
# from any of the 24 non-absorbing states in the matrix.

simulate_half_inning <- function(P, start = 1) {
  
  # Use our count_runners_out function to apply the formula
  # on page 206 of ABDwR.
  
  runners_out <- sapply(row.names(P), count_runners_out)[-25]
  
  # R is an outer product matrix that finishes the formula
  # on page 206 of ABDwR. For any state change, we can refer
  # to this matrix to see how many runs were gained during
  # the play.
  
  R <- outer(runners_out + 1, runners_out, FUN="-")
  names(R) <- dimnames(P)[[1]][-25]
  R <- cbind(R, rep(0, 24))
  
  s <- start
  path <- NULL
  runs <- 0
  while (s < 25) {
    
    s.new <- sample(1:25, size = 1, prob = P[s,])
    path <- c(path, s.new)
    runs <- runs + R[s, s.new]
    s <- s.new
    
  }
  
  return(runs)
  
}

# RUNS.j is a helper function meant to be used with the apply
# family of functions to find the run expectancy from every state.

RUNS.j <- function(j, iterations = 100000) {
  
  mean(replicate(iterations, simulate_half_inning(P_matrix, j)))
  
}