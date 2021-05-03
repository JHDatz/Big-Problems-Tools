library(RMySQL)
library(tidyverse)
library(modelr)
library(Lahman)
require(DBI)
setwd("~/Desktop/Mathletics Code-Alongs")
source('inning_simulation.R')

# To be added in once I have a much stronger foundation in STATS1152 Material:
#
# Chapter 11: Streakiness in Sports

# This file was made to provide an explanation to students how some
# analysis in the book "Mathletics" can be done in R.

# Chapter 1: Baseball's Pythagorean Theorem
# Here we find the best choice for the exponent in formula (2).

# To Math Majors and Statisticians: since the book uses absolute error and not the least squares
# error, this is done with brute force instead of some linear algebra.

conn <- dbConnect(MySQL(), 
                  dbname = "lahman",
                  user = "redacted", 
                  password = "redacted",
                  host = "redacted",
                  port = 3306)

grid_search <- function(conn, string) {
  
  modified_string <- paste0('select yearID, teamID, W, L, R, RA,
          R/RA as scoring_ratio,
          W/(W+L) as winLoss,
          power(R/RA, ' , string, ')/(power(R/RA, ', string, ') + 1) as predicted_winLoss,
          abs(W/(W+L) - power(R/RA, ', string, ')/(power(R/RA, ', string, ') + 1)) as absolute_error
          from teams
          where yearID between 1980 and 2006
          order by yearID desc;')
  
  data <- dbGetQuery(conn, n = -1, modified_string)
  
  return(mean(data$absolute_error))

}

iterations <- as.character(seq(0.1, 3, 0.1))

results <- map(iterations, conn = conn, grid_search)
framed_results <- data.frame(exponent_choice = iterations, mean_abs_error = unlist(results))

# The best result by hundreths of a decimal place is 1.9 (effectively just 2).

# Chapter 2 is entirely done in the MySQL file.

# Chapter 3: Linear Weights

data <- dbGetQuery(conn, n = -1, 'select yearID, R, AB, H,
                  H - X2B - X3B - HR as Singles,
                  X2B, X3B, HR,
                  BB + HBP as Walks,
                  SB, CS
                  from teams
                  where yearID in (2000, 2006)')

lm.fit <- lm(R~Walks + Singles + X2B + X3B + HR + SB + CS, data=data)

summary(lm.fit)
anova(lm.fit)

rmse(lm.fit, data = data)

# My analysis resulted in generally agreeing, but not-quite-the-same numbers for coefficients of each variable.
# I'm fairly certain this is because R and Excel are using different algorithms for convergence (ie least squares
# versus gradient descent), but since I don't own excel I can't pursue why further.

# Chapter 4: Monte Carlo Simulation
#
# Let's do Joe Hardy First.

event_list <- c(.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, .5, 0, 0, 0, 0, 0, 0)

mean(replicate(10000, inning_simulation(event_list)))

# The function for simulating an inning is huge, so it is resting in the file inning_simulation.R with some added commentary there.
# Now, let's simulate Ichiro's innings. Some extrapolation for the events is done using the computations available in the book.


IchiroExtrap <- get_batting_prob(conn, 'suzukic01', 2004)
mean(replicate(50000, inning_simulation(IchiroExtrap)))*26.72/3 

# The 26.72 is the mean # of outs in a game, so 26.72/3 is the mean # of innings in a game, so
# the whole statement should be read as the amount of runs that 9 Ichiros as batters would get
# per game.

# I found a bug in the author's excel spreadsheet model for GIDP that causes each GIDP to count as a triple play.
# When reproduced my inning simulator gets essentially the same runs as the author. When removed Ichiro
# gets .2 to .3 more runs then the author's model.

# Let's do the same for 2006 Pujols and see how many wins he contributed to the cardinals.

PujolsExtrap <- get_batting_prob(conn, 'pujolal01', 2006)
mean(replicate(50000, inning_simulation(PujolsExtrap)))*26.72/3

SLNExtrap_noPujols <- team_batting_1out(conn, 'SLN', 2006, 'pujolal01')

season_runs_projection <- mean(replicate(50000, inning_simulation(SLNExtrap_noPujols)))*(26.72/3)*161 # Expected runs in season without Pujols
actual_runs_scored <- dbGetQuery(conn, 'select R from teams where teamID = \'SLN\' and yearID = 2006;')$R
runs_allowed <- dbGetQuery(conn, 'select RA from teams where teamID = \'SLN\' and yearID = 2006;')$RA

actual_score_ratio <- actual_runs_scored/runs_allowed
(actual_score_ratio^2)/((actual_score_ratio)^2 + 1)*161 # Expected Wins

noPujols_score_ratio <- season_runs_projection/runs_allowed
(noPujols_score_ratio^2)/((noPujols_score_ratio)^2 + 1)*161 # Expected Wins without Pujols

(actual_score_ratio^2)/((actual_score_ratio)^2 + 1)*161 - (noPujols_score_ratio^2)/((noPujols_score_ratio)^2 + 1)*161 # Wins added due to Pujols' Batting

# This can also be done for an average team; not just the Cardinals.

# Chapters 5, 6, 7, 8 TBD.

# Chapter 9: The Value of Replacement Players

# There's some threshold of PAs being used that is not listed in the book. I found
# The bottom 20% without a threshold to be absurdly underestimating replacement players.

getBatters <- dbGetQuery(conn, 'select *, 
                  AB + BB + SH + SF + HBP as PA
                  from batting
                  WHERE 
                  yearID between 2000 and 2006
                  and AB + BB + IBB + SH + SF > 40
                  ORDER BY
                  AB + BB + IBB + SH + SF')

getBatters <- getBatters[1:ceiling(dim(getBatters)[[1]]*0.2),]

getBatters %>% summarize(PA = sum(PA),
                         Errors = ceiling(sum(0.018*AB)),
                         OutsInPlay = ceiling(sum(AB + SF + SH - H - 0.018*AB - SO)),
                         SO = sum(SO), BB = sum(BB), HBP = sum(HBP),
                         Singles = sum(H - X2B - X3B - HR),
                         X2B = sum(X2B),
                         X3B = sum(X3B),
                         HR = sum(HR)) -> replacement_player_count

replacement_player_prob <- (replacement_player_count/replacement_player_count[[1]])[2:length(replacement_player_count)]
replacement_player_extrap <- extrapolate_event_list(replacement_player_prob)

replacement_simulations <- replicate(50000, inning_simulation(replacement_player_extrap))

mean(replacement_simulations)*26.72/3 * 162

305^2 / (305^2 + 775^2) * 162