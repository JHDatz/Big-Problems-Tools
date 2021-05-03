library(tidyverse)
library(Lahman)
library(RMySQL)
require(DBI)
source("helper_code/one_simulation_68.R")
source("~/Desktop/coding/Big Problems (Github)/2021 - All Groups/R Tools & Tutorials/markov_inning_simulation.R")

# This code is a shortened and modified version of Chapter 9 in ABDwR.
#
# It is modified to:
#
#   1. Make calls to the MySQL server instead of heavily modifying a CSV file.
#   2. By referring to a pre-constructed views and functions, many lines can be simplified.
#   3. Provide some commentary to compare/contrast methods with other books.
#   4. Fix some minor errors in code in the book (ex: mislabling column names).
#
# Let's start by getting data from the MySQL server.

conn <- dbConnect(MySQL(), 
                  dbname = "merged",
                  user = "redacted", 
                  password = "redacted",
                  host = "redacted",
                  port = 3306)

P_matrix <- construct_markov_matrix(conn, 2016, 2016)

dbDisconnect(conn)

# Lets look at P_matrix like it's a list and see some particular states.

P_matrix %>% as_tibble(rownames = "start_state") %>%
  filter(start_state == "010 2") %>%
  gather(key = "end_state", value = "Prob", -start_state) %>%
  filter(Prob > 0)

# Now let's simulate some innings! We'll use the information from
# our inning simulations to see what the average number of runs
# we'll get from the start of an inning.

set.seed(111653)
mean(replicate(100000, simulate_half_inning(P_test_matrix)))

# Use Markov Chains to get the Run Expectancy Matrix!

RE_bat <- sapply(1:24, RUNS.j) %>%
  matrix(nrow = 8, ncol = 3, byrow = TRUE,
         dimnames = list(c("000", "001", "010", "011", "100", "101", "110", "111"),
                         c("0 outs", "1 out", "2 outs")))

# Since P_Matrix is a Markov Chain, we can analyze the Prob of moving to 
# a particular state after 3 state changes, and see the average number 
# of state changes (i.e, PAs) before moving to the 3-out absorbing state.

P_matrix_3 <- P_matrix %*% P_matrix %*% P_matrix

P_matrix_3 %>%
  as_tibble(rownames = 'start_state') %>%
  filter(start_state == "000 0") %>%
  gather(key = "end_state", value = "Prob", -start_state) %>%
  arrange(desc(Prob)) %>%
  head()

# In being an absorbing state Markov Chain, we can use the properties
# of Markov Chain to analyze the average number of Plate Appearances
# in an inning as well.

Q <- P_matrix[-25, -25]
N <- solve(diag(rep(1,24)) - Q)

N.0000 <- round(N["000 0", ], 2)
head(data.frame(N = N.0000))

sum(N.0000)

Length <- round(t(N %*% rep(1, 24)), 2)
data.frame(Length = Length[1, 1:8])

# We can also use construct_markov_matrix for team-specific
# Markov Chains that we'd like to take a look at. More details
# are available in the file markov_inning_simulation.

WAS_P_matrix <- construct_markov_matrix(conn, 2016, 2016, team = "WAS")

WAS_P_matrix

# Moving on to 9.3 - Simulating a Baseball Season...
#
# The functions used in this analysis were moved to the file
# one_simulation_68.R after creating them. Additional commentary
# can be found there.

s.talent <- 0.20
RESULTS <- one.simulation.68(0.20)

map(1:2, display_standings, data = RESULTS) %>%
  bind_cols()

RESULTS %>%
  filter(Winner.Lg == 1) %>%
  select(Team, Winner.WS)       

# Let's simulate many seasons to see how closely talent allows 
# a team to win.

Many.Results <- map_df(rep(0.2, 1000), one.simulation.68)

ggplot(Many.Results, aes(Talent, Wins)) + geom_point(alpha = 0.05)

ggplot(filter(Many.Results, Talent > -0.05, Talent < 0.05), aes(Wins)) + geom_histogram(color = "red", fill = "white")

# Surprisingly, the variance is pretty high. Even an average team has
# not insignificant chances of winning.
#
# Now let's use binomial logistic regression to see how big the
# relationship is between talent and winning the world series.

fit1 <- glm(Winner.Lg ~ Talent, data = Many.Results, family = "binomial")
fit2 <- glm(Winner.WS ~ Talent, data = Many.Results, family = "binomial")

talent_values <- seq(-0.4, 0.4, length.out = 100)
tdf <- tibble(Talent = talent_values)
df1 <- tibble(
  Talent = talent_values,
  Probability = predict(fit1, tdf, type = "response"),
  Outcome = "Pennant")
df2 <- tibble(
  Talent = talent_values,
  Probability = predict(fit2, tdf, type = "response"),
  Outcome = "World Series")

ggplot(bind_rows(df1, df2), aes(Talent, Probability, linetype = Outcome)) + geom_line() + ylim(0,1)