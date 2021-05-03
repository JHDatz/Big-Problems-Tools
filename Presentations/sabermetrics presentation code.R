library(RMySQL)
library(tidyverse)
library(modelr)
library(Lahman)
require(DBI)

source('inning_simulation.R')

# Unused for presentation - plot of WPct over Pythagorean Theorem of Baseball

Teams %>% filter(yearID > 2000) %>% 
    select(teamID, W, L, R, RA) %>% 
    mutate(WPct = W/(W + L), Pythag = R^2/(R^2 + RA^2)) -> new_teams

ggplot(new_teams, aes(x = Pythag, y = WPct)) + geom_point() + scale_x_continuous('Baseball\'s Pythag') + scale_y_continuous('Winning Percentage') + geom_smooth(method = 'lm', se = FALSE, color='red')

Teams %>% filter(yearID == 2006) %>% 
    select(teamID, W, L, R, RA) %>% 
    mutate(WPct = W/(W + L), Pythag = R^2/(R^2 + RA^2), resid = (WPct-Pythag)^2) -> new_teams

sqrt(mean(new_teams$resid))

# Calculating Ichirio's Batting Contributions

IchiroExtrap <- get_batting_prob(conn, 'suzukic01', 2004)
mean(replicate(50000, inning_simulation(IchiroExtrap)))*26.72/3 

SEAExtrap_noIchiro <- team_batting_1out(conn, 'SEA', 2006, 'suzukic01')
season_runs_projection <- mean(replicate(50000, inning_simulation(SEAExtrap_noIchiro)))*(26.72/3)*162
actual_runs_scored <- dbGetQuery(conn, 'select R from teams where teamID = \'SLN\' and yearID = 2006;')$R
runs_allowed <- dbGetQuery(conn, 'select RA from teams where teamID = \'SLN\' and yearID = 2006;')$RA

actual_score_ratio <- actual_runs_scored/runs_allowed
(actual_score_ratio^2)/((actual_score_ratio)^2 + 1)*162

noIchiro_score_ratio <- season_runs_projection/runs_allowed
(noIchiro_score_ratio^2)/((noIchiro_score_ratio)^2 + 1)*162

(actual_score_ratio^2)/((actual_score_ratio)^2 + 1)*162 - (noIchiro_score_ratio^2)/((noIchiro_score_ratio)^2 + 1)*162

# Unused - plot of aging curves for players

batting <- Batting %>% replace_na(list(SF = 0, HBP = 0))

batting %>%
  group_by(playerID) %>%
  summarize(Career.H = sum(H)) %>%
  filter(Career.H > 3200) %>%
  inner_join(batting, by = "playerID") %>%
  inner_join(Master, by = "playerID") %>%
  mutate(Birthyear = ifelse(birthMonth >= 7, birthYear + 1, birthYear),
         Age = yearID - Birthyear,
         AVG = H/AB) -> batting_3200

models3200 <- batting_3200 %>%
  split(pull(., playerID)) %>%
  map(~lm(AVG ~ I(Age-30) + I((Age - 30)^2), data = .)) %>%
  map_df(tidy, .id = "playerID")

models3200 %>%
  group_by(playerID) %>%
  summarize(A = estimate[1],
            B = estimate[2],
            C = estimate[3]) %>%
  mutate(Peak.Age = 30 - B / 2 / C,
         Max = round(A - B^2 / C / 4, 3)) %>% arrange(C) -> beta3200_estimates


ggplot(data = batting_3200, aes(Age, AVG, group=playerID, col=playerID)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, formula = y ~ poly(x, 2, raw = TRUE))

# To reduce clutter, here's the top 3:

batting_3200 %>% pull(playerID) %>% unique() %>% head(3) -> top3
batting_3200 %>% filter(playerID %in% top3) %>% mutate(Names = paste(nameFirst, nameLast)) -> batting_3200_top3

ggplot(data = batting_3200_top3, aes(Age, AVG, group=Names, col=Names)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, formula = y ~ poly(x, 2, raw = TRUE)) + ggtitle("Aging Curves, using Batting Average (AVG)")

# Approximating pi with random numbers:

x <- runif(10000, 0, 1)
y <- runif(10000, 0, 1)

sum(x^2 + y^2 < 1)/length(x) * 4 # Approximation of pi
ggplot() + aes(x, y, group = factor(x^2 + y^2 < 1), col= factor(x^2 + y^2 < 1)) + geom_point()