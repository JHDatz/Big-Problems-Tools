library(tidyverse)
library(Lahman)
library(RMySQL)
library(broom)
library(ggrepel)
require(DBI)
setwd("~/Desktop/coding/Big Problems (Github)/2021 - All Groups/Code-Alongs/ABDwR, Recoded")
source("helper_code/age_curves.R")

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
                  dbname = "lahman",
                  user = "redacted", 
                  password = "redacted",
                  host = "redacted",
                  port = 3306)

# Now let's get a simple aging curve for Mickey Mantle and plot him to see that his trajectory is like.

pull(dbGetQuery(conn, n = -1, "select playerID from master where nameFirst = 'Mickey' and nameLast = 'Mantle'")) -> mantle_id
Mantle <- get_stats(mantle_id)

F2 <- fit_model(Mantle)
coef(F2$fit)
c(F2$Age.max, F2$Max)

F2 %>% pluck("fit") %>% summary()

ggplot(Mantle) + aes(Age, OPS) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, formula = y ~ poly(x, 2, raw = TRUE)) +
  geom_vline(xintercept = F2$Age.max, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = F2$Max, linetype = "dashed", color = "darkgrey") +
  annotate(geom = "text", x = c(29, 20), y = c(0.72, 1.1), label = c("Peak Age", "Max"), size = 5)

# As you see we get a simple curve - he starts off young and he is still learning, so his hitting abilities
# go up. However, their reaches a point where due to his age he physically declines faster than their
# skill improves. Eventually a point is reached where Mickey Mantle is no longer Major League viable.

# Let's go a little further with this aging curve idea. Let's find a player's most "similar" players
# and plot a group of aging curves for all of them. The "similarity" function is given via the aging curves.R
# file.

# To start, we'd like to classify players by position. We'll do this be classifying them at the position
# they played the most at.

vars <- c("G", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "BB", "SO", "SB")

Fielding %>%
  group_by(playerID, POS) %>%
  summarize(Games = sum(G)) %>%
  arrange(playerID, desc(Games)) %>%
  filter(POS == first(POS)) -> Positions

# Then we take this position information and join it back onto our Batting table. Once we do this,
# we add a column to our batting table which is used for our similarity score function.

Batting %>%
  replace_na(list(SF = 0, HBP = 0)) %>%
  group_by(playerID) %>%
  summarize_at(vars, sum, na.rm = TRUE) %>%
  mutate(AVG = H/AB,
         SLG = (H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/AB) %>%
  inner_join(Positions, by = "playerID") %>%
  mutate(Value.POS = case_when(
    POS == "C" ~ 240,
    POS == "SS" ~ 168,
    POS == "2B" ~ 132,
    POS == "3B" ~ 84,
    POS == "OF" ~ 48,
    POS == "1B" ~ 12,
    TRUE ~ 0)) -> C.totals

# Now that we have that, who is mantle's most similar players?

similar(mantle_id, C.totals, 6)

# For our next analysis, let's look at players who have over 2000 ABs
# just to make sure we have enough data to work with on individual players.
# The plot_trajectories function will also sum up their yearly stats in case they swapped 
# teams in the middle of the season and add in their seasonal SLG, OBP and OPS. 
# Lastly, we'll add age back in.

plot_trajectories("Mickey Mantle", C.totals, 2000, 6, 2)

# How about Derek Jeter? Let's also look at his similar player's data while we're at it.

dj_plot <- plot_trajectories("Derek Jeter", C.totals, 2000, 9, 3)
dj_plot

# These trajectories are remarkably different. Let's plot how different these curves can be.

regressions <- dj_plot$data %>%
  split(pull(., Name)) %>%
  map(~lm(OPS ~ I(Age-30) + I((Age - 30) ^ 2), data = .)) %>%
  map_df(tidy, .id = "Name") %>%
  as_tibble() %>%
  group_by(Name) %>%
  summarize(b1 = estimate[1],
            b2 = estimate[2],
            Curve = estimate[3],
            Age.max = round(30 - b2 / Curve / 2, 1),
            Max = round(b1 - b2 ^2 / Curve / 4, 3)) -> S
S

ggplot(S) + aes(Age.max, Curve, label = Name) + geom_point() + geom_label_repel()

# Next we'd like to see if the "peak year" has changed over the course of baseball
# and compare this against the actual middle of their career. To do this we'll pull
# out the batting table and restrict ourselves to those batters who have had more
# than 2000 ABs again.

Batting %>%
  group_by(playerID) %>%
  summarize(Career.AB = sum(AB, na.rm = TRUE)) %>%
  inner_join(Batting, by = "playerID") %>%
  replace_na(list(SF = 0, HBP = 0)) %>%
  filter(Career.AB >= 2000) %>% # filter players
  inner_join(Positions, by = "playerID") %>%
  group_by(playerID, yearID) %>%
  summarize(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B), 
            HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SH = sum(SH),
            SF = sum(SF), HBP = sum(HBP), Career.AB = first(Career.AB), POS = first(POS)) %>% # summarize yearly stats
  mutate(SLG = (H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/AB,
         OBP = (H + BB + HBP)/(AB + BB + HBP + SF),
         OPS = SLG + OBP) %>% # Add in target variables we want to model our regression over
  inner_join(Master, by = "playerID") %>%
  mutate(Birthyear = ifelse(birthMonth >= 7, birthYear + 1, birthYear),
         Age = yearID - Birthyear) %>%
  drop_na(Age) -> batting_2000

# Add the midcareer column to the batting table.

midcareers <- batting_2000 %>%
  group_by(playerID) %>%
  summarize(Midyear = (min(yearID) + max(yearID)) / 2,
            AB.total = first(Career.AB))

batting_2000 %>%
  inner_join(midcareers, by = "playerID") -> batting_2000

# Now we'll need to produce quadratic fits to each player's data. Once we've done
# that, we can create a plot of how peak age has deviated from a player's midcareer.

models <- batting_2000 %>%
  split(pull(., playerID)) %>%
  map(~lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = .)) %>%
  map_df(tidy, .id = "playerID")

models %>%
  group_by(playerID) %>%
  summarize(A = estimate[1],
            B = estimate[2],
            C = estimate[3]) %>%
  mutate(Peak.age = 30 - B / 2 / C) %>%
  inner_join(midcareers, by = "playerID") -> beta_coefs

age_plot <- ggplot(beta_coefs) + aes(Midyear, Peak.age) + geom_point(alpha = 0.5) + 
  geom_smooth(color = "red", method = "loess") + ylim(20, 40) + xlab("Mid Career") + ylab("Peak Age")
age_plot

# We see that a player's peak age was consistent until the 1970's, in which case the peak age
# had climbed to 2 years later, only begin trending downwards rather quickly by the early 2000s.

age_plot + aes(x = log2(AB.total)) + xlab("Log2 of Career AB")

# The log plot over career ABs indicate that players with longer Career ABs tended to have
# A higher peak age. Makes sense!
#
# Now, let's say we wanted a strip chart of peak ages over specific positions, and wanted them
# between 1985 and 1995. Then we'd filter our existing batting table:

batting_2000a <- batting_2000 %>%
  filter(Midyear >= 1985, Midyear <= 1995)

# Redo the models for each player:

models <- batting_2000a %>%
  split(pull(., playerID)) %>%
  map(~lm(OPS ~ I(Age-30) + I((Age - 30)^2), data = .)) %>%
  map_df(tidy, .id = "playerID")

models %>%
  group_by(playerID) %>%
  summarize(A = estimate[1],
            B = estimate[2],
            C = estimate[3]) %>%
  mutate(Peak.Age = 30 - B / 2 / C) %>%
  inner_join(midcareers) %>%
  inner_join(Positions) %>%
  rename(Position = POS) -> beta_estimates

beta_estimates %>%
  filter(Position %in% c("1B", "2B", "3B", "SS", "C", "OF")) %>%
  inner_join(Master) -> beta_fielders

ggplot(beta_fielders) + aes(Position, Peak.Age) + geom_jitter(width = 0.2) + ylim(20, 40) + 
  geom_label_repel(data = filter(beta_fielders, Peak.Age > 37)) + aes(Position, Peak.Age, label = nameLast)

# Generally it seems to be between 27 and 32, with a rather tight packing for catchers (poor guys)
# and a very loose packing for outfielders.

# Exercise 1a

get_stats('mayswi01')

# 1b

ggplot(data = get_stats('mayswi01')) + aes(Age, OPS) + geom_point() + geom_smooth(method = "lm", se = FALSE, size = 1.5, formula = y ~ poly(x, 2, raw = TRUE))

# 1c

estimates <- coef(lm(OPS ~ I(Age) + I((Age-30)^2), data = get_stats('mayswi01')))
estimates[1] - estimates[2]^2 / 4 / estimates[3]

# Exercise 2a

similar('mayswi01') %>% inner_join(Master, by = "playerID") %>% select(playerID, nameFirst, nameLast, sim_score) %>% head(6) -> top5_similar_mays

# 2b

plot_trajectories('Willie Mays', 6, ncol = 3)

# 2c: Smallest peak age looks like pujols :(

# Exercise 3a

batting %>%
  group_by(playerID) %>%
  summarize(Career.H = sum(H)) %>%
  filter(Career.H > 3200) %>%
  inner_join(batting, by = "playerID") %>%
  inner_join(Master, by = "playerID") %>%
  mutate(Birthyear = ifelse(birthMonth >= 7, birthYear + 1, birthYear),
          Age = yearID - Birthyear,
          AVG = H/AB) -> batting_3200

# 3b

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

# 3c
#
# I define the "most consistent" as having the smallest C value, so the smallest curvature in their parabola.
# The winner is Tris Speaker, who is found at:

beta3200_estimates %>% head(1) %>% select(playerID) %>% inner_join(Master, by = "playerID") %>% select(nameFirst, nameLast)

# Exercise 4a

Batting %>% group_by(playerID) %>% summarize(Career.HR = sum(HR)) %>% arrange(desc(Career.HR)) %>% head(10) -> top_hr

# 4b

Batting %>% 
  inner_join(top_hr, by = "playerID") %>% 
  inner_join(Master, by = "playerID") %>%
  mutate(Birthyear = ifelse(birthMonth >= 7, birthYear + 1, birthYear),
                     Age = yearID - Birthyear,
                     HR.RATE = HR/AB) -> top_hr_yearly

modelsHR <- top_hr_yearly %>%
  split(pull(., playerID)) %>%
  map(~lm(HR.RATE ~ I(Age-30) + I((Age - 30)^2), data = .)) %>%
  map_df(tidy, .id = "playerID")

# 4c

modelsHR %>%
  group_by(playerID) %>%
  summarize(A = estimate[1],
            B = estimate[2],
            C = estimate[3]) %>%
  mutate(Peak.Age = 30 - B / 2 / C,
         Max = round(A - B^2 / C / 4, 3)) %>% arrange(desc(Max)) -> betaHR_estimates

betaHR_estimates

# Barry Bonds had the Max HR rate at 10% (wow), and Frank Robinson the least.

# 4d

ggplot(top_hr_yearly) + aes(Age, HR.RATE, group=playerID, col=playerID) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, formula = y ~ poly(x, 2, raw = TRUE))

# Barry Bonds HR rate went up as he got considerably older; likely due do steroids.
# ARod's is remarably consistently low.

# Exercise 5a

Batting %>% 
  filter(yearID < 1940) %>% 
  group_by(playerID) %>% 
  summarize(Career.AB = sum(AB)) %>% 
  inner_join(Batting, by = "playerID") %>% 
  filter(yearID > 1939, yearID < 1946, Career.AB > 2000) %>% select(playerID) -> batting1945

# 5b

Batting %>% 
  filter(yearID < 1970) %>% 
  group_by(playerID) %>% 
  summarize(Career.AB = sum(AB)) %>% 
  inner_join(Batting, by = "playerID") %>% 
  filter(yearID > 1969, yearID < 1976, Career.AB > 2000) %>% select(playerID) -> batting1975

# 5c

batting1945 %>%
  inner_join(Batting, by = "playerID") %>%
  replace_na(list(SF = 0, HBP = 0)) %>%
  group_by(playerID, yearID) %>%
    summarize(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B), 
              HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SH = sum(SH),
              SF = sum(SF), HBP = sum(HBP)) %>% # summarize yearly stats
    mutate(SLG = (H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/AB,
           OBP = (H + BB + HBP)/(AB + BB + HBP + SF),
           OPS = SLG + OBP) %>%
  inner_join(Master, by = "playerID") %>%
  mutate(Birthyear = ifelse(birthMonth >= 7, birthYear + 1, birthYear),
         Age = yearID - Birthyear) %>%
  drop_na(Age) %>%
  split(pull(., playerID)) %>%
  map(~lm(OPS ~ I(Age-30) + I((Age - 30)^2), data = .)) %>%
  map_df(tidy, .id = "playerID") -> models1945 

batting1975 %>%
  inner_join(Batting, by = "playerID") %>%
  replace_na(list(SF = 0, HBP = 0)) %>%
  group_by(playerID, yearID) %>%
  summarize(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B), 
            HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SH = sum(SH),
            SF = sum(SF), HBP = sum(HBP)) %>% # summarize yearly stats
  mutate(SLG = (H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/AB,
         OBP = (H + BB + HBP)/(AB + BB + HBP + SF),
         OPS = SLG + OBP) %>%
  inner_join(Master, by = "playerID") %>%
  mutate(Birthyear = ifelse(birthMonth >= 7, birthYear + 1, birthYear),
         Age = yearID - Birthyear,) %>%
  drop_na(Age) %>%
  split(pull(., playerID)) %>%
  map(~lm(OPS ~ I(Age-30) + I((Age - 30)^2), data = .)) %>%
  map_df(tidy, .id = "playerID") -> models1975

models1945 %>%
  group_by(playerID) %>%
  summarize(A = estimate[1],
            B = estimate[2],
            C = estimate[3]) %>%
  mutate(Peak.Age = 30 - B / 2 / C,
         Max = round(A - B^2 / C / 4, 3)) %>% arrange(desc(Max)) -> beta1945_estimates

models1975 %>%
  group_by(playerID) %>%
  summarize(A = estimate[1],
            B = estimate[2],
            C = estimate[3]) %>%
  mutate(Peak.Age = 30 - B / 2 / C,
         Max = round(A - B^2 / C / 4, 3)) %>% arrange(desc(Max)) -> beta1975_estimates

# 5d

beta1945_estimates %>% filter(Peak.Age > 0) %>% summarize(Mean = mean(Peak.Age), SD = sd(Peak.Age))
beta1975_estimates %>% filter(Peak.Age > 0) %>% summarize(Mean = mean(Peak.Age), SD = sd(Peak.Age))

# The mean did not change, but the spread tightened up a bit.