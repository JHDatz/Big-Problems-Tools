library(Lahman)
library(tidyverse)

get_stats <- function(player.id) {
  Batting %>%
    replace_na(list(SF =0, HBP = 0)) %>%
    filter(playerID == player.id) %>%
    inner_join(Master, by = "playerID") %>%
    mutate(birthyear = ifelse(birthMonth >= 7, birthYear + 1, birthYear),
           Age = yearID - birthyear,
           SLG = (H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/AB,
           OBP = (H + BB + HBP)/(AB + BB + HBP + SF),
           OPS = SLG + OBP) %>%
    select(Age, SLG, OBP, OPS)
}

fit_model <- function(d) {
  fit <- lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = d)
  b <- coef(fit)
  Age.max <- 30 - b[2] / b[3] / 2
  Max <- b[1] - b[2] ^ 2 / b[3] / 4
  list(fit = fit, Age.max = Age.max, Max = Max)
}

similar <- function(p, C.totals, number = 10) {
  C.totals %>% filter(playerID == p) -> P
  C.totals %>%
    mutate(sim_score = 1000 -
             floor(abs(G - P$G) / 20) -
             floor(abs(AB - P$AB) / 75) -
             floor(abs(R - P$R) / 10) -
             floor(abs(H - P$H) / 15) -
             floor(abs(X2B - P$X2B) / 5) -
             floor(abs(X3B - P$X3B) / 4) -
             floor(abs(HR - P$HR) / 2) -
             floor(abs(RBI - P$RBI) / 10) -
             floor(abs(BB - P$BB) / 25) -
             floor(abs(SO - P$SO) / 150) -
             floor(abs(SB - P$SB) / 20) -
             floor(abs(AVG - P$AVG) / 0.001) -
             floor(abs(SLG - P$SLG) / 0.002) -
             abs(Value.POS - P$Value.POS)) %>%
    arrange(desc(sim_score)) %>%
    head(number)
}

plot_trajectories <- function(player, C.totals, CareerAB, n.similar = 5, ncol) {
  flnames <- unlist(strsplit(player, " "))
  
  Master %>%
    filter(nameFirst == flnames[1],
           nameLast == flnames[2]) %>%
    select(playerID) -> player
  
  player.list <- player %>%
    pull(playerID) %>%
    similar(C.totals, n.similar) %>%
    pull(playerID)
  
  Batting %>%
    group_by(playerID) %>%
    summarize(Career.AB = sum(AB, na.rm = TRUE)) %>%
    inner_join(Batting, by = "playerID") %>%
    replace_na(list(SF = 0, HBP = 0)) %>%
    filter(Career.AB >= CareerAB) %>% # filter players
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
    drop_na(Age) %>%
    filter(playerID %in% player.list) %>%
    mutate(Name = paste(nameFirst, nameLast)) -> Batting.new
  
  ggplot(Batting.new) + aes(Age, OPS) + 
    geom_smooth(method="lm", formula = y ~ x + I(x^2), size = 1.5) +
    facet_wrap(~ Name, ncol=ncol) + theme_bw()
}