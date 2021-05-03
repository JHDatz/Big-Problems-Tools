# Spray Simulation Tools
#
# Written By: Joe Datz
# Date created: 4/14/21
#
# This R file contains the functions used to produce an optimized set of fielder
# positions using a probabilistic simulator. The goal is to simulate a player's
# batted balls P(X,Y | Batter), then to simulate 
# P(Out | Fielder Coordinates, Batted Balls) and pick the coordinates that most
# maximizes P(Out).
#
# This represents a modeling improvement over the nonlinear optimizer because
# this can be directly translated into BABIP = 1 - P(Out). This is also a much
# more realistic representation of how the relationship between getting a player
# out and a batted ball coincide.
#
# At this point we only have two weeks to finish, so here's what we would have
# done had we had extra time:
#
# 1. Incorporate the Pitcher and Catcher into the simulator's P(Out) function.
#
# 2. Ask for more detail about grounders and do some EDA to consider modeling
# options. Batted balls to the infield are much more complicated. Some balls
# are strictly the responsibility of pitchers and catchers (choppers that hit
# the ground in front of the pitcher's mound and go no further), some are the
# closest fielder's responsibility, and some even make it to the outfield and
# become the outfielder's responsibility.
#
# 3. Also about grounders, we'd need to P(Out) in such a way for fielders that
# incorporates a distinction between the probability of getting the ball and
# the probability of getting it to first base on time.
#
# 4. Our cubic spline is only for the 2D boundary of the wall; a more realistic
# version would include calculating whether the ball would be high enough to get
# over the wall and bouncing off of it if it does not.

library(tidyverse)
library(GeomMLBStadiums)

# draw.rand.normal() draws a random number from the normal distribution, centered
# at a chosen mean centerX. This function uses the Box-Mueller method of
# producing a randomly generated number from the Normal Distribution. It returns
# a single randomly generated nu

draw.rand.normal <- function(centerX) {
  u <- runif(1, 0, 1)*2*pi
  t <- log(1/(1 - runif(1, 0, 1)))
  x <- sqrt(2*t)*cos(u)
  x <- x + centerX
  return(x)
}

# draw.batted.ball draws a random number from the sum of bivariate, independent,
# normal distributions centered at an individual X-Y coordinate for a batted
# ball. It draws two random normally distributed numbers from the
# draw.rand.normal() function and returns an X-Y coordinate pair stored in a
# numeric vector. It relies on a player's batted ball dataset to produce a spray
# similar to the player's actual data, and so data is passed into the function
# as well.

draw.batted.ball <- function(data) {
  index <- sample(1:nrow(data), 1)
  x <- draw.rand.normal(data[index,1])
  y <- draw.rand.normal(data[index,2])
  return(as.numeric(c(x,y)))
}

# draw.spray() is a helper function meant to simulated batted ball data and
# convert it back into the tibble data structure as it is more convenient to
# work with. It takes in the batted ball data needed for the draw.batted.ball()
# function and returns a tibble with X-Y coordinates of the randomly generated
# data.

draw.spray <- function(batted_balls_data, count) {
  
  synthetic.data <- replicate(count, draw.batted.ball(batted_balls_data))
  synthetic.data <- as.tibble(t(as.matrix(synthetic.data)))
  names(synthetic.data) <- c("ballpos_x", "ballpos_y")
  
  return(synthetic.data)
}

# The draw.coordinates() function is used to randomly select a set of X-Y
# coordinates for a fielder given their grid of valid points. This returns
# a numeric vector of the chosen X-Y coordinate for the player.

draw.coordinates <- function(grid) {
  
  index <- sample(1:nrow(grid), 1)
  
  XCoord <- grid[[index, 1]]
  YCoord <- grid[[index, 2]]
  
  return(c(XCoord, YCoord))
  
}

# get.Wall.Spline() is a function used to produce a cubic spline approximation of
# a field's outfield wall, using coordinates from the R package GeomMLBStadiums.
#
# A cubic spline is a curve fitting tool for fitting curves when randomness is
# not in question (ie geometry shapes). It's most common application is in 
# autoCAD software tools for engineers. Students who are interested in learning
# more about these can do so by taking MATH1070 - Numerical Analysis.
#
# The function requires that we specify a team so that we know which stadium
# we'd like to produce an outfield wall for. Once this is done, a set of X-Y
# coordinates are taken from the MLBStadiumsPathData tibble to produce a spline
# function with.
#
# The function then returns a cubic spline as well as where the foul lines end
# for filtering purposes in the get.grid() function.

get.Wall.Spline <- function(Team) {
  
  teamField <- MLBStadiumsPathData %>%
    filter(team == Team) %>% 
    mlbam_xy_transformation(x = "x", y = "y")
  
  teamField %>% 
    filter(segment == 'foul_lines') %>% 
    select(y_) %>% max() -> foulLine
  
  teamField %>% 
    filter(segment == 'outfield_outer', y_ > foulLine) -> outfieldWall
  
  outfieldWallX <- outfieldWall %>% select(x_) %>% pull()
  outfieldWallY <- outfieldWall %>% select(y_) %>% pull()
  
  spliner <- splinefun(outfieldWallX, outfieldWallY)
  
  return(list(min(outfieldWallX), max(outfieldWallX), spliner))
  
}

# The get.grid() function produces a grid of coordinates unique for each field
# that are .5 feet away from each other, which become the points we may place
# players at. We start with a simple mesh box, and then use the get.Wall.Spline()
# function to perform a curve fit for the outfield and foul lines. This
# information is used firstly to filter out all coordinates not within the field
# dimensions, and then the points are partitioned further to separate the set of
# coordinates for the First Basemen, other infielders, and outfielders.

get.grid <- function(Team) {
  
  width <- seq(-300, 300, .5)
  depth <- seq(60, 400, .5)
  
  grid <- matrix(numeric(), nrow = length(width)*length(depth), ncol = 2)
  
  k <- 1
  
  for (i in width) {
    for (j in depth) {
      grid[[k, 1]] <- i
      grid[[k, 2]] <- j
      k <- k + 1
    }
  }
  
  grid <- as_tibble(grid)
  names(grid) <- c("X", "Y")
  
  output <- get.Wall.Spline(Team)
  
  wallMin <- output[[1]]
  wallMax <- output[[2]]
  wallSpline <- output[[3]]
  
  grid %>% filter(X < Y, -X < Y) -> grid # No one past foul lines
  grid.infield <- grid %>% filter(X**2 + Y**2 < 140**2) # No infielders in outfield
  grid.infield %>% filter(X + 120 < Y | -X + 120 < Y) -> grid.infield # No one in front of baseline
  grid.outfield <- grid %>% filter(X**2 + Y**2 > 175**2, # No outfielders encroaching on infield
                                   X > wallMin, 
                                   X < wallMax,
                                   Y < wallSpline(X)) # Outfielders stay within outfield wall
  grid.1b <- grid.infield %>% filter((X-63.64)**2 + (Y-63.64)**2 < 30**2) # First Baseman stays near 1st
  
  return(list(grid.1b, grid.infield, grid.outfield))
  
}

# The grid.puncher() function is meant to update a fielder's coordinate grid
# based on where another player has been placed. It eliminates all points within
# a certain radius of another player and eliminates all points within a certain
# angle to avoid players blocking another player's line-of-sight. To avoid
# precision errors, the angle formula between two vectors that one would learn
# in Calc III is used. It returns an updated grid with some coordinates filtered
# out.

grid.puncher <- function(grid, XCoord, YCoord, radius, angle) {
  
  grid %>% filter((XCoord - X)**2 + (YCoord - Y)**2 > radius**2, 
                  acos((XCoord*X + YCoord*Y)/(sqrt(XCoord**2 + YCoord**2)*sqrt(X**2 + Y**2)))*180/pi > angle) -> new.grid
  
  return(new.grid)
  
}

# apply.outfield.model() is a function used to simulate catches of the ball for
# outfielders. A discrete probability function provides a probability of a
# successful catch based on how close the closest fielder is and was created
# using in-game data. This function then simulates based on the existing
# probability model and returns simulations for whether or not the ball was
# caught.

apply.outfield.model <- function(synthetic) {
  
  generic.outfield.pdf <- read_csv('newOutfieldModel.csv', col_types = cols())
  
  synthetic %>%
    mutate(FirstBaseDistance = sqrt((X3 - ballpos_x)**2 + (Y3 - ballpos_y)**2),
           SecondBaseDistance = sqrt((X4 - ballpos_x)**2 + (Y4 - ballpos_y)**2),
           ThirdBaseDistance = sqrt((X5 - ballpos_x)**2 + (Y5 - ballpos_y)**2),
           shortstopDistance = sqrt((X6 - ballpos_x)**2 + (Y6 - ballpos_y)**2),
           leftFieldDistance = sqrt((X7 - ballpos_x)**2 + (Y7 - ballpos_y)**2),
           centerFieldDistance = sqrt((X8 - ballpos_x)**2 + (Y8 - ballpos_y)**2),
           rightFieldDistance = sqrt((X9 - ballpos_x)**2 + (Y9 - ballpos_y)**2),
           InfOf = ifelse(ballpos_x**2 + ballpos_y**2 < 175**2, "Infield", "Outfield"),
           outOfPark = ifelse(ballpos_x**2 + ballpos_y**2 < 400**2, FALSE, TRUE),
           foulOrBad = ifelse((ballpos_x < ballpos_y) && (-ballpos_x < ballpos_y), FALSE, TRUE),
           cannot.model = outOfPark || foulOrBad,
           responsibility = ifelse(InfOf == "Infield", pmin(FirstBaseDistance, SecondBaseDistance, ThirdBaseDistance, shortstopDistance),
                                   pmin(leftFieldDistance, centerFieldDistance, rightFieldDistance)),
           responsibility1B = ifelse(responsibility == FirstBaseDistance, "1B", ""),
           responsibility2B = ifelse(responsibility == SecondBaseDistance, "2B", ""),
           responsibility3B = ifelse(responsibility == ThirdBaseDistance, "3B", ""),
           responsibilitySS = ifelse(responsibility == shortstopDistance, "SS", ""),
           responsibilityLF = ifelse(responsibility == leftFieldDistance, "LF", ""),
           responsibilityCF = ifelse(responsibility == centerFieldDistance, "CF", ""),
           responsibilityRF = ifelse(responsibility == rightFieldDistance, "RF", ""),
           responsibility.text = paste0(responsibility1B, responsibility2B, responsibility3B, responsibilitySS,
                                        responsibilityLF, responsibilityCF, responsibilityRF),
           responsibility1BX = ifelse(responsibility == FirstBaseDistance, X3, NA),
           responsibility2BX = ifelse(responsibility == SecondBaseDistance, X4, NA),
           responsibility3BX = ifelse(responsibility == ThirdBaseDistance, X5, NA),
           responsibilitySSX = ifelse(responsibility == shortstopDistance, X6, NA),
           responsibilityLFX = ifelse(responsibility == leftFieldDistance, X7, NA),
           responsibilityCFX = ifelse(responsibility == centerFieldDistance, X8, NA),
           responsibilityRFX = ifelse(responsibility == rightFieldDistance, X9, NA),
           responsibility1BY = ifelse(responsibility == FirstBaseDistance, Y3, NA),
           responsibility2BY = ifelse(responsibility == SecondBaseDistance, Y4, NA),
           responsibility3BY = ifelse(responsibility == ThirdBaseDistance, Y5, NA),
           responsibilitySSY = ifelse(responsibility == shortstopDistance, Y6, NA),
           responsibilityLFY = ifelse(responsibility == leftFieldDistance, Y7, NA),
           responsibilityCFY = ifelse(responsibility == centerFieldDistance, Y8, NA),
           responsibilityRFY = ifelse(responsibility == rightFieldDistance, Y9, NA),
           responsibility.buckets = cut(responsibility, seq(0,175,5))) -> synthetic
  
  synthetic %>%
    select(responsibility1BX, responsibility2BX, responsibility3BX, responsibilitySSX,
           responsibilityLFX, responsibilityCFX, responsibilityRFX) %>% 
    rowSums(na.rm=TRUE) -> synthetic$responsibility.X
  
  synthetic %>%
    select(responsibility1BY, responsibility2BY, responsibility3BY, responsibilitySSY,
           responsibilityLFY, responsibilityCFY, responsibilityRFY) %>% 
    rowSums(na.rm=TRUE) -> synthetic$responsibility.Y
  
  synthetic %>% 
    mutate(frontOrBack = ifelse(ballpos_y < responsibility.Y, "Front", "Back")) -> synthetic
  
  synthetic %>% filter(cannot.model == FALSE, InfOf == "Outfield")
  
  synthetic %>% inner_join(generic.outfield.pdf, by = c("responsibility.buckets", 'frontOrBack')) -> synthetic
  
  synthetic$drawn.uniform <- runif(nrow(synthetic), 0, 1)
  
  synthetic %>% mutate(outs = ifelse(drawn.uniform > 1 - successRate, 1, 0)) -> synthetic
  
  return(synthetic)
  
}

# apply.infield.model() is a function used to simulate catches of the ball for
# outfielders. A discrete probability function provides a probability of a
# successful catch based on the angle the fielder is to the ball, and is modeled
# using in-game data. This function then simulates based on the existing
# probability model and returns a simulation of whether or not the batted balls
# were successfully fielded.

apply.infield.model <- function(synthetic) {
  
  generic.infield.pdf <- read_csv('infieldModel.csv', col_types = cols())
  
  synthetic %>%
    mutate(InfOf = ifelse(ballpos_x**2 + ballpos_y**2 < 175**2, "Infield", "Outfield"),
           responsibilityAngle1B = acos((ballpos_x*X3 + ballpos_y*Y3)/(sqrt(ballpos_x**2 + ballpos_y**2)*sqrt(X3**2 + Y3**2)))*180/pi,
           responsibilityAngle2B = acos((ballpos_x*X4 + ballpos_y*Y4)/(sqrt(ballpos_x**2 + ballpos_y**2)*sqrt(X4**2 + Y4**2)))*180/pi,
           responsibilityAngle3B = acos((ballpos_x*X5 + ballpos_y*Y5)/(sqrt(ballpos_x**2 + ballpos_y**2)*sqrt(X5**2 + Y5**2)))*180/pi,
           responsibilityAngleSS = acos((ballpos_x*X6 + ballpos_y*Y6)/(sqrt(ballpos_x**2 + ballpos_y**2)*sqrt(X6**2 + Y6**2)))*180/pi,
           responsibilityAngleLF = acos((ballpos_x*X7 + ballpos_y*Y7)/(sqrt(ballpos_x**2 + ballpos_y**2)*sqrt(X7**2 + Y7**2)))*180/pi,
           responsibilityAngleCF = acos((ballpos_x*X8 + ballpos_y*Y8)/(sqrt(ballpos_x**2 + ballpos_y**2)*sqrt(X8**2 + Y8**2)))*180/pi,
           responsibilityAngleRF = acos((ballpos_x*X9 + ballpos_y*Y9)/(sqrt(ballpos_x**2 + ballpos_y**2)*sqrt(X9**2 + Y9**2)))*180/pi,
           foulOrBad = ifelse((ballpos_x < ballpos_y) && (-ballpos_x < ballpos_y), FALSE, TRUE),
           responsibility = ifelse(InfOf == "Infield", pmin(responsibilityAngle1B, responsibilityAngle2B, responsibilityAngle3B, 
                                                            responsibilityAngleSS),
                                   pmin(responsibilityAngleLF, responsibilityAngleCF, responsibilityAngleRF)),
           responsibility1B = ifelse(responsibility == responsibilityAngle1B, "1B", ""),
           responsibility2B = ifelse(responsibility == responsibilityAngle2B, "2B", ""),
           responsibility3B = ifelse(responsibility == responsibilityAngle3B, "3B", ""),
           responsibilitySS = ifelse(responsibility == responsibilityAngleSS, "SS", ""),
           responsibilityLF = ifelse(responsibility == responsibilityAngleLF, "LF", ""),
           responsibilityCF = ifelse(responsibility == responsibilityAngleCF, "CF", ""),
           responsibilityRF = ifelse(responsibility == responsibilityAngleRF, "RF", ""),
           responsibility.text = paste0(responsibility1B, responsibility2B, responsibility3B, responsibilitySS,
                                        responsibilityLF, responsibilityCF, responsibilityRF),
           responsibility.buckets = cut(responsibility, seq(0,15,1))) -> synthetic
  
  synthetic %>% inner_join(generic.infield.pdf, by = c("responsibility.buckets")) -> synthetic
  
  synthetic$drawn.uniform <- runif(nrow(synthetic), 0, 1)
  
  synthetic %>% mutate(outs = ifelse(drawn.uniform > 1 - successRate, 1, 0)) -> synthetic
  
  return(synthetic)
  
}

# The simulate.catches() function returns an approximation for 
# P(Out | Fielder Coordinates, Batted Balls). It firstly used the draw.spray()
# function to simulate batted balls for an individual batter, and then appends
# the chosen player X-Y coordinates to a tibble for simulating according to our
# probability models.

simulate.catches <- function(batted.balls, X3, Y3, X4, Y4, X5, Y5, X6, Y6, X7, Y7, X8, Y8, X9, Y9) {
  
  synthetic <- draw.spray(batted.balls, 2000) %>% na.omit()
  synthetic$X3 <- replicate(nrow(synthetic), X3)
  synthetic$Y3 <- replicate(nrow(synthetic), Y3)
  synthetic$X4 <- replicate(nrow(synthetic), X4)
  synthetic$Y4 <- replicate(nrow(synthetic), Y4)
  synthetic$X5 <- replicate(nrow(synthetic), X5)
  synthetic$Y5 <- replicate(nrow(synthetic), Y5)
  synthetic$X6 <- replicate(nrow(synthetic), X6)
  synthetic$Y6 <- replicate(nrow(synthetic), Y6)
  synthetic$X7 <- replicate(nrow(synthetic), X7)
  synthetic$Y7 <- replicate(nrow(synthetic), Y7)
  synthetic$X8 <- replicate(nrow(synthetic), X8)
  synthetic$Y8 <- replicate(nrow(synthetic), Y8)
  synthetic$X9 <- replicate(nrow(synthetic), X9)
  synthetic$Y9 <- replicate(nrow(synthetic), Y9)
  
  synthetic <- bind_rows(apply.outfield.model(synthetic), apply.infield.model(synthetic))
  
  return(sum(synthetic$outs)/nrow(synthetic))
  
}

# The simulate.positions() function is the final function and is what is used in
# tandem with the replicate() function to simulate various P(Out)'s given a set
# of player positions. It requires that the batted balls be given for simulating,
# the grids to be chosen for drawing positions over, and radii/angle constraints
# be specified. It returns a large numeric vector with the positions of each
# player and the approximated P(Out).

simulate.positions <- function(batted.balls, grid.1b, grid.infield, grid.outfield, 
                               infielder_radii, outfielder_radii, infOf_radii, angle_inhibited) {
  
  P3 <- draw.coordinates(grid.1b)
  working.grid.infield <- grid.puncher(grid.infield, P3[1], P3[2], infielder_radii, angle_inhibited)
  working.grid.outfield <- grid.puncher(grid.outfield, P3[1], P3[2], infOf_radii, angle_inhibited)
  
  P4 <- draw.coordinates(working.grid.infield)
  working.grid.infield <- grid.puncher(working.grid.infield, P4[1], P4[2], infielder_radii, angle_inhibited)
  working.grid.outfield <- grid.puncher(grid.outfield, P4[1], P4[2], infOf_radii, angle_inhibited)
  
  P5 <- draw.coordinates(working.grid.infield)
  working.grid.infield <- grid.puncher(working.grid.infield, P5[1], P5[2], infielder_radii, angle_inhibited)
  working.grid.outfield <- grid.puncher(grid.outfield, P5[1], P5[2], infOf_radii, angle_inhibited)
  
  P6 <- draw.coordinates(working.grid.infield)
  working.grid.infield <- grid.puncher(working.grid.infield, P6[1], P6[2], infielder_radii, angle_inhibited)
  working.grid.outfield <- grid.puncher(grid.outfield, P6[1], P6[2], infOf_radii, angle_inhibited)
  
  P7 <- draw.coordinates(working.grid.outfield)
  working.grid.outfield <- grid.puncher(grid.outfield, P7[1], P7[2], outfielder_radii, angle_inhibited)
  
  P8 <- draw.coordinates(working.grid.outfield)
  working.grid.outfield <- grid.puncher(grid.outfield, P8[1], P8[2], outfielder_radii, angle_inhibited)
  
  P9 <- draw.coordinates(working.grid.outfield)
  
  outs <- simulate.catches(batted.balls, P3[1], P3[2], P4[1], P4[2], P5[1], P5[2],
                           P6[1], P6[2], P7[1], P7[2], P8[1], P8[2], P9[1], P9[2])
  
  return(c(outs, P3[1], P3[2], P4[1], P4[2], P5[1], P5[2], P6[1], P6[2], P7[1], P7[2], P8[1], P8[2], P9[1], P9[2]))
  
}