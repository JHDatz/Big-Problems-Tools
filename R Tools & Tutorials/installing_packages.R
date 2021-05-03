# Some important notes:
#
# Windows Users: 
#
# 1.You may need to install RTools as well.
#
# 2. You may get stuck in a loop where RStudio asks if you
# want to restart R repeatedly. Try completely closing RStudio
# if you have this problem, or answering no in the prompt.
#
# 3. During the install_github() commands I found that updating rlang
# made a mess, so do not choose to update it when prompted.
#
# Linux users: Running this will make your computer very upset. Run it
# while you make breakfast or something.
#
# The following are all required packages and dependencies of packages
# for Analyzing Baseball Data with R.

install.packages('tidyverse')
install.packages('doParallel')
install.packages('DBI')
install.packages('RMySQL')
install.packages('latticeExtra')
install.packages('cli')
install.packages('gh')
install.packages('usethis')
install.packages('devtools')
install.packages('xml2')
install.packages('pitchRx')
install.packages('mlbgameday')
install.packages('Lahman')
install.packages("RSQLite")
devtools::install_github("BillPetti/baseballr")
devtools::install_github("keberwein/mlbgameday")
devtools::install_github("beanumber/retro") # Added 3/23/21

# The next set is packages I found necessary for other projects. For the Aging
# Curves project we've had in the past, here is the hyperlink to the article
# we found and the packages in the code that was required to run their R tools:
#
# https://www.baseballprospectus.com/news/article/59972/the-delta-method-revisited/

install.packages("pacman")
pacman::p_install_gh("datarootsio/artyfarty")

pacman::p_load(tidyverse,artyfarty,ggthemes,
               mgcv,radiant.data,doFuture,future.apply)

# For the fielding optimization project, here is the library used to produce the
# final spray chart as well as other packages that were found helpful:

devtools::install_github("bdilday/GeomMLBStadiums")
install.packages("plot3D")
install.packages("rgl")
install.packages("xlsx")
