library(data.table)
library(ggplot2)
library(dplyr)
setwd("C:/Users/ACER/Desktop/FDS-Assignment")
batting <- read.csv('Batting.csv')
sal<-read.csv("Salaries.csv")


batting$BA <- batting$H / batting$AB
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B <- (batting$H-batting$X2B-batting$X3B-batting$HR)
batting$SLG <- (((batting$X1B)+(2 * batting$X2B)+(3 * batting$X3B)+(4 * batting$HR))/batting$AB)
batting_1985_onwards <- subset(batting,yearID >= 1985)
#Combining the Salaries  with Batting using common labels playerID and yearID.

combo <- merge(batting_1985_onwards,sal,by=c('playerID','yearID'))


# Extract out the details of the lost players, Giambi, Damon and Isringhausen from the 'combo' ie combination dataset
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))
# Since they left in 2001, we can further narrow them down to 2001.
lost_players <- subset(lost_players,yearID==2001)
# We extract the data required for the analysis. I.e. PlayerID, H, X2B, X3B, HR, OBP, SLG, BA, AB
lst<-select(lost_players,playerID,OBP,SLG)
mean(lst$OBP)
mean(lst$SLG)
# So the challenge is to find 3 players to replace the lost players based on 2001 performance.Constraints are:
# Mean OBP >=0.364
# Mean SLG >=0.469
# No of games played is atleast 80(162 game season) for statistical validity 
combo <- subset(combo,yearID == 2001)
possible_options <- filter(combo,SLG >=.469,OBP >=0.364,G>80)

possible<-  select(possible_options,playerID,OBP,SLG,salary,teamID.x)#subsetting relevant info for easier display

View(possible)
#writing to csv

write.csv(possible_options,"results_full.csv")
write.csv(possible,"results.csv")
