
library(ggplot2)
setwd("C:/Users/ACER/Desktop/FDS-Assignment")
moneyball <- read.csv("baseball.csv")



dataBefore2002 <- subset(moneyball, Year < 2002)

#plotting info wrt the team wins in each year
m<- ggplot(dataBefore2002, aes(x = W,
                              y = Team,
                              color = factor(Playoffs))) +
  geom_point() + scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs")
#Asuming a safe limit on the number of wins required to qualify for the playoffs
n<-geom_vline(aes(xintercept = 95), color = "black", linetype = "dashed", size=1)

m+n+xlab("Wins")


dataBefore2002$RD <- dataBefore2002$RS - dataBefore2002$RA



ggplot(dataBefore2002, aes(x = RD,
                           y = W,
                           color = factor(Playoffs))) +
  geom_point() + scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs")

#linear regressinon function in R implements feature extraction by itself
#so we don't need to explicitly do it
winsReg <- lm(W ~ RD, data = dataBefore2002)
summary(winsReg)

ggplot(dataBefore2002, aes(x = RD,
                           y = W,
                           color = factor(Playoffs))) +
  geom_point() + scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs") +
  geom_vline(aes(xintercept = 133), color = "black", linetype = "dashed", size=1)

runsScoredReg <- lm(RS ~ OBP + SLG + BA, data = dataBefore2002)
summary(runsScoredReg)

runsScoredRegNoBA <- lm(RS ~ OBP + SLG, data = dataBefore2002)
summary(runsScoredRegNoBA)

runsAllowedReg <- lm(RA ~ OOBP + OSLG, data = dataBefore2002)
summary(runsAllowedReg)