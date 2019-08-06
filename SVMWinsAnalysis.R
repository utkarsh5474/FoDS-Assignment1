moneyball <- read.csv("baseball.csv")
dataBefore2002 <- subset(moneyball, Year < 2002)
#View(dataBefore2002)
svmspltr<-dataBefore2002[,c(1,6,10)]

View(svmspltr)
#View(dataBefore2002)
svmspltr$Year<-dataBefore2002$Year
#View(svmspltr)

bak<-svmspltr
bak$Year<-as.factor(svmspltr$Year)
View(svmspltr)
View(bak)
plot(bak$W,bak$Playoffs)
wvsplayoff<-bak[,c(2,3)]

wvsplayoff$Playoffs<-factor(wvsplayoff$Playoffs,levels= c(0,1))
View(wvsplayoff)
library(caTools)
set.seed(123)
split = sample.split(wvsplayoff$Playoffs, SplitRatio = 0.75)
training_set = subset(wvsplayoff, split == TRUE)
test_set = subset(wvsplayoff, split == FALSE)
training_set$W<-scale(training_set$W)
#View(training_set)

#feature scaling (Z index)
test_set$W<-scale(test_set$W)

library(e1071)
classifier = svm(formula = Playoffs ~ .,data = training_set,type = 'C-classification',kernel = 'linear')

y_pred = predict(classifier, newdata = test_set)
View(y_pred)

plot(test_set$W,y_pred)
sum(test_set$Playoffs==y_pred)
sum(test_set$Playoffs!=y_pred)
207/(207+18)

#92 percent accurate model

a<-y_pred
a<-factor(a,levels = c(0,1))
View(a)
a<-cbind(a,test_set$W)
View(a)
#min value at which playoff goues from 0 to 1
#1.1323
wins<-1.1325
split = sample.split(wvsplayoff$Playoffs, SplitRatio = 0.75)
training_set = subset(wvsplayoff, split == TRUE)
test_set = subset(wvsplayoff, split == FALSE)
mean(test_set$W)
m<-mean(test_set$W)
s<-sd(test_set$W)

wins*s+m
#approximately 94
