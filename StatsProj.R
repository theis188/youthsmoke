mydata <- read.csv('c:/StatsHW/200PROJ.csv')

mydata$steadyfd[mydata$steadyfd == -8] <- 2
mydata$famargue[mydata$famargue == -8] <- 2
mydata$parntsmk[mydata$parntsmk == -8] <- 2
mydata$nofuture[mydata$nofuture == -8] <- 3
mydata$cigmonth[mydata$cigmonth == 2] <- 0

mydata$rdaganst[mydata$rdaganst == 3] <- 1
mydata$rdaganst[mydata$rdaganst == 5] <- 1
mydata$rdaganst[mydata$rdaganst == 4] <- 2

mydata$tvaganst[mydata$tvaganst == 5] <- 3
mydata$tvaganst[mydata$tvaganst == 4] <- 3

mydata$bdaganst[mydata$bdaganst == 3] <- 2

mydata$ethnic[mydata$ethnic == 3] <- 1
mydata$ethnic[mydata$ethnic == 4] <- 2
mydata$ethnic[mydata$ethnic == 5] <- 1

mydata$religsvc[mydata$religsvc == 2] <- 1
mydata$religsvc[mydata$religsvc == 3] <- 1

mydata$ethnic <- factor(mydata$ethnic)
mydata$tvaganst <- factor(mydata$tvaganst)
mydata$rdaganst <- factor(mydata$rdaganst)
mydata$bdaganst <- factor(mydata$bdaganst)
mydata$nofuture <- factor(mydata$nofuture)
mydata$ethnic <- factor(mydata$ethnic)

aggregate(cigmonth~age, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~ethnic, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~sex, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~parntsmk, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~tvaganst, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~rdaganst, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~bdaganst, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~famargue, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~unhappy, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~nofuture, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~steadyfd, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~tmsport, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~talkhelp, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))
aggregate(cigmonth~religsvc, data=mydata, FUN=function(x) c(mean=mean(x), count=length(x)))

logit <- glm(cigmonth ~ age + ethnic + parntsmk + tmsport + rdaganst + bdaganst, data = mydata, family = "binomial")
summary(logit)

mydata$predictP <- predict(logit, newdata = mydata, type = "response")
mydata

hoslem.test(mydata$cigmonth, mydata$predictP, g = 10)