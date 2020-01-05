#read data set
data.train<-read.csv("D:\\quera\\dataq\\data.csv")
str(data.train)
# Remove NA's in dataset
data.train<-data.train[,-13]
data.train<- na.omit(data.train[,-19])
names(data.train)
attach(data.train)
#================================================================================
# A matrix of scatterplots is produced
pairs(data.train, upper.panel = NULL)
plot(Market.Share_total)

#================================================================================
# train and test data are devided
index<- sample(1: length(data.train[,1])/3, size = length(data.train[,1])/5)
data.train<- data.train[-index,]
data.test<- data.train[index,]

#================================================================================
# based on train data a linear regressian model is built
linearMod <- lm(Market.Share_total ~ Station+Channel.Type+Season+Year+
                  Day.of.week+Length+Genre+First.time.or.rerun+
                  X..of.episode.in.the.season+Movie. , data=data.train)  
summary(linearMod)

#================================================================================
# Model prediction
prediction<-  predict(linearMod , data.train , interval = "prediction")

# the predicted value of market which were below zero are removed
predMark<- cbind(Market.Share_total,prediction[,1])
for(i in 1:length(predMark[,2])){
  if(predMark[i,2] < 0) predMark[i,2] <- mean(predMark[,2])
}
# predict value of Market with regession linear modeling is ploted
plot(predMark[,2])
# predict value of Market VS Market
plot(predMark[,1]~predMark[,2])
abline(0,1, col ="red" )
#================================================================================
#================================================================================
# Model prediction
prediction<-  predict(linearMod , data.test , interval = "prediction")

# the predicted value of market which were below zero are removed
pred.test<- cbind(data.test$Market.Share_total,prediction[,1])
for(i in 1:length(pred.test[,2])){
  if(pred.test[i,2] < 0) pred.test[i,2] <- mean(pred.test[,2])
}
# predict value of Market with regession linear modeling is ploted
plot(pred.test[,2])
# predict value of Market VS Market
plot(pred.test[,2], pred.test[,1], xlab = "Market.Share_total", ylab= "prediction", main="Market.Share_total ~ prediction")
abline(0,1, col ="red")

#================================================================================
#read data set
data.test.total<-read.csv("D:\\quera\\dataq\\data.csv")
data.test.total<- data.test.total[,-13]
str(data.test.total)
# Remove NA's in test data
data.test.total<- na.omit(data.test.total)

# Model prediction
predtest<- predict(linearMod, data.test.total , interval = "prediction")
for(i in 1:length(predtest[,2])){
  if(predtest[i,1] < 0) predtest[i,1] <- mean(predtest[,1])
}

# predict value of Market with regession linear modeling is ploted
plot(predtest[,1])
summary(predtest)
#================================================================================
