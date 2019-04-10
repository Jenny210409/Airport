# **Airport**

## Table of contents

- [Introduction](#introduction)
- [Preparation](#preparation)
- [Prediction](#prediction)
- [Conclusion](#conclusion)

## Introduction
I will be making a model to estimate the number of passengers arriving to Chiristchurch airport in the future.

## Preparation
#### Initial works
```
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201') 

library(rJava) # load and attach 'rJava' no
library(tabulizer)

f2 <- "https://www.queenstownairport.co.nz/assets/documents/ZQN-monthly-passengers-2016-to-2018-December.pdf"
airport <- extract_tables(f2, pages = 1)

f3 <- "https://www.queenstownairport.co.nz/assets/documents/ZQN-monthly-passengers-2011-to-2015.pdf"
airport1 <- extract_tables(f3, pages = 1)
```
#### Data Cleaning
```
# change to data frame from list (only arrivals)

airport.1 <- airport[[1]][,c(3,6)]

airport.1 <- as.data.frame(airport.1)

airport1

airport1.1 <- airport1[[1]][,c(4,7)]

airport1.1 <- as.data.frame(airport1.1)

#cleaning data

airport.1<-airport.1[-c(1,2,3),]
row.names(airport.1)<-1:nrow(airport.1)
airport.1<-airport.1[-c(37:nrow(airport.1)),]



library(stringr)
airport.1$V2<-as.character(airport.1$V2)
airport.1$V2 <-  str_replace(airport.1$V2, ",", "")

airport.1[1,2]<-76950
airport.1$V2<-as.numeric(airport.1$V2)
str(airport.1)

airport.1$V1<-as.character(airport.1$V1)
date<- strsplit(airport.1$V1,' ')
airport.1$Year<-NA
airport.1$month<-NA
for (i in 1:nrow(airport.1)) {
  
  
  airport.1$Year[i]<-date[[i]][2]
  airport.1$month[i]<-date[[i]][1]
}
airport.1$V1<-NULL
names(airport.1)[1] <- "Passenger"
airport.1$Year<-as.numeric(airport.1$Year)
str(airport.1)

airport1.1<-airport1.1[-c(1,2),]
row.names(airport1.1)<-1:nrow(airport1.1)
airport1.1<-airport1.1[-c(61:nrow(airport1.1)),]

airport1.1$V1<-as.character(airport1.1$V1)
date<- strsplit(airport1.1$V1,' ')
airport1.1$Year<-NA
airport1.1$month<-NA
for (i in 1:nrow(airport1.1)) {
  
  
  airport1.1$Year[i]<-date[[i]][2]
  airport1.1$month[i]<-date[[i]][1]
}
airport1.1$V1<-NULL
names(airport1.1)[1] <- "Passenger"
airport1.1$Year<-as.numeric(airport1.1$Year)
airport1.1$Passenger<-as.character(airport1.1$Passenger)
airport1.1$Passenger <-str_replace(airport1.1$Passenger, ",", "")
airport1.1$Passenger<-as.numeric(airport1.1$Passenger)
str(airport1.1)

airport_final<-rbind(airport1.1,airport.1)

airport_final1<-airport_final

airport_final1$month <- as.factor(airport_final1$month)
```
## Prediction
#### Split into train and test and make a model 
```
train<-airport_final1[1:round(0.7*nrow(airport_final1)),]
test<-airport_final1[(round(0.7*nrow(airport_final1))+1):nrow(airport_final1),]

#using ranger : 22736.25
library(ranger)
model<-ranger(Passenger~.,train)
test$prediction<-predictions(predict(model,test))
error<-test$Passenger-test$prediction
sqrt(mean(error^2))

#using randomforest
library(randomForest)
model<-randomForest(Passenger~.,train)
test$prediction<-predict(model,test)

error<-test$Passenger-test$prediction
sqrt(mean(error^2))

#using rpart : 24819.12
library(rpart)
model<-rpart(Passenger~.,train)
test$prediction<-(predict(model,test))
error<-test$Passenger-test$prediction
sqrt(mean(error^2))

#using lm : 11398.93
model<-lm(Passenger~.,train)
test$prediction<-(predict(model,test))
error<-test$Passenger-test$prediction
sqrt(mean(error^2))
```
#### Plot and make graph
```
#plot the trend

partition<-round(0.7*nrow(airport_final))
first<-airport_final[1:partition,]
first$Time<- paste(first$Year, first$month, sep="-")
first$var<-"actual"
first<-first[,c(1,4,5)]

second<-airport_final[(partition+1):nrow(airport_final),]
second$Time<- paste(second$Year, second$month, sep="-")
second$var<-"Future"

second<-second[,c(1,4,5)]

third<-test[1:nrow(test),]
third$Time<- paste(third$Year, third$month, sep="-")
third$var<-"lm_prediction"
third<-third[,c(4,5,6)]
names(third)[1] <- "Passenger"

library(randomForest)
model<-randomForest(Passenger~.,train)
test$prediction1<-predict(model,test)
fourth<-test[1:nrow(test),]
fourth$Time<- paste(fourth$Year, fourth$month, sep="-")
fourth$var<-"rf_prediction"
fourth<-fourth[,c(5,6,7)]
names(fourth)[1] <- "Passenger"

airport_final<-rbind(first,second,third,fourth)

airport_final$Time <-  ymd(paste( airport_final$Time ,"-15", sep =""))

library(ggplot2)

ggplot(data=airport_final,
       aes(y=Passenger, x=Time, colour=var)) +
  geom_line()
```
## Conclusion
The prediction shows that the passengers in the future would have a slight increase trend into the future.
       

