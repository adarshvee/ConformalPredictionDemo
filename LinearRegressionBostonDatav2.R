####
#Conformal prediction for linear regression in classic data set Boston Data
# Author : Adarsh Vijayaraghavan
####
#Start by clearing all variables in the evironment
rm(list=ls())

#Import conformalInference package. Install instructions at : https://github.com/ryantibs/conformal
library(conformalInference)

#linear Regression - data setup
library(MASS)
#Step 1 - import Boston data to a dataframe variable
bostonData <- Boston
summary(bostonData)
dim(bostonData)

#Partition data set into training and test data
set.seed(4)
bostonTrain<-bostonData[samp<-sample(1:nrow(bostonData),floor(0.78*nrow(bostonData))),]
dim(bostonTrain)
bostonTest<-bostonData[-samp,]
dim(bostonTest)

#Simple linear regression for training data set
lm.fit= lm(medv~lstat, data = bostonTrain)
#summarize regression data
summary(lm.fit)

#Step 5
plot(bostonData$lstat, bostonData$medv)
abline(lm.fit, lwd=3, col="red")

#Predict confidence interval for test data set
abcde <- data.frame(lstat = bostonTest$lstat)
#lm.pred = predict(lm.fit, list(bostonTest$lstat), interval="confidence")
lm.pred = predict(lm.fit, newdata= abcde, interval="prediction")

#Start conformal preidction
funs = lm.funs()
out.conf = conformal.pred(data.matrix(bostonTrain["lstat"]), unlist(bostonTrain["medv"], use.names=FALSE), 
                          data.matrix(bostonTest["lstat"]), alpha=0.05, 
                          train.fun = funs$train, predict.fun = funs$predict, verb=TRUE)

#Combine results from the two regressions into a matrix
#tab = matrix(c(lm.pred[-samp,"lwr"], lm.pred[-samp,"upr"], out.conf$lo, out.conf$up, bostonTest[,'medv']), ncol=5)
tab = matrix(c(lm.pred[,"lwr"], lm.pred[,"upr"], out.conf$lo, out.conf$up, bostonTest[,'medv']), ncol=5)
colnames(tab) = c("Clsc_L","Clsc_H", "Con_L", "Con_H", "Obs")

#Convert to dataframe for convenience and add Truth columns
data.final <- data.frame(tab)
data.final$Classic_truth = data.final$Clsc_L <= data.final$Obs &  data.final$Obs <= data.final$Clsc_H
#data.final$Classic_truth = data.final$Clsc_L >= data.final$Obs & data.final$Clsc_H <= data.final$Obs
data.final$Classic_truth <- as.factor(data.final$Classic_truth)
data.final$Con_truth = data.final$Con_L <= data.final$Obs & data.final$Con_H >= data.final$Obs
#data.final$Con_truth = data.final$Con_L >= data.final$Obs & data.final$Con_H <= data.final$Obs
data.final$Con_truth <- as.factor(data.final$Con_truth)

data.final$Classic_width = abs(data.final$Clsc_L - data.final$Clsc_H)
data.final$Con_width = abs(data.final$Con_L - data.final$Con_H) 

#Summarize results
cat("Vanilla linear regression error prediction\n")
summary(data.final$Classic_truth)

cat("Conformal prediction using linear regression error prediction\n")
summary(data.final$Con_truth)
cat("Average width of linear regression prediction interval")
mean(data.final$Classic_width)
cat("Average width of CONFORMAL linear regression prediction interval")
mean(data.final$Con_width)
data.final


