library(xgboost)
library(caret)

setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single Model")

#Reading in training and testing data.
train = as.data.frame(read.csv("train.csv"))
test = as.data.frame(read.csv("test.csv"))