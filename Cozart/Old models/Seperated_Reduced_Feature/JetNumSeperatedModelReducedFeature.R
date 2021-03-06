setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 4")

dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)

# Convert PRI_jet_num to factor as instructed on the website.
dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)
str(dfTrain)

#add xgboost predictions as feature
featureTrain <- read.csv('train_stacked_avg.csv', header=T)
featureTest <- read.csv('test_stacked_avg.csv', header=T)

dfTrain = cbind(dfTrain,Feature = featureTrain[,2])
dfTest = cbind(dfTest,Feature = featureTest[,2])

labels <- dfTrain$Label

train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

library(randomForest)
library(dplyr)
library(caret)
library(pROC)

#seperate the data into 3 groups

jetnum0  = filter(dfTrain,PRI_jet_num == '0')

jetnum1  = filter(dfTrain,PRI_jet_num == '1')

jetnum23  = filter(dfTrain,PRI_jet_num == '2'|PRI_jet_num == '3')

#save the labels for those groups 

labels0 <- jetnum0$Label

labels1 <- jetnum1$Label

labels23 <- jetnum23$Label

#create training sets to determin threshold

train0 <- jetnum0[, -c(1,32,33)]

train1 <- jetnum1[, -c(1,32,33)]

train23 <- jetnum23[, -c(1,32,33)]

#split the training sets into same groups

testnum0  = filter(dfTest,PRI_jet_num == '0')

testnum1  = filter(dfTest,PRI_jet_num == '1')

testnum23  = filter(dfTest,PRI_jet_num == '2'|PRI_jet_num == '3')

#save the event ids for later use

testId0 = testnum0$EventId

testId1 = testnum1$EventId

testId23 = testnum23$EventId

#create training sets without eventID and weight

jetnumtrain0 = jetnum0[,-c(1,32)]

jetnumtrain1 = jetnum1[,-c(1,32)]

jetnumtrain23 = jetnum23[,-c(1,32)]

#Remove constant features if there are any.  (Might arise due to number of jets being split into 
#different groups along with the NA's

#jet num 0
cat("\n## Removing the constants features.\n")
for (f in names(jetnumtrain0)) {
  if (length(unique(jetnumtrain0[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    jetnumtrain0[[f]] <- NULL
    testnum0[[f]] <- NULL
    train0[[f]] <- NULL
  }
}

#jet num 1
cat("\n## Removing the constants features.\n")
for (f in names(jetnumtrain1)) {
  if (length(unique(jetnumtrain1[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    jetnumtrain1[[f]] <- NULL
    testnum1[[f]] <- NULL
    train1[[f]] <- NULL
  }
}

#jet num 2-3
cat("\n## Removing the constants features.\n")
for (f in names(jetnumtrain23)) {
  if (length(unique(jetnumtrain23[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    jetnumtrain23[[f]] <- NULL
    testnum23[[f]] <- NULL
    train23[[f]] <- NULL
  }
}


#initial random forest jet number 0

set.seed(0)
rf1 = randomForest(Label ~ ., data = jetnumtrain0, importance = TRUE)
rf1

#OOB error 14.87

varImpPlot(rf1)

#find the best mtry for model
#set.seed(0)
#oob.err = numeric(10)
#for (mtry in 1:10) {
#  fit = randomForest(Label ~ ., data = jetnumtrain0, mtry = mtry)
#  oob.err[mtry] = fit$err.rate[500]
#  cat("We're performing iteration", mtry, "\n")
# }

#mtry 3

#Visualizing the OOB error.
#plot(1:10, oob.err, pch = 16, type = "b",
#     xlab = "Variables Considered at Each Split",
#     ylab = "OOB Mean Squared Error",
#     main = "Random Forest OOB Error Rates\nby # of Variables")


#finding threshold based on accuracy
rf1TrainPred <- predict(rf1, newdata=train0, type="prob")

labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels0, rf1TrainPred[,2])
plot(auc, print.thres=TRUE)


#use this threshold to make predictions
threshold0 <- 0.493

rf1TestPred <- predict(rf1, newdata=testnum0[,-1], type="prob")

predicted0 <- rep("b",220156)
predicted0[rf1TestPred[,2]>=threshold0] <- "s"
weightRank0 = rf1TestPred[,2]

submission0 = data.frame(EventId = testId0, RankOrder = weightRank0, Class = predicted0)


#initial random forest jet number 1

set.seed(0)
rf2 = randomForest(Label ~ ., data = jetnumtrain1, importance = TRUE)
rf2


#OOB error 18.41%

varImpPlot(rf2)

#find the best mtry for model
#set.seed(0)
#oob.err2 = numeric(10)
#for (mtry in 1:20) {
#  fit = randomForest(Label ~ ., data = jetnumtrain1, mtry = mtry)
#  oob.err2[mtry] = fit$err.rate[500]
#  cat("We're performing iteration", mtry, "\n")
# }

#mtry 12

#Visualizing the OOB error.
#plot(1:20, oob.err2, pch = 16, type = "b",
#     xlab = "Variables Considered at Each Split",
#     ylab = "OOB Mean Squared Error",
#     main = "Random Forest OOB Error Rates\nby # of Variables")

#finding threshold based on accuracy
rf2TrainPred <- predict(rf2, newdata=train1, type="prob")

labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels1, rf2TrainPred[,2])
plot(auc, print.thres=TRUE)


#use this threshold to make predictions
threshold1 <- 0.481

rf2TestPred <- predict(rf2, newdata=testnum1[,-1], type="prob")

predicted1 <- rep("b",169716)
predicted1[rf2TestPred[,2]>=threshold1] <- "s"
weightRank1 = rf2TestPred[,2]

submission1 = data.frame(EventId = testId1, RankOrder = weightRank1, Class = predicted1)


#initial random forest jet number 2-3

set.seed(0)
rf3 = randomForest(Label ~ ., data = jetnumtrain23, importance = TRUE)
rf3

#OOB error 14.99%

varImpPlot(rf3)


#find the best mtry for model
#set.seed(0)
#oob.err3 = numeric(30)
#for (mtry in 1:30) {
#  fit = randomForest(Label ~ ., data = jetnumtrain23, mtry = mtry)
#  oob.err3[mtry] = fit$err.rate[500]
#  cat("We're performing iteration", mtry, "\n")
#}

#mtry 7

#Visualizing the OOB error.
#plot(1:30, oob.err3, pch = 16, type = "b",
#     xlab = "Variables Considered at Each Split",
#     ylab = "OOB Mean Squared Error",
#     main = "Random Forest OOB Error Rates\nby # of Variables")

#finding threshold based on accuracy
rf3TrainPred <- predict(rf3, newdata=train23, type="prob")

labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels23, rf3TrainPred[,2])
plot(auc, print.thres=TRUE)


#use this threshold to make predictions
threshold23 <- 0.506

rf3TestPred <- predict(rf3, newdata=testnum23[,-1], type="prob")

predicted23 <- rep("b",160128)
predicted23[rf3TestPred[,2]>=threshold23] <- "s"
weightRank23 = rf3TestPred[,2]

submission23 = data.frame(EventId = testId23, RankOrder = weightRank23, Class = predicted23)


#combine the 3 models and rank/reorder

submission = rbind(submission0,submission1,submission23)

submission$RankOrder = rank( submission$RankOrder,ties.method= "random")

write.csv(submission, "rfjetnum_submissionReducedfeature2.csv", row.names=FALSE)

#reorder by rank
submission = submission[order(submission[,2]),]


#make the top 15% of rankorder s.

submission = mutate(submission, Class = ifelse(RankOrder>467500,"s","b"))

write.csv(submission, "rfjetnum_submissionReducedfeature.csv", row.names=FALSE)



