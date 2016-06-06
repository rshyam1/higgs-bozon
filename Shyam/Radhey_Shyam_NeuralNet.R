#Hayes random Forest
setwd("~/classnotes/kaggle_jumpstart/data/")
dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)

# Convert PRI_jet_num to factor as instructed on the website.
dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)
str(dfTrain)

labels <- dfTrain$Label

train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

library(randomForest)
library("neuralnet")
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


set.seed(0)
normalize = function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}



#Imputing -999 values to 0 
jetnumtrain0[jetnumtrain0==-999.0] <- 0
jetnumtrain1[jetnumtrain1==-999.0] <- 0
jetnumtrain23[jetnumtrain23==-999.0] <- 0

testnum0[testnum0==-999]=0
testnum1[testnum1==-999]=0
testnum23[testnum23==-999]=0

#Scaling all values to 0 to 1

jetnumtrain0_scaled=as.data.frame(lapply(jetnumtrain0[,-19],normalize))
jetnumtrain0_scaled=cbind(jetnumtrain0_scaled,jetnumtrain0$Label)

#Changing the columns names back to Label
colnames(jetnumtrain0_scaled)[19]="Label"

summary(jetnumtrain0_scaled)

#Making the frmula for Neural Network!!!
formula=as.formula(paste("Label ~",paste(names(jetnumtrain0_scaled[,-19]),collapse = " + ")))
#Removing columns having phi 
formula2=as.formula(paste("Label ~",paste(names(jetnumtrain0_scaled[,c(-19,-12,-15,-17)]),collapse = " + ")))
#Running the basic neural network!!!!
target.y <- jetnumtrain0_scaled$Label
jetnumtrain0_scaled$Label <- as.numeric(jetnumtrain0_scaled$Label)

set.seed(0)
jetnumtrain0_scaled_random=dplyr::sample_n(tbl = jetnumtrain0_scaled,size=10000,replace=FALSE)

#Only selecting 1000 for try
#jetnumtrain0_scaled_random2=dplyr::sample_n(tbl = jetnumtrain0_scaled,size=1000,replace=FALSE)


#jetnumtrain0_basic=neuralnet(formula = formula,data=jetnumtrain0_scaled_random,hidden = c(14,10,6),stepmax=1e+06,rep=5)

#Another Neural network with err.fun,  and act.fct and reduced training set 

#jetnumtrain0_med=neuralnet(formula = formula2,data=jetnumtrain0_scaled_random,hidden = c(20,12,8),stepmax=1e+06,rep=5,err.fct="ce", act.fct="logistic",linear.output=FALSE)

jetnumtrain0_med_results= neuralnet::compute(jetnumtrain0_med,testnum0[,c(-19,-12,-15,-17)])
predicted_basic_numtrain0=jetnumtrain0_med_results$net.result
#jetnumtrain0_med_results_try= neuralnet::compute(jetnumtrain0_med,jetnumtrain0_scaled_random2[,-19])
#predicted_jetnumtrain0_med=jetnumtrain0_med_results$net.result

#result_class=predict(jetnumtrain0_med_try, data=train0[,c(-19,-12,-15,-17)],type = "class")

labels0 <- ifelse(labels0=='s', 1, 0)
auc = roc(labels0, predicted_basic_numtrain0)
plot(auc, print.thres=TRUE)

jetnumtrain0_med_try=neuralnet(formula = formula2,data=jetnumtrain0_scaled,hidden = c(20,12,8),stepmax=1e+06,rep=5,err.fct="ce", act.fct="logistic",linear.output=FALSE,lifesign="minimal",lifesign.step=1000)

