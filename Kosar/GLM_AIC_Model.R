library(VIM)
library(car)
library(psych)
library(mice)
library(doMC)
library(caret)
library(dplyr)
library(tree)
library(MASS)



#Some basic exploratory functions
summary(training)
head(training)
sapply(training,sd)
cor(training[,-33])


#Visualizing the missingness
aggr(train1)

#Splitting the training set into three different dataset according to the missing values
train_prijet_0 = training[training$PRI_jet_num == 0,]
train_prijet_1 = training[training$PRI_jet_num == 1,]
train_prijet_2_3 = training[training$PRI_jet_num == 2 | training$PRI_jet_num == 3,]

#Splitting the test set into three different dataset according to the missing values
test_prijet_0 = test[test$PRI_jet_num == 0,]
test_prijet_1 = test[test$PRI_jet_num == 1,]
test_prijet_2_3 = test[test$PRI_jet_num == 2 | test$PRI_jet_num == 3,]


train_prijet_0 = train_prijet_0[,-c(6:8)]
train_prijet_0 = train_prijet_0[,-11]
train_prijet_0 = train_prijet_0[,-c(21:26)]#######################Dropped the -999 columns
train_prijet_0 = train_prijet_0[,-c(20:21)]#######################Dropped the columns which I used as a factor (they include all 0
train_prijet_0$DER_mass_MMC[train_prijet_0$DER_mass_MMC == -999] = 0.0001 

train_prijet_1 = train_prijet_1[,-c(6:8)]
train_prijet_1 = train_prijet_1[,-11]
train_prijet_1 = train_prijet_1[,-c(24:26)]
train_prijet_1 = train_prijet_1[,-20]

train_prijet_2_3 = train_prijet_2_3[,-24]
train_prijet_2_3$DER_mass_MMC[train_prijet_2_3$DER_mass_MMC == -999] = 0.0001 



test_prijet_0 = test_prijet_0[,-c(6:8)]
test_prijet_0 = test_prijet_0[,-11]
test_prijet_0 = test_prijet_0[,-c(21:26)]#######################Dropped the -999 columns
test_prijet_0 = test_prijet_0[,-c(20:21)]#######################Dropped the columns which I used as a factor (they include all 0
test_prijet_0$DER_mass_MMC[test_prijet_0$DER_mass_MMC == -999] = 0.0001 

test_prijet_1 = test_prijet_1[,-c(6:8)]
test_prijet_1 = test_prijet_1[,-11]
test_prijet_1 = test_prijet_1[,-c(24:26)]#######################Dropped the -999 columns
test_prijet_1 = test_prijet_1[,-20]#######################Dropped the columns which I used as a factor (they include all 0
test_prijet_1$DER_mass_MMC[test_prijet_1$DER_mass_MMC == -999] = 0.0001 


#PCA for training set where PRI_Jet_Num = 0
fa.parallel(train_prijet_0[-21], #The data in question.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100)

pc_train_pri_jet_0 = principal(train_prijet_0[-21], #The data in question.
                      nfactors = 7, #The number of PCs to extract.
                      rotate = "none")

pc_train_pri_jet_0

#PC1 highly correlates with DER_mass_MMC, DER_mass_vis, DER_sum_pt, PRI_tau_pt
#PC2 highly correlates with DER_mass_transverse_met_lep, DER_pt_ratio_lep_tau, PRI_lep_pt
#PC3 highly correlates with DER_pt_h, DER_pt_tot
#PC4 does not seem to highly correlate with anything (everything less than .6)
#PC5 highly correlates with PRI_tau_eta, PRI_lep_eta
#PC6 highly correlates with PRI_tau_phi / negatively correlates with PRI_lep_phi
#PC7 highly correlates with PRI_met_phi


######################################################
##########GENERALIZED MODEL WITH FORWARD AIC##########
######################################################

#__________For Prijet_Num_0__________

train_logistic = train_prijet_0[,-c(1,20)]
levels(train_logistic$Label) = c(0,1)
train_logistic$Label = as.numeric(train_logistic$Label) - 1

#Building the models for stepwise regression
model.empty.prijet_0 = glm(Label ~ 1, family = "binomial", data = train_logistic)
model.full.prijet_0 = glm(Label ~ ., family = "binomial", data = train_logistic)
scope = list(lower = formula(model.empty.prijet_0), upper = formula(model.full.prijet_0))

forwardAIC = step(model.empty.prijet_0, scope, direction = "forward", k = 2) #AIC = 78217
summary(forwardAIC)
plot(forwardAIC)

#Making predictions
logit.predict.prijet_0 = predict(forwardAIC, test_prijet_0, interval = "prediction")

predictions.logit.prijet_0 = data.frame(test_prijet_0$EventId, logit.predict.prijet_0)
colnames(predictions.logit.prijet_0) = c("EventId", "Label")

write.csv(predictions.logit.prijet_0, file = "Pred_Logit_Prijet_0.csv")

#__________For Prijet_Num_1__________

train_logistic_prijet_1 = train_prijet_1[,-c(1,24)]
levels(train_logistic_prijet_1$Label) = c(0,1)
train_logistic_prijet_1$Label = as.numeric(train_logistic_prijet_1$Label) - 1

#Building the models for stepwise regression
model.empty.prijet_1 = glm(Label ~ 1, family = "binomial", data = train_logistic_prijet_1)
model.full.prijet_1 = glm(Label ~ ., family = "binomial", data = train_logistic_prijet_1)
scope = list(lower = formula(model.empty.prijet_1), upper = formula(model.full.prijet_1))

forwardAIC_prijet_1 = step(model.empty.prijet_1, scope, direction = "forward", k = 2) 
summary(forwardAIC_prijet_1) #AIC = 83792

logit.predict.prijet_1 = predict(forwardAIC_prijet_1, test_prijet_1, interval = "prediction")

predictions.logit.prijet_1 = data.frame(test_prijet_1$EventId, logit.predict.prijet_1)
colnames(predictions.logit.prijet_1) = c("EventId", "Label")

write.csv(predictions.logit.prijet_1, file = "Pred_Logit_Prijet_1.csv")

#__________For Prijet_Num_2_3__________

train_logistic_prijet_2_3 = train_prijet_2_3[,-c(1,31)]
levels(train_logistic_prijet_2_3$Label) = c(0,1)
train_logistic_prijet_2_3$Label = as.numeric(train_logistic_prijet_2_3$Label) - 1

#Building the models for stepwise regression
model.empty.prijet_2_3 = glm(Label ~ 1, family = "binomial", data = train_logistic_prijet_2_3)
model.full.prijet_2_3 = glm(Label ~ ., family = "binomial", data = train_logistic_prijet_2_3)
scope = list(lower = formula(model.empty.prijet_2_3), upper = formula(model.full.prijet_2_3))

forwardAIC_prijet_2_3 = step(model.empty.prijet_2_3, scope, direction = "forward", k = 2) 
summary(forwardAIC_prijet_2_3) #AIC = 76366

logit.predict.prijet_2_3 = predict(forwardAIC_prijet_2_3, test_prijet_2_3, interval = "prediction")

predictions.logit.prijet_2_3 = data.frame(test_prijet_2_3$EventId, logit.predict.prijet_2_3)
colnames(predictions.logit.prijet_2_3) = c("EventId", "Label")

write.csv(predictions.logit.prijet_2_3, file = "Pred_Logit_Prijet_2_3.csv")

total = rbind(predictions.logit.prijet_0, predictions.logit.prijet_1, predictions.logit.prijet_2_3)
total_sorted = total[order(total$Label),]

total_sorted$RankOrder = 1:length(total_sorted$Label)

threshold = length(total_sorted$Label) - as.integer(0.15*length(total_sorted$Label))

total_sorted$Class = ifelse(total_sorted$RankOrder <= threshold, "b", "s")
total_sorted$Label = NULL

write.csv(total_sorted, file = "model_AIC.csv", row.names = FALSE)















