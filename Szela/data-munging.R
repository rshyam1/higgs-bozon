library(xgboost)
library(caret)
library(neuralnet)

setwd("~/GitHub-kszela24/Higgs")

train = read.csv("training.csv")
test = read.csv("test.csv")

train[train == -999] = NA
test[test == -999] = NA

train.NA.DER_mass_MMC = train[rowSums(is.na(train['DER_mass_MMC'])) > 0,]
test.NA.DER_mass_MMC = test[rowSums(is.na(test['DER_mass_MMC'])) > 0,]

train = train[rowSums(is.na(train['DER_mass_MMC'])) == 0,]
test = test[rowSums(is.na(test['DER_mass_MMC'])) == 0,]

train.NA.PRI_jet_leading_pt = train[rowSums(is.na(train['PRI_jet_leading_pt'])) > 0,]
test.NA.PRI_jet_leading_pt = test[rowSums(is.na(test['PRI_jet_leading_pt'])) > 0,]

train = train[rowSums(is.na(train['PRI_jet_leading_pt'])) == 0,]
test = test[rowSums(is.na(test['PRI_jet_leading_pt'])) == 0,]

train.NA.DER_mass_jet_jet = train[rowSums(is.na(train['DER_mass_jet_jet'])) > 0,]
test.NA.DER_mass_jet_jet = test[rowSums(is.na(test['DER_mass_jet_jet'])) > 0,]

train.no.NA = train[rowSums(is.na(train['DER_mass_jet_jet'])) == 0,]
test.no.NA = test[rowSums(is.na(test['DER_mass_jet_jet'])) == 0,]

train.NA.DER_mass_jet_jet = train.NA.DER_mass_jet_jet[, colSums(is.na(train.NA.DER_mass_jet_jet)) != nrow(train.NA.DER_mass_jet_jet)]
test.NA.DER_mass_jet_jet = test.NA.DER_mass_jet_jet[, colSums(is.na(test.NA.DER_mass_jet_jet)) != nrow(test.NA.DER_mass_jet_jet)]

train.NA.DER_mass_MMC = train.NA.DER_mass_MMC[, colSums(is.na(train.NA.DER_mass_MMC)) != nrow(train.NA.DER_mass_MMC)]
test.NA.DER_mass_MMC = test.NA.DER_mass_MMC[,colSums(is.na(test.NA.DER_mass_MMC)) != nrow(test.NA.DER_mass_MMC)]

train.NA.PRI_jet_leading_pt = train.NA.PRI_jet_leading_pt[, colSums(is.na(train.NA.PRI_jet_leading_pt)) != nrow(train.NA.PRI_jet_leading_pt)]
test.NA.PRI_jet_leading_pt = test.NA.PRI_jet_leading_pt[, colSums(is.na(test.NA.PRI_jet_leading_pt)) != nrow(test.NA.PRI_jet_leading_pt)]

train.no.NA.w_l = train.no.NA[,c(32, 33)]
train.no.NA$Weight = NULL
train.no.NA$Label = NULL

train.NA.PRI_jet_leading_pt.w_l = train.NA.PRI_jet_leading_pt[,c(22, 23)]
train.NA.PRI_jet_leading_pt$Weight = NULL
train.NA.PRI_jet_leading_pt$Label = NULL

train.NA.DER_mass_jet_jet.w_l = train.NA.DER_mass_jet_jet[,c(25, 26)]
train.NA.DER_mass_jet_jet$Weight = NULL
train.NA.DER_mass_jet_jet$Label = NULL

train.NA.DER_mass_MMC.w_l = train.NA.DER_mass_MMC[,c(31, 32)]
train.NA.DER_mass_MMC$Weight = NULL
train.NA.DER_mass_MMC$Label = NULL

train.no.NA.index = train.no.NA$EventId
test.no.NA.index = test.no.NA$EventId
train.no.NA$EventId = NULL
test.no.NA$EventId = NULL

cat("\n## Removing the constants features.\n")
for (f in names(train.no.NA)) {
  if (length(unique(train.no.NA[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train.no.NA[[f]] <- NULL
    test.no.NA[[f]] <- NULL
  }
}


train.no.NA$Target = train.no.NA.w_l$Weight
train.no.NA.model <- sparse.model.matrix(Target ~ ., data = train.no.NA)




