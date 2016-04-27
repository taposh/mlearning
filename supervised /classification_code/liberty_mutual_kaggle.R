#######################################################
# Part of my code for https://www.kaggle.com/c/liberty-mutual-group-property-inspection-prediction
# This was ranked in the top 10%  
​#######################################################​

library(ggplot2)
library(randomForest)
library(readr)
library(h2o)

localH2O = h2o.init()

set.seed(1)

cat("Reading data\n")

#Ingest
train.hex <- h2o.uploadFile(localH2O, path = '../input/train.csv', destination_frame = "train.hex")
#train.hex <- h2o.importFile(path=../input/train.csv,destination_frame = "train",H2OConnection=localH2O)
summary(train.hex)
test.hex <- h2o.uploadFile(localH2O, path = '../input/test.csv', destination_frame = "test.hex")
summary(test.hex)

colnames(train.hex)


# We'll convert all the characters to factors so we can train a randomForest model on them
extractFeatures <- function(data) {
  allcols <- c()
  allcols <- colnames(data)
  for (col in allcols) {
    data[,col] <- as.factor(data[,col])
  }
  return(data)
}

trainFea.hex <- extractFeatures(train.hex)
testFea.hex  <- extractFeatures(test.hex)

str(trainFea.hex)

xvar <- c("T1_V1","T1_V2","T1_V3","T1_V4","T1_V5","T1_V6","T1_V7","T1_V8","T1_V9","T1_V10","T1_V11","T1_V12","T1_V13","T1_V14","T1_V15","T1_V16","T1_V17","T2_V1","T2_V2","T2_V3","T2_V4","T2_V5","T2_V6","T2_V7","T2_V8","T2_V9","T2_V10","T2_V11","T2_V12", "T2_V13","T2_V14","T2_V15")

yvar<-c('Hazard')

cat("Training model\n")
##################################################################
# GBM
#################################################################

# Run regression GBM (BEST MODEL)
#modelgbm <- h2o.gbm(y = yvar, x = xvar, training_frame = trainFea.hex, n.trees = 50, interaction.depth = 5, n.minobsinnode = 2, shrinkage = 0.2)


modelgbm = h2o.gbm(x = 3:34, y = 2, 
              training_frame = trainFea.hex,
              distribution="AUTO",
              nfolds = 1,
              seed = 666,
              ntrees = 700,
              max_depth = 7,
              min_rows = 5,
              learn_rate = 0.02)

#modelgbm <- h2o.randomForest(y = yvar, x = xvar, training_frame = trainFea.hex, n.trees = 50,max_depth = 5 )

modelgbm

modelgbm_1 = h2o.gbm(x = 3:34, y = 2, 
                  training_frame = trainFea.hex,
                  distribution="AUTO",
                  nfolds = 1,
                  seed = 666,
                  ntrees = 700,
                  max_depth = 7,
                  min_rows = 5,
                  learn_rate = 0.02)

modelgbm_1

cat("Making predictions\n")
pred<-as.data.frame(h2o.predict(modelgbm, testFea.hex))
pred_log<-as.data.frame(h2o.predict(modelgbm_1, testFea.hex))

pred_final_h2o<-0.4*pred+0.6*exp(pred_log)



##################
# R XGBOOST starter script

# The readr library is the best way to read and write CSV files in R
library(readr)
library(reshape2)
library(xgboost)

# The competition datafiles are in the directory ../input
# Read competition data files:
X <- read.csv("../input/train.csv")
X.test <- read.csv("../input/test.csv")

# extract id
id.test <- X.test$Id
X.test$Id <- NULL
X$Id <- NULL
n <- nrow(X)

# extarct target
y <- X$Hazard
X$Hazard <- NULL

# replace factors with level mean hazard
for (i in 1:ncol(X)) {
  if (class(X[,i])=="factor") {
    mm <- aggregate(y~X[,i], data=X, mean)
    levels(X[,i]) <- as.numeric(mm[,2]) 
    levels(X.test[,i]) <- mm[,2] 
    X[,i] <- as.numeric(as.character(X[,i]))  
    X.test[,i] <- as.numeric(as.character(X.test[,i]))
  }
}
X <- as.matrix(X)
X.test <- as.matrix(X.test)

# train & tune --skipped--
logfile <- data.frame(shrinkage=c(0.04, 0.03, 0.03, 0.03, 0.02),
                      rounds = c(140, 160, 170, 140, 180),
                      depth = c(8, 7, 9, 10, 10),
                      gamma = c(0, 0, 0, 0, 0),
                      min.child = c(5, 5, 5, 5, 5),
                      colsample.bytree = c(0.7, 0.6, 0.65, 0.6, 0.85),
                      subsample = c(1, 0.9, 0.95, 1, 0.6))

# generate final prediction -- bag of 50 models --
models <- 5
repeats <- 10
yhat.test  <- rep(0,nrow(X.test))
for (j in 1:repeats) {
  for (i in 1:models){
    set.seed(j*1000 + i*100)
    xgboost.mod <- xgboost(data = X, label = y, max.depth = logfile$depth[i], eta = logfile$shrinkage[i],
                           nround = logfile$rounds[i], nthread = 4, objective = "reg:linear", subsample=logfile$subsample[i],
                           colsample_bytree=logfile$colsample.bytree[i], gamma=logfile$gamma[i], min.child.weight=logfile$min.child[i])
    yhat.test  <- yhat.test + predict(xgboost.mod, X.test)  
  }
}
yhat.test <-  yhat.test/(models*repeats)

id<-as.data.frame(test.hex$Id)[,1]
#write.csv(data.frame(id, Hazard=pred$predict),"R_H2O_gbm_benchmark.csv",row.names=F, quote=FALSE)
pred_final_h2o<-0.4*pred+0.6*exp(pred_log)

#Geometric mean
yhat <- SQRT(pred_final_h2o*yhat.test)

write.csv(data.frame(Id=id.test, Hazard=yhat),"R_xgboost_h2o.csv",row.names=F, quote=FALSE)
