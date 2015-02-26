#--------------------------------------------------------
#  Main Program for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------
setwd("/Users/taposh/workspace/kaggle/bikeshare/")
#sink the output
#sink("bikeshare.log", split = T)
#source the libraries
source("mylibraries.R")

#read in train/test
test_classes = c(
  "character", # datetime
  "numeric", # season
  "numeric", # holiday
  "numeric", # workingday
  "numeric", # weather
  "numeric", # temp
  "numeric", # atemp
  "numeric", # humidity
  "numeric", # windspeed
  "numeric", #registered
  "numeric" #count
)
set_up_features <- function(df) {
  df$datetime <- strptime(df$datetime, format="%Y-%m-%d %H:%M:%S")
  df$time <- df$datetime$hour + df$datetime$min/60. + df$datetime$sec/3600.
  df$wday <- as.numeric(df$datetime$wday)
  df$month <- as.numeric(df$datetime$mon)
  df$year <- as.numeric(df$datetime$year - 111)
  df
}

train <- read.csv("train.csv", colClasses=test_classes)
test <- read.csv("test.csv", colClasses=test_classes[1:9])
train$count <- as.integer(train$count)

train_factor <- set_up_features(train)
test_factor <- set_up_features(test)


train_factor<- train_factor[,-11]
train_factor<- train_factor[,-10]
train_factor<- train_factor[,-1]
test_factor<- test_factor[,-1]

head(test_factor)

temp <- train_factor[,!(colnames(train_factor) %in% 'count')]
pp.rule <- preProcess(temp, method=c('range'))
pp.train <- predict(pp.rule,temp)
pp.test <- predict(pp.rule,test_factor)
pp.train$count <- train_factor[,'count']
pp.test$datetime <- test$datetime

head(train_factor)

set.seed(1523)
trainIndex <- createDataPartition(pp.train$count, p = 0.8, list=FALSE, times=1)
nrow(trainIndex)
subTrain <- pp.train[trainIndex,]
subTest <- pp.train[-trainIndex,]

# factorize the features with few levels
subTrain2<-subTrain
subTest2 <- subTest
pp.2.test <- pp.test
ind <- c(1,2,3,4,10,11,12)
for (i in ind) {subTrain2[,i] <- as.factor(subTrain2[,i])}
for (i in ind) {subTest2[,i] <- as.factor(subTest2[,i])}
for (i in ind) {pp.2.test[,i] <- as.factor(pp.2.test[,i])}

head(subTest)

source("utils.R")
source("models.R")

load(file='fit_gbm.RData')
load(file='fit_gbmf.RData')
load(file='fit_enet.RData')
load(file='fit_pcr.RData')
load(file='fit_glmnet.RData')
load(file='fit_glmnet2.RData')
load(file='fit_qrf.RData')
load(file='fit_rf.RData')
load(file='fit_cforest.RData')
load(file='fit_rvmRadial.RData')
load(file='fit_nnet.RData')
load(file='fit_neuralnet.RData')

compare <- data.frame()
compare<-tool.performance(fit.gbm,subTest,subTrain,'gbm.CV',compare)
compare<-tool.performance(fit.gbm.f,subTest2,subTrain2,'gbm.CV.f',compare)
compare<-tool.performance(fit.enet,subTest,subTrain,'enet.CV',compare)
compare<-tool.performance(fit.pcr,subTest,subTrain,'pcr.CV',compare)
compare<-tool.performance(fit.glmnet,subTest,subTrain,'glmnet.CV',compare)
compare<-tool.performance(fit.glmnet.2OrderInt,subTest,subTrain,'glmnet.2OInt.CV',compare)
compare<-tool.performance(fit.qrf,subTest,subTrain,'qrf.CV',compare)
compare<-tool.performance(fit.qrf.f,subTest2,subTrain2,'qrf.CV.f',compare)
compare<-tool.performance(fit.rf,subTest2,subTrain2,'rf.oob.3',compare)
compare<-tool.performance(fit.cforest,subTest2,subTrain2,'cforest.oob',compare)
compare<-tool.performance(fit.pcaNNet,subTest,subTrain,'pcaNNet.CV.3',compare)
compare<-tool.performance(fit.nnet,subTest,subTrain,'nnet.CV.2',compare)
compare<-tool.performance(fit.neuralnet,subTest,subTrain,'neuralnet',compare)
compare<-tool.performance(fit.rvmRadial,subTest,subTrain,'rvmRadial.CV',compare)

show(compare)
