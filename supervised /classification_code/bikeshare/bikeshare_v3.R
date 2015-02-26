#--------------------------------------------------------
#  Main Program for kaggle-bike-sharing Revision #3
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------
setwd("/Users/taposh/workspace/kaggle/bikeshare/")
#sink the output
#sink("bikeshare.log", split = T)
#source the libraries
source("mylibraries.R")
#input data
source("inputdata.R")
#View the data
head(bike)

#Visualize the data
#source("visualize.R")

#Factorengineering
source("/Users/taposh/workspace/kaggle/bikeshare/factorengineering_v1.R")

subTrain <- as.list(as.data.frame(t(subTrain)))
subTest <- as.list(as.data.frame(t(subTest)))

###################
###Result Variables
##################
result.train<-c()
result.train <- data.frame(countresult)
head(result.train)
colnames(result.train) <- c("actual")
result.stat<-c()
result.test<-c()


#forumla_count <-count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + month + dayof + temp2 + atemp2 + humid2 + windspeed2
forumla_count <-count ~.
#forumla_causal <-causal ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + month + dayof + temp2 + atemp2 + humid2 + windspeed2
forumla_causal <-causal ~.
#forumla_registered <-registered ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + month + dayof + temp2 + atemp2 + humid2 + windspeed2
forumla_registered <-registered ~.


source("utils.R")

test_dt<-read.csv("test.csv")
test_dt<-(as.data.frame(test_dt))

source("models.R")
#write.submission.file(countFit,test)

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

