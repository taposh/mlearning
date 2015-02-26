#--------------------------------------------------------
#  Non-Grid based models for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------


#For counts
train_factor <- cbind(countresult,bike)
colnames(train_factor)[1] <- "count"
colnames(train_factor)

#For causal
train_factor_causal <- cbind(causal,bike)
colnames(train_factor_causal)[1] <- "causal"
colnames(train_factor_causal)

#For registered
train_factor_registered <- cbind(registered,bike)
colnames(train_factor_registered)[1] <- "registered"
colnames(train_factor_registered)

#Columns Created
colnames(bike)
colnames(test)
#write.table(train_factor,file="train_factors_h2o.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
#write.table(test,file="test_factors_h2o.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)


###################
###Result Analysis
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

#times<-paste(test[,"datetime"])
##################################
# Method 1 : randomForest
##################################

#####RANDOM FOREST STARTS HERE#########
#variables
myNtree = 600
myMtry = 15
myImportance = TRUE
#set the random seed
set.seed(415)

#Counts
countFit <- randomForest(forumla_count, data=train_factor, ntree=myNtree, mtry=myMtry, importance=myImportance,na.action = na.omit)
train_count_predict_1 <- predict(countFit,bike)
test_count_predict_1 <- predict(countFit,test)
compare.rf.bike.count <- cbind(train_factor[,1],train_count_predict_1)
head(compare.rf.bike.count,10)

auc_Counts <-auc(train_factor[,1],train_count_predict_1)
rmsle_Counts_rf<-rmsle(train_factor[,1],train_count_predict_1)
rmsle_Counts_rf
# myroc <- roc(train_factor[,1],train_count_predict_1)
# myci <- ci(train_factor[,1],train_count_predict_1)
# plot(myroc)



####create output file from dataset test with predictions
# test_dt<-read.csv("test.csv")
# test_dt<-(as.data.frame(test_dt))
# submit <- data.frame (datetime = test_dt$datetime, count =test_count_predict_1 )
# head(submit)
# curtime <- Sys.time()
# timestamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
# timestamp2 <-paste("Submission_rf_method1_count",timestamp,".csv", sep="_")
# write.table(submit,file=timestamp2,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)


#fit and predict casual
casualFit <- randomForest(forumla_causal, data=train_factor_causal, ntree=myNtree, mtry=myMtry, importance=myImportance,na.action = na.omit)
train_causal_predict <- predict(casualFit,bike)
test_causal_predict <- predict(casualFit, test)


#fit and predict registered
registeredFit <- randomForest(forumla_registered, data=train_factor_registered, ntree=myNtree, mtry=myMtry, importance=myImportance,na.action = na.omit)
train_registered_predict <- predict(registeredFit, bike)
test_registered_predict <- predict(registeredFit, test)
#add both columns into final count, round to whole number

#### RESULT ##############
result.train <- cbind(result.train,rf=train_count_predict_1)
result.test <- cbind(result.test,rf=test_count_predict_1)

# head(test_causal_predict)
# head(test_registered_predict)
# 
# test_count_predict <- cbind(test_causal_predict+test_registered_predict)
# train_count_predict <- cbind(train_causal_predict+train_registered_predict)
# 
# compare.rf.bike <- cbind(train_factor[,1],train_count_predict)
# head(compare.rf.bike,10)
# 
# auc_Counts <-auc(train_factor[,1],train_count_predict)
# rmsle_Counts<-rmsle(train_factor[,1],train_count_predict)
# rmsle_Counts
# myroc3 <- roc(train_factor[,1],train_count_predict)
# myci2 <- ci(train_factor[,1],train_count_predict)
# plot(myroc3)



# test_result <- cbind(test_causal_predict+test_registered_predict)
# head(test_result)
# #testplot
# #plot(test_result)
# #plot(test_result)
# 
# 
# test_dt<-read.csv("test.csv")
# test_dt<-(as.data.frame(test_dt))
# ####create output file from dataset test with predictions
# submit <- data.frame (datetime = test_dt$datetime, count =test_result )
# head(submit)
# curtime <- Sys.time()
# timestamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
# timestamp2 <-paste("Submission_rf_method1",timestamp,".csv", sep="_")
# write.table(submit,file=timestamp2,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)


##################################
# Method 2 : randomForest
##################################

require(randomForest) 
set.seed(17)
mdl_rf <- randomForest(count ~., data=train_factor, mtry = 16,ntree=600, importance = TRUE,na.action = na.omit)  
varImpPlot(mdl_rf)

#training data 
prd.rf <- predict(mdl_rf, data=bike,type='response')
prd.rf <- as.data.frame(prd.rf)
# compare.rf.bike <- cbind(train_factor[,1],prd.rf)
# head(compare.rf.bike,10)
# 
# auc_Counts <-auc(train_factor[,1],prd.rf)
# rmsle_Counts<-rmsle(train_factor[,1],prd.rf)
# rmsle_Counts
# myroc21 <- roc(train_factor[,1],prd.rf)
# myci2 <- ci(train_factor[,1],prd.rf)
# plot(myroc21)

#test data
prd.test.rf.bike <- predict(mdl_rf, newdata=as.data.frame(test),type='response')  
bike.test.rf <- as.data.frame(prd.test.rf.bike)

# head(bike.test.rf)
# nrow(prd.rf)
# head(prd.rf)

error<-sqrt((sum((train_factor[,1]-prd.rf)^2))/nrow(bike))
error

######################

# #Count Predictions
# datetimes_test = as.data.frame(test_dt[,"datetime"])
# colnames(datetimes_test) <- c("datetime")
# 
# nrow(datetimes_test)
# nrow(prd.test.rf.bike)
# 
# Predictions<-cbind(datetimes_test,prd.test.rf.bike)
# colnames(Predictions)<-c("datetime","count")
# head(Predictions)

#### RESULT ##############
result.train <- cbind(result.train,rf2=prd.rf)
result.test <- cbind(result.test,prd.rf=prd.test.rf.bike)

# head(result.train)
# ########################################################
# ## Output
# ########################################################
# 
# curtime <- Sys.time()
# timestamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
# timestamp2 <-paste("Submission_rf",timestamp,".csv", sep="_")
# write.table(Predictions,file=timestamp2,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
# x<-paste(timestamp2,"AUC",auc_Counts,"Logloss",rmsle_Counts,collapse="  ")
# print(x)
# write(x, file = "Results_compare.txt",append = TRUE, sep = " ")
# 


########################################################
## Forumula
########################################################
#All factors determined

#forumla <- toString(paste("count ~",paste(unique(colnames(as.data.frame(bike))), collapse=" + ")))
# forumla <-count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + month + dayof + temp2 + atemp2 + humid2 + windspeed2

#forumla <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + month + dayof + temp2 + atemp2 + humid2 + windspeed2 + temp3 + atemp3 + humid3 + windspeed3 + logtemp + logatemp + sinhumid + coshumid + sinhumid2 + coshumid2 + sinhumid3 + coshumid3 + sinwind + coswind + sinwind2 + coswind2 + sinwind3 + coswind3 + btsaweather + btsaweathertemp + Number

# t1 + t3 + t5 + t6 + t8 + t9 + t11 + t13 + t14 + t15 + t16 + tt +

###################################
# Algorithms
##################################

#head(bike)


##################################
# Method 1: GBM
##################################
library(gbm)
#Gradient Boosted Method
modelgbm<-gbm(count ~., data=as.data.frame(train_factor),distribution='poisson',n.trees =1500,train.fraction = 1.0,cv.folds=25,shrinkage = 0.001,interaction.depth=5)
#Build generalized boosted model (gradient boosted macine).
#modelgbm
# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(modelgbm,plot.it = TRUE,overlay=TRUE,method="OOB",oobag.curve=TRUE)
print(best.iter)

#show(fit.gbm)
# gbmVarImp<-varImp(best.iter)  
# plot(gbmVarImp)

# plot the performance # plot variable influence
#summary(modelgbm,n.trees=800)         # based on the first tree
summary(modelgbm,n.trees=best.iter) # based on the estimated best number of trees

# compactly print the first and last trees for curiosity
#print(pretty.gbm.tree(modelgbm,1))
#print(pretty.gbm.tree(modelgbm,gbm$n.trees))


#bst <- gbm.perf(modelgbm,method="OOB")                                                #Show plot of performance and store best

colnames(bike)
#colnames(train_factor)

predict.train.gbm <- predict(modelgbm, data=as.data.frame(bike), type="response")

# #comparegbm <- cbind(countresult,predict.train.gbm)
# head(comparegbm)
# #auc_gbm<-auc(countresult,predict.train.gbm)
# rmsle_gbm <-rmsle(countresult,predict.train.gbm)
# myrocgbm <- roc(countresult,predict.train.gbm)
# plot(myrocgbm)

#Get prediction.
predict.test.gbm <- predict(modelgbm, data=test, type="response")
#head(predict.test.gbm)

#### RESULT ##############
result.train <- cbind(result.train,gbm=predict.train.gbm)
result.test <- cbind(result.test,gbm=predict.test.gbm)

# curtime <- Sys.time()
# timestampgbm <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
# timestampgbm1 <-paste("Submission_gbm",timestampgbm,".csv", sep="_")
# write.table(predict.test.gbm ,file=timestampgbm1,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
# 
# xgbm<-paste(timestampgbm1,"AUC",auc_gbm,"Logloss",rmsle_gbm,collapse="  ")
# print(xgbm)
# write(xgbm, file = "Results_compare.txt",append = TRUE, sep = " ")



##################################
# Method 3: Neural Net1
##################################
require(neuralnet)
mdl_nnet <- nnet( count~., data=train_factor, size=10,decay=5e-4,rang = 0.1, maxit=1000)                      #Build neural net.
mdl_nnet

train.prob.nnet<- compute(mdl_nnet,bike)
# compare.bike <- cbind(train_factor[1],train.prob.nnet)
# head(compare.bike)
# 


test.prd.ann <- predict(mdl_nnet, newdata=test)  #Get prediction.#Clean up.
# compare.nn.blood <- cbind(train_factor[1],train.prob.nnet)
# head(compare.nn.blood)
# auc_Counts3 <-auc(train_factor[1],train.prob.nnet)
# rmsle_Counts3<-rmsle(train_factor[1],train.prob.nnet)
# rmsle_Counts3
# myroc3 <- roc(train_factor[1],train.prob.nnet)
# myci3 <- ci(train_factor[1],train.prob.nnet)
# plot(myroc3)
# 
# head(test.prd.ann) 

#### RESULT ##############
result.train <- cbind(result.train,neuralnet=train.prob.nnet)
result.test <- cbind(result.test,neuralnet=test.prd.ann)

# #Count Predictions
# nnPredictions<-cbind(test[,"id"],test.prd.ann)
# colnames(nnPredictions)<-c("id","Made Donation in March 2007")
# head(nnPredictions)
# 
# ########################################################
# ## Output
# ########################################################
# 
# curtime <- Sys.time()
# timestamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
# timestamp1 <-paste("Submission_combo_nn",timestamp,".csv", sep="_")
# timestamp2 <-paste("Submission_nn",timestamp,".csv", sep="_")
# 
# write.table(Predictions_comb,file=timestamp1,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
# write.table(Predictions,file=timestamp2,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
# 
# x<-paste(timestamp2,"AUC",auc_Counts,"Logloss",rmsle_Counts,collapse="  ")
# print(x)
# write(x, file = "Results_compare.txt",append = TRUE, sep = " ")

##
## Output
##

# curtime <- Sys.time()
# timestampnn <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
# timestampnn1 <-paste("Submission_nn",timestamp,".csv", sep="_")
# 
# write.table(nnPredictions,file=timestampnn1,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
# 
# x<-paste(timestampnn1,"AUC",auc_nn,"Logloss",rmsle_nn,collapse="  ")
# print(x)
# write(x, file = "Results_compare.txt",append = TRUE, sep = " ")

##################################
# Method  : CTREE
##################################
# source("ctree.R")
# #head(Predictionsct)
# 



bestresult <- read.csv("/Users/taposh/Desktop/deduced.csv")
head(bestresult)

result.test <- cbind(result.test,bestresult)
head(result.train,10)
head(result.test,10)

result.train1 <- data.frame(result.train)
result.test1<- data.frame(result.test)

colnames(result.test) <- c(rf,prd.rf,gbm)

#result.test <- result.test[-4]

colnames(result.test)

##########################################################
#Final GLM
##########################################################
set.seed(123)
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
Grid <- expand.grid( n.trees = seq(100,3000,100), interaction.depth = c(30), shrinkage = c(0.075))
formula <- actual ~.
#fit.gbm<-gbm(count ~., data=as.data.frame(train_factor),verbose=FALSE,distribution='poisson')

fit.gbm <- train(actual ~., data=result.train, method = 'gbm', trControl=fitControl, verbose=FALSE,tuneGrid=Grid,metric='RMSLE',maximize=FALSE,distribution='poisson')


plot(fit.gbm)
gbmVarImp<-varImp(fit.gbm)  
plot(gbmVarImp)



write.submission(fit.gbm, 'submission_gbm_layer2.csv',result.test,test_dt)
save(fit.gbm,file='fit_gbm_layer2.RData')

  
#fit <- glm(actual ~., result.train, family='poisson')
#summary(fit)


########################################################
## Output
########################################################

# curtime <- Sys.time()
# timestamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
# timestamp1 <-paste("Submission_combo_ctree",timestamp,".csv", sep="_")
# timestamp2 <-paste("Submission_ctree",timestamp,".csv", sep="_")
# 
# write.table(Predictions_comb,file=timestamp1,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
# write.table(Predictions,file=timestamp2,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
# 
# x<-paste(timestamp2,"AUC",auc_Counts,"Logloss",rmsle_Counts,collapse="  ")
# print(x)
# write(x, file = "Results_compare.txt",append = TRUE, sep = " ")
# 
# 
