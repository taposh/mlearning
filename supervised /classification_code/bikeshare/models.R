
#--------------------------------------------------------
#  Grid based models for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
# http://topepo.github.io/caret/training.html
# http://topepo.github.io/caret/modelList.html
#--------------------------------------------------------

#For counts
train_factor <- cbind(countresult,bike)
colnames(train_factor)[1] <- "count"
colnames(train_factor)
train_factor <- as.data.frame(train_factor)

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

# Split back into test and train sets
subTrain <- train_factor[1:8709,]
subTest <- train_factor[8710:10886,]

head(subTrain)
head(subTest)

#-------------------------------------
#  Model 2: GBM
#-------------------------------------
## gbm fitting

set.seed(123)
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
Grid <- expand.grid( n.trees = seq(100,3000,100), interaction.depth = c(30), shrinkage = c(0.075))
formula <- count ~.
#fit.gbm<-gbm(count ~., data=as.data.frame(train_factor),verbose=FALSE,distribution='poisson')

fit.gbm <- train(formula, data=as.data.frame(subTrain), method = 'gbm', trControl=fitControl, verbose=FALSE,tuneGrid=Grid,metric='RMSLE',maximize=FALSE,distribution='poisson')

#show(fit.gbm)
plot(fit.gbm)
gbmVarImp<-varImp(fit.gbm)  
plot(gbmVarImp)

write.submission(fit.gbm, 'submission_gbm_v3.csv',test,test_dt)
save(fit.gbm,file='fit_gbm.RData')

#-------------------------------------
#  Model 2: GBM
#-------------------------------------

set.seed(123)
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
Grid <- expand.grid( n.trees = seq(100,3000,100), interaction.depth = c(30), shrinkage = c(0.075))

formula <- count ~ .
fit.gbm.f <- train(formula, data=subTrain, method = 'gbm', trControl=fitControl, verbose=FALSE,tuneGrid=Grid,metric='RMSLE',maximize=FALSE,distribution='poisson')
#show(fit.gbm.f)
plot(fit.gbm.f)
gbmFVarImp<-varImp(fit.gbm.f)  
plot(gbmFVarImp)
write.submission(fit.gbm.f, 'submission_gbm_v4.csv',test,test_dt)
save(fit.gbm.f,file='fit_gbmf.RData')

#enet in caret/elasticnet 
#when lambda = 0 -> lasso
set.seed(123)
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
Grid <- expand.grid( fraction = seq(0.05,1.0,0.05),lambda=seq(0,1.0,0.2))
formula <- count ~ .
fit.enet <- train(formula, data=subTrain, method = 'enet', trControl=fitControl,tuneGrid=Grid,metric='RMSLE', maximize=FALSE)
#show(fit.enet)
plot(fit.enet)

enetVarImp<-varImp(fit.enet)  
plot(enetVarImp)
save(fit.enet,file='fit_enet.RData')

#-------------------------------------
#  Model 2: PCR
#-------------------------------------
#pcr in caret/pls 
set.seed(123)
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
Grid <- expand.grid(ncomp = seq(3,10))
formula <- count ~ .
fit.pcr <- train(formula, data=subTrain, method = 'pcr', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid)
#show(fit.pcr)
plot(fit.pcr)
pcrVarImp<-varImp(fit.pcr)  
plot(pcrVarImp)
save(fit.pcr,file='fit_pcr.RData')

#-------------------------------------
#  Model 2: GLMNET
#-------------------------------------

#glmnet in caret/glmnet 
set.seed(123)
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
Grid <- expand.grid(lambda = seq(0.1,50.0,0.5),alpha=c(1))
formula <- count ~ .
fit.glmnet <- train(formula, data=subTrain, method = 'glmnet', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid)
#show(fit.glmnet)
plot(fit.glmnet)
glmnetVarImp<-varImp(fit.glmnet)  
plot(glmnetVarImp)
save(fit.glmnet,file='fit_glmnet.RData')


#-------------------------------------
#  Model 2: GLMNET
#-------------------------------------

set.seed(123)
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
Grid <- expand.grid(lambda = seq(0.1,50.0,0.5),alpha=c(1))
formula <- count ~ .
fit.glmnet.2OrderInt <- train(formula, data=subTrain, method = 'glmnet', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid)
plot(fit.glmnet.2OrderInt)
glmnet2VarImp<-varImp(fit.glmnet.2OrderInt)  
plot(glmnet2VarImp)
save(fit.glmnet.2OrderInt,file='fit_glmnet2.RData')

#-------------------------------------
#  Model : Random Forest
#-------------------------------------

#qrf in caret/quantregForest 
set.seed(123)
fitControl <- trainControl(method = 'cv', number=10, summaryFunction=my2metric)
Grid <- expand.grid(mtry = c(5,7,10))
formula <- count ~ .
fit.qrf <- train(formula, data=subTrain, method = 'qrf', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid)
#show(fit.qrf)
plot(fit.qrf)
qrfVarImp<-varImp(fit.qrf)  
plot(qrfVarImp)
write.submission(fit.qrf, 'submission_qrf_v2.csv',test,test_dt)


#-------------------------------------
#  Model : Random Forest
#-------------------------------------
# set.seed(123)
# fitControl <- trainControl(method = 'cv', number=6, summaryFunction=my2metric)
# Grid <- expand.grid(mtry = c(7,10,15))
# formula <- count ~ .
# fit.qrf.f <- train(formula, data=subTrain2, method = 'qrf', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid)
# #show(fit.qrf)
# plot(fit.qrf.f)
# qrffVarImp<-varImp(fit.qrf.f)  
# plot(qrffVarImp)
# write.submission(fit.qrf.f, 'submission_qrff_v3.csv',test,test_dt)
# save(fit.qrf,fit.qrf.f,file='fit_qrf.RData')

#-------------------------------------
#  Model : Random Forest
#-------------------------------------
#rf in caret/randomForest 
set.seed(123)
fitControl <- trainControl(method = 'oob')
Grid <- expand.grid(mtry = c(15,20,25,30))
formula <- count ~ .
fit.rf <- train(formula, data=subTrain, method = 'rf', trControl=fitControl,metric='RMSE', maximize=FALSE,tuneGrid=Grid)
#show(fit.rf)
plot(fit.rf$results$mtry,fit.rf$results$RMSE)
write.submission(fit.rf, 'submission_rf_v3.csv',test,test_dt)
save(fit.rf,file='fit_rf.RData')

#-------------------------------------
#  Model : Random Forest
#-------------------------------------
#rf in caret/randomForest 
set.seed(123)
fitControl <- trainControl(method = 'oob',summaryFunction=my2metric)
Grid <- expand.grid(mtry = c(15,30,45))
formula <- count ~ .
fit.cforest <- train(formula, data=subTrain2, method = 'cforest', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid)
#show(fit.cforest)
plot(fit.cforest$results$mtry,fit.cforest$results$RMSE)
write.submission(fit.cforest, 'submission_cforest_v3.csv',test,test_dt)
save(fit.cforest,file='fit_cforest.RData')

#######################
## SVM Family
#######################

set.seed(123)
Grid <- expand.grid(sigma = c(0.05,0.1,0.2))
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
formula <- count ~ .
fit.rvmRadial <- train(formula, data=subTrain, method = 'rvmRadial', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid)
#show(fit.rvmRadial)
plot(fit.rvmRadial)
rvmRadialVarImp<-varImp(fit.rvmRadial)  
plot(rvmRadialVarImp)
save(fit.rvmRadial,file='fit_rvmRadial.RData')


######################
## Neural Net
######################

set.seed(123)
Grid <- expand.grid(size=seq(3,20,5),decay=c(0.01,0.1,0.2,0.3))
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
formula <- count ~ .
fit.pcaNNet <- train(formula, data=subTrain, method = 'pcaNNet', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid,linout=TRUE,maxit=5000,trace=FALSE)
#show(fit.rvmRadial)
plot(fit.pcaNNet)
pcaNNetVarImp<-varImp(fit.pcaNNet)  
plot(pcaNNetVarImp)

#-------------------------------------
#  Model : Neural Nets
#-------------------------------------

set.seed(123)
Grid <- expand.grid(size=seq(3,20,5),decay=c(0.01,0.1,0.2,0.3))
fitControl <- trainControl(method = 'cv', number = 10, summaryFunction=my2metric)
formula <- count ~ .
fit.nnet <- train(formula, data=subTrain, method = 'nnet', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid,linout=TRUE,maxit=5000,trace=FALSE)
#show(fit.rvmRadial)
plot(fit.nnet)
nnetVarImp<-varImp(fit.nnet)  
plot(nnetVarImp)
save(fit.nnet,fit.pcaNNet,file='fit_nnet.RData')

#-------------------------------------
#  Model : Neural Nets
#-------------------------------------
set.seed(123)
Grid <- expand.grid(layer1=c(2,6),layer2=c(2,6),layer3=c(2,6))
fitControl <- trainControl(method = 'cv', number=10, summaryFunction=my2metric)
formula <- count ~ .
fit.neuralnet <- train(formula, data=subTrain, method = 'neuralnet', trControl=fitControl,metric='RMSLE', maximize=FALSE,tuneGrid=Grid,linear.output=TRUE)
#show(fit.rvmRadial)
neuralnetVarImp<-varImp(fit.neuralnet)  
plot(neuralnetVarImp)
save(fit.neuralnet,file='fit_neuralnet.RData')

#################################################





