##################################
# Method 2 : CTREE
##################################

# define training control
train_control <- trainControl(method="cv", number=10)

# # train the model 
themodel <- train(formula_count, data=train_factor, trControl=train_control, method="ctree")

#predict for count using train 
predict.train.count.ctree <-predict(themodel,bike )

#actual <- data.frame(train_factor[1])
sim <-data.frame(predict.train.count.ctree)

auc_ctree<-auc(countresult,predict.train.count.ctree)
log_ctree<-rmsle(countresult,predict.train.count.ctree)
myroc_ctree <- roc(countresult,predict.train.count.ctree)
myci2 <- ci(countresult,predict.train.count.ctree)
plot(myroc_ctree)


#build a dataframe with our results
valid.train.count<-cbind(traintimes,count=predict.train.count,countresult)
colnames(valid.train.count)<-c("datetime","count","actual")
head(valid.train.count)

#predict for count using test
predict.test.ctree <-predict(themodel,test )

#Count Predictions
Predictions<-cbind(times,predict.test.ctree)
colnames(Predictions)<-c("datetime","count")
head(Predictions)

########################################################
## Output
########################################################

curtime <- Sys.time()
timestamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
#timestamp1 <-paste("Submission_combo_ctree",timestamp,".csv", sep="_")
timestamp2 <-paste("Submission_ctree",timestamp,".csv", sep="_")
#write.table(Predictions_comb,file=timestamp1,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
write.table(Predictions,file=timestamp2,row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)

x1 <- paste(timestamp2,"AUC",auc_ctree,"Logloss",log_ctree,collapse="  ")
print(x1)
write(x1, file = "Results_compare.txt",append = TRUE, sep = " ")