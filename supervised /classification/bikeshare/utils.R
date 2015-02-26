
#--------------------------------------------------------
#  Utils for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

#################
# Finding Days
##################
dayof<-function(X){
  month<-as.matrix(as.numeric(X[,"month"]))
  month[month==1]<-13
  month[month==2]<-14
  day<-as.matrix(as.numeric(X[,"day"]))
  year<-cbind(as.numeric(X[,"year"]),as.numeric(X[,"month"]))
  for(i in 1:nrow(year)){
    if((year[i,2]<=2)==TRUE){year[i,1]<-year[i,1]-1}
  }
  year<-as.matrix(year[,1])
  y<-year %% 2000
  c<-year - y
  c<-c/100
  DAY<-(day+floor(13*(month+1)/5)+y+floor(y/4)+floor(c/4)-c) %% 7
  return(DAY)}

#-----------------------------
#submission tool
#-----------------------------

write.submission <- function(model,name, test_factors,test_dt){
  
  predict.model <- predict(model, test_factors)
  submit <- data.frame(datetime = test_dt$datetime, count=predict.model)
  #write results to .csv for submission
  curtime <- Sys.time()
  timestamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
  name <-paste("kaggle_bikeshare_",name,timestamp,".csv", sep="_")
  
  write.csv(submit, file=name,row.names=FALSE,quote=FALSE)
}



#submission tool
write.submission.file <- function(model,name, test_factors){
  test_dt<-read.csv("test.csv")
  test_dt<-(as.data.frame(test_dt))
  
  predict.model <- predict(model, test_factors)
  submit <- data.frame(datetime = test_dt$datetime, count=predict.model)
  #write results to .csv for submission
  curtime <- Sys.time()
  timestamp <-  strftime(curtime,"%Y-%m-%d-%H-%M-%S")
  name <-paste("Submission_",model,timestamp,".csv", sep="_")
  
  write.csv(submit, file=name,row.names=FALSE,quote=FALSE)
}

#performance comparison
tool.performance <- function(fit.model,subTest,subTrain,name.=name,compare.=compare,check.lgcount=FALSE,check.update=TRUE){
  
  if(check.lgcount){
    predictT.model.testing <- predict(fit.model, newdat=subTest)
    predictT.model.testing<- exp(predictT.model.testing)-1
    predictT.model.Training <- predict(fit.model, newdat=subTrain)
    predictT.model.Training<- exp(predictT.model.Training)-1
    subTest$count <- as.numeric(exp(subTest$lgcount)-1)
    subTrain$count <- as.numeric(exp(subTrain$lgcount)-1)
    
  } else{ 
    predictT.model.testing <- predict(fit.model, newdat=subTest)
    if (any(predictT.model.testing<0)) {
      predictT.model.testing[predictT.model.testing<0] <- 0
    }
    predictT.model.Training <- predict(fit.model, newdat=subTrain)
    if (any(predictT.model.Training<0)) {
      predictT.model.Training[predictT.model.Training<0] <- 0
    }  
  }
  
  rmsle.Test <- rmsle(subTest$count,predictT.model.testing)
  rmse.Test <- rmse(subTest$count,predictT.model.testing)
  rmsle.Train <- rmsle(subTrain$count,predictT.model.Training)
  rmse.Train <- rmse(subTrain$count,predictT.model.Training)
  
  diff <- summary(as.numeric(subTrain$count - predictT.model.Training))
  diff.Test <- summary(as.numeric(subTest$count - predictT.model.testing))
  ll <- list('Test.rmsle'=rmsle.Test, 'Test.rmse'=rmse.Test, 'Train.RMSLE'=rmsle.Train, 
             'Train.RMSE'=rmse.Train,'Test.diff.median'=as.numeric(diff.Test[3]),'Test.diff.mean'=as.numeric(diff.Test[4]),
             'Train.diff.median'=as.numeric(diff[3]),'Train.diff.mean'=as.numeric(diff[4]))
  if (check.update) {
    compare.<-rbind(compare., mmm=ll)
    rname <- rownames(compare.)
    rname[nrow(compare.)] <- name.  
    rownames(compare.) <- rname 
  } else {
    compare. <- as.data.frame(ll,row.names=name.)
  }
  compare.
}


##DEFINE Metric function

mymetric <- function(data, lev=NULL, model=NULL){
  require(Metrics)
  pred<-data[,'pred']
  obs<-data[,'obs']
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  
  if (length(obs) + length(pred) == 0) {
    out <- rep(NA, 2)
  }
  else {
    if (length(unique(pred)) < 2 || length(unique(obs)) < 
          2) {
      resamplCor <- NA
    }
    else {
      resamplCor <- try(cor(pred, obs, use = "pairwise.complete.obs"), 
                        silent = TRUE)
      if (class(resamplCor) == "try-error") 
        resamplCor <- NA
    }
    
    #      msle<-msle(obs, pred)
    rmsle<-rmsle(obs,pred)
    #      rmse<-rmse(obs,pred)
    out <- rmsle
  }
  names(out) <- "RMSLE"
  
  if (any(is.nan(out))) 
    out[is.nan(out)] <- NA
  out
  
}

my2metric <- function(data, lev=NULL, model=NULL){
  pred<-data[,'pred']
  obs<-data[,'obs']
  rmse <- rmse(pred,obs)
  isNA <- is.na(pred)
  pred[isNA] <- 0
  obs[isNA] <- 0
  pred[which(pred < 0)] <- 0.0
  obs[which(pred < 0)] <- 0.0 
  rmsle <- rmsle(obs,pred)
  out <- c(rmsle,rmse)
  names(out) <- c("RMSLE","RMSE")
  
  if (any(is.nan(out))) 
    out[is.nan(out)] <- 0.0
  out
  
}

