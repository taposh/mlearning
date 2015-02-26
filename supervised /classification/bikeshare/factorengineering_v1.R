#--------------------------------------------------------
#  Factor Engineering for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

setwd("/Users/taposh/workspace/kaggle/bikeshare/")
source("utils.R")

#Seperating out the outputs from training
actual<-c()
countresult <-c()
causal<-c()
registered<-c()
countresult<-cbind("count"=countresult,bike[,"count"])
actual<-cbind("count"=actual,bike[,"count"])
causal<-cbind(causal,bike[,"casual"])
registered<-cbind(registered,bike[,"registered"])

causal<-log(causal)
causal[causal[,1]<0,1]<-0
registered<-log(registered)
registered[registered[,1]<0,1]<-0
actual<-log(actual)
#actual[actual[,1]<0,1]<-0

###################
###Result Analysis
##################
result.train<-c()
result.train <- data.frame(countresult)
head(result.train)
colnames(result.train) <- c("actual")
result.stat<-c()
result.test<-c()


###########################
#Dealing with datetimes
###########################

traintimes<-paste(bike[,"datetime"])
times<-paste(test[,"datetime"])

#4 variables - hour,day, month and year
bike<-cbind(bike,"hour"=format(as.POSIXct(bike[,"datetime"], format="%Y-%m-%d %H:%M"), format="%H"),
            "day"=format(as.POSIXct(bike[,"datetime"], format="%Y-%m-%d %H:%M"), format="%d"),
            "month"=format(as.POSIXct(bike[,"datetime"], format="%Y-%m-%d %H:%M"), format="%m"),
            "year"=format(as.POSIXct(bike[,"datetime"], format="%Y-%m-%d %H:%M"), format="%Y")
)
bike[,c("hour","day","month","year")]<-as.matrix(bike[,c("hour","day","month","year")])

for(i in 1:nrow(bike)){
  for(j in c("hour","day","month","year")){
    if(is.na(bike[i,j])){
      bike[i,j]<-mean(as.numeric(bike[i-1,j]),as.numeric(bike[i+1,j]))}
  }}

test<-cbind(test,"hour"=format(as.POSIXct(test[,"datetime"], format="%Y-%m-%d %H:%M"), format="%H"),
            "day"=format(as.POSIXct(test[,"datetime"], format="%Y-%m-%d %H:%M"), format="%d"),
            "month"=format(as.POSIXct(test[,"datetime"], format="%Y-%m-%d %H:%M"), format="%m"),
            "year"=format(as.POSIXct(test[,"datetime"], format="%Y-%m-%d %H:%M"), format="%Y")
)
test[,c("hour","day","month","year")]<-as.matrix(test[,c("hour","day","month","year")])

######################
# Actual biketime
#####################
START<-ISOdate(min(bike[,"year"]),min(bike[,"month"]),min(bike[,"day"]),min(bike[,"hour"]))
bikeTIME<-c()
testTIME<-c()
for(i in 1:nrow(bike)){
  bikeTIME<-rbind(bikeTIME,difftime(ISOdate(bike[i,"year"],bike[i,"month"],bike[i,"day"],bike[i,"hour"]),START,units="hours"))}
for(i in 1:nrow(test)){
  testTIME<-rbind(testTIME, difftime(ISOdate(test[i,"year"],test[i,"month"],test[i,"day"],test[i,"hour"]),START,units="hours"))}

head(bike,4)

#Weather : removing 4
#bike$weather[bike$weather == 4] <- 3
#test$weather[test$weather == 4] <- 3

#http://www.r-bloggers.com/from-continuous-to-categorical/
#bike$tempfactor<-cut(as.numeric(bike$temp), c(0,5,10,15,20,25,34,41))
#test$tempfactor<-cut(as.numeric(test$temp), c(0,5,10,15,20,25,34,41))

#bike$tempfactor[bike$tempfactor == "NA"] <- "0"


################################
#Note: move this to utils.R
################################
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

bike<-cbind(bike,"dayof"=dayof(bike))
test<-cbind(test,"dayof"=dayof(test))

test<-test[,colnames(test)!="datetime"]
test<-test[,colnames(test)!="day"]
test<-test[,colnames(test)!="year"]
bike<-bike[,colnames(test)]

######################
## Matrix Interactions
######################
bike1<- model.matrix(~(bikeTIME+bike[,"season"]+bike[,"weather"]+bike[,"temp"]+bike[,"atemp"]+bike[,"humidity"])^6,bike)

test1<- model.matrix(~(testTIME+test[,"season"]+test[,"weather"]+test[,"temp"]+test[,"atemp"]+test[,"humidity"])^6,test)

colnames(bike)
colnames(test)


bike <- cbind(bike,bike1)
test <- cbind(test,test1)
###########################
# Other Factors
###########################
bike<-cbind(bike,
            btsaweather= bikeTIME*bike[,"season"]*bike[,"weather"],
            btsaweathertemp= bikeTIME*bike[,"season"]*bike[,"weather"]*bike[,"temp"]
)

test<-cbind(test,
            btsaweather= testTIME*test[,"season"]*test[,"weather"],
            btsaweathertemp= testTIME*test[,"season"]*test[,"weather"]*test[,"temp"]
)

bike<-cbind(bike,
            temp2=(bike[,"temp"])^2,
            atemp2=(bike[,"atemp"])^2,
            humid2=(bike[,"humidity"])^2,
            windspeed2=(bike[,"windspeed"])^2,
            temp3=(bike[,"temp"])^3,
            atemp3=(bike[,"atemp"])^3,
            humid3=(bike[,"humidity"])^3,
            windspeed3=(bike[,"windspeed"])^3)

test<-cbind(test,
            temp2=test[,"temp"]^2,
            atemp2=test[,"atemp"]^2,
            humid2=test[,"humidity"]^2,
            windspeed2=(test[,"windspeed"])^2,
            temp3=(test[,"temp"])^3,
            atemp3=(test[,"atemp"])^3,
            humid3=(test[,"humidity"])^3,
            windspeed3=(test[,"windspeed"])^3)

test<-cbind(test,
            t1=sin(4*pi*testTIME/(24)),
            t2=sin(2*pi*testTIME/(1000))*cos(2*pi*testTIME/(24)),
            t3=cos(4*pi*testTIME/(24)),
            #t4=cos(4*pi*testTIME/(1000)),
            t5=tanh(2*pi*testTIME/(24)),
            #tanh(2*pi*testTIME/(1000)),
            t6=tanh(2*pi*testTIME/(365*24)),
            #t7=tanh(4*pi*testTIME/(24)),
            #tanh(4*pi*testTIME/(1000)),
            t8=tanh(4*pi*testTIME/(365*24)),
            t9=sin(2*pi*testTIME/(24)),
            #t10=sin(2*pi*testTIME/(1000)),
            t11=cos(2*pi*testTIME/(24)),
            #t12=cos(2*pi*testTIME/(1000)),
            t13=sin(4*pi*testTIME/(356*24)),
            t14=cos(4*pi*testTIME/(365*24)),
            t15=sin(2*pi*testTIME/(365*24)),
            t16=cos(2*pi*testTIME/(365*24)),
            tt=testTIME)


bike<-cbind(bike,
            t1=sin(4*pi*bikeTIME/(24)),
            t2=sin(2*pi*bike[,"temp2"]/(1000))*cos(2*pi*bikeTIME/(24)),
            t3=cos(4*pi*bikeTIME/(24)),
            #t4=cos(4*pi*bikeTIME/(1000)),
            t5=tanh(2*pi*bikeTIME/(24)),#tanh(2*pi*bikeTIME/(1000)),
            t6=tanh(2*pi*bikeTIME/(365*24)),
            #t7=tanh(4*pi*bikeTIME/(24)),#tanh(4*pi*bikeTIME/(1000)),
            t8=tanh(4*pi*bikeTIME/(365*24)),
            t9=sin(2*pi*bikeTIME/(24)),
            #t10=sin(2*pi*bikeTIME/(1000)),
            t11=cos(2*pi*bikeTIME/(24)),
            #t12=cos(2*pi*bikeTIME/(1000)),
            t13=sin(4*pi*bikeTIME/(356*24)),
            t14=cos(4*pi*bikeTIME/(365*24)),
            t15=sin(2*pi*bikeTIME/(365*24)),
            t16=cos(2*pi*bikeTIME/(365*24)),
            tt=bikeTIME)

# humid and windspeed
test<-cbind(test,
            sinhumid=sin(2*pi*test[,"humidity"]/4),
            coshumid=cos(2*pi*test[,"humidity"]/4),
            sinhumid2=sin(2*pi*test[,"humid2"]/10),
            coshumid2=cos(2*pi*test[,"humid2"]/10),
            sinhumid3=sin(2*pi*test[,"humid3"]/(10)),
            coshumid3=cos(2*pi*test[,"humid3"]/(10)),
            sinwind=sin(2*pi*test[,"windspeed"]/(10)),
            coswind=cos(2*pi*test[,"windspeed"]/(10)),
            sinwind2=sin(2*pi*test[,"windspeed2"]/(0.00001)),
            coswind2=cos(2*pi*test[,"windspeed2"]/(0.00001)),
            sinwind3=sin(2*pi*test[,"windspeed3"]/(0.00001)),
            coswind3=cos(2*pi*test[,"windspeed3"]/(0.00001)))

bike<-cbind(bike,
            sinhumid=sin(2*pi*bike[,"humidity"]/4),
            coshumid=cos(2*pi*bike[,"humidity"]/4),
            sinhumid2=sin(2*pi*bike[,"humid2"]/10),
            coshumid2=cos(2*pi*bike[,"humid2"]/10),
            sinhumid3=sin(2*pi*bike[,"humid3"]/(10)),
            coshumid3=cos(2*pi*bike[,"humid3"]/(10)),
            sinwind=sin(2*pi*bike[,"windspeed"]/(10)),
            coswind=cos(2*pi*bike[,"windspeed"]/(10)),
            sinwind2=sin(2*pi*bike[,"windspeed2"]/(0.00001)),
            coswind2=cos(2*pi*bike[,"windspeed2"]/(0.00001)),
            sinwind3=sin(2*pi*bike[,"windspeed3"]/(0.00001)),
            coswind3=cos(2*pi*bike[,"windspeed3"]/(0.00001)))

# # #temp and atemp
bike<-cbind(bike,
            sintemp2=sin(2*pi*bike[,"temp2"]/(10000)),
            costemp2=cos(2*pi*bike[,"temp2"]/(10000)),
            sintemp3=sin(2*pi*bike[,"temp3"]/(1000000)),
            costemp3=cos(2*pi*bike[,"temp3"]/(1000000)),
            sinatemp2=sin(2*pi*bike[,"atemp2"]/(10000)),
            cosatemp2=cos(2*pi*bike[,"atemp2"]/(10000)),
            sinatemp3=sin(2*pi*bike[,"atemp3"]/(1000000)),
            cosatemp3=cos(2*pi*bike[,"atemp3"]/(1000000)))


test<-cbind(test,
            sintemp2=sin(2*pi*test[,"temp2"]/(10000)),
            costemp2=cos(2*pi*test[,"temp2"]/(10000)),
            sintemp3=sin(2*pi*test[,"temp3"]/(1000000)),
            costemp3=cos(2*pi*test[,"temp3"]/(1000000)),
            sinatemp2=sin(2*pi*test[,"atemp2"]/(10000)),
            cosatemp2=cos(2*pi*test[,"atemp2"]/(10000)),
            sinatemp3=sin(2*pi*test[,"atemp3"]/(1000000)),
            cosatemp3=cos(2*pi*test[,"atemp3"]/(1000000)))

bike<-cbind(bike,
            bts= bikeTIME*bike[,"season"],
            btw= bikeTIME*bike[,"weather"],
            btt= bikeTIME*bike[,"temp"]
)

test<-cbind(test,
            bts= testTIME*test[,"season"],
            btw= testTIME*test[,"weather"],
            btt= testTIME*test[,"temp"]
)



# bike<-cbind(bike,
#             sqrttemp=sqrt(bike[,"temp"]),
#             sqrtatemp=sqrt(bike[,"atemp"]),
#             sqrthumid=sqrt(bike[,"humidity"]),
#             sqrtwindspeed=sqrt(bike[,"windspeed"]))
# 
# test<-cbind(test,
#             sqrttemp=sqrt(test[,"temp"]),
#             sqrtatemp=sqrt(test[,"atemp"]),
#             sqrthumid=sqrt(test[,"humidity"]),
#             sqrtwindspeed=sqrt(test[,"windspeed"]))

# bike<-cbind(bike,
#             logtemp=log(bike[,"temp"]))
#loghumid=log(bike[,"humidity"]))
#logwindspeed=log(bike[,"windspeed"]))

# test<-cbind(test,
#             logtemp=log(test[,"temp"]))

#loghumid=log(test[,"humidity"]))
#logwindspeed=log(test[,"windspeed"]))





bike<-data.matrix(bike)
test<-data.matrix(test)
