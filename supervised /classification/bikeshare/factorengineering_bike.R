#--------------------------------------------------------
#  Factor Engineering for kaggle-bike-sharing
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

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
# bike1<- model.matrix(~(bikeTIME+bike[,"windspeed"]+bike[,"temp"]+bike[,"atemp"]+bike[,"humidity"])^5,bike)
# test1<- model.matrix(~(testTIME+test[,"windspeed"]+test[,"temp"]+test[,"atemp"]+test[,"humidity"])^5,test)
# 
# colnames(bike)
# colnames(test)
# 
# 
# bike <- cbind(bike,bike1)
# test <- cbind(test,test1)

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
            btt= bikeTIME*bike[,"temp"],
            btsaweather= bikeTIME*bike[,"season"]*bike[,"weather"],
            btsaweathertemp= bikeTIME*bike[,"season"]*bike[,"weather"]*bike[,"temp"]
)

test<-cbind(test,
            bts= testTIME*test[,"season"],
            btw= testTIME*test[,"weather"],
            btt= testTIME*test[,"temp"],
            btsaweather= testTIME*test[,"season"]*test[,"weather"],
            btsaweathertemp= testTIME*test[,"season"]*test[,"weather"]*test[,"temp"]
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






#################################################################
#  Matrix Fun
#################################################################

A<-cbind(bike[,"hour"],bike[,"dayof"])
B<-cbind(test[,"hour"],test[,"dayof"])

A2<-c()
B2<-c()

for(i in unique(B[,1])){
  for(j in unique(B[,2])){
    TEMPA<-matrix(data=0,ncol=1,nrow=nrow(bike))
    colnames(TEMPA) <-paste("tmp", toString(j),toString(i), sep="_")
    TEMPB<-matrix(data=0,ncol=1,nrow=nrow(test))
    colnames(TEMPB) <-paste("tmp", toString(j),toString(i), sep="_")
    TEMPA[A[,1]==i & A[,2]==j,1]<-1
    TEMPB[B[,1]==i & B[,2]==j,1]<-1
    A2<-cbind(A2,tmp=TEMPA)
    B2<-cbind(B2,tmp=TEMPB)
  }}

#Using hour & Season
C<-cbind(bike[,"hour"],bike[,"season"])
D<-cbind(test[,"hour"],test[,"season"])

C2<-c()
D2<-c()

for(i in unique(C[,1])){
  for(j in unique(C[,2])){
    TEMPC<-matrix(data=0,ncol=1,nrow=nrow(bike))
    colnames(TEMPC) <-paste("tmp_c", toString(j),toString(i), sep="_")
    TEMPD<-matrix(data=0,ncol=1,nrow=nrow(test))
    colnames(TEMPD) <-paste("tmp_c", toString(j),toString(i), sep="_")
    TEMPC[C[,1]==i & C[,2]==j,1]<-1
    TEMPD[D[,1]==i & D[,2]==j,1]<-1
    C2<-cbind(C2,tmp_c=TEMPC)
    D2<-cbind(D2,tmp_c=TEMPD)
  }}

#head(bike)


bike<-cbind(bike,tmp=A2)
test<-cbind(test,tmp=B2)

bike<-cbind(bike,tmp_c=C2)
test<-cbind(test,tmp_c=D2)



#Columns Created
colnames(bike)
colnames(test)








##-----tanhh--------### 
#  test<-cbind(test,tanhhwind2=tanh(2*pi*test[,"windspeed2"]),tanhwind3=tanh(2*pi*test[,"windspeed3"]),tanhumid2=tanh(2*pi*test[,"humid2"]),tanhhumid3=tanh(2*pi*test[,"humid3"]),tanhwind24=tanh(4*pi*test[,"windspeed2"]),tanhwind34=tanh(4*pi*test[,"windspeed3"]),tanhumid24=tanh(4*pi*test[,"humid2"]),tanhhumid34=tanh(4*pi*test[,"humid3"]))
# #             
#  bike<-cbind(bike,tanhwind2=tanh(2*pi*bike[,"windspeed2"]),tanhwind3=tanh(2*pi*bike[,"windspeed3"]),tanhumid2=tanh(2*pi*bike[,"humid2"]),tanhhumid3=tanh(2*pi*bike[,"humid3"]),tanhwind24=tanh(4*pi*bike[,"windspeed2"]),tanhwind34=tanh(4*pi*bike[,"windspeed3"]),tanhumid24=tanh(4*pi*bike[,"humid2"]),tanhhumid34=tanh(4*pi*bike[,"humid3"]))









###################
#
##################

D<-cbind(bike[,"holiday"],bike[,"weather"])
E<-cbind(test[,"holiday"],test[,"weather"])

D2<-c()
E2<-c()

for(i in unique(D[,1])){
  for(j in unique(D[,2])){
    TEMPD<-matrix(data=0,ncol=1,nrow=nrow(bike))
    TEMPE<-matrix(data=0,ncol=1,nrow=nrow(test))
    TEMPD[D[,1]==i & D[,2]==j,1]<-1
    TEMPE[E[,1]==i & E[,2]==j,1]<-1
    D2<-cbind(D2,tmp=TEMPD)
    E2<-cbind(E2,tmp=TEMPE)
  }}



G<-cbind(bike[,"weather"],bike[,"month"])
H<-cbind(test[,"weather"],test[,"month"])

G2<-c()
H2<-c()

for(i in unique(G[,1])){
  for(j in unique(G[,2])){
    TEMPG<-matrix(data=0,ncol=1,nrow=nrow(bike))
    TEMPH<-matrix(data=0,ncol=1,nrow=nrow(test))
    TEMPG[G[,1]==i & G[,2]==j,1]<-1
    TEMPH[H[,1]==i & H[,2]==j,1]<-1
    G2<-cbind(G2,tmp=TEMPG)
    H2<-cbind(H2,tmp=TEMPH)
  }}

J<-cbind(bike[,"hour"],bike[,"month"])
K<-cbind(test[,"hour"],test[,"month"])

J2<-c()
K2<-c()

for(i in unique(J[,1])){
  for(j in unique(J[,2])){
    TEMPJ<-matrix(data=0,ncol=1,nrow=nrow(bike))
    TEMPK<-matrix(data=0,ncol=1,nrow=nrow(test))
    TEMPJ[J[,1]==i & J[,2]==j,1]<-1
    TEMPK[K[,1]==i & K[,2]==j,1]<-1
    J2<-cbind(J2,tmp=TEMPJ)
    K2<-cbind(K2,tmp=TEMPK)
  }}

L<-cbind(bike[,"holiday"],bike[,"month"])
M<-cbind(test[,"holiday"],test[,"month"])

L2<-c()
M2<-c()

for(i in unique(L[,1])){
  for(j in unique(L[,2])){
    TEMPL<-matrix(data=0,ncol=1,nrow=nrow(bike))
    TEMPM<-matrix(data=0,ncol=1,nrow=nrow(test))
    TEMPL[L[,1]==i & L[,2]==j,1]<-1
    TEMPM[M[,1]==i & M[,2]==j,1]<-1
    L2<-cbind(L2,tmp=TEMPL)
    M2<-cbind(M2,tmp=TEMPM)
  }}

N<-cbind(bike[,"dayof"],bike[,"month"])
O<-cbind(test[,"dayof"],test[,"month"])

N2<-c()
O2<-c()

for(i in unique(N[,1])){
  for(j in unique(N[,2])){
    TEMPN<-matrix(data=0,ncol=1,nrow=nrow(bike))
    TEMPO<-matrix(data=0,ncol=1,nrow=nrow(test))
    TEMPN[N[,1]==i & N[,2]==j,1]<-1
    TEMPO[O[,1]==i & O[,2]==j,1]<-1
    N2<-cbind(N2,tmp=TEMPN)
    O2<-cbind(O2,tmp=TEMPO)
  }}

bike<-cbind(bike,day_month=N2)
test<-cbind(test,day_month=O2)

bike<-cbind(bike,holiday_month=L2)
test<-cbind(test,holiday_month=M2)

bike<-cbind(bike,hour_month=J2)
test<-cbind(test,hour_month=K2)

bike<-cbind(bike, holiday_weather=D2)
test<-cbind(test, holiday_weather=E2)

bike<-cbind(bike, weather_month=G2)
test<-cbind(test, weather_month=H2)

# # # # 
# # colnames(bike)
# # 
# # ####Level 2 A2N2
# AN<-cbind(bike[,"weather"],bike[,"dayof"],bike[,"holiday"])
# BO<-cbind(test[,"weather"],test[,"dayof"],test[,"holiday"])
# 
# AN2<-c()
# BO2<-c()
# 
# for(i in unique(AN[,1])){
#   for(j in unique(AN[,2])){
#     for(k in unique(AN[,3])){
#     TEMPAN<-matrix(data=0,ncol=1,nrow=nrow(bike))
#     TEMPBO<-matrix(data=0,ncol=1,nrow=nrow(test))
#     TEMPAN[AN[,1]==i & AN[,2]==j & AN[,3]==k,1]<-1
#     TEMPBO[BO[,1]==i & BO[,2]==j & BO[,3]==k,1]<-1
#     AN2<-cbind(AN2,tmp=TEMPAN)
#     BO2<-cbind(BO2,tmp=TEMPBO)
#   }}}
# 
# bike<-cbind(bike,holiday_month_weather=AN2)
# test<-cbind(test,holiday_month_weather=BO2)
# 
# ###
# ANY<-cbind(bike[,"dayof"],bike[,"hour"],bike[,"month"])
# BOY<-cbind(test[,"dayof"],test[,"hour"],test[,"month"])
# 
# ANY2<-c()
# BOY2<-c()
# 
# for(i in unique(ANY[,1])){
#   for(j in unique(ANY[,2])){
#     for(k in unique(ANY[,3])){
#       TEMPANY<-matrix(data=0,ncol=1,nrow=nrow(bike))
#       TEMPBOY<-matrix(data=0,ncol=1,nrow=nrow(test))
#       TEMPANY[ANY[,1]==i & ANY[,2]==j & ANY[,3]==k,1]<-1
#       TEMPBOY[BOY[,1]==i & BOY[,2]==j & BOY[,3]==k,1]<-1
#       ANY2<-cbind(ANY2,tmp=TEMPANY)
#       BOY2<-cbind(BOY2,tmp=TEMPBOY)
#     }}}
# 
# bike<-cbind(bike,day_hour_month=ANY2)
# test<-cbind(test,day_hour_month=BOY2)
# 
# ######
# 
# ANXW<-cbind(bike[,"weather"],bike[,"holiday"],bike[,"hour"],bike[,"dayof"])
# BOXW<-cbind(test[,"weather"],test[,"holiday"],test[,"hour"],test[,"dayof"])
# 
# ANXW2<-c()
# BOXW2<-c()
# 
# for(i in unique(ANXW[,1])){
#   for(j in unique(ANXW[,2])){
#     for(k in unique(ANXW[,3])){
#       for(l in unique(ANXW[,4])){
#       TEMPANXW<-matrix(data=0,ncol=1,nrow=nrow(bike))
#       TEMPBOXW<-matrix(data=0,ncol=1,nrow=nrow(test))
#       TEMPANXW[ANXW[,1]==i & ANXW[,2]==j & ANXW[,3]==k & ANXW[,4]==l,1]<-1
#       TEMPBOXW[BOXW[,1]==i & BOXW[,2]==j & BOXW[,3]==k & BOXW[,3]==l,1]<-1
#       ANXW2<-cbind(ANXW2,tmp=TEMPANXW)
#       BOXW2<-cbind(BOXW2,tmp=TEMPBOXW)
#     }}}}
# 
# 
# 
# bike<-cbind(bike,weather_holiday_hr_day=ANXW2)
# test<-cbind(test,weather_holiday_hr_day=BOXW2)

#################################################################
#  Categorical Fun
#################################################################

#"holiday","workingday","weather","month","hour","dayof","season"
# bike2<- model.matrix(~(bike[,"holiday"]+bike[,"workingday"]+bike[,"weather"]+bike[,"hour"]+bike[,"dayof"]+bike[,"season"])^6,bike)
# test2<- model.matrix(~(test[,"holiday"]+test[,"workingday"]+test[,"weather"]+test[,"hour"]+test[,"dayof"]+test[,"season"])^6,test)
# 
# bike <- cbind(bike,bike2)
# test <- cbind(test,test2)
# 
# colnames(bike)
# colnames(test)




Categorical<-c("holiday","workingday","weather","month","hour","dayof","season")
for(i in Categorical){
  BT<-bike[i]
  TT<-test[i]
  BT2<-c()
  TT2<-c()
  for(j in unique(BT[,i])){
    TempMat<-matrix(data=0,nrow=nrow(BT),ncol=1)
    colnames(TempMat) <-paste("tmpmat", toString(j),toString(i), sep="_")
    TempMat2<-matrix(data=0,nrow=nrow(TT),ncol=1)
    colnames(TempMat2) <-paste("tmpmat", toString(j),toString(i), sep="_")
    TempMat[BT==j]<-1
    TempMat2[TT==j]<-1
    BT2<-cbind(BT2,TempMat)
    TT2<-cbind(TT2,TempMat2)
  }
  bike<-cbind(bike[,colnames(bike)!=i],BT2)
  test<-cbind(test[,colnames(test)!=i],TT2)
}

bike<-cbind(bike,Number=matrix(data=1,nrow=nrow(bike),ncol=1))
test<-cbind(test,Number=matrix(data=1,nrow=nrow(test),ncol=1))

bike<-data.matrix(bike)
test<-data.matrix(test)


colnames(bike)
#summary(bike)
colnames(test)
#summary(test)
