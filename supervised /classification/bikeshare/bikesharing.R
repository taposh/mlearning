#--------------------------------------------------------
#  Main Program for kaggle-bike-sharing
#  Taposh Roy @taposh_dr
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
#source("/Users/taposh/workspace/kaggle/bikeshare/visualize.R")
#Utility Functions
source("/Users/taposh/workspace/kaggle/bikeshare/utils.R")

#################################
# Deep Learning
#################################

library(h2o)
#localH2O <- h2o.init(ip = 'http://172.16.23.21/', port = 54321, max_mem_size = '24g')
localH2O = h2o.init()

bike_train.hex = h2o.importFile(localH2O, path = "/Users/taposh/workspace/kaggle/bikeshare/train.csv", key = "bike_train.hex")
bike_test.hex = h2o.importFile(localH2O, path = "/Users/taposh/workspace/kaggle/bikeshare/test.csv",
                               key = "bike_test.hex")


bike_model <- h2o.deeplearning(x=1:9,
                                y=12, #response (ignored - pick any non-constant column)
                                data=bike_train.hex,
                                activation="Tanh",
                                classification=F,variable_importances=T,
                                hidden=c(100,200,300),
                                epochs=25,nfolds=25
)

bike_model
dl.VI =bike_model@model$varimp
plot(dl.VI)

prd.test.dl <- h2o.predict(bike_model, bike_test.hex)

prd.test.dl <- as.data.frame(prd.test.dl)

head(prd.test.dl,10)


#################################
### Method #1 Matrix Computation
#################################
#Factorengineering
source("/Users/taposh/workspace/kaggle/bikeshare/factorengineering_bike.R")
source("matrix.R")
#################################
### Method #2 Models
#################################
#Factorengineering
source("/Users/taposh/workspace/kaggle/bikeshare/factorengineering_v1.R")
colnames(bike)
colnames(test)
source("models_nongrid.R")
#################################
### Method #2 Models
#################################
#Factorengineering
source("models.R")
