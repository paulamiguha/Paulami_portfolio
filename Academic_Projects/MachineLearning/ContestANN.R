# Code for Kaggle Contest
# Developed By : Paulami Guha
#================================
#Packages
#============
library(readr)
library(tidyverse)
library(dplyr)    # alternatively, this also loads %>%
install.packages("stats")
library(grid, MASS)
require(neuralnet)
library(zoo)
#============

setwd("/Users/moon/Desktop/AdvMachinelearning/Contest") # Set Home Directory
getwd()

# Functions start here
#=========================
# Function to change submission format from wide to long
wide2long <- function(preddata)  {
  
  ##### Change the file path!!! #####
  submission = preddata
  submission$IDTime = paste(submission$ID, submission$Time)
  ## Find the row index of the test set
  k = as.vector(by(submission$ID,submission$ID,length)) 
  a = lapply(k, function(x){seq(1,x,by=16)})
  m = c(0,cumsum(k)[-80])
  b = lapply(1:80,function(i){a[[i]]+m[i]})
  i = unlist(b)
  ## Remove the test set from pr ediction and generate the long table
  dat = submission[-i,-(1:2)]
  dat = reshape2::melt(dat,id="IDTime") 
  dat$ID = paste(dat$IDTime, dat$variable)
  ## Write the data into a new submission file
  write.csv(dat[,4:3], file="sample_submission_long.csv", row.names=FALSE)  
  
}

imputefn <- function(ds) {
  # Imputation function for ADS-B Flight Data
  
  example = ds
  for (i in unique(ds$ID)){
    for (j in 3:7){
      parameter = example[example$ID == i, j]
      complete_parameter = na.omit(parameter)
      nRecords = length(complete_parameter)
      locRecords = which(parameter %in% complete_parameter)[-nRecords] 
      for (k in 1:15){
        parameter[locRecords+k] = (1-k/16)*complete_parameter[-nRecords] + (k/16)*complete_parameter[-1]
      }
      example[example$ID == i, j] = parameter }
  }
  return(example)
}

# End of functions

#==== Main Program starts here
traindata <- read.csv("train.csv")
traindata <- data.frame(traindata)
testdata <- read.csv("test.csv")
testdata <- data.frame(testdata)

#Imputing test data
#Making data after 15 seconds NAs for all variables except ID and Time
natraindata <- arrange(traindata,ID,Time)
maxId =max(traindata$ID)
maxTime=max(traindata$Time[ID==maxId])
attach(natraindata)
natraindata<- mutate(natraindata,Latitude = ifelse(Time==1 | (Time%%16-1)==0| (ID==maxId & Time==maxTime),Latitude ,NA))
natraindata<- mutate(natraindata,Longitude = ifelse(Time==1 | (Time%%16-1)==0| (ID==maxId & Time==maxTime),Longitude,NA))
natraindata<- mutate(natraindata,AltB = ifelse(Time==1 | (Time%%16-1)==0| (ID==maxId & Time==maxTime),AltB,NA))
natraindata<- mutate(natraindata,GndSpd = ifelse(Time==1 | (Time%%16-1)==0| (ID==maxId & Time==maxTime),GndSpd,NA))
natraindata<- mutate(natraindata,VSpd = ifelse(Time==1 | (Time%%16-1)==0| (ID==maxId & Time==maxTime),VSpd,NA))

# Create Linear Interpolated Data based on training dataset with NA values

#imptrain=imputefn(natraindata)
#imptrain <- data.frame(imptrain)

imptrain=data.frame(na.approx(natraindata))


# Create Linear Interpolated Data based on test dataset
#imptest=imputefn(testdata)
#imptest <- data.frame(imptest)

imptest=data.frame(na.approx(testdata))

# Calculate difference between actual and Imputed - training set

innerJoindata <- inner_join(traindata,imptrain,by=c("ID","Time"), suffix = c(".act", ".imp"))

innerJoindata$Latitude.diff <- innerJoindata$Latitude.act - innerJoindata$Latitude.imp
innerJoindata$Longitude.diff <- innerJoindata$Longitude.act - innerJoindata$Longitude.imp
innerJoindata$AltB.diff <- innerJoindata$AltB.act - innerJoindata$AltB.imp
innerJoindata$GndSpd.diff <- innerJoindata$GndSpd.act - innerJoindata$GndSpd.imp
innerJoindata$VSpd.diff <- innerJoindata$VSpd.act - innerJoindata$VSpd.imp

# Create input dataset for ANN - data with variables before and after gap of 15 secs 

#Train
innerJoinfil <-innerJoindata %>% filter(Time==1 | (Time%%16-1==0))


# ANN Starts Here

# Inputs to ANN will be records before and after gap
# Target variable will be difference between Imputed Variables and actual variables from training set


nnLat <- neuralnet(Latitude.diff+Longitude.diff+AltB.diff+GndSpd.diff+VSpd.diff~Time+Latitude.imp+Longitude.imp+AltB.imp+GndSpd.imp+VSpd.imp,data=innerJoinfil, hidden=3,act.fct = "logistic",
             linear.output = FALSE)

plot(nnLat)

# Constructing test data for ANN
innerJointest <- inner_join(testdata,imptest,by=c("ID","Time"), suffix = c(".act", ".imp"))
innerJointest$Latitude.diff <- innerJointest$Latitude.act - innerJointest$Latitude.imp
innerJointest$Longitude.diff <- innerJointest$Longitude.act - innerJointest$Longitude.imp
innerJointest$AltB.diff <- innerJointest$AltB.act - innerJointest$AltB.imp
innerJointest$GndSpd.diff <- innerJointest$GndSpd.act - innerJointest$GndSpd.imp
innerJointest$VSpd.diff <- innerJointest$VSpd.act - innerJointest$VSpd.imp



pred <- data.frame(predict(nnLat, innerJointest))

colnames(pred) <- c("Latitude","Longitude","AltB","GndSpd","VSpd")
pred$ID=innerJointest$ID
pred$Time=innerJointest$Time
pred <-pred[,c(6,7,1,2,3,4,5)]

#
finaldata<-data.frame(pred$ID)
colnames(finaldata) <- c("ID")
finaldata$Time<-pred$Time
finaldata$Latitude <- pred$Latitude+innerJointest$Latitude.imp
finaldata$Longitude <- pred$Longitude+innerJointest$Longitude.imp
finaldata$AltB <- pred$AltB+innerJointest$AltB.imp
finaldata$GndSpd <- pred$GndSpd+innerJointest$GndSpd.imp
finaldata$VSpd <- pred$VSpd+innerJointest$VSpd.imp


wide2long(finaldata)
write.csv(finaldata,"sample_submission_wide.csv")