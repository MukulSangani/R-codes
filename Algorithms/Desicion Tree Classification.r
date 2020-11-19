# Objectives : to predict whether a employee will exist or not using Decision Tree


################### Importing Data #################
bankcust<-read.csv("F:/data/employee (1).csv")

######## To check data type ###########
str(bankcust)
names(bankcust)

##### Just taking a subset of column for model building ########################
bankcust=subset(bankcust,select=-c(7,9,10,27,22))
str(bankcust)

################# Data Conversion ##################
# All int variable to convert to numeric
bankcust1=subset(bankcust,select=-c(2,3,5,7,9,13,15,19))
names(bankcust1)
str(bankcust1)
bankcust1<-data.frame(apply(bankcust1,2,as.numeric))
str(bankcust1)

# All int variable to convert to numeric 
bankcust2=subset(bankcust,select=-c(2,3,5,7,9,13,15,19))
str(bankcust2)
bankcust<-data.frame(bankcust2,bankcust1)
str(bankcust)
names(bankcust)

############### 333#Data Partition ################
set.seed(231)
library(caret)
train<-createDataPartition(bankcust$Attrition,p=0.7,list=FALSE)
training<-bankcust[train,]
testing<-bankcust[-train,]

############## building model & Plotting model ###############
library(rpart)
Model=rpart(Attrition~.,data=training,method="class")
summary(Model)
