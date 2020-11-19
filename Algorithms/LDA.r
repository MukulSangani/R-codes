Data=read.csv("F:/data/telecom_lda.csv")
str(Data)

Data$Customer.segment<-as.factor(Data$Customer.segment)
str(Data)

head(Data)
tail(Data)
attach(Data)
names(Data)

# Assumption of LDA
#H0:- the observe co-variance matrices of the dependent 
# variable are not equal across group
# H11:- the observe co-variance matrices of the dependent 
# variable ar equal across group 

library(rattle)
library(heplots)
boxM(Data[,1:5],Customer.segment)

Data$Customer.segment<-as.factor(Data$Customer.segment)

str(Data)

# using subset function
newdata1<- Data[which(Data$Customer.segment=="1"),]
newdata2<- Data[which(Data$Customer.segment=="2"),]
newdata3<- Data[which(Data$Customer.segment=="3"),]

# 2 assumption scatter plot & correlations
library(psych)
pairs.panels(newdata1[,-6],gap=0,
                           bg=c("red","yellow","blue")[newdata1$Customer.segment],
                           pch = 21)
pairs.panels(newdata2[,-6],gap=0,
                          bg=c("red","yellow","blue")[newdata2$Customer.segment],
                          pch=21)
pairs.panels(newdata3[,-6],gap=0,
             bg=c("red","yellow","blue")[newdata3$Customer.segment],
             pch=21)
# divide the data into test and train
set.seed(11)
library(caret)
train<-createDataPartition(Data$Customer.segment,p=0.7,list = FALSE)
training<-Data[train,]
testing<-Data[-train,]

# scaling the data for better results 
training[-6]=scale(training[-6])
testing[-6]=scale(testing[-6])

# Appying LDA
# if assumption of co-variance is not met them we have to run qda
# in code replace lda by qda
library(MASS)
output=lda(formula=Customer.segment~.,data=training)
output

# to get group wise meean of all variable
tapply(training$number.vmail.messages,training$Customer.segment,mean)

attributes(output)

result<-predict(output,training)
result

#ldahist(data=result$x[,1],g=training$Customer.segment)
#ldahist(data=result$x[,2],g=training$Customer.segment)

# Classification on 2 variable 
library(klaR)
library(devtools)
partimat(Customer.segment~.,data=training,method="lda")
#partimat(Customer.segment~.,data=training,method="qda")

# confusion matrix and accuracy on taining data
training$result<-predict(output,training)$class
confusionMatrix(training$result,training$Customer.segment)

# confusion Matrix and accuracy on testing data
testing$result<-predict(output,testing)$class
confusionMatrix(testing$result,testing$Customer.segement)

# Note1:- if the assumption is not met then LDA is not applicable then used QDA
# Note2:- if both the assumption is not met then LDA&QDA both not applicable
# then used Multi-nomial naive bayes or logistics

# note3:-  if its mandatory to used lda or qda then data reduction technique
# should be used like PCA and take a output of pca column and then run lda on pca column