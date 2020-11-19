# Training data
my<-read.csv("F:/data/Stolen data.csv")

###########
library(e1071)
a<-naiveBayes(Stolen~., data=my)
a


########### testing data 
b<-read.csv("F:/data/Stolen  data test.csv")

############### Prediction on test data 
ab<-predict(a,b,"raw")
ab

b1<-data.frame(cbind(ab[,2],b))
b1
