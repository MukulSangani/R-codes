Credit<-read.csv("F:/data/Credit_Card.csv")
View(Credit)
str(Credit)
names(Credit)
attach(Credit)
Credit$target_30<-as.factor(ifelse(Credit$DPD>30,1,0))
Credit$target_60<-as.factor(ifelse(Credit$DPD>60,1,0))
Credit$target_90<-as.factor(ifelse(Credit$DPD>90,1,0))
table(Credit$target_30)
4922/10000

table(Credit$target_60)
333/10000

table(Credit$target_90)
162/10000

Credit<-Credit[-c(4,5,7)]
## simple Logistics Regression
logit<-glm(target_60~balance,data = Credit,family = "binomial")
summary(logit)
anova(logit,test="Chisq")
### Prediciton try with 2000
testing<-data.frame(balance=2000)
testing.probs<-predict(logit,testing,type="response")
testing.probs

### different way
## simple Logistics Regression
Credit<-read.csv("F:/data/Credit_Card.csv")
Credit$target_60<-as.factor(ifelse(Credit$DPD>60,1,0))
Credit$Dummy<-as.factor(ifelse(Credit$Gender=='M',1,0))
Credit$DPD<-NULL
Credit$Gender<-NULL

### Multiple Logistic Regression
library(caret)
Train<-createDataPartition(Credit$target_60,p=0.7,list = FALSE)
training<-Credit[Train,]
testing<-Credit[-Train,]

logit<-glm(target_60~income+balance+Dummy,family = "binomial",data = training)
summary(logit)

# model including all variable
# relevel(Pclass,ref=2)
logit2<-step(glm(target_60~income+balance+Dummy,family = "binomial",data = training),direction = "both")
summary(logit2)
anova(logit2,test="Chisq")

#accuracy
Acc(logit2)

#odd Ratio
exp(coef(logit2))
logit2$coefficients

# Mathematical calculation check
y=-11.342378671+0.005683168*1161.05785+0.726030094*1
a<-exp(-y)
b<-1+a
c<-1/b
c

# for female
y=-10.6730065+0.005316505*2343.786+0.580975737*0
a<-exp(-y)
b<-1+a
c<-1/b
c

## Prediction on testing data
testing$probs<-predict(logit2,testing,type="response")
testing$Predict<-as.factor(ifelse(testing$probs>0.70,1,0))

# Accuracy of testing data
table(testing$Predict,testing$target_60)
confusionMatrix(testing$target_60,testing$Predict)

# Concordance (testing$target_60,testing$probs)

## Rov Curve
library(ROCR)
# Make predictions on training set
predictTrain=predict(logit2,testing,type="response")
# prediction function
ROCRpred=prediction(predictTrain,testing$target_60)
# performance function
ROCRpref=performance(ROCRpred,"tpr","fpr")
# plot ROC curve
plot(ROCRpref)
library(ROCR)
pred=prediction(testing$probs,testing$target_60)
as.numeric(performance(pred,"auc")@y.values)
