################# Linear Regression #################################

################### Importing data ##################
library(openxlsx)
Employee<-read.xlsx("E:/data science praticals/hr_dataset.xlsx")
str(Employee)

############# Data Conversion
Employee$Gender <-as.numeric(as.factor(Employee$Gender))
Employee$JobRole <-as.numeric(as.factor(Employee$JobRole))
Employee$EducationField <-as.numeric(as.factor(Employee$EducationField))


hist(Employee$MonthlyIncome)
boxplot(Employee$MonthlyIncome)


##############333#Data Partition#789#######################
set.seed(231)
library(caret)
train<-createDataPartition(Employee$MonthlyIncome,p=0.7,list=FALSE)
training<-Employee[train,]
testing<-Employee[-train,]

############## backward method
model3 <- lm(MonthlyIncome~., data = training)
summary(model3)

model3 <- step(lm(MonthlyIncome~., data = training),direction = "both")
summary(model3)
anova(model3)

# Variance Inflation Factor
library(car)
vif(model3)

############################## Increasing the Accuracy of Model
Employee$MonthlyIncome<-log(Employee$MonthlyIncome)
hist(Employee$MonthlyIncome)
boxplot(Employee$MonthlyIncome)

##############333#Data Partition#789#######################
set.seed(231)
library(caret)
train<-createDataPartition(Employee$MonthlyIncome,p=0.7,list=FALSE)
training<-Employee[train,]
testing<-Employee[-train,]

############## backward method
model3 <- lm(MonthlyIncome~., data = training)
summary(model3)

model3 <- step(lm(MonthlyIncome~., data = training),direction = "both")
summary(model3)
anova(model3)

# Variance Inflation Factor
library(car)
vif(model3)

training$log_Predicted<-model3$fitted.values

training$Original<-exp(training$MonthlyIncome)
training$Predicted<-exp(model3$fitted.values)


############ Prediction on Testing data
testing$log_Predicted<-predict(model3,testing)

testing$Original<-exp(testing$MonthlyIncome)
testing$Predicted<-exp(testing$log_Predicted)


############# comparing Training & Testing Performance ################

library(MLmetrics)
exp(MSE(y_pred = training$log_Predicted, y_true = training$MonthlyIncome))

library(MLmetrics)
exp(MSE(y_pred = testing$log_Predicted, y_true = testing$MonthlyIncome))

boxplot(Employee)
