################### Importing data ##################
Employee<-read.csv("E:/data science praticals/employee.csv")

################### To check Data Type ##################
str(Employee)
names(Employee)

#################### Just taking a subset of column for model building ##################
Employee = subset(Employee, select = -c(7,9,10,27,22))
names(Employee)
str(Employee)

################## Data Conversion#############################

# Taken All integar variable &  convert to numeric
Employee1 = subset(Employee, select = -c(2,3,5,7,9,13,15,19))
names(Employee1)
str(Employee1)
Employee1<-data.frame(apply(Employee1, 2, as.numeric))
str(Employee1)

# Taken all categorical variable
Employee2 = subset(Employee, select = c(2,3,5,7,9,13,15,19))
str(Employee2)

# combined Numeric &categorical Variable 
Employee<-data.frame(Employee2,Employee1)
str(Employee)
names(Employee)

Employee$Attrition<-as.factor(Employee$Attrition)

##############333#Data Partition#789#######################
set.seed(231)
library(caret)
train<-createDataPartition(Employee$Attrition,p=0.7,list=FALSE)
training<-Employee[train,]
testing<-Employee[-train,]

############ To Verified the Partition
prop.table(table(Employee$Attrition))
prop.table(table(training$Attrition))
prop.table(table(testing$Attrition))

############## Logistics Regression ##########

### Dataset need to be used is Training & Testing
names(training)


###################### Variable Selection Method #####################  
#relevel(Pclass,ref = 2)
Model2 <- step(glm(Attrition ~.,
                   family='binomial', data=training),direction = "both")
summary(Model2)

anova(Model2,test='Chisq')

### Removing Variable Maually based on Anova
Model2 <- step(glm(Attrition ~.-Gender-RelationshipSatisfaction
                   -StockOptionLevel-TotalWorkingYears-YearsAtCompany,
                   family='binomial', data=training),direction = "both")
summary(Model2)
anova(Model2,test='Chisq')


###################### Accuracy of model ##################### 
Acc(Model2)

##################### # odds Ratio ##################### 
exp(coef(Model2))
##################### ## Prediction on testing data ##################### 

testing$probs <-predict(Model2, testing, type='response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,'Yes','No'))

###################### Accuracy of testing data  #####################
library(e1071)
confusionMatrix(testing$Predict,testing$Attrition,positive = "Yes")

