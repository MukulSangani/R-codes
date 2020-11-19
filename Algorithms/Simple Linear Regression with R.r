View(faithful)
attach(faithful)  #attach the data frame
names(faithful)
str(faithful)
head(faithful,10)
input<-faithful
# behaviour and missing value and univariate analysis
######## EDA##############

########## Univariate Analysis############
summary(faithful)

## another way missing value-----
sapply(faithful,function(x) sum(is.na(x)))

### scatter plot and correlation
plot(waiting,eruptions)
plot(eruptions~waiting)
cor(faithful)
input<- faithful
### Regression model
Model=lm(eruptions~waiting,data = input)
summary(Model)
#anova(Model)
#### Assumption of regression model

## below command will plot all the asumption plot 2*2 format
par(mfrow=c(2,2))
plot(Model)

library(lmtest)
dwtest(Model)
### linearity
plot(eruptions~waiting)
 
 #  Prediction of 80 waiting
 y=-1.87402+0.07563*80
 y
 
 # Prediction Method on entire testing data set
 newdata=data.frame(waiting=80)
 predict(Model,newdata)
 
 ## JUST TO CHECK MATHEMATICALLY of linear Model
 input$Fitted_value<-Model$fitted.values
 input$Residual<-Model$residuals
 sum(input$Residual)
 
 # Best fit line
 plot(eruptions~waiting, data = input)
 abline(Model,col="red")
 