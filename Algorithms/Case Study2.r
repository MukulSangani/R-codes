#### mtcars data
library(car)
input<-mtcars
names(input)

#Correlation
cor(input)

# Model1 Creation
model<-lm(mpg~.,data = input)
summary(model)

# variance inflation factor
library(car)
vif(model)

## Removing Disp based on Vif value
model<-lm(mpd~.,-disp,data = input)  ##### enter method
summary(model)
vif(model)

## doing forward and backward and stepwise regression
model1<-lm(mpg~.,data=input)  ####### enter method
summary(model1)
vif(model1)

## backward method
model2<-step(lm(mpg~.,data = input),direction = "backward")
summary(model2)

# Variance inflation factor
vif(model2)

# Assumption of Model
par(mfrow=c(2,2))
plot(model2)
library(lmtest)
dwtest(model2)
ncvTest(model2)

## stepwise method
model3<-step(lm(mpg~.,data = input), directon="both")
summary(model3)
vif(model3)

# Forward Method
model4<-step(lm(mpg~.,data = input),direction = "forward")
summary(model4)
vif(model4)
