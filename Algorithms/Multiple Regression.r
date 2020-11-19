#### Multiple Regression
### mtcars data
  
#### where are taking specific column to Predict mpg car
input<- mtcars[,c("mpg","disp","hp","wt")]


#column names
names(input)

# behaviour and mising values and univariate analysis
summary(input)

## another way missing value-----
sapply(input,function(x) sum(is.na(x)))

# Correlation Matrix & plot(Scatter plot) , co-linearity & multi colinearity
attach(input)
cor(input)
plot(wt,hp)

#Model Building
model<-lm(mpg~disp+hp+wt,data = input)
summary(model)

#Removing variable based on P-value
model2<-lm(mpg~hp+wt,data = input)
summary(model2)

#Assumption of Model
par(mfrow=c(2,2))
plot(model2)

#Prediction on test Data
y=37.22727-0.03177*110-3.87783*3.435
y



### Task ###
money<-read.csv(file.choose())
str(money)
money$GDP<-as.numeric(money$GDP)
