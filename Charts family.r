####################Pie charts ##################
# create the data for graph
x<-c(44,12,39,53)
labels<-c("California","Paris","Moscow","Mumbai")
# plot the chart
pie(x,labels)

# create the data for graph
x<-c(44,12,39,53)
labels<-c("California","Paris","Moscow","Mumbai")
pct<-round(x/sum(x)*100)
lbls<-paste(labels,pct)
lbls<-paste(lbls,"%",sep = "")
pei(x,labels=lbls,col=rainbow(length(lbls)),main="city_pie_chart"
    
############### bar charts ##################
#1) simple bar chart
h<-c(25,12,70,55)
 barplot(h)
 
 h<-c(20,12,70,55)
 m<-c("Delhi","Mumabi","Bangaluru","Pune")
 barplot(h,
         xlab="Month",
         ylab="happy index",
         col="red",
         names.arg = m, # name plotted below each bar 
         main = "xyz",
         border = "black")
#,horiz=TRUE)  #if want horizontal

 #2) stacked bar chart
 counts<-table(mtcars$vs,mtcars$gear)
 barplot(count,
         main = "car distribution by gears & vs",
         xlab = "number of gear",
         col = c("darkblue","red"),
         legend=rownames(counts)),
 
 # Grouped bar chart
 counts<-table(mtcars$vs,mtcars$gear),
 barplot(counts,
         main = "car distribtion by gears & vs",
         xlab = "number of gear",
         col = c("darkblue","red"),
         legend=rownames(counts),besides=TRUE),
 
 ###################### Histogram charts #####################
 
 # plotting distribution of Airpassengers data
 hist(AirPassengers,
      main="histogram for Air Passenger",
      xlab="Passengers",
      border = "red",
      col = "blue",
      xlim = c(100,700),
      breaks = 5) # break is usd to mention the width of each bar
 
 #################  BoxPlot  ################
 boxplot(mpg~cyl,
         data=mtcars,
         xlab="number of cylinder",
         ylab="miles per gallon",
         main="mileage data")
 
 ################### Scatter plot #################
 plot(mtcars$wt,mtcars$mpg)
 ##################### Line Plot ################
 v<- c(12,1,25,83,43)
 pot(v,type="o",col="red")
 v<-c(12,14,28,5,44)
 t<-c(15,8,8,10,13)
 plot(v,type = "o",col="red")
 lines(t,type = "o",col="blue")
 
 
 