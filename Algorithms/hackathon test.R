test=read.csv("E:/data/test.csv")
names(test)
str(test)
input<- test[,c("category_id","subscriber","Tag_count","Trend_tag_count","comment_count","likes","dislike")]
names(input)
str(input)
summary(input)



input$category_id=as.integer(input$category_id)
is.integer(input$category_id)
input$Tag_count=as.integer(input$Tag_count)
is.integer(input$Tag_count)
input$Trend_tag_count=as.integer(input$Trend_tag_count)
is.integer(input$Trend_tag_count)
input$dislike=as.integer(input$dislike)
is.integer(input$dislike)
input$views=as.integer(input$views)

is.integer(input$views)

str(input)
## another way missing value-----
sapply(input,function(x) sum(is.na(x)))

## replacing missing values with mean 
input$likes[is.na(input$likes)] <- round(mean(input$likes, na.rm = TRUE))
input$subscriber[is.na(input$subscriber)] <- round(mean(input$subscriber, na.rm = TRUE))
input$comment_count[is.na(input$comment_count)] <- round(mean(input$comment_count, na.rm = TRUE))
input$category_id[is.na(input$category_id)] <- round(mean(input$category_id, na.rm = TRUE))
sapply(input,function(x) sum(is.na(x)))

#Prediction on test Data
Y_pred=1570.333+(-20.51*(input$Trend_tag_count))+0.0004267*(input$comment_count)
Y_pred

input$Y_pred
input
write.csv(input,file = "E:/data/mukuldata.csv")

