

setwd("/Users/taposh/workspace/kaggle/bikeshare/")

#pairs(bike)
op <- par(mfrow = c(2, 2), pty = "s")  
plot(bike[,"count"],bike[,"temp"], main="Scatterplot of count vs. temp")
plot(bike[,"count"],bike[,"atemp"], main="Scatterplot of count vs. atemp")
plot(bike[,"count"],bike[,"humidity"], main="Scatterplot of count vs. humidity")
plot(bike[,"count"],bike[,"windspeed"], main="Scatterplot of count vs. windspeed")
abline(0,1)

# (hist(bike[,"atemp"]))
# (hist(bike[,"temp"]))
# (hist(bike[,"humidity"]))
# (hist(bike[,"windspeed"]))
# 
# plot(hist(test[,"count"]))
# plot(hist(test[,"season"]))
# plot(hist(test[,"humidity"]))
# plot(hist(test[,"windspeed"]))

bike$datetime=strptime(as.character(bike$datetime), format="%Y-%m-%d %H:%M:%S")
bike$weekday = as.factor(weekdays(bike$datetime))
bike$hour = as.numeric(bike$datetime$hour)

#pivot table
WeekHour=aggregate(count ~ + hour+ weekday, data =bike, FUN=mean)

#Line chart
ggplot(WeekHour, aes(x=hour, y=count)) + geom_line(aes(group=weekday, color=weekday),size=2,alpha=0.5)

#Heat map
ggplot(WeekHour, aes(x=hour, y=weekday)) + geom_tile(aes(fill = count))+ scale_fill_gradient(low="white", high="blue")