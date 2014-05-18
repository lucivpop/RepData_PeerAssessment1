data<-read.csv("activity.csv",header=T,colClasses=c("numeric","character","numeric"))
#head(data)
#str(data)
data$date<-as.Date(data$date,format="%Y-%m-%d",tz="")
data[order(data$date, data$interval),]
#tail(data,20)


totalStepsPerDay<-aggregate(steps ~ date, data, sum)
#str(totalStepsPerDay)

hist(totalStepsPerDay$steps,breaks=10)
mean(totalStepsPerDay$steps)
median(totalStepsPerDay$steps)

averageStepsPerInterval<-aggregate(steps ~ interval, data, mean)
#str(averageStepsPerInterval)

plot(averageStepsPerInterval$interval,averageStepsPerInterval$steps,type="l")
max(averageStepsPerInterval$steps)
i<-which(averageStepsPerInterval$steps==max(averageStepsPerInterval$steps))
averageStepsPerInterval$interval[i]

#sum(is.na(data$interval))
#sum(is.na(data$date))
#sum(is.na(data$steps))
#is.na(data$steps)==TRUE

data.replacena<-data
str(data.replacena)
l<-nrow(data.replacena)
for(j in 1:l){
  if(is.na(data.replacena$steps[j])){
    interval<-data.replacena$interval[j]
    nrSteps<-averageStepsPerInterval$steps[averageStepsPerInterval$interval==interval]
    data.replacena$steps[j]<-nrSteps
  }
}


totalStepsPerDayCor<-aggregate(steps ~ date, data=data.replacena, sum)
hist(totalStepsPerDayCor$steps,breaks=10)
mean(totalStepsPerDayCor$steps)
median(totalStepsPerDayCor$steps)

data.replacena$weekday<-as.factor(weekdays(data.replacena$date))
weekend.subset <- subset(data.replacena, weekday %in% c("Saturday","Sunday"))
weekday.subset <- subset(data.replacena, !weekday %in% c("Saturday","Sunday"))

averageStepsPerIntervalWE<-aggregate(steps ~ interval, weekend.subset, mean)
averageStepsPerIntervalWD<-aggregate(steps ~ interval, weekday.subset, mean)

par(mfrow=c(2,1))
plot(averageStepsPerIntervalWD$interval,averageStepsPerIntervalWD$steps,type="l")
plot(averageStepsPerIntervalWE$interval,averageStepsPerIntervalWE$steps,type="l")

