library(reshape)
library(ggplot2)
library(lattice)

activity.data <- read.table(file="activity.csv",
                            header=TRUE,
                            sep=",",
                            stringsAsFactors=FALSE)
activity.data$date <- as.Date(activity.data$date,"%Y-%m-%d")
activity.melt <- melt (activity.data,id=c("interval","date"),measured="steps",na.rm=TRUE)
activity.daily.sum <- cast (activity.melt,date~variable,fun=sum)
ggplot(data=activity.daily.sum, aes(x=date, y=steps)) + 
        geom_bar(stat="identity") +
        xlab("Date")+
        ylab("Total Number of Steps per Day")+
        ggtitle("Figure 1. Total Number of Steps per Day)")
activity.daily.mean <- cast (activity.melt,date~variable,fun=mean)
activity.daily.median <- cast (activity.melt,date~variable,fun=median)
activity.interval.mean <- cast (activity.melt,interval~variable,fun=mean)
activity.interval.mean[which.max(activity.interval.mean$steps),1]
ggplot(data=activity.interval.mean, aes(x=interval, y=steps)) + 
        geom_line(aes(group=1)) +
        xlab("Five Minute Interval")+
        ylab("Mean Number of Steps")+
        ggtitle("Figure 2. Mean Number of Steps for Each Interval Over All Days")
missing.data <- is.na(activity.data)
sum(missing.data)
temp <- merge(activity.data[missing.data,],activity.interval.mean,by="interval")
activity.clean <- activity.data
activity.clean[missing.data,1]<-temp[order(temp$date,temp$interval),4]
activity.clean.melt <- melt (activity.clean,id=c("interval","date"),measured="steps",na.rm=TRUE)
activity.clean.sum <- cast (activity.clean.melt,date~variable,fun=sum)
ggplot(data=activity.clean.sum, aes(x=date, y=steps)) + 
        geom_bar(stat="identity") +
        xlab("Date")+
        ylab("Total Number of Steps per Day")+
        ggtitle("Figure 3. Total Number of Steps per Day with Mean Interval Imputed Results")
activity.clean.daily.mean <- cast (activity.clean.melt,date~variable,fun=mean)
activity.clean.daily.median <- cast (activity.clean.melt,date~variable,fun=median)
activity.clean$weekday <- weekdays(activity.clean$date)
weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
status <- c("weekday","weekday","weekday","weekday","weekday","weekend","weekend")
daysofweek <- data.frame(weekday,status)
activity.clean.coded <- merge(activity.clean,daysofweek,by="weekday")
activity.clean.coded <- activity.clean.coded[order(activity.clean.coded$date,activity.clean.coded$interval),]

