Q1

activity <- read.csv("activity.csv")

dim(activity)
names(activity)
head(activity)
str(activity)
summary(activity)
sum(is.na(activity$steps))
pairs(activity)
----------------------------------------------------------------------------------------
Q2
  library(ggplot2)
activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")
png("plott1.png")
hist(activity_total_steps$steps)
dev.off()
----------------------------------------------------------------------------------------
Q3
mean(activity_total_steps$steps)
median(activity_total_steps$steps)
-----------------------------------------------------------------------------------------
Q4
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")
png("plott2.png")
plot(average_daily_activity$interval, average_daily_activity$mean,
type = "l", col="darkblue", lwd = 2, xlab="Interval",
ylab="Average number of steps", main="Average number of steps per intervals")
dev.off()
-----------------------------------------------------------------------------------------
Q5
which.max(average_daily_activity$interval)
which.max(average_daily_activity$mean)
average_daily_activity[which.max(average_daily_activity$mean), ]$interval
-----------------------------------------------------------------------------------------
Q6
  sum(is.na(activity$steps))
  imputed_steps <- average_daily_activity$mean[match(activity$interval, average_daily_activity$interval)]
  activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
  total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
  names(total_steps_imputed) <- c("date", "daily_steps") 
---------------------------------------------------------------------------------------
Q7
  png("plott3.png")
  hist(total_steps_imputed$daily_steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
  dev.off()
  mean(total_steps_imputed$daily_steps)
  median(total_steps_imputed$daily_steps)
----------------------------------------------------------------------------------------  
Q8 
  library(ggplot2)
  activityDataNoNA <- activity

  activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
  activityDataNoNA$day <- weekdays(activityDataNoNA$date)
  for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
      activityDataNoNA[i,]$day<-"weekend"
    }
    else{
      activityDataNoNA[i,]$day<-"weekday"
    }
  }
  stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
------------------------------------------------------------------------------------------  
    names(stepsByDay) <- c("interval", "day", "steps")
  library(lattice)
  png("plott4.png")
  xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
         xlab = "Interval", ylab = "Number of steps")
  dev.off()