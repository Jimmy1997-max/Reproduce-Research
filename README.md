# Reproduce-Research
---
title: "Reproduce Research"
author: "Jimmy"
date: "5/22/2020"
output: html_document
---

##Assignment Instructions
1.Code for reading in the dataset and/or processing the data
2.Histogram of the total number of steps taken each day
3.Mean and median number of steps taken each day
4.Time series plot of the average number of steps taken
5.The 5-minute interval that, on average, contains the maximum number of steps
6.Code to describe and show a strategy for imputing missing data
7.Histogram of the total number of steps taken each day after missing values are imputed
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

##Step 1
##Code for reading in the dataset and/or processing the data

```{r, echo = TRUE}
setwd("C:\\Users\\asd\\Documents")
activity <- read.csv("activity.csv")
```

Exploring the basics of this data
```{r, echo = TRUE}
dim(activity)
names(activity)
head(activity)
str(activity)
summary(activity)
sum(is.na(activity$steps))
pairs(activity)
```

##Step 2
##Histogram of the total number of steps taken each day

```{r, echo = TRUE}

  library(ggplot2)
activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")

hist(activity_total_steps$steps)

```


##Step 3
##Mean and median number of steps taken each day
```{r, echo = TRUE}
mean(activity_total_steps$steps)
median(activity_total_steps$steps)
```

##Step 4
##Time series plot of the average number of steps taken
```{r, echo = TRUE}
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")

plot(average_daily_activity$interval, average_daily_activity$mean,
type = "l", col="darkblue", lwd = 2, xlab="Interval",
ylab="Average number of steps", main="Average number of steps per intervals")

```

##Step 5
##The 5-minute interval that, on average, contains the maximum number of steps
```{r, echo = TRUE}
which.max(average_daily_activity$interval)
which.max(average_daily_activity$mean)
average_daily_activity[which.max(average_daily_activity$mean), ]$interval
```

##Step 6
Code to describe and show a strategy for imputing missing data
There are multiple strategies to deal with multiple value imputations.

```{r, echo = TRUE}
sum(is.na(activity$steps))
  imputed_steps <- average_daily_activity$mean[match(activity$interval, average_daily_activity$interval)]
  activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
  total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
  names(total_steps_imputed) <- c("date", "daily_steps")
```

## Step 7
Histogram of the total number of steps taken each day after missing values are imputed

```{r, echo = TRUE}

  hist(total_steps_imputed$daily_steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
 
  mean(total_steps_imputed$daily_steps)
  median(total_steps_imputed$daily_steps)
```

## Step 8
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r, echo=TRUE}
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
```


Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```{r, echo=TRUE}
 names(stepsByDay) <- c("interval", "day", "steps")
  library(lattice)
  
  xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
         xlab = "Interval", ylab = "Number of steps")
 
```


