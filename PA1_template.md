---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())
Clean old data, prepare for file download and setting home directory

```r
rm(list=ls())
setInternet2(use = TRUE)
setwd("E:/R")
```

Donwload file from Internet and read it 

```r
 if (!file.exists("repdata-data-activity.zip")) { 
     fileUrl1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
     download.file(url=fileUrl1, destfile="repdata-data-activity.zip")
   unzip("repdata-data-activity.zip") 
 }
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
suppressMessages(require(lattice))
activity["date"] <- as.Date(activity$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
 1. Make a histogram of the total number of steps taken each day

```r
steps_per_day <- aggregate(steps ~ date, data = activity, sum)

hist(steps_per_day$steps, main = paste("Total Number of Steps Each Day"), col="blue", xlab="Number of Steps", ylab = "Days")
```

<img src="figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />
 2. Calculate and report the mean and median total number of steps taken per day
The mean of the  total number of steps taken per day

```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```
 The median of the total number of steps taken per day

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_per_interval <- aggregate(steps ~ interval, activity, mean)

library(lattice)

xyplot(steps_per_interval$steps ~ steps_per_interval$interval, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",type="l")
```

<img src="figure/unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" style="display: block; margin: auto;" />

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Max number of steps

```r
max(steps_per_interval$steps)
```

```
## [1] 206.1698
```
Interval number containing max number

```r
which.max(steps_per_interval$steps)
```

```
## [1] 104
```
The starting minute for this internal with max number of steps

```r
steps_per_interval[which.max(steps_per_interval$steps),1]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to use the mean for that 5-minute interval to fill each missing value in the steps.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityNew <- activity

for (i in 1:nrow(activityNew)){
  if (is.na(activityNew$steps[i])){
    interval_val <- activityNew$interval[i]
    row_id <- which(steps_per_interval$interval == interval_val)
    steps_val <- steps_per_interval$steps[row_id]
    activityNew$steps[i] <- steps_val
  }
}

head(activityNew)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
table_date_steps_imputed <- aggregate(steps ~ date, activityNew, sum)

hist(table_date_steps_imputed$steps, main="(Imputed) Total Number of Steps Each Day", col="blue", xlab="Number of Steps", ylab = "Days")
```

<img src="figure/unnamed-chunk-13-1.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" style="display: block; margin: auto;" />

The mean of the total number of steps taken per day

```r
mean(table_date_steps_imputed$steps)
```

```
## [1] 10766.19
```
 The median of the total number of steps taken per day

```r
median(table_date_steps_imputed$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activityNew$day <- weekdays(activityNew$date)
activityNew$day_type <- c("weekday")
for (i in 1:nrow(activityNew)){
  if (activityNew$day[i] == "Saturday" || activityNew$day[i] == "Sunday"){
    activityNew$day_type[i] <- "weekend"
  }
}
activityNew$day_type <- as.factor(activityNew$day_type)
table_interval_steps_imputed <- aggregate(steps ~ interval+day_type, activityNew, mean)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)

xyplot(table_interval_steps_imputed$steps ~ table_interval_steps_imputed$interval|table_interval_steps_imputed$day_type, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

<img src="figure/unnamed-chunk-17-1.png" title="plot of chunk unnamed-chunk-17" alt="plot of chunk unnamed-chunk-17" style="display: block; margin: auto;" />
