---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
# Reproducible Research: Peer Assessment 1
Hans-Christian Schaefer<br>
15.11.2014

## 0. Load necessary libraries and set locale for weekdays

```r
library(plyr)
library(ggplot2)
library(lattice)
Sys.setlocale(category = "LC_ALL", "C")
```

## 1. Loading and preprocessing the data

```r
## Load the activity data in a data frame
data <- read.csv("./activity.csv")
## Create a sum aggragation of the steps based on the date
sum <- aggregate(data$steps, by=data["date"], FUN=sum)
## Set new column headers for the sum data frame
colnames(sum) <- c("date", "sumofsteps")
```
## 2. What is mean total number of steps taken per day?

```r
## Transform the date into a date type
sum$date <- as.Date(sum$date, "%Y-%m-%d")
## Make a histogram of the total number of steps taken each day
qplot(sumofsteps, data=sum, geom="histogram", binwidth = 500, main = "Histogram of the total number of steps taken each day", xlab = "Steps per Day", ylab = "Count of Days")
```

![plot of chunk mean](figure/mean-1.png) 

```r
## Calculate the mean total number of steps taken per day
mean <- as.double(mean(sum$sumofsteps, na.rm = TRUE))
sprintf("The mean of the total number of steps taken per day is: %f", mean)
```

```
## [1] "The mean of the total number of steps taken per day is: 10766.188679"
```

```r
## Calculate the median total number of steps taken per day
median <- median(sum$sumofsteps, na.rm = TRUE)
sprintf("The median of the total number of steps taken per day is: %f",  median)
```

```
## [1] "The median of the total number of steps taken per day is: 10765.000000"
```

## 3. What is the average daily activity pattern?

```r
## Create a mean aggragation of the steps based on the interval
average <- aggregate(data$steps, by=data["interval"], FUN=mean, na.rm=TRUE)
## Set new column headers for the mean/average data frame
colnames(average) <- c("interval", "averagesteps")
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
xyplot(averagesteps ~ interval, data = average, type = "l", main = "Interval and the average number of steps taken per day", xlab = "5-minute Interval", ylab = "Average Steps per Day")
```

![plot of chunk activity_pattern](figure/activity_pattern-1.png) 

```r
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max <- (average[which.max(average$averagesteps), ])$interval
sprintf("The maxmium number of average steps are during the following interval: %i",  max)
```

```
## [1] "The maxmium number of average steps are during the following interval: 835"
```

```r
## Print the data frame row of the maxmium steps
maxsteps <- (average[which.max(average$averagesteps), ])
print(maxsteps)
```

```
##     interval averagesteps
## 104      835     206.1698
```
## 4. Imputing missing values

```r
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missingSteps <- sum(is.na(data$steps))
sprintf("How many rows have missing steps: %i",  missingSteps)
```

```
## [1] "How many rows have missing steps: 2304"
```

```r
missingDates <- sum(is.na(data$date))
sprintf("How many rows have missing dates: %i",  missingDates)
```

```
## [1] "How many rows have missing dates: 0"
```

```r
missingInterval <- sum(is.na(data$interval))
sprintf("How many rows have missing intervals: %i",  missingInterval)
```

```
## [1] "How many rows have missing intervals: 0"
```

```r
missingRows <- sum(!complete.cases(data))
sprintf("Sum of how many rows are missing one value: %i",  missingRows)
```

```
## [1] "Sum of how many rows are missing one value: 2304"
```

```r
## For the strategy the mean for that 5-minute interval is used:
## Get all rows with NAs and store them in a new data frame
rowsNA <- data[!complete.cases(data),]
## Get all rows without NAs and store them in a new data frame
rowsCO <- data[complete.cases(data),]
## Throw away the NA steps and merge the data frame with the average steps per interval
rowsNA <- merge(subset(rowsNA, select = -steps),average,by="interval")
## Set new column headers for the data frame
colnames(rowsNA) <- c("interval", "date", "steps")
## Combine the new data frame with the average steps for the NA intervals with the complete rows
rowsNA <- rbind(rowsCO, rowsNA)
## Delete the old data frame
rm(rowsCO)
## Duplicate the original data frame and throw away the steps
data2 <- subset(data, select = -steps)
## Join (same as merge but keeps the order) the data frame to get the steps
data2 <- join(data2, rowsNA, by=c("interval", "date"))
## Delete the old data frame
rm(rowsNA)
## Transform the data type to integer for the steps
data2 <- transform(data2, steps = as.integer(steps))
## Reorder the match the original data frame
data2 <- data2[ ,c("steps","date","interval")]
## Export the new data frame without NAs (replaced by the average for the interval)
write.table(data2, "./activity_withoutNA.csv", sep=",", row.name=FALSE)

## Create a new dataset that is equal to the original dataset but with the missing data filled in.
## The full set can be found in ./activity_withoutNA.csv
print(head(data2))
```

```
##   steps       date interval
## 1     1 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```

```r
## Create a sum aggragation of the steps based on the date for the new data frame
sum2 <- aggregate(data2$steps, by=data2["date"], FUN=sum)
## Set new column headers for the sum data frame
colnames(sum2) <- c("date", "sumofsteps")
## Transform the date into a date type
sum2$date <- as.Date(sum2$date, "%Y-%m-%d")
## Make a histogram of the total number of steps taken each day
qplot(sumofsteps, data=sum2, geom="histogram", binwidth = 500, main = "Histogram of the total number of steps taken each day (whNA)", xlab = "Steps per Day", ylab = "Count of Days")
```

![plot of chunk missing_values](figure/missing_values-1.png) 

```r
## Calculate the mean total number of steps taken per day
mean2 <- as.double(mean(sum2$sumofsteps, na.rm = FALSE))
sprintf("The mean of the total number of steps taken per day is: %f", mean2)
```

```
## [1] "The mean of the total number of steps taken per day is: 10749.770492"
```

```r
## Calculate the median total number of steps taken per day
median2 <- median(sum2$sumofsteps, na.rm = FALSE)
sprintf("The median of the total number of steps taken per day is: %f",  median2)
```

```
## [1] "The median of the total number of steps taken per day is: 10641.000000"
```

```r
## Do these values differ from the estimates from the first part of the assignment?
## What is the impact of imputing missing data on the estimates of the total daily number of steps?
print("The change of the mean and median is based on the strategy and whether it is or is not valid. In other words, the median and mean values could change substantially but they don't necessarily have to. By applying the strategy based on the average per interval, the mean and median is reduced, because there a more valid intervals now.")
```

```
## [1] "The change of the mean and median is based on the strategy and whether it is or is not valid. In other words, the median and mean values could change substantially but they don't necessarily have to. By applying the strategy based on the average per interval, the mean and median is reduced, because there a more valid intervals now."
```
## 5. Are there differences in activity patterns between weekdays and weekends?

```r
## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

## Tranform the date to date type
data2$date <- as.Date(data2$date, "%Y-%m-%d")
## Get the Weekday of the date and create a new column (weekday) in the data frame
data2$weekday <- weekdays(data2$date)
## Based on the weekday and an ifelse findout if the day is during the week or on the weekend. The result is saved in a new column (type)
data2$type <- ifelse(data2$weekday == "Saturday","weekend", ifelse(data2$weekday == "Sunday","weekend", "weekday"))

## Create a mean aggragation of the steps based on the interval and the type (weekday/weekend)
average2 <- aggregate(data2$steps, by=data2[c("interval","type")], FUN=mean, na.rm=TRUE)
## Set new column headers for the mean/average data frame
colnames(average2) <- c("interval", "type", "averagesteps")
## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
xyplot(averagesteps ~ interval | type, data = average2, layout = c(1, 2), type = "l", main = "Interval and the average steps per weekday and weekend", xlab = "5-minute Interval", ylab = "Average Number of Steps")
```

![plot of chunk weekdays_end](figure/weekdays_end-1.png) 

```r
## Export the new data frame with the day type
write.table(data2, "./activity_withoutNA_weekdays.csv", sep=",", row.name=FALSE)
```
