---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

library("plyr"")
library("lattice")
Sys.setlocale(category = "LC_ALL", "C")

## Loading and preprocessing the data

data <- read.csv("./activity.csv")
sum <- aggregate(data$steps, by=data["date"], FUN=sum)
colnames(sum) <- c("date", "sumofsteps")

## What is mean total number of steps taken per day?

hist(sum$sumofsteps, col = "red", main = "Histogram of the total number of steps taken each day", xlab = "Steps")
mean <- mean(sum$sumofsteps, na.rm = TRUE)
median <- median(sum$sumofsteps, na.rm = TRUE)

## What is the average daily activity pattern?

average <- aggregate(data$steps, by=data["interval"], FUN=mean, na.rm=TRUE)
colnames(average) <- c("interval", "averagesteps")
xyplot(averagesteps ~ interval, data = average, type = "l")

max <- (average[which.max(average$averagesteps), ])$interval

## Imputing missing values

missingSteps <- sum(is.na(data$steps))
missingDates <- sum(is.na(data$date))
missingInterval <- sum(is.na(data$interval))
missingRows <- sum(!complete.cases(data))

rowsNA <- data[!complete.cases(data),]
rowsCO <- data[complete.cases(data),]
rowsNA <- merge(subset(rowsNA, select = -steps),average,by="interval")
colnames(rowsNA) <- c("interval", "date", "steps")
rowsNA <- rbind(rowsCO, rowsNA)
rm(rowsCO)

data2 <- subset(data, select = -steps)
data2 <- join(data2, rowsNA, by=c("interval", "date"))
rm(rowsNA)
data2 <- transform(data2, steps = as.integer(steps))
data2 <- data2[ ,c("steps","date","interval")] 

sum2 <- aggregate(data2$steps, by=data2["date"], FUN=sum)
colnames(sum2) <- c("date", "sumofsteps")

hist(sum2$sumofsteps, col = "red", main = "Histogram of the total number of steps taken each day", xlab = "Steps")
mean2 <- mean(sum2$sumofsteps, na.rm = TRUE)
median2 <- median(sum2$sumofsteps, na.rm = TRUE)

## Are there differences in activity patterns between weekdays and weekends?

data2$date <- as.Date(data2$date, "%Y-%m-%d")
data2$weekday <- weekdays(data2$date)
data2$type <- ifelse(data2$weekday == "Saturday","weekend", ifelse(data2$weekday == "Sunday","weekend", "weekday"))

average2 <- aggregate(data2$steps, by=data2[c("interval","type")], FUN=mean, na.rm=TRUE)
colnames(average2) <- c("interval", "type", "averagesteps")
xyplot(averagesteps ~ interval | type, data = average2, layout = c(1, 2), type = "l")
