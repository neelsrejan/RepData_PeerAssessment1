---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Using ggplot2 for graphs.

```r
library(ggplot2)
```

## Loading and preprocessing the data

The code below unzips the zipped data into the current working directory if the
data is not already loaded in. The downloaded file is then read into the df activity.

```r
zipped_data <- "./activity.zip"
unzipped <- "./activity.csv"

if (!file.exists(unzipped)) {
    unzip(zipped_data)
}

activity <- read.csv("./activity.csv", sep = ",", header = TRUE)
```

## What is mean total number of steps taken per day?

The following plot is a histogram of the total number of steps taken per day.

```r
steps_df <- activity[!(is.na(activity$steps) | activity$steps == 0),]
unique_dates <- unique(steps_df$date)
sum_steps <- rep(NA, length(unique_dates))
count <- 1
for (day in unique_dates) {
    sum_steps[count] <- sum(steps_df[steps_df$date == day, "steps"])
    count <- count + 1
}
steps_per_day <- data.frame("Steps" = sum_steps, "Date" = unique_dates)

hist <- ggplot(steps_per_day, aes(Steps))
hist <- hist + geom_histogram(color = "red", bins = 10) + labs(title ="Steps per day")
print(hist)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The mean of the total number of steps taken per day is:

```r
mean(steps_per_day$Steps)
```

```
## [1] 10766.19
```
The median of the total number of steps taken per day is:

```r
median(steps_per_day$Steps)
```

```
## [1] 10765
```

The plot is a time series of the total number of steps per day.

```r
ts_steps <- ggplot(data = steps_per_day, aes(x = as.Date(Date), y = Steps))
ts_steps <- ts_steps + geom_line() + labs(title = "Steps per day") + xlab("Steps") + ylab("Count")
print(ts_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## What is the average daily activity pattern?

The following is a time series plot of steps per 5 minute interval.

```r
unique_interval <- unique(activity$interval)
average_steps_interval <- rep(NA, length(unique_interval))
count_intervals <- 1
for (interval in unique_interval) {
    average_steps_interval[count_intervals] <- mean(activity[activity$interval == interval, "steps"], na.rm = TRUE)
    count_intervals <- count_intervals + 1
}
steps_per_interval <- data.frame("Steps" = average_steps_interval, "Interval" = unique_interval)

ts_interval <- ggplot(data = steps_per_interval, aes(x = Interval, y = Steps))
ts_interval <- ts_interval + geom_line() + labs(title = "Steps per 5 min interval")
print(ts_interval)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The interval with the mean maximum number of steps is:

```r
steps_per_interval[which.max(steps_per_interval$Steps), "Interval"]
```

```
## [1] 835
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
