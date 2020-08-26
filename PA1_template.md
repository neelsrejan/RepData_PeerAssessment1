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
mean_steps_per_day <- mean(steps_per_day$Steps)
mean_steps_per_day
```

```
## [1] 10766.19
```
The median of the total number of steps taken per day is:

```r
median_steps_per_day <- median(steps_per_day$Steps)
median_steps_per_day
```

```
## [1] 10765
```
The total number of steps taken per day is:

```r
tot_steps_per_day <- sum(steps_per_day$Steps)
tot_steps_per_day
```

```
## [1] 570608
```


The plot is a time series of the total number of steps per day.

```r
ts_steps <- ggplot(data = steps_per_day, aes(x = as.Date(Date), y = Steps))
ts_steps <- ts_steps + geom_line() + labs(title = "Steps per day") + xlab("Steps") + ylab("Count")
print(ts_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

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

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The interval with the mean maximum number of steps is:

```r
steps_per_interval[which.max(steps_per_interval$Steps), "Interval"]
```

```
## [1] 835
```

## Imputing missing values

The total number of missing values in the activity dataset is:

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

The strategy for filling in the missing values in the activity dataframe is to use the mean steps per 
interval in order to approximate the probable amount of steps taken had the data been present. The now
filled dataframe is plotted as a histogram of steps per day with replaced NA values.

```r
activity_replace_NA <- activity
index_NA <- which(is.na(activity$steps))
for (idx in index_NA) {
    idx_interval <- activity[idx,"interval"]
    activity_replace_NA[idx, "steps"] <- steps_per_interval[steps_per_interval$Interval == idx_interval,"Steps"]
}

unique_dates_replace_NA <- unique(activity_replace_NA$date)
sum_steps_replace_NA <- rep(NA, length(unique_dates_replace_NA))
count_dates_replace_NA <- 1
for (day in unique_dates_replace_NA) {
    sum_steps_replace_NA[count_dates_replace_NA] <- sum(activity_replace_NA[activity_replace_NA$date == day, "steps"])
    count_dates_replace_NA <- count_dates_replace_NA + 1
}
steps_per_day_replace_NA <- data.frame("Steps" = sum_steps_replace_NA, "Date" = unique_dates_replace_NA)
hist_replace_NA <- ggplot(steps_per_day_replace_NA, aes(Steps))
hist_replace_NA <- hist_replace_NA + geom_histogram(color = "red", bins = 10) + labs(title ="Steps per day with replaced NA")
print(hist_replace_NA)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

The mean of the total number of steps taken per day with replaced NA's is:

```r
mean_steps_per_day_replace_NA <- mean(steps_per_day_replace_NA$Steps)
mean_steps_per_day_replace_NA
```

```
## [1] 10766.19
```
The median of the total number of steps taken per day with replaced NA's is:

```r
median_steps_per_day_replace_NA <- median(steps_per_day_replace_NA$Steps)
median_steps_per_day_replace_NA
```

```
## [1] 10766.19
```
The total number of steps taken per day with replaced NA's is:

```r
tot_steps_per_day_replace_NA <- sum(steps_per_day_replace_NA$Steps)
tot_steps_per_day_replace_NA
```

```
## [1] 656737.5
```

The difference in means of the total number of steps taken per day between replaced NA's vs. non-replaced NA's is:

```r
difference_mean_steps_per_day <- mean_steps_per_day_replace_NA - mean_steps_per_day
difference_mean_steps_per_day
```

```
## [1] 0
```
The difference in medians of the total number of steps taken per day between replaced NA's vs. non-replaced NA's is:

```r
difference_median_steps_per_day <- median_steps_per_day_replace_NA - median_steps_per_day
difference_median_steps_per_day
```

```
## [1] 1.188679
```
The difference in means of the total number of steps taken per day between replaced NA's vs. non-replaced NA's is:

```r
difference_tot_steps_per_day <- tot_steps_per_day_replace_NA - tot_steps_per_day
difference_tot_steps_per_day
```

```
## [1] 86129.51
```

## Are there differences in activity patterns between weekdays and weekends?

Creating a dataframe with a factor variable day using the activity dataframe without missing values:

```r
activity_which_day <- activity_replace_NA
activity_which_day$day <- weekdays(as.Date(activity_which_day$date))
activity_which_day$type_day <- ifelse(activity_which_day$day %in% c("Saturday","Sunday"), "weekend", "weekday")
activity_which_day$type_day <- as.factor(activity_which_day$type_day)
```

Creating a time series plot to show the difference in average steps per 5 minute interval based on weekdays vs weekends:

```r
unique_intervals_activity <- sort(unique(activity_which_day$interval))
average_step_arr <- rep(NA, 2*length(unique_intervals_activity))
interval_arr <- rep(NA, 2*length(unique_intervals_activity))
day_arr <- rep(NA, 2*length(unique_intervals_activity))
num_itr <- 1
days <- c("weekday","weekend")
for (day in days) {
    for (interval in unique_intervals_activity) {
        average_step_arr[num_itr] <- mean(activity_which_day[activity_which_day$type_day == day & activity_which_day$interval == interval, "steps"], na.rm = TRUE)
        interval_arr[num_itr] <- interval
        day_arr[num_itr] <- day
        num_itr <- num_itr + 1
    }
}
interval_by_day <- data.frame("interval" = interval_arr, "day" = day_arr, "steps" = average_step_arr)
ts_day <-ggplot(data = interval_by_day, aes(x = interval, y = steps)) + 
    geom_line() +
    facet_grid(day ~ .) +
    ggtitle("Steps per 5 minute interval from weekday and weekend") +
    xlab("Interval") +
    ylab("Average steps") +
    theme(plot.title = element_text(hjust = 0.5))
print(ts_day)
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

It appears that on weekdays there is a sharp rise in average steps before work time and then is very low for the rest of the workday and night, whereas during weekends there seem to be peaks that appear through the day and less steps all at once like weekdays. Weekends seem to spread the average steps out more.

