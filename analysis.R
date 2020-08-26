library(ggplot2)
zipped_data <- "./activity.zip"
unzipped <- "./activity.csv"

if (!file.exists(unzipped)) {
    unzip(zipped_data)
}

activity <- read.csv("./activity.csv", sep = ",", header = TRUE)

#clean NA, and days with 0 steps into steps_per_day df
steps_df <- activity[!(is.na(activity$steps) | activity$steps == 0),]
unique_dates <- unique(steps_df$date)
sum_steps <- rep(NA, length(unique_dates))
count_dates <- 1
for (day in unique_dates) {
    sum_steps[count_dates] <- sum(steps_df[steps_df$date == day, "steps"])
    count_dates <- count_dates + 1
}
steps_per_day <- data.frame("Steps" = sum_steps, "Date" = unique_dates)

#plot histogram of steps per day
hist <- ggplot(steps_per_day, aes(Steps))
hist <- hist + geom_histogram(color = "red", bins = 10) + labs(title ="Steps per day")
png("./plots/steps_per_day_histogram.png")
print(hist)
dev.off()

#get mean, median, and total steps per day 
mean_steps_per_day <- mean(steps_per_day$Steps)
median_steps_per_day <- median(steps_per_day$Steps)
tot_steps_per_day <- sum(steps_per_day$Steps)

#plot a time series of steps per day
ts_steps <- ggplot(data = steps_per_day, aes(x = as.Date(Date), y = Steps))
ts_steps <- ts_steps + geom_line() + labs(title = "Steps per day") + xlab("Steps") + ylab("Count")
png("./plots/steps_per_day_time_series.png")
print(ts_steps)
dev.off()

##create df of steps per interval
unique_interval <- unique(activity$interval)
average_steps_interval <- rep(NA, length(unique_interval))
count_intervals <- 1
for (interval in unique_interval) {
    average_steps_interval[count_intervals] <- mean(activity[activity$interval == interval, "steps"], na.rm = TRUE)
    count_intervals <- count_intervals + 1
}
steps_per_interval <- data.frame("Steps" = average_steps_interval, "Interval" = unique_interval)

#plot a time series of steps per 5 min interval
ts_interval <- ggplot(data = steps_per_interval, aes(x = Interval, y = Steps))
ts_interval <- ts_interval + geom_line() + labs(title = "Steps per 5 min interval")
png("./plots/steps_per_interval_time_series.png")
print(ts_interval)
dev.off()

#The interval with the most number of steps
max_steps_interval <- steps_per_interval[which.max(steps_per_interval$Steps), "Interval"]

#The number of missing step values in the activity df
num_NA <- sum(is.na(activity$steps))

#Replace missing NA values in activity_replace_na by average steps per interval
activity_replace_NA <- activity
index_NA <- which(is.na(activity$steps))
for (idx in index_NA) {
    idx_interval <- activity[idx,"interval"]
    activity_replace_NA[idx, "steps"] <- steps_per_interval[steps_per_interval$Interval == idx_interval,"Steps"]
}

#plot a histogram of steps per day with replaced NA values
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
png("./plots/steps_per_day_histogram_replaced_NA.png")
print(hist_replace_NA)
dev.off()

#calculate mean, median, and total steps with replaced NA
mean_steps_per_day_replace_NA <- mean(steps_per_day_replace_NA$Steps)
median_steps_per_day_replace_NA <- median(steps_per_day_replace_NA$Steps)
tot_steps_per_day_replace_NA <- sum(steps_per_day_replace_NA$Steps)

#calculate difference between mean, median, and total for replaced NA vs non_replaced NA
difference_mean_steps_per_day <- mean_steps_per_day_replace_NA - mean_steps_per_day
difference_median_steps_per_day <- median_steps_per_day_replace_NA - median_steps_per_day
difference_total_steps_per_day <- tot_steps_per_day_replace_NA - tot_steps_per_day

#create a factor variable in the activity dataframe based on weekday or weekend from the date column
activity_which_day <- activity_replace_NA
activity_which_day$day <- weekdays(as.Date(activity_which_day$date))
activity_which_day$type_day <- ifelse(activity_which_day$day %in% c("Saturday","Sunday"), "weekend", "weekday")
activity_which_day$type_day <- as.factor(activity_which_day$type_day)

#plot time series of steps per interval by day of the week
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
png("./plots/steps_per_interval_by_day_time_series.png")
print(ts_day)
dev.off()