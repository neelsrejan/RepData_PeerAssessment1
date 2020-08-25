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
png("./steps_per_day_histogram.png")
print(hist)
dev.off()

#get mean and median of steps per day
mean_steps_per_day <- mean(steps_per_day$Steps)
median_steps_per_day <- median(steps_per_day$Steps)

#plot a time series of steps per day
ts_steps <- ggplot(data = steps_per_day, aes(x = as.Date(Date), y = Steps))
ts_steps <- ts_steps + geom_line() + labs(title = "Steps per day") + xlab("Steps") + ylab("Count")
png("./steps_per_day_time_series.png")
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
png("./steps_per_interval_time_series.png")
print(ts_interval)
dev.off()

#The interval with the most number of steps:
max_steps_interval <- steps_per_interval[which.max(steps_per_interval$Steps), "Interval"]