library(ggplot2)
zipped_data <- "./activity.zip"
unzipped <- "./activity.csv"

if (!file.exists(unzipped)) {
    unzip(zipped_data)
}

activity <- read.csv("./activity.csv", sep = ",", header = TRUE)

steps_df <- activity[!(is.na(activity$steps) | activity$steps == 0),]
unique_dates <- unique(steps_df$date)
sum_steps <- rep(NA, length(unique_dates))
count <- 1
for (day in unique_dates) {
    sum_steps[count] <- sum(steps_df[steps_df$date == day, "steps"])
    count <- count + 1
}
steps_per_day <- data.frame(sum_steps, unique_dates)
names(steps_per_day) <- c("steps", "date")
hist <- ggplot(steps_per_day, aes(steps))
hist <- hist + geom_histogram(color = "red", bins = 10) + labs(title ="Steps per day")
png("./steps_per_day_histogram.png")
print(hist)
dev.off()
mean_steps_per_day <- mean(steps_per_day$steps)
median_steps_per_day <- median(steps_per_day$steps)