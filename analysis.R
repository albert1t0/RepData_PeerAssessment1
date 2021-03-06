setwd("~/Documentos/Cursos/Curso de Data Sicence/Reproducible-research")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "data-activity.zip")
unzip("data-activity.zip", exdir = "DATA")
activity <- read.csv("./DATA/activity.csv", header = TRUE)
activity$date <- as.Date(as.character(activity$date), "%Y-%m-%d")

steps.pear.day <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
hist(steps.pear.day, main = "Histogram steps per day", xlab = "Steps per day", col = "green")
mean.perday <- mean(steps.pear.day, na.rm = TRUE)
meadian.perday <- median(steps.pear.day, na.rm = TRUE)

steps.pear.interval <- tapply(activity$steps, activity$interval, FUN = mean, na.rm = TRUE)
plot(names(steps.pear.interval), steps.pear.interval, type = "l", col = "red", main = "Time series")
sort(steps.pear.interval, decreasing = TRUE)[1]

sum(is.na(activity[,1:dim(activity)[2]]))
activity.notna <- activity
activity.notna$steps[is.na(activity.notna$steps)] <- mean(activity.notna$steps, na.rm = TRUE)
steps.pear.day.notna <- tapply(activity.notna$steps, activity.notna$date, FUN = sum, na.rm = TRUE)
hist(steps.pear.day.notna, main = "Histogram steps per day Not NA", xlab = "Steps per day", col = "blue")
mean.perday <- mean(steps.pear.day.notna, na.rm = TRUE)
meadian.perday <- median(steps.pear.day.notna, na.rm = TRUE)

activity.notna$day <- ifelse(weekdays(activity.notna$date) %in% c("sábado", "domingo"), "weekend", "weekday")
steps.pear.interval.notna <- aggregate(activity.notna$steps, list(activity.notna$interval, activity.notna$day), FUN = mean, na.rm = TRUE)
library(lattice)
xyplot(x ~ Group.1 | Group.2, data = steps.pear.interval.notna, layout = c(1,2), type = "l", xlab = "Interval", ylab = "Number of steps")

