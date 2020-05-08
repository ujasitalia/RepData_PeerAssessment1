# ----> Loading and preprocessing the data <---- #
#data = read.csv("activity.csv", header = TRUE, sep = ",", col.names = c("steps", "date", "interval"))


# ----> What is mean total number of steps taken per day? <---- #
# library(ggplot2)
# stepsEachDay <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
# png("histo.png")
# print(qplot(stepsEachDay, binwidth = 1000, xlab = "total number of steps taken each day", ylab = "Frequency"))
# dev.off()

# ----> What is the average daily activity pattern? <---- #
# avgSteps <- aggregate(x=list(aSteps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
# ggplot(data=avgSteps, aes(x=interval, y=aSteps)) +
#     geom_line() +
#     xlab("5-minute interval") +
#     ylab("average number of steps taken") 

# ----> Imputing missing values <---- #
#print(length(which(is.na(data$steps)))
# imput <- function(steps, interval){
#     new <- NA
#     if (is.na(steps)){
#         new <- avgSteps[avgSteps$interval == interval,]$aSteps
#     }
#     return(new)
# 
# }
# newData <- data
# newData$steps <- as.list(data$steps)
# newData$steps <- mapply(newData, FUN=imput, newData$steps, newData$interval)

#library(Hmisc)
# completedData <- data
# completedData$steps <- impute(data$steps, fun=mean)
# stepsEachDay <- tapply(completedData$steps, completedData$date, FUN = sum, na.rm = TRUE)
# png("imputedHist.png")
# print(qplot(stepsEachDay, binwidth = 1000, xlab = "total number of steps taken each day", ylab = "Frequency"))
# dev.off()

# Are there differences in activity patterns between weekdays and weekends?

weekdayOrweekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
            return("weekend") else stop("invalid date")
}
completedData$date <- as.Date(completedData$date)
completedData$day <- sapply(completedData$date, FUN = weekdayOrweekend)
averages <- aggregate(steps ~ interval + day, data = completedData, mean)
print(ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps"))













