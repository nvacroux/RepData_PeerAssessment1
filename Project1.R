## R Program for Johns Hopkins Data Science Certificate through Coursera
## Reproducible Research, Project 1

library(lattice)

## Read file if it has not already been done
if (!exists("rawdata")){
    print("Reading data file")
    rawdata <- read.csv("activity.csv")
}

## Aggregate data by steps per day (total) and plot histogram
data <- aggregate(steps ~ date, data=rawdata, sum, na.rm = TRUE)
hist(data$steps, breaks=20, main="Total Steps per Day", xlab="Steps", ylab="Frequency")

## Calculate mean and median per interval and then print them
## This should really be in a helper function as it is repeated below but I haven't the time left to
## insure that it is correct and debug it.
rawsteps_mean <- mean(rawdata$steps, na.rm=TRUE)
rawsteps_median <- median(rawdata$steps, na.rm=TRUE)
print(paste("The mean steps per day is: ", rawsteps_mean))
print(paste("The median steps per day is: ", rawsteps_median))

## Plot the average number of steps taken per five minute interval
stepsdata <- aggregate(steps ~ interval, data=rawdata, mean, na.rm=TRUE)
plot(stepsdata$interval, stepsdata$steps, type="l", main="Average Steps per Five Minute Interval",
     xlab="Interval No.", ylab="steps")

## Identify the five minute interval with the maximum number of steps
maxsteps <- max(stepsdata$steps)
print(paste("The maximum number of steps in a five minute interval was: ", maxsteps))

## Count total number of na entries for steps
missingdata <- sum(is.na(rawdata$steps))
print(paste("There are", missingdata, "missing data points."))

## Replace na entries in the data set with the interval median
betterdata <- rawdata
betterdata$steps[is.na(betterdata$steps)] <- median(rawdata$steps, na.rm=TRUE)

## Aggregate new data set by steps per day and plot a histogram
betterdataday <- aggregate(steps ~ date, data=betterdata, sum, na.rm=TRUE)
hist(betterdataday$steps, breaks=20, main="Total Steps per Day \n Adjusted Data",
     xlab="Steps", ylab="Frequency")

## Calculae mean and median and then print them
bsteps_mean <- mean(betterdata$steps)
bsteps_median <- median(betterdata$steps)
print(paste("The mean is: ", bsteps_mean))
print(paste("The median is: ", bsteps_median))

## Convert date from factor to date class.  Add day names to table then use them to filter.
betterdata$date <- as.Date(betterdata$date)
betterdata$dayname <- weekdays(betterdata$date)
betterdata$weekend <- as.factor(ifelse(betterdata$dayname == "Saturday" |
                                 betterdata$dayname == "Sunday", "weekend", "weekday"))

plotdata <- aggregate(steps ~ interval + weekend, betterdata, mean)
xyplot(steps ~ interval | factor(weekend), data=plotdata, aspect=1/3, type="l")