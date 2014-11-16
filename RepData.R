install.packages("ggplot2")

library(ggplot2)
x<- read.csv("C:/Users/Mike/Desktop/Homework/R slides/Coursera Files/repdata-data-activity/activity.csv")

total.steps <- tapply(x$steps, x$date, FUN = sum, na.rm = TRUE)
x.date<-x$date

qplot(total.steps,margins = FALSE, binwidth = 1000, geom = "auto", xlab = "total number of steps taken each day")


mean(total.steps, na.rm = TRUE)

median(total.steps, na.rm = TRUE)

averages <- aggregate(x = list(steps = x$steps), by = list(interval = x$interval),FUN = mean, na.rm = TRUE)

ggplot(data = averages, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
  ylab("average number of steps taken")

averages[which.max(averages$steps), ]

miss <- is.na(x$steps)
miss
table(miss)

fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}

filled.x <- x
filled.x$steps <- mapply(fill.value, filled.x$steps, filled.x$interval)


total.steps <- tapply(filled.x$steps, filled.x$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)

weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

filled.x$date <- as.Date(filled.x$date)
filled.x$day <- sapply(filled.x$date, FUN=weekday.or.weekend)


averages <- aggregate(steps ~ interval + day, data=filled.x, mean)

ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
