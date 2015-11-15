## R-Script for Reproducible Research Course Project 1 - Coursera repdata-034
## Gabriel Lages - gabrielclages@gmail.com

# Seting the Working Directory
setwd("C:/Gabriel/Biblioteca/Cursos/R - Assignment/RR - 1")
if (!file.exists("data")){dir.create("data")}

## Course Project 1 - Reproducible Research

## Installing Packages
install.packages("downloader")
library(downloader)

install.packages("ggplot2")
library(ggplot2)

install.packages("grid")
library(grid)

install.packages("gridExtra")
library(gridExtra)


#Downloading Data 
fileurl<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download(fileurl, dest="./data/dataset.zip", mode="wb")
unzip("./data/dataset.zip", exdir = "data")

# Reading the Databases

database<-read.csv("./data/activity.csv")

#head(database,n=20)
#tail(database,n=20)

# Answers

# 1 - What is mean total number of steps taken per day?

#1.1 Calculate the total number of steps taken per day

stepsperday <- data.frame(tapply(database$steps, database$date, FUN=sum, na.rm=TRUE))
colnames(stepsperday)<-c("steps")

#1.2 If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

qplot(steps , data=stepsperday, geom="histogram",binwidth=3000) + geom_histogram(colour = "black", fill = "tomato",binwidth=3000) + labs(x="Total steps per day", title="Histogram of the total number of steps taken each day")

#1.3 Calculate and report the mean and median of the total number of steps taken per day

mean(stepsperday$steps, na.rm=TRUE)
median(stepsperday$steps, na.rm=TRUE)

# 2 - What is the average daily activity pattern?

# 2.1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
average_all <- aggregate(x=list(steps=database$steps), by=list(interval=database$interval), FUN=mean, na.rm=TRUE)
ggplot(data=average_all, aes(x=interval, y=steps)) + geom_line(colour = "tomato") + labs(x="5 minute interval", y="Average number of steps (across all days)", title="Average daily activity")

# 2.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_steps<- average_all[which.max(average_all$steps),]


# 3 - Imputing missing values

# 3.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing_values <- is.na(database$steps)
count_missing<-sum(missing_values)

# 3.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

fill.missing <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (average_all[average_all$interval==interval, "steps"])
  return(filled)
}

# 3.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.

filled.database <- database
filled.database$steps <- mapply(fill.missing, filled.database$steps, filled.database$interval)

#head(filled.database)
#tail(filled.database)

# 3.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

stepsperday_filled <- data.frame(tapply(filled.database$steps, filled.database$date, FUN=sum, na.rm=TRUE))
colnames(stepsperday_filled)<-c("steps")

qplot(steps , data=stepsperday_filled, geom="histogram",binwidth=3000) + geom_histogram(colour = "black", fill = "lightblue",binwidth=3000) + labs(x="Total steps per day", title="Histogram of the total number of steps taken each day (NA's Filled)")

mean(stepsperday_filled$steps)
median(stepsperday_filled$steps)

# Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

p1<- qplot(steps , data=stepsperday, geom="histogram",binwidth=3000) + geom_histogram(colour = "black", fill = "tomato",binwidth=3000) + labs(x="Total steps per day", title="Histogram of the total number of steps taken each day")
p2<- qplot(steps , data=stepsperday_filled, geom="histogram",binwidth=3000) + geom_histogram(colour = "black", fill = "lightblue",binwidth=3000) + labs(x="Total steps per day", title="Histogram of the total number of steps taken each day (NA's Filled)")

grid.arrange(p1, p2, ncol = 1)


# 4 - Are there differences in activity patterns between weekdays and weekends?

#4.1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

#Changing the time locale 
#(I've just used it because I live in Brazil and the results are supposed to be in english)

Sys.setlocale("LC_TIME", "English")

weekday_weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}


filled.database$date <- as.Date(filled.database$date)
filled.database$day <- sapply(filled.database$date, FUN=weekday_weekend)

#4.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

averages_day <- aggregate(steps ~ interval + day, data=filled.database, mean)
ggplot(averages_day, aes(interval, steps, colour = day)) + geom_line() + facet_grid(day ~ .) + labs(x="5 minute interval", y="Average number of steps (across all days)", title="Average weekends x weekdays")
