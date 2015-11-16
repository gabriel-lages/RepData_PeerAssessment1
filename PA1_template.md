---
title: "Reproducible Research: Peer Assessment 1"
author: "Gabriel Lages - gabrielclages@gmail.com"
date: "11/15/2015"
output: html_document
---

This is an R Markdown document created to present the results of the first "Peer Assesment" of the [Reproducible Research](https://class.coursera.org/repdata-034) course from John Hopkins University.

## Part 1 - Preparing Data
___________________

###Seting the Working Directory

As the first step I've created the working directory of the project


```r
setwd("C:/Gabriel/Biblioteca/Cursos/R - Assignment/RR - 1")
if (!file.exists("data")){dir.create("data")}
```

### Installing Packages

These are the packages that I've used in the project


```r
library(downloader)
library(ggplot2)
library(grid)
library(gridExtra)
```

###Downloading and Unziping Data


```r
fileurl<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download(fileurl, dest="./data/dataset.zip", mode="wb")
unzip("./data/dataset.zip", exdir = "data")
```

### Reading the Database

```r
database<-read.csv("./data/activity.csv")
```

## Part 2 - Answering the Questions
___________________

### 1 - What is mean total number of steps taken per day?

First we need to create a database with the total number of steps taken per day

```r
stepsperday <- data.frame(tapply(database$steps, database$date, FUN=sum, na.rm=TRUE))
colnames(stepsperday)<-c("steps")
```

To analyse the number of steps taken per day we need to plot a Histogram 
*(Obs: I choose to use 3000 as the widht of the histogram bars).*

```r
qplot(steps , data=stepsperday, geom="histogram",binwidth=3000) +
  geom_histogram(colour = "black", fill = "tomato",binwidth=3000) +
  labs(x="Total steps per day", title="Histogram of the total number of steps taken each day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Another important step in the analysis is to calculate the mean and median of the total number of steps taken per day

```r
mean(stepsperday$steps, na.rm=TRUE)
median(stepsperday$steps, na.rm=TRUE)
```

Measure | Result
--------|---------------------------------------
Mean    | 9354
Median  | 10395

### 2 - What is the average daily activity pattern?

First we need to calculate the number of steps taken, averaged across all days and keep it in a data frame


```r
average_all <- aggregate(x=list(steps=database$steps), 
                         by=list(interval=database$interval), FUN=mean, na.rm=TRUE)
```

With the new data frame we can plot a time serie to analyse the average daily activity


```r
ggplot(data=average_all, aes(x=interval, y=steps)) + 
  geom_line(colour = "tomato") + 
  labs(x="5 minute interval", y="Average number of steps (across all days)", title="Average daily activity")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

Now we need to discover Which 5-minute interval contains the maximum number of steps


```r
max_steps<- average_all[which.max(average_all$steps),]
```

**Maximum number of steps**

Interval           | Average number of Steps
-------------------|---------------------------------------
835   | 206.1698113

### 3 - Imputing missing values

In this part of the study we will analyse the number of missing values and try to fill they with other values

So, first we need to create a database containing just the missing values

```r
missing_values <- is.na(database$steps)
```

Now we can count the number of missing values in our database

```r
sum(missing_values)
```

```
## [1] 2304
```

As you could see we have a lot of missing values. The next step is to fill the missing values. For this we will use a strategy for filling in all of the missing values in the dataset with the average of that interval.

I've created a function to fill the missing values:

```r
fill.missing <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (average_all[average_all$interval==interval, "steps"])
  return(filled)
}
```

Now we need to run the function in our database to replicate it, filling all the missing values.

```r
filled.database <- database
filled.database$steps <- mapply(fill.missing, filled.database$steps, filled.database$interval)
```

To complete this part of the analysis we will create a histogram that can help us to analyse the data, but first we nees to create a new data frame with the number of steps taken, averaged across all days:

```r
stepsperday_filled <- data.frame(tapply(filled.database$steps, filled.database$date, FUN=sum, na.rm=TRUE))
colnames(stepsperday_filled)<-c("steps")
```

Now we can plot the histogram

```r
qplot(steps , data=stepsperday_filled, geom="histogram",binwidth=3000) + 
  geom_histogram(colour = "black", fill = "lightblue",binwidth=3000) +
  labs(x="Total steps per day", title="Histogram of the total number of steps taken each day (NA's Filled)")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

And the mean and median of the total number of steps taken per day

Measure | Result
--------|---------------------------------------
Mean    | 10766
Median  | 10766

#### Comparing the both Databases - Imputing missing values and not Imputing

Now we need to compare the differences between the both databases to try to find the best way to work with missing values in this study.

We will compare the histograms, the mean and the median

Plotting the comparation Histogram


```r
p1<- qplot(steps , data=stepsperday, geom="histogram",binwidth=3000) +
  geom_histogram(colour = "black", fill = "tomato",binwidth=3000) +
  labs(x="Total steps per day", title="Histogram of the total number of steps taken each day")

p2<- qplot(steps , data=stepsperday_filled, geom="histogram",binwidth=3000) +
  geom_histogram(colour = "black", fill = "lightblue",binwidth=3000) +
  labs(x="Total steps per day", title="Histogram of the total number of steps taken each day (NA's Filled)")

grid.arrange(p1, p2, ncol = 1)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 

Measure | Removing Missing Values|Imputing Missing Values
--------|------------------------|-----------------------------------
Mean    |  9354|10766
Median  | 10395|10766

## 4 - Are there differences in activity patterns between weekdays and weekends?

To answer this question we will need to work with date functions

Because I live in Brazil, I will need to change the local time of my computer to present the results in English *(If your computer is already in english skip this part)*

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

We will also need a function to separate all the information by Weekday or weekend

```r
weekday_weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
```

Now we will just run the function in the "Filled Missing Values" database


```r
filled.database$date <- as.Date(filled.database$date)
filled.database$day <- sapply(filled.database$date, FUN=weekday_weekend)
```

To finish, we will plot a time series to compare the average number of steps taken in weekdays and weekends

So we will create a database aggregating the values by Mean

```r
averages_day <- aggregate(steps ~ interval + day, data=filled.database, mean)
```

And now, plot the Time Series

```r
ggplot(averages_day, aes(interval, steps, colour = day)) + geom_line() + facet_grid(day ~ .) + labs(x="5 minute interval", y="Average number of steps (across all days)", title="Average weekends x weekdays")
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png) 
