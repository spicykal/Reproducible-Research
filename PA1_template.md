---
title: "Reproducible Research"
author: "Calvin Hutto"
date: "June 19, 2016"
output: html_document
keep_md: yes
---
## Downloading and Initial Processing of Data

Here we download the activity dataset from the UCI data repository.

```{r setup}
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  unzip(temp)
  unlink(temp)
}

data <- read.csv("activity.csv")
```

## Initial Data Visualization

The below code and embedded graph is a simple look at the daily step totals for the entirety of the data set. The histogram indicates that the vast majority of those individuals in thd dataset walked between 10000-15000 steps each day.

```{r, intervals}
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps")

mean1 <- mean(steps_by_day$steps)
median1 <- median(steps_by_day$steps)
```

The data indicates that the mean number of steps in the dataset was 10766.19 and the median number of steps was 10765 including imputed data.

## Determining Steps Based on 5 Minute Intervals 

```{r, cars}
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="5 Minute Interval", ylab="Number of Steps",main="Average Number of Steps ~ Interval")

max <- steps_by_interval[which.max(steps_by_interval$steps),1]
```

The data indicates that the 835th five minute period in the data frame has, on average, the greatest number of steps.

## Imputing Data
I used the mice package in order to impute the missing data in the frame.  The below code analyzes the data frame for NAs and imputes them. The data indicate 2304 NAs within the steps variable in the data frame.

```{r impute, echo=FALSE}
library(mice)
md.pattern(data)
```
The code below actually imputes the data which has little to no effect on the analysis of the entire data frame.  The NAs in the steps variable account for only 11% of the total data.

```{r impute1}
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
completedData <- complete(tempData,1)
steps_by_day1 <- aggregate(steps ~ date, completedData, sum)
mean2 <- mean(steps_by_day1$steps)
median2 <- median(steps_by_day1$steps)
```
With the addition of the imputed data, the mean was adjusted down from 10766.19 to 10716.41 and the median was adjusted lower from 10765 to 10600. The additon of the imputed data had realatively minimal impact on the results especially for the purposes of our research.

##Weekend Analysis
The code below allows us to analyze weekend data and determine if there are differences in steps during the week versus during the weekend.

```{r weekend}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
completedData$dow = as.factor(ifelse(is.element(weekdays(as.Date(completedData$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, completedData, mean)

library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

The time series line graphs indicate an increased number of steps early in the day and then a marked decrease for the majority of the work day during the workweek.  On the other hand, the weekend graph indicates less severe spikes and a more stable level of an elevated number of steps throughout the day.
