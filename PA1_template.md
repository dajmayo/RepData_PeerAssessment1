---
title: "Reproducible Research: Peer Assessment 1"
author: "Danna Ashley Mayo"
date: "May 16, 2019"
output:  
  html_document:
    keep_md: true
---



## Loading and preprocessing the data
Load the data 

```r
###Reading the csv file
activity <- read.csv("activity.csv")
View(activity)
```

Process/transform the data into suitable format for analysis

```r
### Transforming date to date format and interval into factor
activity$date <- as.Date(activity$date)
activity$interval <- as.factor(activity$interval)
```
## What is the mean total number of steps taken per day?
### Assuming missing values are ignored in the data set
Group the data by date to know total number of steps per day

```r
###Grouping to date and getting total number by the dplyr package
library(dplyr)
groupday <- group_by(activity, date)
perday <- summarise(groupday, totalsteps = sum(steps))
```

Make a Histogram of the total number of steps taken each day

```r
###Plotting the histogram
hist(perday$totalsteps, main = "Total Steps per Day", xlab = "Number of Steps", ylab = "Number of Days", col = "orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day (With NA)

```r
###Getting the mean and median
StepsMean <- mean(perday$totalsteps, na.rm = TRUE)
head(StepsMean)
StepsMedian <- median(perday$totalsteps, na.rm = TRUE)
head(StepsMedian)
```

## What is the average  daily activity pattern?
Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
###Plotting using the ggplot2 package
library(ggplot2)
StepsMeanbyInterval <- aggregate(steps ~ interval, activity, mean)
View(StepsMeanbyInterval)
ggplot(data = StepsMeanbyInterval, aes(x = interval, y = steps, group = 1)) +
  geom_line(lwd = 1, col = "darkblue") +
  ggtitle("Average Daily Activity Pattern") + 
  xlab("5-minute Interval") + 
  ylab("Average number of steps") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains  the maximum number of steps?


Now we try to remove the missing values
## Imputing missing (NA) values
Calculate and report the total number of missing (NA) values in the dataset (number of NA rows)

```r
MissingValues <- is.na(activity$steps)
MissingValues
```

Create a new data set that is equal to the original dataset but with the missing (NA) data filled in

```r
newactivity <- group_by(activity, interval) %>%
            mutate(MeanbyInt = mean(activity$steps, na.rm = TRUE))
          
newactivity$steps <-ifelse(is.na(newactivity$steps), newactivity$MeanbyInt, newactivity$steps)
head(newactivity)
```

Histogram of the total number of steps taken each day 

```r
###Group and summarise the data in preparation for histogram
newactivity1 <- newactivity %>%
           group_by(date) %>%
           summarise(steps = sum(steps))  %>%
           select(date, steps)
View(newactivity1)
###Plotting the histogram per se
hist(newactivity1$steps, col = 'brown', main = 'Total Number of Steps per Day (Without missing values)', xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day

```r
StepsMeanNoNA <- mean(newactivity1$steps)
head(StepsMeanNoNA)
StepsMedianNoNA <- median(newactivity1$steps)
head(StepsMedianNoNA)
```

Do these values differ from the estimates from the first part (With NAs)? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
### Trying to display the two differing histograms, this is the result:
  hist(perday$totalsteps, main = "Total Steps per Day", xlab = "Number of Steps", ylab = "Number of Days", col = "orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
 hist(newactivity1$steps, col = 'brown', main = 'Total Number of Steps per Day (Without missing values)', xlab = "Steps")
 box()
```

![](PA1_template_files/figure-html/unnamed-chunk-12-2.png)<!-- -->
Based from the result, the histogram with missing values is higher compared to the ones without missing values. 
Furthermore, comparing the median of with and without missing values, it shifted from 10765 to 10766.19

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
###Convert date into specific day and make if else statement indicating whether the day is a weekday or a weekend
newactivity$Day <- weekdays(as.Date(activity$date))

for(i in 1:nrow(newactivity))
{
  if(newactivity$Day[i] == "Saturday")
  {
    newactivity$DayType[i] = "Weekend"
  }
  else if (newactivity$Day[i] == "Sunday")
  {
    newactivity$DayType[i] = "Weekend"
  }
  else
  {
    newactivity$DayType[i] = "Weekday"
  }
}
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
###Plot is made using the lattice package

```r
library(lattice)
xyplot(steps ~ interval | DayType, layout = c(1, 2), data = newactivity, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

