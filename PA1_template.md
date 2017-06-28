# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
setwd("I:/CI05780/Matt/coursera_data_science_specialization/reproducible_research/RepData_PeerAssessment1/activity")
df <- read.csv("activity.csv")
str(df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
df$date <- as.Date(df$date, format = "%Y-%m-%d")
str(df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

```r
library(ggplot2)
df.agg <- aggregate(df$steps, by=list(group_date = df$date), FUN = sum, na.rm = TRUE)
ggplot(data = df.agg, aes(x = x)) + geom_histogram(fill = "green", binwidth = 2000) + labs(title = "Total Number of Steps Taken Each Day", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
paste("The mean is: ",mean(df.agg$x))
```

```
## [1] "The mean is:  9354.22950819672"
```

```r
paste("The median is: ",median(df.agg$x))
```

```
## [1] "The median is:  10395"
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
df.ada <- aggregate(df$steps, by=list(interval = df$interval), FUN = mean, na.rm = TRUE)
ggplot(data = df.ada, aes(interval, x)) + stat_summary(fun.y = mean, geom = "line", color = "red") + labs(title = "Average Number of Step Taken Per Interval", x = "Interval", y = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
subset(df.ada, x == max(df.ada$x), select = "interval")
```

```
##     interval
## 104      835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
paste("The number of missing values is: ", sum(is.na(df$steps)))
```

```
## [1] "The number of missing values is:  2304"
```

```r
df_all <- df
df_na <- is.na(df$steps)
avg_interval <- tapply(df_all$steps, df_all$interval, mean, na.rm=TRUE, simplify=TRUE)
df_all$steps[df_na] <- avg_interval[as.character(df_all$interval[df_na])]

df_all.agg <- aggregate(df_all$steps, by=list(group_date = df_all$date), FUN = sum, na.rm = TRUE)
ggplot(data = df_all.agg, aes(x = x)) + geom_histogram(fill = "green", binwidth = 2000) + labs(title = "Total Number of Steps Taken Each Day (Missing Imputed)", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
paste("The mean is: ",mean(df_all.agg$x))
```

```
## [1] "The mean is:  10766.1886792453"
```

```r
paste("The median is: ",median(df_all.agg$x))
```

```
## [1] "The median is:  10766.1886792453"
```


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
df_all$weekday <- weekdays(df_all$date)
df_all$weekend <- ifelse(df_all$weekday == "Saturday" | df_all$weekday == "Sunday", "Weekend", "Weekday")

step_day <- aggregate(df_all$steps,list(interval=df_all$interval,weekend=df_all$weekend),mean,na.rm=TRUE)
ggplot(step_day, aes(x=interval, y=x, color = weekend)) + geom_line() + facet_wrap(~weekend, ncol = 1, nrow=2) + labs(title = "Average Number of Steps Taken Per Interval (Missing Imputed)", x = "Interval", y = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->






