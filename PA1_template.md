# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
## set locale
Sys.setlocale("LC_TIME", "us")
```

```
## [1] "English_United States.1252"
```

```r

unzip("activity.zip")
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
### 1. Make a histogram of the total number of steps taken each day

```r
steps_per_day <- tapply(data$steps, data$date, sum)
hist(steps_per_day, breaks = 10)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### 2. Calculate and report the mean and median total number of steps taken per day

```r
mean(steps_per_day, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(steps_per_day, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_per_interval <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
plot(names(steps_per_interval), steps_per_interval, type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_value <- max(steps_per_interval)
index_max <- grep(max_value, steps_per_interval)
interval_max <- names(steps_per_interval)[index_max]
interval_max
```

```
## [1] "835"
```


## Imputing missing values
### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
set_na <- is.na(data$steps)
number_na <- sum(set_na)
number_na
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
### => Strategy: use the mean for that 5-minute interval to replace NA

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_new <- data
number <- dim(data_new)[1]
for (i in 1:number) {
    if (is.na(data_new[i, 1])) 
        data_new[i, 1] <- steps_per_interval[[as.character(data_new[i, 3])]]
}
```


### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_per_day_new <- tapply(data_new$steps, data_new$date, sum)
hist(steps_per_day_new, breaks = 10)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(steps_per_day_new, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(steps_per_day_new, na.rm = TRUE)
```

```
## [1] 10766
```

### => If using the mean for that 5-minute interval to replace NA, "Mean" is the same, "Median" is offset by 1.

## Are there differences in activity patterns between weekdays and weekends?
### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
library(lattice)
wd <- weekdays(as.Date(data_new[, 2]))
data_new$Weekend <- as.factor((wd == "Sunday" | wd == "Saturday"))
```

### 2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
steps_per_interval_new <- tapply(data_new$steps, list(data_new$interval, data_new$Weekend), 
    mean)
df <- data.frame(steps = c(steps_per_interval_new[, 1], steps_per_interval_new[, 
    2]), weekday = c(rep("weekday", 288), rep("weekend", 288)), interval = as.numeric(rownames(steps_per_interval_new)))
xyplot(steps ~ interval | weekday, data = df, layout = c(1, 2), type = "l", 
    ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

