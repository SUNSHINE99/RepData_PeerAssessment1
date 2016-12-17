# Reproducible Research: Peer Assessment 1
Ravi Verma  
December 17, 2016  


## Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
2. Process/transform the data (if necessary) into a format suitable for your analysis.

**Note:**  I am not preprocessing the data set here, I am doing where it is required.


```r
  unzip("activity.zip")
  activityData <- read.csv("activity.csv")
  summary(activityData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

```r
  stepsPerDay <- with(activityData, tapply(steps, date, FUN=sum, na.rm=TRUE))
  stepsPerDayDF <- data.frame(Date=names(stepsPerDay), Steps=stepsPerDay)
  hist(stepsPerDayDF$Steps, 
       breaks=seq(from=0, to=25000, by=2000),
       col = "grey",
       ylim = c(0,20),
       xlab="Total Number Of Steps Taken Each Day",
       ylab="Frequency(In Months)",
       main = "Histogram (Break By 2000)"
  )
```

![](PA1_template_files/figure-html/computemean-1.png)<!-- -->

```r
  mean(stepsPerDayDF$Steps)
```

```
## [1] 9354.23
```

```r
  median(stepsPerDayDF$Steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
  stepsPerInteval <- with(activityData, tapply(steps, interval, FUN=mean, na.rm=TRUE))
  stepsPerIntevalDF <- data.frame(Interval=names(stepsPerInteval), 
                                  Steps=stepsPerInteval, stringsAsFactors = FALSE)
  plot(stepsPerIntevalDF$Interval, 
       stepsPerIntevalDF$Steps, 
       type='l', 
       xlab="Interval", 
       ylab="Number of Steps", 
       main="Average Number of Steps per Day by Interval"
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
  stepsPerIntevalDF[which.max(stepsPerIntevalDF$Steps),]
```

```
##     Interval    Steps
## 835      835 206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
  missingValues <- is.na(activityData)
  table(missingValues)
```

```
## missingValues
## FALSE  TRUE 
## 50400  2304
```

```r
  result <- function(inputData){
    inputData$steps[is.na(inputData$steps)] <- stepsPerIntevalDF$Steps
    #activityData$steps[is.na(activityData$steps)] <- stepsPerIntevalDF$Steps
    return(inputData)
  }
  
  
  
  filledData <- result(activityData)
  head(filledData)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
  stepsEachDay <- aggregate(filledData$steps, by=list(filledData$date), FUN=sum)
  names(stepsEachDay) <- c("Date", "Steps")
  hist(stepsEachDay$Steps, 
       breaks=seq(from=0, to=25000, by=2000),
       col = "grey",
       ylim = c(0,25),
       xlab="Total Number Of Steps Taken Each Day",
       ylab="Frequency(In Months)",
       main = "Histogram (Break By 2000)"
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
  mean(stepsEachDay$Steps)
```

```
## [1] 10766.19
```

```r
  median(stepsEachDay$Steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. 

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

I am using filledData i.e the data set which does not contains the NA values for the comparing the weekdays and weekends activity pattern. In this part of the assignment I am creating a factor variable weekFactor and cbind with the filledData.

```r
  weekFactor <- ifelse(weekdays(as.Date(filledData$date)) %in%  
                         c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),"weekday", "weekend")
  fillData <- cbind(filledData, weekFactor)

  
  
  library(lattice)
  filleDatadAndMean <- aggregate(steps ~ interval + weekFactor, data=fillData, mean)
  xyplot(steps ~ interval | weekFactor, 
         filleDatadAndMean, 
         layout = c(1,2), 
         xlab="Interval", 
         ylab="Number of steps", 
         type="l"
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
