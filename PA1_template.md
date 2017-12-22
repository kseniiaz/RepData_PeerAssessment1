# Coursera Reproducible Research - Assessment 1



## Loading and preprocessing the data
### 1.Load the data (i.e. read.csv())
### 2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity<-read.csv("activity.csv")
activity$date<-as.Date(activity$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?
### 1.Calculate the total number of steps taken per day
### 2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
### 3.Calculate and report the mean and median of the total number of steps taken per day


```r
N_steps<-by(activity$steps,activity$date,sum,na.rm=TRUE)
head(N_steps)
```

```
## activity$date
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

```r
hist(N_steps,breaks=nrow(N_steps), main = "Number of steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
N_steps_mean<-mean(N_steps)
N_steps_med<-median(N_steps)
paste("Mean number of steps per day:",N_steps_mean)
```

```
## [1] "Mean number of steps per day: 9354.22950819672"
```

```r
paste("Median number of steps per day:",N_steps_med)
```

```
## [1] "Median number of steps per day: 10395"
```




## What is the average daily activity pattern?
### 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
Mean_steps_by_interval<-by(activity$steps,activity$interval,mean,na.rm=TRUE)

plot(Mean_steps_by_interval~levels(as.factor(activity$interval)),type="l",xlab="Interval",
     ylab="Mean Number of Steps",main="Mean Number of Steps by 5-minute Interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
max_steps<-which.max(Mean_steps_by_interval)
paste("5-minute interval with max number of steps:",names(max_steps))
```

```
## [1] "5-minute interval with max number of steps: 835"
```

## Imputing missing values

### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
paste("Total number of NA observations",nrow(activity[is.na(activity$steps)==TRUE,]))
```

```
## [1] "Total number of NA observations 2304"
```

```r
activity_filled<-activity

temp<-aggregate(activity$steps,by=list(activity$interval),mean,na.rm=TRUE)
names(temp)[1]<-paste("Interval")
names(temp)[2]<-paste("Mean_Steps")

for (i in 1:nrow(activity)){
  if (is.na(activity_filled[i,1])==TRUE){
    activity_filled[i,1]<-temp[temp$Interval==activity[i,3],2]
  }
}

N_steps_filled<-by(activity_filled$steps,activity_filled$date,sum)
hist(N_steps_filled,breaks=nrow(N_steps_filled), main = "Number of steps per day, filled data")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
N_steps_mean<-mean(N_steps)
N_steps_med<-median(N_steps)
paste("Mean number of steps per day:",mean(N_steps_filled))
```

```
## [1] "Mean number of steps per day: 10766.1886792453"
```

```r
paste("Median number of steps per day:",median(N_steps_filled))
```

```
## [1] "Median number of steps per day: 10766.1886792453"
```

## Are there differences in activity patterns between weekdays and weekends?

### 1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
### 2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
activity_filled<-cbind(activity_filled,as.data.frame(matrix(nrow=nrow(activity_filled),ncol=1)))
names(activity_filled)[4]<-paste("Weekday_flag")

for (i in 1:nrow(activity_filled)){
  if (weekdays(activity_filled[i,2]) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")){
    activity_filled[i,4]<-paste("Weekday")
  }
  else {activity_filled[i,4]<-paste("Weekend")}
}

agg_activity_filled<-aggregate(steps~interval+Weekday_flag,data=activity_filled,mean)
plot1<-ggplot(agg_activity_filled, aes(interval, steps)) + geom_line() + 
  facet_grid(Weekday_flag ~ .) +
  xlab("5-minute interval") + 
  ylab("Mean Number of Steps")

plot1
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
