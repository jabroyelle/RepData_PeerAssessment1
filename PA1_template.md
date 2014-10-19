### Loading and preprocessing the data
- Load the data (i.e. read.csv())
- Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity = na.omit(read.csv("activity.csv",na.strings = "NA"))
activity$Date  = strptime(activity$date, "%Y-%m-%d")
activity$Day = as.numeric(format (activity$Date, "%d"))
activity$Month = format (activity$Date, "%b %Y")
```

### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.
- Make a histogram of the total number of steps taken each day
- Calculate and report the mean and median total number of steps taken per day

```r
temp.steps = tapply(activity$steps, activity$date, sum)
steps = temp.steps[complete.cases(temp.steps)]
hist (steps, main="Histogram of the total number of steps taken each day", xlab = "sum of steps/day", ylab = "Frequency", breaks= 12)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
steps.mean = mean(steps,na.rm = TRUE)
steps.median = median (steps,na.rm = TRUE)
```
The mean of total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>  
The median of total number of steps taken per day is 10765


### What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
temp.intervals = tapply(activity$steps, activity$interval, mean)
intervals = temp.intervals[complete.cases(temp.intervals)]
plot (intervals, type="l",main="average number of steps taken, averaged across all days per 5min interval", xlab="intervals", ylab="average number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
interval.max = which.max(intervals)
```
the 5-minute interval which contains the maximum number of steps in average per day is 206.1698113

### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3 Create a new dataset that is equal to the original dataset but with the missing data filled in.

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
actwNA = read.csv("activity.csv",na.strings = "NA")
cc = complete.cases(actwNA)
nb.false = table(cc)["FALSE"]
temp.intervals = tapply(activity$steps, activity$interval, mean)
for (i in 1:length(actwNA$interval)) {
  if (is.na(actwNA[i+1,"steps"])) {
    actwNA[i, "steps"] = temp.intervals[actwNA[i,"interval"]/5+1]
    if (is.na(actwNA[i, "steps"])) {
      actwNA[i, "steps"] = 0
    }
    }
}
steps = tapply(actwNA$steps, actwNA$date, sum)
hist (steps, main="Histogram of the total number of steps taken each day 2", xlab = "sum of steps/day", ylab = "Frequency", breaks=12)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
steps.mean = mean(steps,na.rm = TRUE)
steps.median = median (steps,na.rm = TRUE)
```

The total number of missing value is 2304 among 17568 records  
The mean of total number of steps taken per day is 1.0631976 &times; 10<sup>4</sup>  
The median of total number of steps taken per day is 1.06 &times; 10<sup>4</sup>

 
### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

Your plot will look different from the one above because you will be using the activity monitor data. Note that the above plot was made using the lattice system but you can make the same version of the plot using any plotting system you choose.

```r
actwNA$Date  = strptime(actwNA$date, "%Y-%m-%d")
WeekPart = function(x) {
  if (x %in% c("Monday", "Tuesday", "Thursday","Wednesday","Friday"))
    {
      "WeekDay"
    } else {
      "WeekEnd"
    }
}
actwNA$WeekPart = factor(sapply (weekdays(actwNA$Date),WeekPart))

actWE = subset(actwNA,WeekPart == "WeekEnd",drop=T)
actWD = subset(actwNA,WeekPart == "WeekDay",drop=T)
tempWE = tapply(actWE$steps, actWE$interval, mean)
tempWD = tapply(actWD$steps, actWD$interval, mean)

plot (tempWD, type="l",main="Number of steps taken, averaged across all WE/WD days per 5min interval", xlab="intervals", ylab="average number of steps",col="blue")
points (tempWE, type="l",col="red")
legend("topright", pch = NA, lty=1, col = c("blue", "red"), legend = c("WeekDay","WeekEnd"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
