# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

* Parse raw data


```r
rawData <- read.csv("activity.csv")
```

* Aggregate sum of steps by date and mean of steps by interval accross dates


```r
stepsByDate <- aggregate(rawData$steps,by=list(Date=rawData$date),na.rm=TRUE,FUN=sum)
names(stepsByDate) <- c("Date","Steps")
head(stepsByDate)
```

```
##         Date Steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
stepsByInterval <- aggregate(rawData$steps,by=list(Date=rawData$interval),na.rm=TRUE,FUN=mean)
names(stepsByInterval) <- c("Interval","Steps")
head(stepsByInterval)
```

```
##   Interval   Steps
## 1        0 1.71698
## 2        5 0.33962
## 3       10 0.13208
## 4       15 0.15094
## 5       20 0.07547
## 6       25 2.09434
```

## What is mean total number of steps taken per day?


```r
hist(stepsByDate$Steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


```r
mean(stepsByDate$Steps)
```

```
## [1] 9354
```

```r
median(stepsByDate$Steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?


```r
plot(stepsByInterval$Interval, stepsByInterval$Steps, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

* The maximum number of average steps per interval is:


```r
stepsByInterval[stepsByInterval$Steps == max(stepsByInterval$Steps),]$Interval
```

```
## [1] 835
```


## Imputing missing values

* The number of NAs is:


```r
sum(is.na(rawData$steps))
```

```
## [1] 2304
```

* Strategy for NA replacement: substituting NAs by the mean of that day


```r
filledData <- rawData

for(i in 1:nrow(filledData)) {
  if(is.na(filledData$steps[i])) {
     filledData$steps[i] <- stepsByDate[stepsByDate$Date == filledData$date[i],]$Steps[1]
  }
}

fdSteps <- aggregate(filledData$steps,by=list(Date=filledData$interval),FUN=mean)
names(fdSteps) <- c("Interval","Steps")
head(fdSteps)
```

```
##   Interval   Steps
## 1        0 1.49180
## 2        5 0.29508
## 3       10 0.11475
## 4       15 0.13115
## 5       20 0.06557
## 6       25 1.81967
```

* Histogram, mean and median of the new data


```r
hist(fdSteps$Steps)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
mean(fdSteps$Steps)
```

```
## [1] 32.48
```

```r
median(fdSteps$Steps)
```

```
## [1] 29.64
```


## Are there differences in activity patterns between weekdays and weekends?


```r
stepsByDate$Day <- weekdays(as.Date(stepsByDate$Date))
stepsByDate$WD <- stepsByDate$Day
for(i in 1:nrow(stepsByDate)) {
  if(stepsByDate$Day[i] == "Saturday" || stepsByDate$Day[i] == "Sunday")
    stepsByDate$WD[i] <- "Weekend"
  else
    stepsByDate$WD[i] <- "Weekday"
}

head(stepsByDate)
```

```
##         Date Steps       Day      WD
## 1 2012-10-01     0    Monday Weekday
## 2 2012-10-02   126   Tuesday Weekday
## 3 2012-10-03 11352 Wednesday Weekday
## 4 2012-10-04 12116  Thursday Weekday
## 5 2012-10-05 13294    Friday Weekday
## 6 2012-10-06 15420  Saturday Weekend
```

* Plot steps by type of day


```r
library(lattice)
with(stepsByDate, xyplot(Steps ~ Date | WD, type="l", layout=c(1,2)))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
