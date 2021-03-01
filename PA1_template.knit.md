---
title: "PA1_template"
author: "Mohammadreza Amjadian"
date: "2/26/2021"
output: html_document
---



## Part 1

First we read the raw data


```r
rawdata=read.csv('activity.csv')
head(rawdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
Then we should convert the date column format

```r
class(rawdata$date)
```

```
## [1] "character"
```

```r
rawdata$date<-as.Date(rawdata$date)
class(rawdata$date)
```

```
## [1] "Date"
```
Then we draw a histograms of total steps taken each day (we should ignore the NA values)

```r
A<-tapply(rawdata$steps[!is.na(rawdata$steps)] ,rawdata$date[!is.na(rawdata$steps)], sum)
head(A)
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        126      11352      12116      13294      15420      11015
```

```r
hist(A,breaks=20,xlab = 'Total Steps Taken',main = 'Histogram of Total Steps Taken Each Day')
```

<img src="PA1_template_files/figure-html/total steps each day histogram-1.png" width="672" />

Then we compute the mean and median

```r
median(A)
```

```
## [1] 10765
```

```r
mean(A)
```

```
## [1] 10766.19
```

## Part 2
In this part we compute the averaged steps across all days for different intervals

```r
B<-tapply(rawdata$steps[!is.na(rawdata$steps)] ,rawdata$interval[!is.na(rawdata$steps)], mean)
head(B)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

```r
plot(names(B),B,type = 'l',xlab = 'Interval',ylab = 'Average Steps Taken',main = 'Averaged Steps Across All Days For Different Intervals')
```

<img src="PA1_template_files/figure-html/averaged steps for intervals-1.png" width="672" />

Then we can check exactly which interval has the highest average

```r
names(B)[which(B==max(B))]
```

```
## [1] "835"
```

```r
max(B)
```

```
## [1] 206.1698
```
Now we know the highest average is for interval 835 and the value is 206.1698

## Part 3
In this part we deal with the NA values.
First we calculate the number of rows with NA values

```r
sum(is.na(rawdata$steps))
```

```
## [1] 2304
```
So we have 2304 rows consisting of a NA values.
We now create a new copy of raw data and change all the NA values to the mean of that specific interval. Also we use Integer value because number of steps must be an integer.

```r
newdata<-rawdata
for(i in 1:dim(newdata)[1]){
        if(is.na(newdata$steps[i]))
                newdata$steps[i]<- as.integer(B[names(B)==newdata$interval[i]]) 
}
head(newdata)
```

```
##   steps       date interval
## 1     1 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```
Now we check again and see if there's any NA value left

```r
sum(is.na(newdata$steps))
```

```
## [1] 0
```
Then we make a histogram similar to part1 but this time with new data which doesn't contain any NA values.

```r
C<-tapply(newdata$steps,newdata$date, sum)
head(C)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##      10641        126      11352      12116      13294      15420
```

```r
hist(C,breaks=20,xlab = 'Total Steps Taken',main = 'Histogram of Total Steps Taken Each Day(NA values replaced)')
```

<img src="PA1_template_files/figure-html/total steps each day histogram with new data-1.png" width="672" />
So we can observe that frequency of some of the groups has grown in comparison with the raw data. But how about mean and median?

```r
median(C)
```

```
## [1] 10641
```

```r
mean(C)
```

```
## [1] 10749.77
```
Mean value has barely changed. But median value has changed. Difference is:

```r
median(A)-median(C)
```

```
## [1] 124
```

```r
mean(A)-mean(C)
```

```
## [1] 16.41819
```
So the median value decreased 124 and mean value decreased 16.4. This change is due to choosing the integer values for the NA replacements.
Then we want to see the averages for weekend and weekdays. So we create the necessary factor

```r
newdata$weekday<-factor(x=weekdays(rawdata$date),levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),labels = c('weekday','weekday','weekday','weekday','weekday','weekend','weekend'))
head(newdata$weekday)
```

```
## [1] weekday weekday weekday weekday weekday weekday
## Levels: weekday weekend
```
At last we draw the plot with necessary arguments to check and see if there's a meaningful difference between weekdays and weekends activities.

```r
wdmean<-tapply(newdata[newdata$weekday=='weekday','steps'],newdata$interval[newdata$weekday=='weekday'],mean)
wemean<-tapply(newdata[newdata$weekday=='weekend','steps'],newdata$interval[newdata$weekday=='weekend'],mean)
par(mfcol=c(2,1))
plot(names(wdmean),wdmean,type = 'l',col='blue',ylim = c(0,250),xlab = 'Interval',main = 'Average of Steps Taken for Weekdays',ylab = 'Steps Taken')
plot(names(wemean),wemean,type = 'l',col='red',ylim = c(0,250),xlab = 'Interval',main = 'Average of Steps Taken for Weekends',ylab = 'Steps Taken')
```

<img src="PA1_template_files/figure-html/draw the weekend/weekday plot-1.png" width="672" />
