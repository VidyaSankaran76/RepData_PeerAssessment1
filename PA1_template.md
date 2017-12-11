---
title: "Reproducible Research: Peer Assessment 1"
author: "Vidya Sankaran"
date: "December 8, 2017"
output: 
      html_document:
         keep_md: true
---




## Loading and preprocessing the data
Including Required library files and downloading the zip file. Unziping the same and read the dataset. Inspecting the structure of the variable and identify the contents. 
Following transformations are done:

- Month - calculation from the date given



```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(stringr)
#library(Hmisc)

if (file.exists("activity.zip") == FALSE) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip")  
    unzip("activity.zip")
}

mydf <- read.csv("activity.csv", header = TRUE)
str(mydf)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
dim(mydf)
```

```
## [1] 17568     3
```

```r
head(mydf)
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

```r
mean(is.na(mydf$steps))
```

```
## [1] 0.1311475
```

```r
z<- ymd(mydf$date, tz="GMT")

mydf <- mydf %>% mutate(month_num = month(z)) %>%
        mutate(daycount = ifelse(month_num==10, day(z), day(z)+31))
tail(mydf, n=10)
```

```
##       steps       date interval month_num daycount
## 17559    NA 2012-11-30     2310        11       61
## 17560    NA 2012-11-30     2315        11       61
## 17561    NA 2012-11-30     2320        11       61
## 17562    NA 2012-11-30     2325        11       61
## 17563    NA 2012-11-30     2330        11       61
## 17564    NA 2012-11-30     2335        11       61
## 17565    NA 2012-11-30     2340        11       61
## 17566    NA 2012-11-30     2345        11       61
## 17567    NA 2012-11-30     2350        11       61
## 17568    NA 2012-11-30     2355        11       61
```

Finding:
61 days which is Oct 31 days, and Nov 30 days the readings were taken.


## What is mean total number of steps taken per day?
Following transformations done for answering this question
- Daycount: How many days these readings are taken 
- Steps per day: Total no of steps taken in every day excluding the NA values

Plot:
    A histogram which represents the no of steps taken each day 



```r
mydf <- mydf %>% group_by(daycount) %>% mutate(steps_per_day = sum(steps, na.rm=TRUE)) 

mydf_steps <- mydf %>% group_by(daycount, date) %>% summarise(steps_per_day1=sum(steps,na.rm=TRUE))
mydf_steps
```

```
## # A tibble: 61 x 3
## # Groups:   daycount [?]
##    daycount       date steps_per_day1
##       <dbl>     <fctr>          <int>
##  1        1 2012-10-01              0
##  2        2 2012-10-02            126
##  3        3 2012-10-03          11352
##  4        4 2012-10-04          12116
##  5        5 2012-10-05          13294
##  6        6 2012-10-06          15420
##  7        7 2012-10-07          11015
##  8        8 2012-10-08              0
##  9        9 2012-10-09          12811
## 10       10 2012-10-10           9900
## # ... with 51 more rows
```

```r
hist(x=mydf_steps$steps_per_day1, main="Total No of Steps Taken Each Day", col="blue") 
abline(v=mean(mydf_steps$steps_per_day1), col="red", lwd=2)
abline(v=median(mydf_steps$steps_per_day1), col="black", lwd=2)
```

![](PA1_template_files/figure-html/Total No of steps and Median and Mean of steps taken each day-1.png)<!-- -->

```r
summary(mydf_steps$steps_per_day1)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
mydf
```

```
## # A tibble: 17,568 x 6
## # Groups:   daycount [61]
##    steps       date interval month_num daycount steps_per_day
##    <int>     <fctr>    <int>     <dbl>    <dbl>         <int>
##  1    NA 2012-10-01        0        10        1             0
##  2    NA 2012-10-01        5        10        1             0
##  3    NA 2012-10-01       10        10        1             0
##  4    NA 2012-10-01       15        10        1             0
##  5    NA 2012-10-01       20        10        1             0
##  6    NA 2012-10-01       25        10        1             0
##  7    NA 2012-10-01       30        10        1             0
##  8    NA 2012-10-01       35        10        1             0
##  9    NA 2012-10-01       40        10        1             0
## 10    NA 2012-10-01       45        10        1             0
## # ... with 17,558 more rows
```

 Findings:
 For about 13 days, the steps value is zero. This may be due to NA or due to some
 reason the person not measured it. 
 The person takes about average of 9354 steps in 61 days and the median is 10395 steps


## What is the average daily activity pattern?

For this question following transformation done
-  Padding of time : The readings are taken in 5 minute interval in a day, so 
converted the time to proper 24 hour time format for Hours and                       minutes, so, padded the data with 0s for the previous blank                          lines. Stringr library included for this purpose

-  POSIXct datetime : conversion of date and time values to date time mode
-  average steps taken per day : by grouping the datetime and daycount, we 
summaraise the mean steps taken per day   

- Steps per interval:  Steps for that interval including all 61 days in the dataset
            are taken and added by excluding the NA values.

- five_min_interval:  The pad time is converted to numerical factor number, says, 0000 to 1, 0005 to 2 and 23:55 as 288  This will be useful for showing graphicaly


```r
mydf <- mydf %>% mutate(pad_time= str_pad(as.character(interval), 4, pad="0")) %>%
    mutate(datetime =as.POSIXct(paste(date, pad_time), tz="GMT"))

head(mydf, n=3)
```

```
## # A tibble: 3 x 8
## # Groups:   daycount [1]
##   steps       date interval month_num daycount steps_per_day pad_time
##   <int>     <fctr>    <int>     <dbl>    <dbl>         <int>    <chr>
## 1    NA 2012-10-01        0        10        1             0     0000
## 2    NA 2012-10-01        5        10        1             0     0005
## 3    NA 2012-10-01       10        10        1             0     0010
## # ... with 1 more variables: datetime <dttm>
```

```r
mean_mydf <- mean(mydf$steps, na.rm=TRUE)

mydf2 <- mydf %>% group_by(daycount, datetime ) %>% 
    select(daycount, datetime,  steps_per_day) %>% 
    summarise (avg_steps_day  = mean(steps_per_day)  )

mydf2
```

```
## # A tibble: 61 x 3
## # Groups:   daycount [?]
##    daycount   datetime avg_steps_day
##       <dbl>     <dttm>         <dbl>
##  1        1 2012-10-01             0
##  2        2 2012-10-02           126
##  3        3 2012-10-03         11352
##  4        4 2012-10-04         12116
##  5        5 2012-10-05         13294
##  6        6 2012-10-06         15420
##  7        7 2012-10-07         11015
##  8        8 2012-10-08             0
##  9        9 2012-10-09         12811
## 10       10 2012-10-10          9900
## # ... with 51 more rows
```

```r
mydf1 <- mydf %>% group_by(pad_time)  %>% 
    summarise(avg_steps_per_interval = mean(steps, na.rm=TRUE))%>% 
    select(pad_time,  avg_steps_per_interval ) 


mydf1
```

```
## # A tibble: 288 x 2
##    pad_time avg_steps_per_interval
##       <chr>                  <dbl>
##  1     0000              1.7169811
##  2     0005              0.3396226
##  3     0010              0.1320755
##  4     0015              0.1509434
##  5     0020              0.0754717
##  6     0025              2.0943396
##  7     0030              0.5283019
##  8     0035              0.8679245
##  9     0040              0.0000000
## 10     0045              1.4716981
## # ... with 278 more rows
```

```r
mydf1 <- mydf1 %>% mutate(five_min_interval = 1:nrow(mydf1) )

mydf1
```

```
## # A tibble: 288 x 3
##    pad_time avg_steps_per_interval five_min_interval
##       <chr>                  <dbl>             <int>
##  1     0000              1.7169811                 1
##  2     0005              0.3396226                 2
##  3     0010              0.1320755                 3
##  4     0015              0.1509434                 4
##  5     0020              0.0754717                 5
##  6     0025              2.0943396                 6
##  7     0030              0.5283019                 7
##  8     0035              0.8679245                 8
##  9     0040              0.0000000                 9
## 10     0045              1.4716981                10
## # ... with 278 more rows
```

```r
tmp <- max(mydf1$avg_steps_per_interval)    
xinte <- mydf1$pad_time[mydf1$avg_steps_per_interval==max(mydf1$avg_steps_per_interval) ]  
xinte
```

```
## [1] "0835"
```

```r
xinte <- mydf1$five_min_interval[mydf1$avg_steps_per_interval==max(mydf1$avg_steps_per_interval)]


summary(mydf1$pad_time)
```

```
##    Length     Class      Mode 
##       288 character character
```

```r
ggplot(mydf1, aes(five_min_interval, avg_steps_per_interval))+ 
    geom_line( col="blue")+  
    ggtitle("Steps per day with respect to 5 minute time interval") +
    geom_vline(xintercept = xinte, col="brown")+
    xlab(" 5 minute interval as numeric values from 0000 to 1 and  23:55 as 288")
```

![](PA1_template_files/figure-html/5-minute interval containing max no of steps-1.png)<!-- -->

```r
head(mydf, n=1)
```

```
## # A tibble: 1 x 8
## # Groups:   daycount [1]
##   steps       date interval month_num daycount steps_per_day pad_time
##   <int>     <fctr>    <int>     <dbl>    <dbl>         <int>    <chr>
## 1    NA 2012-10-01        0        10        1             0     0000
## # ... with 1 more variables: datetime <dttm>
```


Findings:

The 104th interval, which is at 0835, the maximum no of steps recorded.


## Imputing missing values
strategy for missing data:

_ When checked all the days are weekdays for the imputing.
- We can impute mean of the steps per interval instead of NA values


```r
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
temp <- which(is.na(mydf$steps))
wday(mydf$datetime[1], label=TRUE)
```

```
## [1] Mon
## Levels: Sun < Mon < Tue < Wed < Thu < Fri < Sat
```

```r
#One and seven are sunday and saturday
mydf_imp <- mydf %>% group_by(pad_time) %>% 
    select (datetime, daycount, pad_time, steps) %>% 
    filter(is.na(steps)) 

mydf_imp
```

```
## # A tibble: 2,304 x 4
## # Groups:   pad_time [288]
##      datetime daycount pad_time steps
##        <dttm>    <dbl>    <chr> <int>
##  1 2012-10-01        1     0000    NA
##  2 2012-10-01        1     0005    NA
##  3 2012-10-01        1     0010    NA
##  4 2012-10-01        1     0015    NA
##  5 2012-10-01        1     0020    NA
##  6 2012-10-01        1     0025    NA
##  7 2012-10-01        1     0030    NA
##  8 2012-10-01        1     0035    NA
##  9 2012-10-01        1     0040    NA
## 10 2012-10-01        1     0045    NA
## # ... with 2,294 more rows
```

```r
mydf
```

```
## # A tibble: 17,568 x 8
## # Groups:   daycount [61]
##    steps       date interval month_num daycount steps_per_day pad_time
##    <int>     <fctr>    <int>     <dbl>    <dbl>         <int>    <chr>
##  1    NA 2012-10-01        0        10        1             0     0000
##  2    NA 2012-10-01        5        10        1             0     0005
##  3    NA 2012-10-01       10        10        1             0     0010
##  4    NA 2012-10-01       15        10        1             0     0015
##  5    NA 2012-10-01       20        10        1             0     0020
##  6    NA 2012-10-01       25        10        1             0     0025
##  7    NA 2012-10-01       30        10        1             0     0030
##  8    NA 2012-10-01       35        10        1             0     0035
##  9    NA 2012-10-01       40        10        1             0     0040
## 10    NA 2012-10-01       45        10        1             0     0045
## # ... with 17,558 more rows, and 1 more variables: datetime <dttm>
```

```r
mydf$steps <-impute(mydf$steps, mean)
mydf
```

```
## # A tibble: 17,568 x 8
## # Groups:   daycount [61]
##           steps       date interval month_num daycount steps_per_day
##    <S3: impute>     <fctr>    <int>     <dbl>    <dbl>         <int>
##  1      37.3826 2012-10-01        0        10        1             0
##  2      37.3826 2012-10-01        5        10        1             0
##  3      37.3826 2012-10-01       10        10        1             0
##  4      37.3826 2012-10-01       15        10        1             0
##  5      37.3826 2012-10-01       20        10        1             0
##  6      37.3826 2012-10-01       25        10        1             0
##  7      37.3826 2012-10-01       30        10        1             0
##  8      37.3826 2012-10-01       35        10        1             0
##  9      37.3826 2012-10-01       40        10        1             0
## 10      37.3826 2012-10-01       45        10        1             0
## # ... with 17,558 more rows, and 2 more variables: pad_time <chr>,
## #   datetime <dttm>
```

```r
mydf <- mydf %>% group_by(daycount) %>% 
    mutate(weekday = wday(datetime)) %>% 
    mutate(steps_per_day= sum(steps))
mydf
```

```
## # A tibble: 17,568 x 9
## # Groups:   daycount [61]
##           steps       date interval month_num daycount steps_per_day
##    <S3: impute>     <fctr>    <int>     <dbl>    <dbl>         <dbl>
##  1      37.3826 2012-10-01        0        10        1      10766.19
##  2      37.3826 2012-10-01        5        10        1      10766.19
##  3      37.3826 2012-10-01       10        10        1      10766.19
##  4      37.3826 2012-10-01       15        10        1      10766.19
##  5      37.3826 2012-10-01       20        10        1      10766.19
##  6      37.3826 2012-10-01       25        10        1      10766.19
##  7      37.3826 2012-10-01       30        10        1      10766.19
##  8      37.3826 2012-10-01       35        10        1      10766.19
##  9      37.3826 2012-10-01       40        10        1      10766.19
## 10      37.3826 2012-10-01       45        10        1      10766.19
## # ... with 17,558 more rows, and 3 more variables: pad_time <chr>,
## #   datetime <dttm>, weekday <dbl>
```

```r
hist(x=mydf$steps_per_day, main="Total No of Steps Taken Each Day", col="blue")
abline(v=mean(mydf$steps_per_day), col="red", lwd=2)
abline(v=median(mydf$steps_per_day), col="black", lwd=2)
```

![](PA1_template_files/figure-html/imputting missing data-1.png)<!-- -->

```r
summary(mydf$steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

Findings:

By imputing the average of steps taken per interval, the mean and median of the steps per day has increased and now they are at same value.

## Are there differences in activity patterns between weekdays and weekends?



```r
mydf
```

```
## # A tibble: 17,568 x 9
## # Groups:   daycount [61]
##           steps       date interval month_num daycount steps_per_day
##    <S3: impute>     <fctr>    <int>     <dbl>    <dbl>         <dbl>
##  1      37.3826 2012-10-01        0        10        1      10766.19
##  2      37.3826 2012-10-01        5        10        1      10766.19
##  3      37.3826 2012-10-01       10        10        1      10766.19
##  4      37.3826 2012-10-01       15        10        1      10766.19
##  5      37.3826 2012-10-01       20        10        1      10766.19
##  6      37.3826 2012-10-01       25        10        1      10766.19
##  7      37.3826 2012-10-01       30        10        1      10766.19
##  8      37.3826 2012-10-01       35        10        1      10766.19
##  9      37.3826 2012-10-01       40        10        1      10766.19
## 10      37.3826 2012-10-01       45        10        1      10766.19
## # ... with 17,558 more rows, and 3 more variables: pad_time <chr>,
## #   datetime <dttm>, weekday <dbl>
```

```r
mydf <- mydf %>% group_by(pad_time) %>% 
    mutate(weekend= ifelse(weekday==1 | weekday==7, TRUE, FALSE))

mydf
```

```
## # A tibble: 17,568 x 10
## # Groups:   pad_time [288]
##           steps       date interval month_num daycount steps_per_day
##    <S3: impute>     <fctr>    <int>     <dbl>    <dbl>         <dbl>
##  1      37.3826 2012-10-01        0        10        1      10766.19
##  2      37.3826 2012-10-01        5        10        1      10766.19
##  3      37.3826 2012-10-01       10        10        1      10766.19
##  4      37.3826 2012-10-01       15        10        1      10766.19
##  5      37.3826 2012-10-01       20        10        1      10766.19
##  6      37.3826 2012-10-01       25        10        1      10766.19
##  7      37.3826 2012-10-01       30        10        1      10766.19
##  8      37.3826 2012-10-01       35        10        1      10766.19
##  9      37.3826 2012-10-01       40        10        1      10766.19
## 10      37.3826 2012-10-01       45        10        1      10766.19
## # ... with 17,558 more rows, and 4 more variables: pad_time <chr>,
## #   datetime <dttm>, weekday <dbl>, weekend <lgl>
```

```r
#mydf_wk_day <- mydf %>% group_by(daycount, date) %>%
#    select(date, daycount, datetime, steps_per_day, weekend) %>% 
#    filter (weekend ==FALSE)

mydf_wk_day <- mydf %>% group_by(pad_time) %>%
    select(pad_time, steps, weekend) %>% 
    filter (weekend ==FALSE) %>%
    summarise(steps_per_interval = mean(steps))%>%
    select(pad_time, steps_per_interval)
    
  
mydf_wk_day
```

```
## # A tibble: 288 x 2
##    pad_time steps_per_interval
##       <chr>              <dbl>
##  1     0000           7.006569
##  2     0005           5.384347
##  3     0010           5.139902
##  4     0015           5.162124
##  5     0020           5.073235
##  6     0025           6.295458
##  7     0030           5.606569
##  8     0035           6.006569
##  9     0040           4.984347
## 10     0045           6.584347
## # ... with 278 more rows
```

```r
#mydf_wk_end <- mydf %>% group_by(daycount, date) %>%
#    select(date, daycount, datetime, steps_per_day, weekend) %>% 
#    filter (weekend ==TRUE)

mydf_wk_end <- mydf %>% group_by(pad_time) %>%
    select(pad_time, steps, weekend) %>% 
    filter (weekend ==TRUE) %>%
    summarise(steps_per_interval = mean(steps))%>%
    select(pad_time, steps_per_interval)    
  
mydf_wk_end
```

```
## # A tibble: 288 x 2
##    pad_time steps_per_interval
##       <chr>              <dbl>
##  1     0000           4.672825
##  2     0005           4.672825
##  3     0010           4.672825
##  4     0015           4.672825
##  5     0020           4.672825
##  6     0025           7.922825
##  7     0030           4.672825
##  8     0035           4.672825
##  9     0040           4.672825
## 10     0045           5.047825
## # ... with 278 more rows
```

```r
#ggplot(mydf2, aes(datetime, avg_steps_day))+ geom_line(pch=8, col="blue")+ geom_hline(aes(yintercept= mean(avg_steps_day))) +
    #ggtitle("Average steps taken per day")


par(mfrow=c(2,1), mar=c(4,4,1,1))
plot(x=mydf_wk_end$pad_time, y=mydf_wk_end$steps_per_interval, main="Weekend" ,
     ylab="Number of Steps", xlab="Interval", type="l" ) + abline(h=mean(mydf_wk_end$steps_per_interval), col="blue")
```

```
## integer(0)
```

```r
plot(x=mydf_wk_day$pad_time, y=mydf_wk_day$steps_per_interval, main="Weekdays " ,ylab="Number of Steps", xlab="Interval", type="l" ) + abline(h=mean(mydf_wk_day$steps_per_interval), col="blue")
```

![](PA1_template_files/figure-html/Weekdays Vs Weekends-1.png)<!-- -->

```
## integer(0)
```

Findings:

Avg steps taken is more or less the same.  
