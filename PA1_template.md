## 1. Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

### 1.1. Data

The data for this assignment can be downloaded from the course web site:

-   **Dataset:** Activity monitoring data \[52K\]

The variables included in this dataset are:

-   **steps:** Number of steps taking in a 5-minute interval (missing
    values are coded as NA)

-   **date:** The date on which the measurement was taken in YYYY-MM-DD
    format

-   **interval:** Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a *comma-separated-value (CSV) file* and there
are a total of *17,568 observations* in this dataset.

## 2. Loading and preprocessing the data

This slice of code **loads data from the ‘activity.csv’** table. The
**output** is the summary of the loaded data.

``` r
Data_Activity <- read.csv("/Users/sharanyasiddeshgowda/Documents/Macbook_Pro/SHARANYA/CERTIFICATIONS/Data_Science_Specialization_JHU/C5_Reproducible Research/R Projects/RepData_PeerAssessment1/activity.csv")

summary(Data_Activity)
```

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

## 3. What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.

-   Calculate the total number of steps taken per day

-   If you do not understand the difference between a histogram and a
    barplot, research the difference between them. Make a histogram of
    the total number of steps taken each day

-   Calculate and report the mean and median of the total number of
    steps taken per day

### 3.1. Total number of steps taken per day

``` r
Total_Steps <- aggregate(steps~date,Data_Activity,sum,na.rm=TRUE)
```
![Screen Shot 2022-07-24 at 9 39 22 PM](https://user-images.githubusercontent.com/106842913/180677068-12c0cb3a-6087-42c2-ac53-2954de2e1328.png)

### 3.2. Histogram of the total steps taken per day

``` r
hist(Total_Steps$steps,main = "DAILY STEP COUNT", xlab = "Total steps/day", col = "lightblue",border="black", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

## 3.3. Mean and Median of the total number of steps taken per day

The **mean** of the total number of steps taken per day is:

``` r
mean(Total_Steps$steps)
```

    ## [1] 10766.19

The **median** of the total number of steps taken per day is:

``` r
median(Total_Steps$steps)
```

    ## [1] 10765

## 4. What is the average daily activity pattern?

-   Make a time series plot (i.e. type = “l”) of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

-   Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

### 4.1. Time Series Plot (Daily Average Activities)

``` r
Total_Steps <- aggregate(steps~interval,Data_Activity,mean,na.rm=T)
plot(Total_Steps$interval,Total_Steps$steps, type='l',col ="maroon",lwd=2,main="DAILY AVERAGE ACTIVITY PATTERN",xlab="Intervals (5 mins.)",ylab="Step Average")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

### 4.2. 5-minute Interval that contains the Maximum Number of Steps, on average across all the days in the dataset

``` r
Total_Steps$interval[which.max((Total_Steps$steps))]
```

    ## [1] 835

## 5. Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

-   Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

-   Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

-   Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

-   Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

### 5.1. Total number of rows with NAs

``` r
sum(is.na(Data_Activity$steps))
```

    ## [1] 2304

### 5.2. Filling in all of the missing values in a dataset

``` r
Data_Activity$steps[is.na(Data_Activity$steps)==T] <- mean(Data_Activity$steps, na.rm=T)
```

### 5.3 New dataset equal to original dataset with NAs filled in

``` r
New_Total_Steps <- aggregate(steps~date,Data_Activity,sum)
```

### 5.4. Histogram for total steps each day

Mean and Median for total steps each day also calculated below, as a
part of this step.

``` r
hist(New_Total_Steps$steps,main='TOTAL STEPS/DAY',xlab = 'Steps/Day', col = "pink",border="black")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
mean(New_Total_Steps$steps)
```

    ## [1] 10766.19

``` r
median(New_Total_Steps$steps)
```

    ## [1] 10766.19

## 6. Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the
dataset with the filled-in missing values for this part.

-   Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

-   Make a panel plot containing a time series plot (i.e. type = “l”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

### 6.1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend”

``` r
Data_Activity_DT <- data.table::fread(input="/Users/sharanyasiddeshgowda/Documents/Macbook_Pro/SHARANYA/CERTIFICATIONS/Data_Science_Specialization_JHU/C5_Reproducible Research/R Projects/RepData_PeerAssessment1/activity.csv")
Data_Activity_DT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
Data_Activity_DT[, `DAY OF THE WEEK`:= weekdays(x = date)]
Data_Activity_DT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `DAY OF THE WEEK`), "WEEKDAY/WEEKEND"] <- "Weekday"
Data_Activity_DT[grepl(pattern = "Saturday|Sunday", x = `DAY OF THE WEEK`), "WEEKDAY/WEEKEND"] <- "Weekend"
Data_Activity_DT[, `WEEKDAY/WEEKEND` := as.factor(`WEEKDAY/WEEKEND`)]
summary(Data_Activity_DT)
```

    ##      steps             date               interval      DAY OF THE WEEK   
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
    ##  NA's   :2304                                                             
    ##  WEEKDAY/WEEKEND
    ##  Weekday:12960  
    ##  Weekend: 4608  
    ##                 
    ##                 
    ##                 
    ##                 
    ## 

### 6.2. Make a panel plot containing a time series plot

``` r
library(ggplot2)
Data_Activity_DT[is.na(steps), "steps"] <- Data_Activity_DT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
intervals <- Data_Activity_DT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `WEEKDAY/WEEKEND`)] 
ggplot(intervals , aes(x = interval , y = steps)) + geom_line(stat = 'identity',col="blue") + labs( x = "Interval", y = "No. of Steps") +  facet_wrap(~`WEEKDAY/WEEKEND` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-13-1.png)
