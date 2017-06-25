# Reproducible Research: Peer Assessment 1
  
Test performed on a computer with:  
- Ubuntu operating system 14.0.4  
- Version 0.98.1103 – © 2009-2014 RStudio  
- R version 3.1.3  

Author:  
Marco Guado  
August, August 2015  
  
##Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Data
The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement data was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Assignment
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2).

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

## Loading and preprocessing the data  

1.- Load the data (i.e. read.csv())  
Download the zipped file and are located in the folder RepData_PeerAssessment1/data  

Set working directory:  
/....PATH...USE...PC.../RepData_PeerAssessment1  
Create new folder "data"  
  

dir.create('data')
download.file("http://d396qusza40orc.cloudfront.net/repdata/data/activity.zip", destfile="data/data_activity.zip")  

  
Nota.- If you use https to download the file and you get a message: "unsopported URL scheme", only use http as in the example above.  
  
unzip the archive "data/data_activity.zip"  
  

unzip('data/data_activity.zip', exdir='data')
  
  
Read data from csv file and store it in a memory variable named 'data.  


```r
  data <- read.csv('data/activity.csv')
  dim(data)
```

```
## [1] 17568     3
```

2.- Process/transform the data (if necessary) into a format suitable for your analysis  

We create a new data set excluding records that contain NA.  
  

```r
subdata = data[!is.na(data$steps), ]  
dim(subdata)
```

```
## [1] 15264     3
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.- Calculate the total number of steps taken per day

2.- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3.- Calculate and report the mean and median of the total number of steps taken per day

```r
#group for day
num.steps.date <- aggregate(subdata$steps, list(subdata$date), sum)
colnames(num.steps.date) <- c("date", "steps")

library(ggplot2)
#hist
ggplot(data=num.steps.date, aes(x=steps)) +
  geom_histogram(fill="#880011") +  
  ggtitle("Steps Taken per Day") +
  labs(x="Number of Steps per Day", y="Number of times in a day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#mean and median
steps_mean   <- mean(num.steps.date$steps)
steps_median <- median(num.steps.date$steps)
steps_mean
```

```
## [1] 10766.19
```

```r
steps_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1.- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

2.- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
steps_per_interval <- aggregate(subdata$steps, 
                                by = list(interval = as.factor(subdata$interval)),
                                FUN=mean, na.rm=TRUE)
    					
steps_per_interval$interval <- 
        as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])

colnames(steps_per_interval) <- c("interval", "steps")
			
ggplot(data=steps_per_interval, aes(x=interval, y=steps)) + 
    geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#maximo intervalo
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  

1.- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  

```r
vals.is.na <- sum(is.na(data$steps))
vals.is.na
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  


```r
#average number of steps as a function of range
steps.iterval <- aggregate(steps ~ interval, data , FUN = mean)

head(data)
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
#change value NA
for (i in 1:nrow(data)){
     tmp <- data$steps[i]
     if(is.na(tmp)){
         for(j in 1:nrow(steps.iterval)){
             if(data$interval[i] == steps.iterval$interval[j]){
                 data$steps[i] = steps.iterval$steps[j]
                 break
             }
         }
     }  
 }

head(data)
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

3.- Create a new dataset that is equal to the original dataset but with the missing data filled in.  


```r
#group for day
new.num.steps.date <- aggregate(data$steps, list(data$date), sum)
colnames(new.num.steps.date) <- c("date", "steps")

library(ggplot2)
#create hist
ggplot(data=new.num.steps.date, aes(x=steps)) +
  geom_histogram(fill="#880011") +  
  ggtitle("Steps Taken per Day") +
  labs(x="Number of Steps per Day", y="Number of times in a day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

4.- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  


```r
#mean and median
new_steps_mean   <- mean(new.num.steps.date$steps)
new_steps_median <- median(new.num.steps.date$steps)
new_steps_mean
```

```
## [1] 10766.19
```

```r
new_steps_median
```

```
## [1] 10766.19
```

```r
new_steps_per_interval <- aggregate(data$steps, 
                                by = list(interval = as.factor(data$interval)),
                                FUN=mean, na.rm=TRUE)
  						
new_steps_per_interval$interval <- 
        as.integer(levels(new_steps_per_interval$interval)[new_steps_per_interval$interval])

colnames(new_steps_per_interval) <- c("interval", "steps")
			
ggplot(data=new_steps_per_interval, aes(x=interval, y=steps)) + 
    geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
max_interval <- new_steps_per_interval[which.max(new_steps_per_interval$steps),]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```

