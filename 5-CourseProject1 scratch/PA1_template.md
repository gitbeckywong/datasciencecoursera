# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
### Parts 1 & 2
- Load the dplyr, stringr, ggplot2, and scales packages (and also install them if needed)


```r
if(!require("dplyr")){
    install.packages("dplyr")
    library(dplyr)
}
if(!require("stringr")){
    install.packages("stringr")
    library(stringr)
}
if(!require("ggplot2")){
    install.packages("ggplot2")
    library(ggplot2)
}
if(!require("scales")){
    install.packages("scales")
    library(scales)
}
```

- Read in the .csv data (and convert to a data frame tbl)

```r
activity <- tbl_df(read.csv("activity.csv"))
```

- All other pre-processing required for the assignment is done within each individual part below.

## What is mean total number of steps taken per day?
### Part 1
- Remove missing values from the data.

```r
actNoNA <- activity[complete.cases(activity),]
```
- Determine total number of steps for each day.

```r
totalbydate <- summarise(group_by(actNoNA,date), sum(steps))
names(totalbydate) <- c("date","totals")
```
- Plot histogram of the total number of steps taken each day.

```r
hist(totalbydate$totals, main = "Total Number of Steps Taken Each Day", xlab = "# Steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

### Part 2
- Mean (see R output below):

```r
summarise(totalbydate, mean(totals))
```

```
## Source: local data frame [1 x 1]
## 
##   mean(totals)
## 1     10766.19
```

- Median (see R output below):

```r
summarise(totalbydate, median(totals))
```

```
## Source: local data frame [1 x 1]
## 
##   median(totals)
## 1          10765
```

## What is the average daily activity pattern?
### Part 1
- Change "interval" variable (an integer in HHMM format) into POSIXct format

```r
totalbyinterval <- summarise(group_by(actNoNA,interval), mean(steps))
totalbyinterval$timeasstring <- str_pad(totalbyinterval$interval, 4, pad = "0")
totalbyinterval$timeasdate <- as.POSIXct(strptime(totalbyinterval$timeasstring, "%H%M"))
totalbyinterval$timeastime <- substr(totalbyinterval$timeasdate, 12, 16)
colnames(totalbyinterval)[2] <- "avgsteps"
```

- Plot 5-minute time interval vs average number of steps taken averaged across all days

```r
p <- ggplot(totalbyinterval, aes(timeasdate, avgsteps))
p + geom_line(aes(group=1)) +
    scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("4 hours")) +
    labs(x = "Time") +
    labs(y = "Average Steps") +
    labs(title = "Average Daily Activity Pattern")
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

### Part 2
- Determine the 5-minute interval that contains the maximum number of steps (see R output below):

```r
maxsteps <- as.numeric(summarise(totalbyinterval, max(avgsteps)))
maxstepdf <- filter(totalbyinterval, avgsteps == maxsteps)
maxstepdf$timeastime
```

```
## [1] "08:35"
```

## Imputing missing values
### Part 1
- Total number of rows with missing values ("NA"):

```r
nrow(activity) - nrow(actNoNA)
```

```
## [1] 2304
```

### Parts 2 & 3
- Create a new dataset with the missing data filled in using "match."  The mean for each 5-minute interval will be used to replace the NA values in the full dataset.

```r
imputedactivity <- activity
imputedactivity$avgsteps <- totalbyinterval$avgsteps[match(imputedactivity$interval,totalbyinterval$interval)]
imputedactivity$steps[is.na(imputedactivity$steps)] <- imputedactivity$avgsteps[is.na(imputedactivity$steps)]
imputedactivity <- subset(imputedactivity, select = -avgsteps)
```

### Part 4
- Determine total number of steps for each day using the imputed dataset.

```r
totalbydateIMP <- summarise(group_by(imputedactivity,date), sum(steps))
names(totalbydateIMP) <- c("date","totals")
```
- Plot histogram of the total number of steps taken each day using the imputed dataset.

```r
hist(totalbydateIMP$totals, main = "Total Number of Steps Taken Each Day - Imputed Data", xlab = "# Steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

- Mean (see R output below):

```r
summarise(totalbydateIMP, mean(totals))
```

```
## Source: local data frame [1 x 1]
## 
##   mean(totals)
## 1     10766.19
```

- Median (see R output below):

```r
summarise(totalbydateIMP, median(totals))
```

```
## Source: local data frame [1 x 1]
## 
##   median(totals)
## 1       10766.19
```

- There is minimal difference between the mean/median of the imputed dataset vs the mean/median of the dataset with missing values excluded.  This is a direct result of the decision to use the average steps per interval to replace the NA values - the mean is preserved.  If different logic is used to replace the NA values, the means/medians could differ more significantly.

## Are there differences in activity patterns between weekdays and weekends?
- Create date-time column, and determine day of week.

```r
imputedactivity$timeasstring <- str_pad(imputedactivity$interval, 4, pad = "0")
imputedactivity$time <- strptime(imputedactivity$timeasstring, "%H%M")
imputedactivity$time <- substr(imputedactivity$time, 12, 19)
imputedactivity$datetime <- as.POSIXct(paste(imputedactivity$date, imputedactivity$time, sep = " "))
imputedactivity$dayofweek <- weekdays(imputedactivity$datetime)
# Saturday and Sunday = Weekend, all other days = Weekdays.  Convert to factors.
imputedactivity["weekdayck"] <- NA
for(i in 1:nrow(imputedactivity)) {
    if(as.character(imputedactivity[i,"dayofweek"]) == "Saturday") {
        imputedactivity[i,"weekdayck"] == "Weekend"
    } else if (as.character(imputedactivity[i,"dayofweek"]) == "Sunday") {
            imputedactivity[i,"weekdayck"] == "Weekend"
        } else {
            imputedactivity[i,"weekdayck"] == "Weekday"
        }
}
```
