#     Personal Activity Monitoring Data Analysis

# Introduction
This assignment makes use of data from a personal activity monitoring device.
This device collects data at 5 minute intervals through out the day.
The data consists of two months of data from an anonymous individual collected
during the months of October and November, 2012 and include the number of steps
taken in 5 minute intervals each day.


```r
library(knitr)
library(ggplot2)
```

# Loading and preprocessing the data


```r
if(!dir.exists("./amd")){dir.create("./amd")}

setwd("./amd")

rm(list=ls())

activity_df<-read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

# What is mean total number of steps taken per day?


```r
tot_steps_per_day<-tapply(activity_df$steps,activity_df$date,sum)
```

```
## Error in tapply(activity_df$steps, activity_df$date, sum): object 'activity_df' not found
```
### Histogram of the total number of steps taken each day


```r
hist(tot_steps_per_day,main="Histogram of total number of steps taken each day",xlab="Total steps per day",xlim=c(0,25000),col="green",breaks = 50)
```

```
## Error in hist(tot_steps_per_day, main = "Histogram of total number of steps taken each day", : object 'tot_steps_per_day' not found
```

### Mean and median number of steps taken each day


```r
mean(tot_steps_per_day,na.rm=TRUE)
```

```
## Error in mean(tot_steps_per_day, na.rm = TRUE): object 'tot_steps_per_day' not found
```

```r
median(tot_steps_per_day,na.rm=TRUE)
```

```
## Error in median(tot_steps_per_day, na.rm = TRUE): object 'tot_steps_per_day' not found
```

# What is the average daily activity pattern?


```r
activity_na_omit_df<-subset(activity_df,is.na(steps) == FALSE,select = c(steps,date,interval))
```

```
## Error in subset(activity_df, is.na(steps) == FALSE, select = c(steps, : object 'activity_df' not found
```

```r
unique_interval<-unique(activity_na_omit_df$interval)
```

```
## Error in unique(activity_na_omit_df$interval): object 'activity_na_omit_df' not found
```

```r
avg_steps_per_interval<-tapply(activity_na_omit_df$steps,activity_na_omit_df$interval,mean)
```

```
## Error in tapply(activity_na_omit_df$steps, activity_na_omit_df$interval, : object 'activity_na_omit_df' not found
```

```r
avg_steps_df<-data.frame(interval=unique_interval,avg_steps=avg_steps_per_interval)
```

```
## Error in data.frame(interval = unique_interval, avg_steps = avg_steps_per_interval): object 'unique_interval' not found
```
### Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
plot(avg_steps_df$interval,avg_steps_df$avg_steps,main="Average daily activity pattern",xlab="Interval",ylab="Average number of steps taken, averaged across all days",type="l",col="blue")
```

```
## Error in plot(avg_steps_df$interval, avg_steps_df$avg_steps, main = "Average daily activity pattern", : object 'avg_steps_df' not found
```

### 5-minute interval with the maximum number of steps


```r
avg_steps_df[avg_steps_df$avg_steps==max(avg_steps_df$avg_steps),]$interval
```

```
## Error in eval(expr, envir, enclos): object 'avg_steps_df' not found
```

# Imputing missing values

### Total number of missing values in the dataset


```r
nrow(subset(activity_df,is.na(steps) == TRUE | is.na(date)== TRUE | is.na(interval)== TRUE ))
```

```
## Error in subset(activity_df, is.na(steps) == TRUE | is.na(date) == TRUE | : object 'activity_df' not found
```
### Filling in the missing values in the dataset.


```r
activity_na_df<-subset(activity_df,is.na(steps) == TRUE,select = c(steps,date,interval))
```

```
## Error in subset(activity_df, is.na(steps) == TRUE, select = c(steps, date, : object 'activity_df' not found
```

```r
avg_steps_per_date<-tapply(activity_na_omit_df$steps,activity_na_omit_df$date,mean)
```

```
## Error in tapply(activity_na_omit_df$steps, activity_na_omit_df$date, mean): object 'activity_na_omit_df' not found
```
### Finding the daily average number of steps


```r
daily_avg_steps<-mean(avg_steps_per_date,na.rm=TRUE)
```

```
## Error in mean(avg_steps_per_date, na.rm = TRUE): object 'avg_steps_per_date' not found
```
### Finding the average number of steps for 5 min interval


```r
five_min_avg_steps <- daily_avg_steps/288
```

```
## Error in eval(expr, envir, enclos): object 'daily_avg_steps' not found
```
Imputing the missing vales with 5 min interval average number of steps


```r
avg_steps_per_date[is.na(avg_steps_per_date) == TRUE]<-five_min_avg_steps
```

```
## Error in eval(expr, envir, enclos): object 'five_min_avg_steps' not found
```


```r
unique_date<-unique(activity_df$date)
```

```
## Error in unique(activity_df$date): object 'activity_df' not found
```

```r
steps_date_df<-data.frame(avg_steps=avg_steps_per_date,date=unique_date)
```

```
## Error in data.frame(avg_steps = avg_steps_per_date, date = unique_date): object 'avg_steps_per_date' not found
```

```r
activity_na_impute_df<-merge(activity_na_df,steps_date_df,by="date")
```

```
## Error in merge(activity_na_df, steps_date_df, by = "date"): object 'activity_na_df' not found
```

```r
activity_na_impute_df<-subset(activity_na_impute_df,select=c(avg_steps,date,interval))
```

```
## Error in subset(activity_na_impute_df, select = c(avg_steps, date, interval)): object 'activity_na_impute_df' not found
```

```r
names(activity_na_impute_df)[names(activity_na_impute_df)=='avg_steps']<-'steps'
```

```
## Error in names(activity_na_impute_df)[names(activity_na_impute_df) == : object 'activity_na_impute_df' not found
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_full_df<-rbind(activity_na_omit_df,activity_na_impute_df)
```

```
## Error in rbind(activity_na_omit_df, activity_na_impute_df): object 'activity_na_omit_df' not found
```
### Histogram of the total number of steps taken each day based on the new data set( no NA values)


```r
tot_steps_per_day_new<-tapply(activity_full_df$steps,activity_full_df$date,sum)
```

```
## Error in tapply(activity_full_df$steps, activity_full_df$date, sum): object 'activity_full_df' not found
```

```r
hist(tot_steps_per_day_new,main="Histogram of total number of steps taken each day after imputing NA values ",xlab="Total steps per day",xlim=c(0,25000),col="green",breaks = 50)
```

```
## Error in hist(tot_steps_per_day_new, main = "Histogram of total number of steps taken each day after imputing NA values ", : object 'tot_steps_per_day_new' not found
```

### Mean and median number of steps taken each day based on the new data set( no NA values)


```r
mean(tot_steps_per_day_new,na.rm=TRUE)
```

```
## Error in mean(tot_steps_per_day_new, na.rm = TRUE): object 'tot_steps_per_day_new' not found
```

```r
median(tot_steps_per_day_new,na.rm=TRUE)
```

```
## Error in median(tot_steps_per_day_new, na.rm = TRUE): object 'tot_steps_per_day_new' not found
```

# Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels weekday and weekend


```r
activity_full_df$date_type<-factor(sapply(weekdays(as.Date(activity_full_df$date)),switch,"Monday"="weekday","Tuesday"="weekday","Wednesday"="weekday","Thursday"="weekday","Friday"="weekday","Saturday"="weekend","Sunday"="weekend"),levels=c("weekday","weekend"))
```

```
## Error in as.Date(activity_full_df$date): object 'activity_full_df' not found
```
### Panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
g <- ggplot(activity_full_df,aes(interval,steps))
```

```
## Error in ggplot(activity_full_df, aes(interval, steps)): object 'activity_full_df' not found
```

```r
g + stat_summary(fun.y=mean, geom="line", color="blue", aes(group = date_type)) + facet_grid(date_type ~ .) + labs(x= "Interval", y= "Number of steps" , title="Activity patterns between weekdays and weekends")
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```
