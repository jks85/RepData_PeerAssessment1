---
title: 'Reproducible Research Peer Assessment 1: Analyzing Step Data'
author: "Julian Simington"
date: "2024-09-27"
output:  
  html_document:
    keep_md: true



---




## Intro
This project analyzes step data from a wearable device for the dates October 1st 2012 to November 30th 2012. An additional goal is to use R Markdown and knitr to communicate the analysis a more friendly, human readable format.  

## Loading and preprocessing the data

Below we install and load required packages as well as unzip and read the data. Various messages and warnings have been masked in the output. Note that code for downloading the data has not been included per course instructions. The data is read into a dataframe labeled `activity` and the first 5 rows have been displayed.


``` r
install.packages("dplyr",repos="http://cran.us.r-project.org")
library(dplyr)
install.packages("ggpot2",repos="http://cran.us.r-project.org")
library(ggplot2)
```



``` r
unzip('./activity.zip')
activity <- read.csv('./activity.csv')
head(activity,n=5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

The interval column of `activity` corresponds to times (e.g. 145 is 1:45,  2355 is 23:55). The data was reformatted to indicate the time of day using the code below. A few rows of the
updated version of `activity` are displayed.


``` r
# divided interval value by 100 and rounded/formatted to 2 decimal places
# replaced the '.' with a ':' to convert the interval to a time (HH:MM)
# 0 --> 0.00 --> 0:00, 2355 --> 23.55 --> 23:55)
activity$time <- format(round(activity$interval/100,2),nsmall = 2)
activity$time <- gsub(pattern ='\\.', replacement = ':', activity$time)
head(activity,n=5)
```

```
##   steps       date interval  time
## 1    NA 2012-10-01        0  0:00
## 2    NA 2012-10-01        5  0:05
## 3    NA 2012-10-01       10  0:10
## 4    NA 2012-10-01       15  0:15
## 5    NA 2012-10-01       20  0:20
```


## What is mean total number of steps taken per day?

The code below performs following analyses:

1. Computes total number of steps (from Oct 1st 2012 to Nov 30th 2012)
2. Plots a histogram of the total number of steps per day
3. Reports the mean number of steps per day
4. Reports the median number of steps per day

*Note: In the histogram a larger number  of bins increased the relative 
frequency of days with 0 steps. There are a nontrivial number of days with 0 or 
"low" step counts. However, the median and mean are considerably larger than 0 
and there is quite a bit of data near the center. As a result, I used 20 bins,
or ~ 1060 steps. This showed the center of the data while displaying the 
variation and indicating the relatively large number of days with a low step 
count.*
\


``` r
daily_steps <- activity %>% group_by(date) %>%  # total steps per day
                        summarize(total_steps = sum(steps, na.rm = TRUE))

grand_total_steps <- sum(daily_steps$total_steps)

steps_plot <- ggplot(daily_steps, aes(x = total_steps)) # create plot object
steps_plot + geom_histogram(color='blue', fill= 'blue', bins = 20) + # annotate 
             xlab('Steps per Day') +
             ylab('Frequency') +
             ggtitle('Daily Steps from Oct 1 2012 to Nov 30 2012') +
             theme(plot.title = element_text(hjust = 0.5)) +
             scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2))
```

![](Figs/totals-1.png)<!-- -->

``` r
# compute mean, median
mean_daily_steps = mean(daily_steps$total_steps)
median_daily_steps= median(daily_steps$total_steps)
```

This individual took **570608 steps per day in total**.  
They **averaged 9354.23 steps per day** with a 
**median of 10395 steps per day**.  


## What is the average daily activity pattern?  


The code below computes the average number of steps during each time interval
throughout the month, ignoring missing values. Additionally, it plots the
average number of steps for each time interval.

The code below performs following analyses:

1. Constructs a time series plots of average number of steps vs time interval
2. Identifies time interval with highest average step count

\
*Note: missing values were ignored*
\


``` r
# compute average steps in each time interval across all days. Ignore NAs
avg_step_intervals <- activity %>% group_by(time) %>% 
                      summarize(mean_steps = mean(steps, na.rm = TRUE))

# get time interval with max avg step count
busy_time <- with(avg_step_intervals,
                  avg_step_intervals[mean_steps == max(mean_steps),1])

# plot time interval vs avg steps
# group aesthetic is provided since the time intervals are strings
step_intervals_plot <- ggplot(avg_step_intervals,  # create plot object
                              aes(x = time, y = mean_steps, group = 1))

step_intervals_plot + geom_line() + 
                    scale_x_discrete(breaks = 
                    avg_step_intervals$time[c(1,37,73,109,145,181,217,253,288)],
                    guide = guide_axis(angle = 90)) + # set & rotate axis labels
                    xlab('Time Since Midnight (Hours:Minutes)') +
                    ylab('Average Steps') +
                    ggtitle('Step Count by Time of Day') +
                    theme(plot.title = element_text(hjust = 0.5))
```

![](Figs/intervals-1.png)<!-- -->
\
\
This individual had their **highest average step count in the five minute**
**interval starting at  8:35**.   

## Imputing missing values


The code below computes the number of rows containing NA values.

``` r
NA_rows <- nrow(activity) - sum(complete.cases(activity) )
```
\
The original `activity` data  contains **2304 rows with missing data**.
The code below replaces missing step data with the average number of steps in 
the corresponding time interval.



``` r
for (i in 1:nrow(activity)){    # loop through rows of step data
  if (is.na(activity[i,'steps'])){ # check if current step value is NA
    activity[i,'steps'] <- 
      avg_step_intervals[avg_step_intervals$time == 
                                          activity[i,'time'],'mean_steps']
    # replace NA with mean number of steps in corresponding time interval
  }
}

head(activity, 5)
```

```
##       steps       date interval  time
## 1 1.7169811 2012-10-01        0  0:00
## 2 0.3396226 2012-10-01        5  0:05
## 3 0.1320755 2012-10-01       10  0:10
## 4 0.1509434 2012-10-01       15  0:15
## 5 0.0754717 2012-10-01       20  0:20
```
\
The first five rows of the imputed `activity` data frame are displayed above.
Note that the step values for these rows contain values, the appropriate time
interval averages, whereas they were previously `NA` values.
\
\
The code below uses the imputed data to do the following:

1. Recompute the total number daily steps
2. Plot a histogram of the total number of daily steps
3. Recompute the mean number of daily steps
4. Recompute the median number of daily steps  


``` r
imputed_steps <- activity %>% group_by(date) %>% 
                          summarize(total_steps = sum(steps))
# replot histogram with imputed values
imputed_steps_plot <- ggplot(imputed_steps, aes(x = total_steps ))

imputed_steps_plot + geom_histogram(color='blue', fill= 'blue', bins = 20) + 
            xlab('Steps per Day') +
            ylab('Frequency') +
            ggtitle('Daily Steps from Oct 1 2012 to Nov 30 2012') +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2))
```

![](Figs/impute-1.png)<!-- -->

``` r
imputed_mean_daily_steps = mean(imputed_steps$total_steps)
imputed_median_daily_steps= median(imputed_steps$total_steps)
```
\
\
The imputed data resulted in a **mean of 10766.19 total steps per day** and 
**median 10766.19 total steps per day**.  
\
Imputing the data in this manner resulted in a more symmetric distribution 
with less overall variation and increased both the mean and median number of 
steps per day. This was expected as the missing values occurred on the lower end
of the original distribution and were replaced by higher values that lied in the
center of the distribution. It is unclear whether imputing the data in this 
manner is justified in reality. We would need more information about the days 
where data was missing to determine whether specific factors may have caused a 
significant decrease in total steps on those days (e.g. rest, injury).
\


## Are there differences in activity patterns between weekdays and weekends?

The code below creates a factor variable called `day_type` indicating whether a 
particular date is a weekday or weekend. Additionally it displays the new factor
variable in `activity`, and shows the levels of `day_type`.


``` r
# create day of week variable
activity$day_of_week <- weekdays(as.Date(activity$date, # day of week var
                                         format = '%Y-%m-%d'))

# create weekday/weekend factor variable "day_type"
week_day = c('Monday','Tuesday','Wednesday','Thursday','Friday') 
week_end = c('Sunday','Saturday')

activity[activity$day_of_week %in% week_day,'day_type'] <- 'weekday'
activity[activity$day_of_week %in% week_end,'day_type'] <- 'weekend'
activity$day_type <- as.factor(activity$day_type)

head(activity,5) # display new factor variable "day_type"
```

```
##       steps       date interval  time day_of_week day_type
## 1 1.7169811 2012-10-01        0  0:00      Monday  weekday
## 2 0.3396226 2012-10-01        5  0:05      Monday  weekday
## 3 0.1320755 2012-10-01       10  0:10      Monday  weekday
## 4 0.1509434 2012-10-01       15  0:15      Monday  weekday
## 5 0.0754717 2012-10-01       20  0:20      Monday  weekday
```

``` r
levels(activity$day_type) # show factor levels
```

```
## [1] "weekday" "weekend"
```

\
The code below analyzes mean steps per time interval based on whether the 
interval occurred on a *weekday* or the *weekend*. It also creates separate
panel plots:  

1. steps vs time interval (weekday)
2. steps vs time interval (weekend)  


``` r
# analyze type of day vs time interval
type_of_day <- activity %>% group_by(day_type,time) %>% 
                        summarize(mean_steps = mean(steps))

type_of_day_plot <- ggplot(type_of_day, aes(x = time, y = mean_steps, 
                                      group = 1)) # create plot object

type_of_day_plot + geom_line(color = 'blue') + 
                   facet_wrap(vars(day_type), nrow = 2, ncol = 1)+
                   scale_x_discrete(breaks = 
                   avg_step_intervals$time[c(1,37,73,109,145,181,217,253,288)],
                   guide = guide_axis(angle = 90)) + # set & rotate axis labels
                   xlab('Interval (Time Since Midnight)') +
                   ylab('Number of Steps') +
                   ggtitle('Step Count by Time of Day') +
                   theme(plot.title = element_text(hjust = 0.5),
                   strip.background = element_rect(fill = rgb(1, 0.83, 0.61))) 
```

![](Figs/patterns-1.png)<!-- -->

``` r
  # use facet_wrap to create separate plots for weekday and weekend
  # used RGB and https://r-charts.com/colors/ to try to match strip color 
  # in READ.ME file
```


Using the imputed data, the mean steps per time interval look somewhat similar
for weekdays and weekends. The active periods on the weekdays begin slightly
earlier, end slightly earlier, and have a higher peak compared to the weekend 
intervals. The weekend intervals have more consistent activity during the active
periods. We also observe a bit more activity at night in the weekend intervals.
It is unclear how much the method of imputing the data contributed to the
similarity of the two plots. In a deeper analysis it would be worthwhile to 
examine whether weekdays or weekends were over-represented in the imputed data. 
It may also be advisable compare weekday vs weekend steps by ignoring the 
original missing data.
