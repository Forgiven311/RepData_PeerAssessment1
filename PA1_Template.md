title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Showing code that is required to load the data into the directory
```{r, echo = TRUE}
activitydf <- read.csv('~/Downloads/activity.csv', sep = ',', header = TRUE)
```
Good to see if the data has actually loaded
```{r, echo = TRUE}
head(activitydf)
```

Then we take time to view the structure of the loaded data
```{r, echo = TRUE}
str(activitydf)
```
## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day

We need to load the dplyr package to so as to access the group_by function which will help us group the data by date
```{r, echo = TRUE}
library(dplyr)
```

To calculate the total number of steps taken per day, we first group the data by date, then summarise it with the sum function.
```{r, echo = TRUE}
perday <- activitydf %>%
        group_by(date = as.Date(date)) %>%
        summarise(steps = sum(steps, na.rm = TRUE))
```
We can then plot a histogram after grouping the data
```{r, echo = TRUE}
hist(perday$steps, main = "Total Number of Steps Taken Per Day", xlab = "Steps")     
```

The we can calculate and report the mean and median of the total number of steps taken per day

```{r, echo = TRUE}
mean(perday$steps, na.rm = TRUE)
median(perday$steps, na.rm = TRUE)
```


## What is the average daily activity pattern?

###### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

First we group the data by interval, then summarise it on the steps variable by its mean and save it into a new object
```{r, echo = TRUE}
avgsteps <- activitydf %>%
        group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
```
From here its easy to plot the graph of the result in the new object.
But first we call the plotting graph device ggplot
```{r, echo = TRUE}
library(ggplot2)
```
Then we are able to plot the result using the ggplot graph ploting device
```{r, echo = TRUE}
ggplot(avgsteps, aes(interval, steps)) + geom_line() + 
        labs(title = "Average Daily Activity Pattern" ) +
        xlab("5-Minute Intervals") + 
        ylab("Average Number of steps taken") + 
        theme(panel.grid.minor = element_line(colour = "blue", linetype = "dotted"))
```
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, echo = TRUE}
avgsteps$interval[which.max(avgsteps$steps)]
```

## Imputing missing values

###### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r, echo = TRUE}
sum(is.na(activitydf$steps))
sum(is.na(activitydf$date))
sum(is.na(activitydf$interval))
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r, echo = TRUE}
newsteps <- data.frame(activitydf$steps)
newsteps[is.na(newsteps),] <- ceiling(tapply(X=activitydf$steps,INDEX=activitydf$interval,FUN=mean,na.rm=TRUE))
```
Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r, echo = TRUE}
newdf <- cbind(newsteps, activitydf[,2:3])
colnames(newdf) <- c("Steps", "Date", "Interval")
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number 
of steps taken per day.

Frist group by date and summariseon steps
```{r, echo = TRUE}
newdata <- newdf %>%
        group_by(Date = as.Date(Date)) %>%
        summarise(Steps = sum(Steps, na.rm = TRUE))
```

then plot the hist
```{r, echo = TRUE}
hist(newdata$Steps, main = "Number of Steps Taken Per Day", xlab = "Steps")     
```
After which, we calculate and report the mean and median total number of steps taken per day.
```{r, echo = TRUE}
mean(newdata$Steps)
median(newdata$Steps)
```
###### Do these values  differ from the estimates from the first part of the assignment? 
Yes, as shown above and below the values do differ

2nd mean  10784.92 & 1st Mean 9354.23
2nd meadian 10909 &   1st meadian 10395

###### What is the impact of imputing missing data on the estimates of the total daily number of steps?
There is some slight difference in the distribution of the data however the impact is not so big


## Are there differences in activity patterns between weekdays and weekends?

###### Create a new factor variable in the dataset with two levels – “weekday” and “weekend”  
indicating whether a given date is a weekday or weekend day.

We shall use the lubridate package to help subset between weekday and weekend
```{r, echo = TRUE}
library(lubridate)
```
Then we create a new collumn using the mutate fuction from the dplyr package to help create the factor variable
```{r, echo = TRUE}
newdf4 <-newdf %>% 
        mutate(DayType = as.factor(ifelse(wday(Date) %in% c(1,7),"weekend","weekday")))
```
###### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository
to see an example of what this plot should look like using simulated data.

We shall use the lattice plotting device
```{r, echo = TRUE}
library("lattice")
```

Then we create a new object in which we group the and then summarise it on the means of the steps variable

```{r, echo = TRUE}
newdf5 <- newdf4 %>% 
group_by(DayType,Interval) %>%
        summarise(avgsteps=mean(Steps))
```

The result of the above object is what we plot in the xyplot to show a distinction between weekdays and weekends steps
```{r, echo = TRUE}
with (newdf5, 
      xyplot(avgsteps ~ Interval|DayType, type="l", 
             ylab="Number of steps",layout=c(1,2)))
```
