#Importing Libraries
library(dplyr)
library(ggplot2)


##Setting working directory
setwd("C:\\Users\\Razzz\\Desktop\\Reproducible research assignment\\Data")

#####################Loading and preprocessing the data###############################
# 1) Importing Dataset and processing
raw_data <- read.csv('activity.csv')


#Basic EDA
summary(raw_data)
head(raw_data)

########################Number of steps per day ###############################

## 2) Histogram of the total number of steps taken each day
daily_step <- raw_data %>%
              group_by(date) %>%
              summarise(Step_count = sum(steps,na.rm = TRUE))
#Histogram of the above data
hist(daily_step$Step_count,breaks=seq(0,25000,by= 300),main = "No of steps per Day",xlab = "Step Count",col = "red")



## 3.a) Mean number of steps taken each day
#mean step per day
mean(daily_step$Step_count)

## 3.b) Median number of steps taken each day
median(daily_step$Step_count)


######################Average daily activity pattern########################

#mean number of steps
daily_activity <- raw_data %>%
                  group_by(interval) %>%
                  summarise(average_steps = mean(steps,na.rm = TRUE))

## 4.)Time series plot of the average number of steps taken
plot(daily_activity$interval,daily_activity$average_steps,type = 'l',
     main = "Daily Activity Pattern",xlab = "Time Interval",ylab = "Average Step taken")

## 5.) The 5-minute interval that, on average, contains the maximum number of steps
daily_activity[(which(daily_activity$average_steps == max(daily_activity$average_steps))),c("interval")]

####################imputing Null values###################
colSums(is.na(raw_data))
#Steps columns have 2304 NAs value

##STRATEGY
##Impute null values of steps with the mean number of steps taken in interval corresponding
## to null values

# 6.) Code to describe and show a strategy for imputing missing data
processed_data <- left_join(raw_data,daily_activity,"interval"="interval")
processed_data$step_imputed <- ifelse(is.na(processed_data$steps),processed_data$average_steps,
                                      processed_data$steps)
processed_data <- processed_data[,c("step_imputed","date","interval")]
colnames(processed_data)= c("Steps","date","interval")

#daily activity pattern after processing data
daily_activity_processed <- processed_data %>%
                            group_by(date) %>%
                            summarise(avg_step_count = sum(Steps))

## 7.)Histogram of the total number of steps taken each day after missing values are imputed
hist(daily_activity_processed$avg_step_count,breaks=seq(0,25000,by= 300),main = "No of steps per Day",xlab = "Step Count",col = "red")

##Mean and median values after imputing nulls
mean(daily_activity_processed$avg_step_count)
median(daily_activity_processed$avg_step_count)



#####################activity patterns between weekdays and weekends############

wday_proc_data <- processed_data
wday_proc_data$date <- as.POSIXct(wday_proc_data$date, "%Y-%m-%d")

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
wday_proc_data$w_day <- weekdays(wday_proc_data$date)
wday_proc_data$w_day_factor <- as.factor(ifelse(wday_proc_data$w_day == 'Saturday' | wday_proc_data$w_day == 'Sunday',"Weekend","Weekdays"))

#Calculating average daily steps
wday_proc_data_avg <- wday_proc_data %>%
                      group_by(interval,w_day_factor) %>%
                      summarise(avg_step_count = mean(Steps))


## 9.)Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

timeseries_plot<- ggplot(wday_proc_data_avg, aes(x = interval , y = avg_step_count, color = w_day_factor)) +
  geom_line() +
  labs(title = "Activity on Weekdays Vs Weekend", x = "Interval", y = "Average number of steps") +
  facet_wrap(~w_day_factor, ncol = 1, nrow=2)
print(timeseries_plot)

