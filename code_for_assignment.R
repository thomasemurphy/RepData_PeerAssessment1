library(dplyr)
library(ggplot2)

all_data <- read.csv("activity.csv")
all_data$date <- as.Date(as.character(all_data$date), format="%Y-%m-%d")

steps_per_day <- all_data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(total_steps=sum(steps))

hist(steps_per_day$total_steps,
     breaks=15,
     main="Histogram of steps per day",
     xlab="total steps")

mean_steps <- mean(steps_per_day$total_steps)
median_steps <- median(steps_per_day$total_steps)

steps_per_interval <- all_data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(avg_steps=mean(steps))

plot(steps_per_interval$interval, steps_per_interval$avg_steps, type="l", xlab="interval", ylab="avg steps")

most_active_interval <- steps_per_interval[steps_per_interval$avg_steps==max(steps_per_interval$avg_steps),
                                           "interval"]

frac_missing <- mean(is.na(all_data$steps))

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
filled_in_NAs <- all_data %>% group_by(interval) %>% mutate(steps = impute.mean(steps))

steps_per_day_imputed <- filled_in_NAs %>%
  group_by(date) %>%
  summarize(total_steps=sum(steps))

hist(steps_per_day_imputed$total_steps,
     breaks=15,
     main="Histogram of steps per day (NAs imputed)",
     xlab="total steps")

mean_steps_imputed <- mean(steps_per_day_imputed$total_steps)
median_steps_imputed <- median(steps_per_day_imputed$total_steps)


filled_in_NAs <- filled_in_NAs %>%
  mutate(day_type = as.factor(ifelse(weekdays(date) == "Saturday" | weekdays(date) == "Sunday",
                                     "weekend","weekday")))

weekday_weekend_avgs <- filled_in_NAs %>% group_by(day_type, interval) %>% summarize(avg_steps=mean(steps))

g <- ggplot(weekday_weekend_avgs, aes(x=interval, y=avg_steps))
g +
  geom_line() +
  ylim(c(0,250)) +
  labs(x="5-minute interval of day", y="average number of steps in interval") +
  facet_grid(rows=vars(day_type))

