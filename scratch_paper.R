
test <- all_data %>% 
  mutate(steps = ifelse(is.na(steps),
                        steps_per_interval["interval"==interval,"avg_steps"][[1]], 
                        steps))

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
all_data <- all_data %>% group_by(interval) %>% mutate(steps = impute.mean(steps))