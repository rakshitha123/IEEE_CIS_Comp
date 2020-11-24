library(stringr)
library(dplyr)
library(data.table)
library(fpp3)

source("./gmean_ensemble/utils/model_utilities.R")

args <- commandArgs(trailingOnly = TRUE)
tau <- as.numeric(args[1])

horizon <- 379
lags<-20
expectiles<-c(tau)

if (tau == 0.4){
  name_part = "short"
} else{
  name_part = "long"
}

output_file_name <- paste("./results/sub-models/daily_forecasts/lr_lags20_", name_part, "_series_forecasts.txt", sep="")
unlink(output_file_name)

# energy data
energy_daily_tsibble_complete <- read.csv("./data/weekday_mean_season_imputed.csv", colClasses = c("character", "Date", "numeric")) 
energy_daily_tsibble_complete <- energy_daily_tsibble_complete %>% as_tsibble(index=date_only, key=meter_id) %>% group_by(meter_id)

# different dates
different_dates <- read.csv("./data/different_dates.csv", colClasses = c("character", "Date"))

temp_data_new <-read.csv("./data/temp_boostrap_median.csv", colClasses = c("character", "numeric", "Date")) %>% rename(meter_id=meterid)
load("./data/future_temp.rda")

temp_data_old <- df_avg_temp_fixed_fill %>% select(meter_id, date_only, avg_temp) %>% rename(date=date_only, temp=avg_temp)
temp_data_new <- append_row(temp_data_new%>%as_tsibble(index=date, key=meter_id), (14)) %>% fill(temp)
temp_data <- rbind.data.frame(temp_data_old, temp_data_new)
temp_data <- temp_data %>% ungroup() %>% mutate(temp=(temp-min(temp))/(max(temp)-min(temp))) %>% select(meter_id, date, temp)
temp_data <- temp_data %>% arrange(meter_id, date)

for (lag in 1:lags){
  temp_data <- temp_data %>% as_tibble() %>% group_by(meter_id) %>% mutate(!!paste0("temp", quo_name(lag)):=lag(temp, lag))
}

temp_data <- na.omit(temp_data)

# pivot wider
energy_data_all <- energy_daily_tsibble_complete %>% as_tibble() %>% pivot_wider(names_from = date_only, values_from = energy_agg) %>% ungroup()
colnames <- colnames(energy_data_all)
energy_data_all <- energy_data_all[,sort(colnames)]


model_results <- NULL

# convert sales data to matrix
energy_data <- energy_data_all %>% select(-meter_id) 

# create metadata for training
dates <- seq(from=as.Date("2017/01/01"), to=as.Date("2019/1/14"),by = "day")
day_of_month <- as.factor(str_split_fixed(dates,"-", 3)[,3])
day_of_week <- as.factor(weekdays(dates))
month_of_year <- as.factor(str_split_fixed(dates,"-", 3)[,2])
metadata <- cbind.data.frame(date=dates, day_of_month, day_of_week, month_of_year)
metadata_training <- metadata %>% filter(dates <= as.Date("2017/12/31"))

energy_data <- as.matrix(energy_data)
energy_data <- energy_data + 1

# normalize sales data by the mean
series_means <- rowMeans(energy_data, na.rm = TRUE)
energy_data_mean_normalized <- energy_data/series_means

# convert sales data to list of vectors
energy_data_mean_normalized.list <- split(energy_data_mean_normalized, seq(nrow(energy_data_mean_normalized)))

# perform embedding on the list elements
embedded_energy_data.list <- lapply(energy_data_mean_normalized.list, embed, dimension = lags + 1)

# create one matrix
embedded_energy_data <- do.call(rbind, embedded_energy_data.list)

# embed the metadata
meta_data_embedded <- metadata_training %>% slice(rep((lags+1):n(), nrow(energy_data_all))) 

embedded_data <- cbind(meta_data_embedded, embedded_energy_data)

# integrate the meter id
meter_id <- rep(energy_data_all$meter_id, each=(nrow(metadata_training) - lags))
embedded_data <- cbind(meter_id, embedded_data)

# integrate temp data
embedded_data <- embedded_data %>% left_join(temp_data, by=c("meter_id", "date"))


# remove the na data
embedded_data_clean <- na.omit(embedded_data)

# convert to correct data types
embedded_data_clean$day_of_month <- factor(embedded_data_clean$day_of_month)
embedded_data_clean$day_of_week <- factor(embedded_data_clean$day_of_week)
embedded_data_clean$month_of_year <- factor(embedded_data_clean$month_of_year)

# fit a normal model
colnames(embedded_data_clean)[which(names(embedded_data_clean) == "1")] <- "y"
for (i in 2:(lags+1)){
  colnames(embedded_data_clean)[which(names(embedded_data_clean) == toString(i))] <- paste("Lag", (i - 1), sep="")
}

print("Embedding Completed")
predictors <- c("day_of_month", "day_of_week", "month_of_year", "temp")
for (lag in 1:lags) {
  predictors <- c(predictors, paste0("temp", lag))
}
model <- fit_expectile_model(fitting_data = embedded_data_clean, lags=lags, predictors=predictors, expectiles=expectiles) 

print("Training Completed")


test_data <- embedded_data_clean %>% group_by(meter_id) %>% slice(n()) %>% ungroup()
test_data <- test_data[, !(names(test_data) %in% c(paste0("Lag", lags)))]

# recursively predict for all the series until the forecast horizon
predictions <- NULL
previous_week <- NULL
for (i in 1:horizon){
  print(paste0("horizon: ",i))
  for (j in (lags-1):1){
    name <- paste("Lag", j, sep="")
    colnames(test_data)[which(names(test_data) == name)] <- paste("Lag", (j+1), sep="")
  }
  if (i == 1){
    colnames(test_data)[which(names(test_data) == "y")] <- paste("Lag", 1, sep="")
  }else{
    colnames(test_data)[which(names(test_data) == "new_predictions$fitted")] <- paste("Lag", 1, sep="")
  }
  
  test_data <- test_data %>% select(-all_of(predictors))
  test_data <- test_data %>% mutate(date=date+1) %>% left_join(metadata, by="date")
  
  # update temp data
  test_data <- test_data %>% left_join(temp_data, by=c("meter_id", "date"))
  
  new_predictions <- predict(object=model, newdata=test_data)
  predictions <- cbind(predictions, new_predictions$fitted)
  
  # update the final lags
  last_lag <- paste("Lag", lags, sep="")
  test_data <- test_data[, !(names(test_data) %in% last_lag)]
  
  test_data <- cbind(new_predictions$fitted, test_data)
  
}

# renormalize the predictions
true_predictions <- predictions * as.vector(series_means)
true_predictions <- true_predictions - 1
true_predictions <- as.data.frame(true_predictions)
true_predictions <- cbind.data.frame(meter_id=energy_data_all$meter_id, true_predictions)
colnames(true_predictions)[2:ncol(true_predictions)] = paste("pred", 1:(ncol(true_predictions)-1))
true_predictions <- true_predictions %>% mutate(meter_id=as.character(meter_id))

# chop the predictions for required horizons
true_predictions_head <- subset(true_predictions, !(meter_id %in% different_dates$meter_id))
true_predictions_head <- true_predictions_head[,1:366]

last_days <- as.numeric(str_split_fixed(different_dates$final_date,"-", 3)[,3])
different_dates$start <- (31 - last_days + 2)
true_predictions_tail<-NULL
for (j in 1:nrow(different_dates)){
  current_meter_id <- different_dates[j, ]$meter_id
  start <- different_dates[j, ]$start
  all_predictions <- true_predictions %>% filter(meter_id==current_meter_id)
  correct_predictions <- all_predictions[start:(start+364)]
  true_predictions_tail<-rbind(true_predictions_tail, as.numeric(correct_predictions))
}
true_predictions_tail <- as.data.frame(true_predictions_tail)
true_predictions_tail <- cbind.data.frame(meter_id=different_dates$meter_id, true_predictions_tail)


# write true predictions to file
write.table(true_predictions_head, file=output_file_name, row.names = F, sep=",", quote=F)
write.table(true_predictions_tail, file=output_file_name, row.names = F, col.names = F, sep=",", quote=F, append = TRUE)


