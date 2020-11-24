library(stringr)
library(dplyr)
library(data.table)
library(fpp3)

source("./gmean_ensemble/utils/model_utilities.R")

horizon <- 379 # to cater for the missing dates at the end of some series
lag<-20

output_file_name <- paste("./results/sub-models/daily_forecasts/lr_boosting_forecasts.txt", sep="")
unlink(output_file_name)

# energy data
load("./data/energy_complete.rda")
catboost_fit <- read.csv("./data/catboost_model_season_impute_modelfit1.csv", colClasses = c("character", "Date", "numeric"))
energy_daily_tsibble_complete <- energy_daily_tsibble_complete %>% left_join(catboost_fit, by=c("meter_id", "date_only")) %>% mutate(energy_agg=energy_agg.x - energy_agg.y) %>% select(-energy_agg.x, -energy_agg.y)
min_y <- min(energy_daily_tsibble_complete$energy_agg)
max_y <- max(energy_daily_tsibble_complete$energy_agg)
energy_daily_tsibble_complete <- energy_daily_tsibble_complete %>% mutate(energy_agg = (energy_agg - min_y)/(max_y - min_y))

# different dates
different_dates <- read.csv("./data/different_dates.csv", colClasses = c("character", "Date"))


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

energy_data_mean_normalized <- energy_data

# convert sales data to list of vectors
energy_data_mean_normalized.list <- split(energy_data_mean_normalized, seq(nrow(energy_data_mean_normalized)))

# perform embedding on the list elements
embedded_energy_data.list <- lapply(energy_data_mean_normalized.list, embed, dimension = lag + 1)

# create one matrix
embedded_energy_data <- do.call(rbind.data.frame, embedded_energy_data.list)

embedded_data <- embedded_energy_data

# integrate the meter id
meter_id <- rep(energy_data_all$meter_id, each=(nrow(metadata_training) - lag))
embedded_data <- cbind.data.frame(meter_id, embedded_data)


# remove the na data
embedded_data_clean <- na.omit(embedded_data)


# fit a normal model
colnames(embedded_data_clean)[which(names(embedded_data_clean) == "V1")] <- "y"
for (i in 2:(lag+1)){
  colnames(embedded_data_clean)[which(names(embedded_data_clean) == paste0("V",i))] <- paste("Lag", (i - 1), sep="")
}

print("Embedding Completed")
predictors <- c()
model <- fit_regul_model(fitting_data = embedded_data_clean, lags=lag, predictors=predictors) 

print("Training Completed")


test_data <- embedded_data_clean %>% group_by(meter_id) %>% slice(n()) %>% ungroup()
test_data <- test_data[, !(names(test_data) %in% c(paste0("Lag", lag)))]


# recursively predict for all the series until the forecast horizon
predictions <- NULL
previous_week <- NULL
for (i in 1:horizon){
  print(paste0("horizon: ",i))
  for (j in (lag-1):1){
    name <- paste("Lag", j, sep="")
    colnames(test_data)[which(names(test_data) == name)] <- paste("Lag", (j+1), sep="")
  }
  if (i == 1){
    colnames(test_data)[which(names(test_data) == "y")] <- paste("Lag", 1, sep="")
  }else{
    colnames(test_data)[which(names(test_data) == "s0")] <- paste("Lag", 1, sep="")
  }
  
  X <- NULL
  
  # add lags
  if (lag != 0){
    lags_columns <- c(paste0("Lag", 1:lag))
    X <- bind_cols(X, test_data%>%select(all_of(lags_columns)))
  }
  
  
  # fit the model
  x_matrix <- model.matrix( ~ .,X)
  
  new_predictions <- predict.glmnet(object=model, newx = x_matrix)
  predictions <- cbind(predictions, new_predictions)
  
  # update the final lags
  last_lag <- paste("Lag", lag, sep="")
  test_data <- test_data[, !(names(test_data) %in% last_lag)]
  
  test_data <- cbind(new_predictions, test_data)
  
}

# renormalize the predictions
true_predictions <- predictions * (max_y - min_y) + min_y
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


