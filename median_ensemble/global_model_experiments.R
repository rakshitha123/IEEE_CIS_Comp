# Comment lines 2-6, if you have already installed the below packages
install.packages("tidyverse")
install.packages("glmnet")
install.packages("catboost")
install.packages("nnet")
install.packages("randomForest")

library(tidyverse)
library(glmnet)
library(catboost)
library(nnet)
library(randomForest)

set.seed(1)


BASE_DIR <- "IEEE_CIS_Comp"
DATA_DIR <- file.path(BASE_DIR, "data")
FORECAST_DIR <- file.path(BASE_DIR, "results")

source(file.path(BASE_DIR, "median_ensemble", "models", "global_models.R", fsep = "/"))
source(file.path(BASE_DIR, "median_ensemble", "utils", "global_model_helper.R", fsep = "/"))
source(file.path(BASE_DIR, "median_ensemble", "utils", "make_submission.R", fsep = "/"))


# Load data
energy_daily_tsibble_complete <- read_csv(file.path(DATA_DIR, "energy_data_with_features.csv"))
all_ids <- unique(energy_daily_tsibble_complete$meter_id)


# Future dates
forecast_start_day <- as.POSIXct("2018-01-01", format = "%Y-%m-%d")
future_days <- data.frame(dates = seq(forecast_start_day, length = 365, by = "1 day"))
future_days$year <- as.numeric(format(future_days$dates, "%Y"))
future_days$month <- as.numeric(format(future_days$dates, "%m"))
future_days$day <- as.numeric(format(future_days$dates, "%d"))
future_days$wday <- as.POSIXlt(future_days$dates)$wday


do_global_forecasting <- function(method, training_set, lag, forecast_horizon, feature_names = NULL, future_features = NULL, other_features = NULL, test_set = NULL, need_meter_id = TRUE){
  
  forecasts <- start_forecasting(training_set, lag, forecast_horizon, method, feature_names, future_features, other_features, test_set)
 
  write.table(forecasts, file.path(FORECAST_DIR, "sub-models", paste0(method, "_daily_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  make_submission(forecasts, paste0(method, "_forecasts.csv"), need_meter_id = need_meter_id)
}


# Experiments - IEEE CIS Techincal Challenge 2020
# -----------------------------------------------

# Pooled regression model with 20 past lags, temporal features, dwelling type and number of rooms
do_global_forecasting("pooled_regression", energy_daily_tsibble_complete, 20, 365, c("month", "wday"), future_days, c("dwelling_type", "num_of_rooms"))

# Catboost with 20 past lags and temporal features
do_global_forecasting("catboost", energy_daily_tsibble_complete, 20, 365, c("month", "wday"), future_days)

# Feed-forward neural network with 20 past lags and temporal features
do_global_forecasting("ffnn", energy_daily_tsibble_complete, 20, 365, c("month", "wday"), future_days)

# Random forest with 20 past lags and temporal features
do_global_forecasting("rf", energy_daily_tsibble_complete, 20, 365, c("month", "wday"), future_days)


# Run median ensemble model
source(file.path(BASE_DIR, "median_ensemble", "ensembles.R", fsep = "/"))



# Experiments - Explainability Competition
# -----------------------------------------------

# Creating the required test set containing the last 20 values of each series - Do this only once
# external_series <- read.csv(file.path(DATA_DIR, "bootstrap_neighbours_new.txt"))
# external_series <- external_series[,(ncol(external_series) - 19) : ncol(external_series)]
# write.table(external_series, file.path(DATA_DIR, "required_test_set.txt"), row.names = F, col.names = F, sep = ",")

  
# Loading test set
external_series <- read.csv(file.path(DATA_DIR, "required_test_set.txt"), header = F)

# Pooled regression model with 20 past lags and temporal features
do_global_forecasting("pooled_regression", energy_daily_tsibble_complete, 20, 365, c("month", "wday"), future_days, test_set = external_series, need_meter_id = FALSE)

# Catboost with 20 past lags and temporal features
do_global_forecasting("catboost", energy_daily_tsibble_complete, 20, 365, c("month", "wday"), future_days, test_set = external_series, need_meter_id = FALSE)

# Feed-forward neural network with 20 past lags and temporal features
do_global_forecasting("ffnn", energy_daily_tsibble_complete, 20, 365, c("month", "wday"), future_days, test_set = external_series, need_meter_id = FALSE)

# Random forest with 20 past lags and temporal features
do_global_forecasting("rf", energy_daily_tsibble_complete, 20, 365, c("month", "wday"), future_days, test_set = external_series, need_meter_id = FALSE)



