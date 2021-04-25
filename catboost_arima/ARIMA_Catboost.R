# Loading the required packages
library(readr)
library(fpp3)
library(forecast)
library(robustbase)

# Setting the seed for reproducability
set.seed(1234)

# Loading the training energy training file.
df <- read_csv("energy_df_full.csv")

# Selecting the required columns.
energy_daily_tsibble_complete <- df %>% select(meter_id, date_only, energy_agg, max_temp, avg_temp, min_temp)
no_of_meters <- unique(energy_daily_tsibble_complete$meter_id)

univariate_forecast <- matrix(NA, nrow = length(no_of_meters), ncol = 365)

# Loading the training energy test file.
df_test <- read_csv("energy_test.csv")

# Loading the Catboost fitting.
df_residuals <- read_csv("catboost_model_season_fitting.csv")

# Loading the temperature file for 2017
df_temp <- read_csv("all_weather_data.csv")

temp <- df_temp

# Loading the imputed data file.
df_imputed <- read_csv("weekday_mean_season_imputed.csv")

# Creating the input to the ARIMA function (which is the residual between the training data and the catboost fit)
energy_daily_tsibble_complete <- cbind(energy_daily_tsibble_complete, imputed = df_imputed$energy_agg)
energy_daily_tsibble_complete <- energy_daily_tsibble_complete %>% select(-energy_agg) %>% rename(energy_agg = imputed) %>% as_tibble()
energy_daily_tsibble_complete <- cbind(energy_daily_tsibble_complete, model_predictions = df_residuals$energy_agg)
energy_daily_tsibble_complete <- energy_daily_tsibble_complete %>% mutate(energy_agg = (energy_agg - model_predictions))
energy_daily_tsibble_complete <- energy_daily_tsibble_complete %>% as_tibble()

# Forecasting the residuals of the catboost model univariately.
for(i in 1: length(no_of_meters)){
  print(i)
  meter <- no_of_meters[i]
  meter_id_series <- energy_daily_tsibble_complete[energy_daily_tsibble_complete$meter_id == meter, 'energy_agg']
  meter_temp <- energy_daily_tsibble_complete[energy_daily_tsibble_complete$meter_id == meter, 'avg_temp']
  meter_date <- energy_daily_tsibble_complete[energy_daily_tsibble_complete$meter_id == meter, 'date_only']
  
  next_year_temp <- temp[temp$meter_id == meter, 'avg_temp']
  
  # The ending date of some of the households are not equal to 2017-12-31. 
  # We capture these households and set the prediction horizon of those households accordingly.
  # i.e prediction horizon would be greater than 365 for those houses, but we only take the forecasts
  # relevant to year 2018 (365 days)
  final_date <- meter_date %>% tail(n =1) %>% pull(date_only)
  final_date <- as.Date(final_date)
  correct_final_date <- as.Date("2017-12-31")
  time_difference <- as.numeric(correct_final_date - final_date)
  
  # We use the boostrapping methodology to estimate the future temperature data.
  if(time_difference  == 0){
    horizon = 365
    future_temp <- as.numeric(next_year_temp$avg_temp)
    bootstrapped_series <- bld.mbb.bootstrap(ts(future_temp), 100)
    temp_matrx <- do.call(rbind, bootstrapped_series)
    future_temp <- colMedians(temp_matrx)
    future_temp <- (future_temp- min(future_temp)) /(max(future_temp)-min(future_temp))
    
  }else{
    horizon = time_difference + 365 
    days_between <- seq(as.Date(final_date + 1), as.Date(correct_final_date), by="days")
    partial_temp <- temp %>% filter(meter_id == meter) %>% filter(date_only %in% days_between) %>% pull(avg_temp)
    
    next_temp <- as.numeric(next_year_temp$avg_temp)
    bootstrapped_series <- bld.mbb.bootstrap(ts(next_temp), 100)
    temp_matrx <- do.call(rbind, bootstrapped_series)
    temp_next <- colMedians(temp_matrx)
    
    future_temp <- c(partial_temp, as.numeric(temp_next))
    future_temp <- (future_temp- min(future_temp)) /(max(future_temp)-min(future_temp))
    
  }
  # Performing the ARIMA forecasts.
  meter_id_series <- as.numeric(meter_id_series$energy_agg)
  meter_temp <- as.numeric(meter_temp$avg_temp)
  meter_temp <- (meter_temp- min(meter_temp)) /(max(meter_temp)-min(meter_temp))
  tryCatch({
  model <- auto.arima(ts(meter_id_series, frequency = 7), xreg = cbind(matrix(meter_temp), matrix(I(meter_temp^2)), matrix(I(meter_temp^3)), matrix(I(meter_temp^4))))
  model_forecast <- forecast(model, h = horizon, xreg = cbind(matrix(future_temp), matrix(I(future_temp^2)), matrix(I(future_temp^3)), matrix(I(future_temp^4))))
  model_forecast_mean <- as.numeric(model_forecast$mean)
  model_forecast_mean <- tail(model_forecast_mean, n = 365)
  }, error = function(e){
  model_forecast_mean <- rep(0,365)
  })
  univariate_forecast[i,] <- model_forecast_mean
}

univariate_forecast <- as.data.frame(univariate_forecast)
univariate_forecast_meters <- cbind(meterid = no_of_meters, univariate_forecast)

energy_df_forecasts <- univariate_forecast_meters %>% pivot_longer(-meterid, names_to = "dates", values_to = "energy_forecasts")

final_df <- cbind(energy_df_forecasts, date = df_test$ds)

# Writing the Catboost residual forecasts.
write.table(final_df, "arima_catboost.csv", sep = ",", col.names = TRUE, row.names = FALSE)