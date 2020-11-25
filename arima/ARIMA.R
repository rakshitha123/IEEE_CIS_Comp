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

# Loading the training energy test file and the temperature file.
df_test <- read_csv("energy_test.csv")
df_temp <- read_csv("all_weather_data.csv")
temp <- df_temp

# Forecasting energy demand for multiple households univariately.
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
  meter_id_series <- as.numeric(meter_id_series$energy_agg)
  meter_temp <- as.numeric(meter_temp$avg_temp)
  meter_temp <- (meter_temp- min(meter_temp)) /(max(meter_temp)-min(meter_temp))
  tryCatch({
  model <- auto.arima(ts(meter_id_series, frequency = 7), xreg = cbind(matrix(meter_temp), matrix(I(meter_temp^2)), matrix(I(meter_temp^3)), matrix(I(meter_temp^4))))
  model_forecast <- forecast(model, h = horizon, xreg = cbind(matrix(future_temp), matrix(I(future_temp^2)), matrix(I(future_temp^3)), matrix(I(future_temp^4))))
  model_forecast_mean <- as.numeric(model_forecast$mean)
  model_forecast_mean[model_forecast_mean < 0] <- 0
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

# Aggregating the daily forecasts to monthly level.
final_forecast_df <-
  final_df %>% select(-c('dates')) %>% mutate(month = month(date)) %>% group_by(meterid, month) %>% summarise(energy_agg = sum(energy_forecasts))

energy_forecast_wider <- final_forecast_df %>%
  pivot_wider(names_from = month, values_from = energy_agg)
energy_forecast_wider <- as.data.frame(energy_forecast_wider)

# Reorder the forecast to compatible with the submission format
df_submission <- read_csv("sample_submission.csv")
df_submission <- as.data.frame(df_submission)
output <- energy_forecast_wider[match(df_submission$meter_id, energy_forecast_wider$meterid),]

colnames(output) <- colnames(df_submission)

# Writing the ARIMA forecasts.
write.table(output, "ARIMA_Forecasts.csv", sep = ",", col.names = TRUE, row.names = FALSE)