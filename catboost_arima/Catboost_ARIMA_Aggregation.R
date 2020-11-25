# Loading the required packages
library(readr)
library(fpp3)
library(forecast)

# Setting the seed for reproducability
set.seed(1234)

# Output from the CatBoostModel.ipynb script (Catboost Regression Forecasts)
forecasts1 <- read_csv("catboost_model_season_forecasts.csv")

# Output from the ARIMA_Catboost.R script (Catvoost residuals on ARIMA model Forecasts)
forecasts2 <- read_csv("arima_catboost.csv")

final_forecasts <- cbind(forecasts1, energy = forecasts2$energy_forecasts) %>% as_tibble()

# Adding the two forecasts for the final output.
forecast_average <- final_forecasts %>% mutate(final_energy = (energy_agg + energy)) %>% select(meter_id, date_only, final_energy)

final_df <- forecast_average

colnames(final_df)[3] = "energy_agg"

final_df$energy_agg[final_df$energy_agg < 0] <- 0

# Aggregating the daily forecasts to monthly level.
final_forecast_df <-
  final_df %>%  mutate(month = month(date_only)) %>% group_by(meter_id, month) %>% summarise(energy_agg = sum(energy_agg))

energy_forecast_wider <- final_forecast_df %>%
  pivot_wider(names_from = month, values_from = energy_agg)
energy_forecast_wider <- as.data.frame(energy_forecast_wider)

# Reorder the forecast to compatible with the submission format
df_submission <- read_csv("sample_submission.csv")
df_submission <- as.data.frame(df_submission)
output <- energy_forecast_wider[match(df_submission$meter_id, energy_forecast_wider$meter_id),]

colnames(output) <- colnames(df_submission)
write.table(output, "CATARIMA_Forecasts.csv", sep = ",", col.names = TRUE, row.names = FALSE)