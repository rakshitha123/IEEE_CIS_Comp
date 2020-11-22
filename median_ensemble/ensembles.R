past_year <- read.csv(file.path(DATA_DIR, "monthly_train_data.csv"))
past_year <- past_year[-1]

NUM_OF_SERIES <- 3248
FORECAST_HORIZON <- 12


get_median_forecasts_with_past_year <- function(sub_forecasts, past_year, considered_months){
  final_forecasts <- matrix(NA, nrow = NUM_OF_SERIES, ncol = FORECAST_HORIZON)
  
  for(s in 1:NUM_OF_SERIES){
    for(f in 1:FORECAST_HORIZON){
      current_forecasts <- NULL
      
      for(i in 1:length(sub_forecasts))
        current_forecasts <- c(current_forecasts, as.numeric(sub_forecasts[[i]][s,f]))
      
      if(f %in% considered_months){
        if(as.numeric(past_year[s,f]) > 0)
          current_forecasts <- c(current_forecasts, as.numeric(past_year[s,f]))
      }
      
      final_forecasts[s,f] <- median(current_forecasts)
    }
  }
  
  final_forecasts <- as.data.frame(final_forecasts)
  final_forecasts <- cbind(all_ids, final_forecasts)
  colnames(final_forecasts) <- c("meter_id", colnames(sub_forecasts[[1]]))
 
  final_forecasts
}


replace_forecasts <- function(original_forecasts, horizons, sub_forecasts, past_year, output_file_name, write = TRUE, month_over_prediction_rate = 1){
  for(f in horizons){
    for(s in 1:NUM_OF_SERIES){
      current_forecasts <- NULL
      
      for(i in 1:length(sub_forecasts))
        current_forecasts <- c(current_forecasts, as.numeric(sub_forecasts[[i]][s,f]))
      
      if(!is.null(past_year)){
        if((as.numeric(past_year[s,f]) > 0) | (length(sub_forecasts) == 0))
          current_forecasts <- c(current_forecasts, as.numeric(past_year[s,f]))
      }
      
      original_forecasts[s,f] <- median(current_forecasts)*month_over_prediction_rate
    }
  }
  
  original_forecasts <- as.data.frame(original_forecasts)
  original_forecasts <- cbind(all_ids, original_forecasts)
  colnames(original_forecasts) <- c("meter_id", colnames(sub_forecasts[[1]]))
  
  if(write)
    write.csv(original_forecasts, file.path(FORECAST_DIR, output_file_name), row.names = F)
  
  original_forecasts
}


# Load sub-model forecasts
ffnn_f <- read_csv(file.path(FORECAST_DIR, "sub-models", "ffnn_forecasts.csv"))
ffnn_f <- ffnn_f[-1]

catboost_f <- read_csv(file.path(FORECAST_DIR, "sub-models", "catboost_forecasts.csv"))
catboost_f <- catboost_f[-1]

pooled_regression_f <- read_csv(file.path(FORECAST_DIR, "sub-models", "pooled_regression_forecasts.csv"))
pooled_regression_f <- pooled_regression_f[-1]

random_forest_f <- read_csv(file.path(FORECAST_DIR, "sub-models", "rf_forecasts.csv"))
random_forest_f <- random_forest_f[-1]

catboost_residuals_f <- read.csv(file.path(FORECAST_DIR, "sub-models", "catboost_residuals_forecasts.csv"))
catboost_residuals_f <- catboost_residuals_f[match(as.numeric(all_ids), catboost_residuals_f$meter_id),]
catboost_residuals_f <- catboost_residuals_f[-1]

catboost_residuals_v2 <- read.csv(file.path(FORECAST_DIR, "sub-models", "catboost_residuals_forecasts_v2.csv"))
catboost_residuals_v2 <- catboost_residuals_v2[match(as.numeric(all_ids), catboost_residuals_v2$meter_id),]
catboost_residuals_v2 <- catboost_residuals_v2[-1]

ffnn_over_prediction_rate <- 0.97
random_forest_over_prediction_rate <- 0.95
pooled_regression_over_prediction_rate <- 0.98
catboost_over_prediction_rate <- 0.98
catboost_residuals_under_prediction_rate <- 1.05
catboost_residuals_v2_over_prediction_rate <- 0.97
catboost_residuals_v2_under_prediction_rate <- 1.13

ffnn_f <- ffnn_f * ffnn_over_prediction_rate
catboost_f <- catboost_f * catboost_over_prediction_rate
pooled_regression_f <- pooled_regression_f * pooled_regression_over_prediction_rate
random_forest_f <- random_forest_f * random_forest_over_prediction_rate
catboost_residuals_f <- catboost_residuals_f * catboost_residuals_under_prediction_rate
catboost_residuals_v2_f <- catboost_residuals_v2 * catboost_residuals_v2_over_prediction_rate
catboost_residuals_v2_dec <- catboost_residuals_v2* catboost_residuals_v2_under_prediction_rate


# Ensembling
median_forecasts <- get_median_forecasts_with_past_year(list(catboost_residuals_f, catboost_residuals_v2_f, ffnn_f, pooled_regression_f,catboost_f, random_forest_f), past_year, c(4,6,7,11,12))
final_forecasts <- replace_forecasts(median_forecasts[-1], 12, list(catboost_residuals_v2_dec), past_year, "median_ensemble_forecasts.csv", write = F)
final_forecasts <- replace_forecasts(final_forecasts[-1], 11, list(catboost_residuals_f, ffnn_f, catboost_f), past_year, "median_ensemble_forecasts.csv", write = F, month_over_prediction_rate = 0.98)
final_forecasts <- replace_forecasts(final_forecasts[-1], 10, list(catboost_residuals_f, ffnn_f, catboost_f), NULL, "median_ensemble_forecasts.csv", write = F)
final_forecasts <- replace_forecasts(final_forecasts[-1], 1, list(catboost_residuals_f, ffnn_f), NULL, "median_ensemble_forecasts.csv", write = F)
replace_forecasts(final_forecasts[-1], 4, list(catboost_residuals_f, ffnn_f, catboost_f, pooled_regression_f, random_forest_f), NULL, "median_ensemble_forecasts.csv")

























