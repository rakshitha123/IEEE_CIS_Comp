BASE_DIR <- "IEEE_CIS_Comp"
DATA_DIR <- file.path(BASE_DIR, "data")
FORECAST_DIR <- file.path(BASE_DIR, "results")

energy_daily_tsibble_complete <- read_csv(file.path(DATA_DIR, "energy_data_with_features.csv"))
all_ids <- unique(energy_daily_tsibble_complete$meter_id)

get_weighted_average <- function(sub_forecasts, weights, output_file_name){ 
  final_forecasts <- weights[1]*sub_forecasts[[1]]
  
  for(i in 2:length(sub_forecasts))
    final_forecasts <- final_forecasts + weights[i]*sub_forecasts[[i]]
  
  final_forecasts <- cbind(all_ids, final_forecasts)
  colnames(final_forecasts) <-  c("meter_id",	"Jan",	"Feb",	"Mar",	"Apr",	"May",	"Jun",	"Jul",	"Aug",	"Sep",	"Oct",	"Nov",	"Dec")
  
  write.csv(final_forecasts, file.path(FORECAST_DIR, output_file_name), row.names = F)
}


# Combining median ensemble and geomean ensemble to obtain the final forecasts
median_ensemble <- read.csv(file.path(FORECAST_DIR, "median_ensemble_forecasts.csv"))
median_ensemble <- median_ensemble[-1]

geomean_ensemble <- read.csv(file.path(FORECAST_DIR, "geomean_ensemble_forecasts.csv"))
geomean_ensemble <- geomean_ensemble[match(as.numeric(all_ids), geomean_ensemble$meter_id),]
geomean_ensemble <- geomean_ensemble[-1]

get_weighted_average(list(geomean_ensemble, median_ensemble), c(0.4, 0.6), "final_forecasts.csv")