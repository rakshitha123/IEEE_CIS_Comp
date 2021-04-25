library(fpp3)

model_files <- c("er_lags20_tau0.39_forecasts.txt", "er_lags20_tau0.57_forecasts.txt", "lr_lags20_long_series_forecasts.txt", "lr_lags20_short_series_forecasts.txt", "lr_boosting_forecasts.txt")

for (file in model_files){
  results <- read.csv(paste0("./results/sub-models/daily_forecasts/", file), colClasses=c("character",rep("numeric",365)))
  results[results < 0] <- 0
  colnames <- as.character(seq(from=as.Date("2018/01/01"), to=as.Date("2018/12/31"),by = "day"))
  colnames(results)[2:ncol(results)] <- colnames
  
  
  # aggregate to monthly
  pivoted_results <- results %>% pivot_longer(-meter_id, names_to = "date", values_to = "value")
  pivoted_results$date <- as.Date(pivoted_results$date)
  pivoted_results$month <- month(pivoted_results$date)
  pivoted_results <- pivoted_results %>% group_by(meter_id, month) %>% summarise(sum(value))
  pivoted_results <- pivoted_results %>% pivot_wider(names_from = month, values_from = `sum(value)`)
  colnames(pivoted_results)[1] <- "meter_id"
  colnames(pivoted_results)[2:ncol(pivoted_results)] <- month.abb[as.numeric(colnames(pivoted_results)[2:ncol(pivoted_results)])]
  
  sample_submission <- read.csv("./data/sample_submission.csv", colClasses = c("character",rep("numeric",12)))
  pivoted_results <- pivoted_results[match(sample_submission$meter_id, pivoted_results$meter_id),]
  
  write.csv(pivoted_results, paste0("./results/sub-models/aggregated_", strsplit(file, ".txt")[[1]], ".csv"), row.names = FALSE, quote = FALSE)
}