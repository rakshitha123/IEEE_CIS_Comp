library(fpp3)

load(file = "./data/energy_complete.rda")
sample_submission <- read.csv("./data/sample_submission.csv", colClasses = c("character",rep("numeric",12)))


# combine the forecasts from different quantiles into one model
############################################## base LR model
results_short <- read.csv("./results/sub-models/aggregated_lr_lags20_short_series_forecasts.csv", colClasses=c("character",rep("numeric",12)))
results_long <- read.csv("./results/sub-models/aggregated_lr_lags20_long_series_forecasts.csv", colClasses=c("character",rep("numeric",12)))
short_series <- energy_daily_tsibble_complete %>% as_tibble() %>% group_by(meter_id) %>% count() %>% filter(n<=70)

results_long = subset(results_long, !(meter_id %in% short_series$meter_id))
results_short = subset(results_short, (meter_id %in% short_series$meter_id))
results_lr = bind_rows(results_long, results_short)
# sort the results based on the sample submission
results_lr = results_lr[match(sample_submission$meter_id, results_lr$meter_id),]

# write results to file
write.csv(results_lr, "./results/aggregated_lr_lags20_forecasts.csv", row.names = FALSE, quote = FALSE)

############################################## ER model

results_0.39 <- read.csv("./results/sub-models/aggregated_er_lags20_tau0.39_forecasts.csv", colClasses=c("character",rep("numeric",12)))
results_0.57 <- read.csv("./results/sub-models/aggregated_er_lags20_tau0.57_forecasts.csv", colClasses=c("character",rep("numeric",12)))
short_series <- energy_daily_tsibble_complete %>% as_tibble() %>% group_by(meter_id) %>% count() %>% filter(n<=70)

results_0.57 = subset(results_0.57, !(meter_id %in% short_series$meter_id))
results_0.39 = subset(results_0.39, (meter_id %in% short_series$meter_id))
results_er = bind_rows(results_0.57, results_0.39)
# sort the results based on the sample submission
results_er = results_er[match(sample_submission$meter_id, results_er$meter_id),]

# replace december with actual data to account for the underestimation in the forecasts
monthly_data = energy_daily_tsibble_complete %>% mutate(month=month(date_only)) %>% as_tibble() %>% group_by(meter_id, month) %>% summarise(sum(energy_agg)) %>% pivot_wider(names_from = month, values_from=`sum(energy_agg)`)
monthly_data = monthly_data[,c("meter_id", "1","2","3","4","5","6","7","8","9","10","11","12")]
monthly_data = monthly_data[match(sample_submission$meter_id, monthly_data$meter_id),]

results_er[,13] = monthly_data[,13]

# write results to file
write.csv(results_er, "./results/aggregated_er_lags20_forecasts.csv", row.names = FALSE, quote = FALSE)


############################################## ensembling with gmean
# ensembling with gmean

# aggregate results from two different results files
results_1 = read.csv("./results/sub-models/aggregated_lr_lags20_forecasts.csv", colClasses=c("character",rep("numeric",12)))
results_2 = read.csv("./results/sub-models/aggregated_er_lags20_forecasts.csv", colClasses=c("character",rep("numeric",12)))
results_3 = read.csv("./results/sub-models/catboost_lr_forecasts.csv", colClasses=c("character",rep("numeric",12)))
results_4 = read.csv("./results/sub-models/catboost_residuals_forecasts.csv", colClasses=c("character",rep("numeric",12)))
results_5 = read.csv("./results/sub-models/catboost_residuals_forecasts_v3.csv", colClasses=c("character",rep("numeric",12)))
results_6 = read.csv("./results/sub-models/arima_with_temp.csv", colClasses=c("character",rep("numeric",12)))

results_1 = results_1[match(sample_submission$meter_id, results_1$meter_id),]
results_2 = results_2[match(sample_submission$meter_id, results_2$meter_id),]
results_3 = results_3[match(sample_submission$meter_id, results_3$meter_id),]
results_4 = results_4[match(sample_submission$meter_id, results_4$meter_id),]
results_5 = results_5[match(sample_submission$meter_id, results_5$meter_id),]
results_6 = results_6[match(sample_submission$meter_id, results_6$meter_id),]

# geometric mean
results = (results_1[2:13] * results_2[2:13] * results_3[2:13] * results_4[2:13] * results_5[2:13] * results_6[2:13]) ^ (1/6)
results = dplyr::bind_cols(sample_submission$meter_id, results)
colnames(results)[1] = "meter_id"

# write to file
write.csv(results, "./results/gmean_ensemble_with_zeros_forecasts.csv", row.names = FALSE, quote = FALSE)
#################################################################################### replace zeros

high_volume_rows=c(200, 252, 331, 590, 652, 855, 1157, 1960, 2097, 2353, 2366, 2450, 2532, 2837, 2875, 2890, 2916, 3061, 3111, 3163, 3228, 3237, 3245,
           258, 278, 292, 468, 529, 945, 964, 994, 1026, 1092, 1151, 1361, 1440, 1546, 1601, 1633, 1765, 2027, 2272, 2312, 2377, 2425,
           2712, 2869, 2896, 2956, 3140, 3212, 3229)

# median replacement for high volume series
for (row in high_volume_rows) {
  for (month in c(2:13)){
    if (results[row, month] == 0){
      results[row, month] = median(c(results_1[row, month], results_2[row, month], results_3[row, month], results_4[row, month], results_5[row, month], results_6[row, month]))
    }
  }
}

# 0 replacement for low volume series
low_volume_rows = c(302, 516, 776, 1740, 1888, 2036, 2043, 2081, 2152,3225)
for (row in low_volume_rows) {
  results[row, 2:13] = 0
}

write.csv(results, "./results/geomean_ensemble_forecasts.csv", row.names = FALSE, quote = FALSE)
