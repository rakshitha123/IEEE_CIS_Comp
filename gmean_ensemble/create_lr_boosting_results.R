library(fpp3)

load(file = "./data/energy_complete.rda")
sample_submission <- read.csv("./data/sample_submission.csv", colClasses = c("character",rep("numeric",12)))

# add back the catboost forecasts
results_catboost <- read.csv("./results/sub-models/catboost_forecasts_v2.csv", colClasses=c("character",rep("numeric",12)))
results_catboost <- results_catboost[match(sample_submission$meter_id, results_catboost$meter_id),]

results_lr <- read.csv("./results/sub-models/aggregated_lr_boosting_forecasts.csv", colClasses=c("character",rep("numeric",12)))
results_lr <- results_lr[match(sample_submission$meter_id, results_lr$meter_id),]

results_added <- results_lr[,2:13] + results_catboost[,2:13]
results_added = dplyr::bind_cols(sample_submission$meter_id, results_added)
colnames(results_added)[1] = "meter_id"

write.csv(results_added, "./results/sub-models/catboost_lr_forecasts.csv", row.names = FALSE, quote = FALSE)