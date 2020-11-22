num_of_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# Aggregate daily forecasts into monthly level
make_submission <- function(forecasts, output_file_name, meter_ids = NULL){
  if(is.null(meter_ids))
    final_forecasts <- cbind(all_ids)
  else
    final_forecasts <- cbind(meter_ids)
  
  for(d in 1:length(num_of_days)){
    if(d == 1){
      start_day <- 1
      end_day <- 31
    }else{
      start_day <- sum(num_of_days[1:(d-1)]) + 1
      end_day <- sum(num_of_days[1:(d-1)]) + num_of_days[d]
    }
    
    month_forecasts <- forecasts[,start_day:end_day]
    final_month_forecasts <- apply(month_forecasts, 1, sum)
    
    final_forecasts <- cbind(final_forecasts, final_month_forecasts)
  }
  
  colnames(final_forecasts) <- c("meter_id",	"Jan",	"Feb",	"Mar",	"Apr",	"May",	"Jun",	"Jul",	"Aug",	"Sep",	"Oct",	"Nov",	"Dec")
  
  write.csv(final_forecasts, file.path(FORECAST_DIR, "sub-models", output_file_name), row.names = F)
}


