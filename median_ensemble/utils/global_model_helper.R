# Create embedded matrix and test set to train the global models
create_input_matrix <- function(dataset, lag, model_type = "pooled_regression", feature_names = NULL, future_features = NULL, other_features = NULL){
  embedded_series <- NULL
  final_lags <- NULL
  series_means <- NULL
  
  all_ids <- unique(dataset$meter_id)
  
  for (i in 1:length(all_ids)) {
    print(i)
    current <- dataset[dataset$meter_id == all_ids[i],]
    time_series <- as.numeric(current$energy_agg)
    
    # Mean normalization
    mean <- mean(time_series)
    series_means <- c(series_means, mean)
    
    if(mean == 0)
      mean <- 1
    
    time_series <- time_series / mean
    
    # Embed the series
    embedded <- embed(time_series, lag + 1)
    
    # Add temporal features
    if(!is.null(future_features)){
      external_features <- current[(lag+1):length(time_series), feature_names]
      embedded <- cbind(embedded, external_features)
    }
    
    # Add other features such as dwelling type and number of rooms
    if(!is.null(other_features)){
      series_other_features_mat <- NULL
      
      for(r in 1:nrow(embedded))
        series_other_features_mat <- rbind(series_other_features_mat, current[1,other_features])
      
      embedded <- cbind(embedded, series_other_features_mat)
    }
    
    if (!is.null(embedded_series)) {
      embedded_series <- as.matrix(embedded_series)
      colnames(embedded) <- colnames(embedded_series)
    }
    embedded_series <- rbind(embedded_series, embedded)
    
    # Create the test set
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))
    
    # Add temporal features to the test set
    if(!is.null(future_features)){
      lag_external_features <- future_features[1, feature_names]
      current_series_final_lags <- cbind(current_series_final_lags, lag_external_features)
    }
    
    # Add other features such as dwelling type and number of rooms to the test set
    if(!is.null(other_features)){
      lag_other_features <- current[1,other_features]
      current_series_final_lags <- cbind(current_series_final_lags, lag_other_features)
    }
    
    if (!is.null(final_lags)) {
      final_lags <- as.matrix(final_lags)
      colnames(current_series_final_lags) <- colnames(final_lags)
    }
   
    final_lags <- rbind(final_lags, current_series_final_lags)
  }
  
  embedded_series <- as.data.frame(embedded_series)
  colnames(embedded_series)[1] <- "y"
  colnames(embedded_series)[2:(lag + 1)] <- paste("Lag", 1:lag, sep = "")
  
  if(!is.null(future_features)){
    indx <- 1:(lag+1)
    embedded_series[indx] <- lapply(embedded_series[indx], function(x) as.numeric(x))
    indx <- (lag+2):ncol(embedded_series)
    embedded_series[indx] <- lapply(embedded_series[indx], function(x) as.factor(x))
  }
  
  final_lags <- as.data.frame(final_lags)
  colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep = "")
  
  if(!is.null(future_features)){
    colnames(final_lags)[(lag + 1):ncol(final_lags)] <- colnames(embedded_series[(lag + 2):ncol(embedded_series)])
    indx <- 1:lag
    final_lags[indx] <- lapply(final_lags[indx], function(x) as.numeric(x))
    indx <- (lag+1):ncol(final_lags)
    final_lags[indx] <- lapply(final_lags[indx], function(x) as.factor(x))
  }
  
  list(embedded_series, final_lags, series_means)
}





