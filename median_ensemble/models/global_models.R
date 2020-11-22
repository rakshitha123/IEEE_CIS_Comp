# Forecast with different lags
start_forecasting <- function(dataset, lag, forecast_horizon, model_type = "pooled_regression", feature_names = NULL, future_features = NULL, other_features = NULL){
  # Create embedded matrix for training models and the first test set
  result <- create_input_matrix(dataset, lag, model_type, feature_names, future_features, other_features)

  embedded_series <- result[[1]]
  final_lags <- result[[2]]
  series_means <- result[[3]]

  fit_model(embedded_series, lag, final_lags, forecast_horizon, series_means, dataset, model_type, feature_names, future_features, other_features)
}


# Fit and forecast from a global model
fit_model <- function(fitting_data, lag, final_lags, forecast_horizon, series_means, dataset, model_type = "pooled_regression", feature_names = NULL, future_features = NULL, other_features = NULL) {
  # Create the formula
  formula <- "y ~ "
  for(predictor in 2:ncol(fitting_data)){
    if(predictor != ncol(fitting_data))
      formula <- paste0(formula, colnames(fitting_data)[predictor], " + ")
    else
      formula <- paste0(formula, colnames(fitting_data)[predictor])
  }
  
  formula <- paste(formula, "+ 0", sep="")
  formula <- as.formula(formula)
  
  # Fit the model based on type
  if(model_type == "pooled_regression")
    model <- glm(formula = formula, data = fitting_data)
  else if(model_type == "catboost"){ 
    catboost_params <- list(iterations=700,
                            learning_rate=0.02,
                            depth=12,
                            loss_function = 'MAE',
                            eval_metric = 'RMSE',
                            random_seed = 23,
                            bagging_temperature = 0.2,
                            od_type='Iter',
                            metric_period = 75,
                            od_wait=100)
    
    if(is.null(future_features))
      train_pool <- catboost.load_pool(data = as.matrix(fitting_data[-1]), label = as.matrix(fitting_data[,1]))
    else{
      cat_feature_indexes <- (lag + 1):ncol(final_lags)
      train_pool <- catboost.load_pool(data = fitting_data[-1], label = as.matrix(fitting_data[,1]), cat_features = cat_feature_indexes-1)
    }
    
    model <- catboost.train(train_pool, params = catboost_params )
  }else if(model_type == "rf")  
    model <- randomForest(formula = formula, data = fitting_data, do.trace=TRUE, ntree = 10)
  else if(model_type == "ffnn")
    model <- nnet(formula = formula, data = fitting_data, size = 20, decay = 0.02, maxit = 110, linout = FALSE)
  
  # Do forecasting
  forec_recursive(fitting_data, lag, model, final_lags, forecast_horizon, series_means, dataset, model_type, feature_names, future_features, other_features)
}


# Recursive forecasting of the series until a given horizon
forec_recursive <- function(fitting_data, lag, model, final_lags, forecast_horizon, series_means, dataset, model_type = "pooled_regression", feature_names = NULL, future_features = NULL, other_features = NULL) {
  
  predictions <- NULL
  
  print("Started forecasting")
  
  for (i in 1:forecast_horizon){
    print(i)
    if(model_type == "pooled_regression")
      new_predictions <- predict.glm(object=model, newdata = as.data.frame(final_lags))  
    else if(model_type == "ffnn" | model_type == "rf") 
      new_predictions <- predict(model, as.data.frame(final_lags))  
    else if(model_type == "catboost"){
    
      if(is.null(future_features))
        catboost_final_lags <- catboost.load_pool(final_lags)
      else{
        cat_feature_indexes <- (lag + 1):ncol(final_lags)
        catboost_final_lags <- catboost.load_pool(final_lags, cat_features = cat_feature_indexes-1)
      }
      
      new_predictions <- catboost.predict(model, catboost_final_lags)
    }
    
    predictions <- cbind(predictions, new_predictions)
    
    # Update the test set for the next horizon
    if(i < forecast_horizon){
      final_lags <- final_lags[-lag]
     
      # Update lags for the next horizon
      final_lags <- cbind(new_predictions, final_lags)
      colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
      
      # Update the external temporal features for the next horizon
      if(!is.null(future_features)){
        final_lags[,(lag+1):(lag+length(feature_names))] <- data.frame(matrix(rep(future_features[(i + 1), feature_names], nrow(final_lags)), nrow = nrow(final_lags), ncol = length(feature_names), byrow = T))
        
        for(col in (lag+1):ncol(final_lags))
          final_lags[col] <- lapply(final_lags[col], as.factor)[[1]]
      }
      
      final_lags <- as.data.frame(final_lags)
    }
  }
  
  # Renormalize the predictions
  true_predictions <- predictions * as.vector(series_means)
  true_predictions
}
