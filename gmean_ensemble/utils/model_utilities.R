library(glmnet)
library(expectreg)

set.seed(1)


# fit a normal model
fit_glmnet_model = function(fitting_data, lags, predictors) {
  X = NULL
  Y = NULL
  
  Y = fitting_data %>% select(y)
  # add predictors
  X = fitting_data %>% select(predictors)
  
  # add lags
  if (lags != 0){
    lags_columns = c(paste0("Lag", 1:lags))
    X = bind_cols(X, fitting_data%>%select(lags_columns))
  }
  
  # fit the model
  x_matrix = model.matrix( ~ .,X)
  y_matrix =  model.matrix( ~ .-1,Y)
  model = glmnet(x = x_matrix, y=y_matrix, lambda = 0)
}

# fit an expectile regression model
fit_expectile_model = function(fitting_data, lags, predictors, expectiles) {
  formula = "y ~ "
  
  if (length(predictors) > 0){
    for (predictor in predictors){
      formula = paste0(formula, predictor)
      if (predictor != predictors[length(predictors)] || lags > 0){
        formula = paste0(formula, "+")
      }
    } 
  }
  
  if (lags != 0){
    for(lag in 1:lags){
      formula = paste0(formula, "Lag", lag)
      if (lag != lags){
        formula = paste0(formula, "+")
      }
    }  
  }
  
  print(formula)
  formula = as.formula(formula)
  
  # fit the model
  model = expectreg.ls(formula = formula, data=fitting_data, expectiles=expectiles)
  
}

# fit a lasso regularized model
fit_regul_model = function(fitting_data, lags, predictors) {
  X = NULL
  Y = NULL
  
  Y = fitting_data %>% select(y)
  
  if (length(predictors)>0){
    # add predictors
    X = fitting_data %>% select(all_of(predictors))
  }
  
  # add lags
  if (lags != 0){
    lags_columns = c(paste0("Lag", 1:lags))
    X = bind_cols(X, fitting_data%>%select(all_of(lags_columns)))
  }
  
  # fit the model
  x_matrix = model.matrix( ~ .,X)
  y_matrix =  model.matrix( ~ .-1,Y)
  
  cv_results = cv.glmnet(x_matrix, y_matrix, alpha=1)
  
  #fit the model
  model = glmnet(x = x_matrix, y=y_matrix, alpha=1, lambda = cv_results$lambda.min)
}