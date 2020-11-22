# IEEE_CIS_Comp
This repository contains the experiments we performed to compute the monthly energy consumption of 3248 homes for IEEE CIS Technical Challenge 2020.

# Instructions for Execution
1. ToDo: add ARIMA model script.
2. ToDo: add catboost residuals script.
3. Run "./median_ensemble/global_model_experiments.R" script. Makesure to change the "BASE_DIR" variable to your project directory before running the script. This script first runs 4 global models: Pooled Regression, Random Forest, Multilayer Perceptron Neural Network and CatBoost (with lags). The computed sub-model forecasts will be stored into "./results/sub-models" folder. Then, using the computed sub-model forecasts, it runs the median ensemble model where the medians of sub-model forecasts are chosen as the resultant forecasts. The resultant forecasts will be stored into "results" folder with the name, "median_ensemble_forecasts.csv". This implementation requires 5 R packages: "tidyverse", "nnet", "catboost", "glmnet" and "randomForest". This script automatically install these packages, if your R environment does not include them. If you have already installed these packages, then you can comment lines 2-6.
4. ToDo: Add lasso model scripts and geomean ensemble related scripts 
5. Run "./final_ensemble.R" script. Makesure to change the "BASE_DIR" variable to your project directory before running the script. It computes the weighted average of median ensemble forecasts and geometric mean ensemble forecasts which is considered as our final submission. The final submission will be stored into the "results" folder with the name, "final_forecasts.csv".

