#!/bin/bash

# install packages
Rscript ./gmean_ensemble/utils/install_packages.R

# run sub models
Rscript ./gmean_ensemble/models/lasso_regression_for_catboost_residuals.R

Rscript ./gmean_ensemble/models/lasso_regression_with_temp.R 0.4
Rscript ./gmean_ensemble/models/lasso_regression_with_temp.R 0.45

Rscript ./gmean_ensemble/models/expectile_regression_with_temp.R 0.39
Rscript ./gmean_ensemble/models/expectile_regression_with_temp.R 0.57

# aggregate to monthly level
Rscript ./gmean_ensemble/results_aggregator.R

# add back the catboost forecasts for catboost + LR
Rscript ./gmean_ensemble/create_lr_boosting_results.R

# perform the ensembling
Rscript ./gmean_ensemble/ensembles.R
