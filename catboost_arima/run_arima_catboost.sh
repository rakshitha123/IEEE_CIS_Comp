#!/bin/bash

# install R packages.
Rscript ./catboost_arima/install_packages.R

# install python packages.
python ./catboost_arima/install_packages.py

# run CatBoostModel.py.
python ./catboost_arima/CatBoostModel.py

# run ARIMA_Catboost.R
Rscript ./catboost_arima/ARIMA_Catboost.R

# run Catboost_ARIMA_Aggregation.R
Rscript ./catboost_arima/Catboost_ARIMA_Aggregation.R
