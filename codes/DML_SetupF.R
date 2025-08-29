##############################################
#          Double Machine Learning Setup     #
#   for Mapping Urban Productivity via NTL   #
##############################################

# 1. Install and Load Necessary Packages

# install.packages("DoubleML")
# install.packages(c("mlr3", "mlr3learners", "mlr3tuning", "mlr3pipelines"))
# install.packages("paradox")

library(DoubleML)      # Main package for Double Machine Learning
library(mlr3)          # Core machine learning framework
library(mlr3learners)  # Provides various ML models (e.g., Lasso, Random Forest, XGBoost)
library(mlr3tuning)    # Tools for hyperparameter tuning within mlr3
library(mlr3pipelines) # For building ML pipelines (if needed)
library(paradox)       # For defining parameter search spaces for tuning

##############################################
# 2. Define and Initialize ML Models for DML
##############################################

# 2A. Main Analysis Learners
# ml_g: Outcome model (E(Y | X)) using Lasso regression via cross-validated glmnet
lasso_learner <- lrn("regr.cv_glmnet")

# ml_m: Treatment model (E(D | X)) using a Random Forest regression learner (rf_learner)
rf_learner <- lrn("regr.ranger")

# 2B. Robustness Check Learners
# Alternative ml_g using XGBoost for regression
xgb_learner <- lrn("regr.xgboost", nrounds = 100, eta = 0.1, max_depth = 6)

# Alternative ml_m using a Random Forest learner for classification
# (This can be compared against other classifiers if desired)
rf_learner_classif <- lrn("classif.ranger")

##############################################
# 3. Define Covariates for the Model
##############################################
# Guangzhou
# X <- c("temp_Lag1", "CDD_Lag1",
       "heatwave_humidity_interaction",
       "hum_adj", "HI")

# São Paulo
# X <- c("temp_Lag1", "CDD_Lag1",
       "heatwave_humidity_interaction",
       "hum_adj")

# Delhi
#X <- c("temp_Lag1", "CDD_Lag1",
       "heatwave_humidity_interaction",
       "hum_adj", "HI",
       "windspeed", "solarenergy", "cloudcover", "precip")

# Cairo
#X <- c("temp_Lag1", "CDD_Lag1",
       "heatwave_humidity_interaction",
       "hum_adj", "HI",
       "windspeed", "solarenergy", "cloudcover", "precip")
# Select the set of predictor variables from the dataset.
# These variables include weather conditions and engineered interaction/lag variables.


# Note: In some regions we extend lags for CDD & temperature to 3 terms.
# (See Appendix A6 & A7 in the supplementary materials for details.)

##############################################
# 4. Construct the DoubleML Data Object
##############################################

# The DoubleMLData object requires:
# - The full dataset ('data')
# - The name of the outcome column (here, log-transformed NTL: "log_Mean.Area.Weighted.Radiance")
# - The treatment column ("is_heatwave")
# - The list of covariate names (colnames(X))
dml_data <- DoubleMLData$new(
  data, 
  y_col = "log_Mean.Area.Weighted.Radiance", 
  d_cols = "is_heatwave", 
  x_cols = colnames(X)
)

##############################################
# 5. Fit the Main DML Model
##############################################

# We set up a DoubleMLPLR (Partially Linear Regression) model:
# - ml_g: Lasso learner (E(Y|X))
# - ml_m: Random Forest regression learner (E(D|X))
# The model uses 10-fold cross-fitting.
dml_model  <- DoubleMLPLR$new(
  data = dml_data,
  ml_g = lasso_learner,  # Nuisance model for the outcome (E(Y | X))
  ml_m = rf_learner,     # Nuisance model for the treatment (E(D | X))
  n_folds = 10
)

# Fit the main model
set.seed(42)
dml_model$fit()

# Display the fit summary (including the estimated effect and its statistical significance)
print(dml_model)

##############################################
# 6. Fit a Robustness Check DML Model
##############################################

# For robustness, we compare results using alternative learners:
# Here, we use XGBoost for the outcome model and a Random Forest classifier for the treatment model.
# Note: For the treatment variable (is_heatwave), which is binary, it is common to use a classifier.
# In this case, rf_learner_classif is used as the treatment model.
dml_data <- DoubleMLData$new(
  data, 
  y_col = "log_Mean.Area.Weighted.Radiance", 
  d_cols = "is_heatwave", 
  x_cols = colnames(X)
)

dml_model  <- DoubleMLPLR$new(
  data = dml_data,
  ml_g = xgb_learner,            # Alternative outcome model using XGBoost regression
  ml_m = rf_learner_classif,     # Treatment model using Random Forest classification
  n_folds = 10
)

set.seed(42)
dml_model$fit()

# Display the robustness check model summary
print(dml_model)