# Regularized Regression and Principal Component Regression
# Dataset: prostate (ElemStatLearn)

library(ElemStatLearn)
library(glmnet)
library(pls)

data("prostate")
summary(prostate)


# DATA PREPARATION --------------------------------------------------------

# Remove the original training-set indicator
prostate_data <- prostate[, -10]

# Create predictor matrix and response vector
x <- model.matrix(lpsa ~ ., prostate_data)[, -1]
y <- prostate_data$lpsa

n <- nrow(x)

# Split the data into training and test sets
set.seed(1234)

train <- sample(1:n, ceiling(n / 2))

x_train <- x[train, ]
x_test  <- x[-train, ]

y_train <- y[train]
y_test  <- y[-train]


# RIDGE REGRESSION --------------------------------------------------------

# Select lambda using cross-validation
set.seed(1234)

ridge_cv <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 0
)

plot(ridge_cv)

lambda_ridge <- ridge_cv$lambda.min
lambda_ridge

# Fit the model on the training set
ridge_model <- glmnet(
  x_train,
  y_train,
  alpha = 0,
  lambda = lambda_ridge
)

# Predictions on the test set
ridge_pred <- predict(
  ridge_model,
  newx = x_test
)

# Test Mean Squared Error
ridge_mse <- mean((ridge_pred - y_test)^2)
ridge_mse

# Refit the model on the complete dataset
ridge_final <- glmnet(
  x,
  y,
  alpha = 0,
  lambda = lambda_ridge
)

coef(ridge_final)


# LASSO REGRESSION --------------------------------------------------------

# Select lambda using cross-validation
set.seed(1234)

lasso_cv <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 1
)

plot(lasso_cv)

lambda_lasso <- lasso_cv$lambda.min
lambda_lasso

# Fit the model on the training set
lasso_model <- glmnet(
  x_train,
  y_train,
  alpha = 1,
  lambda = lambda_lasso
)

# Predictions on the test set
lasso_pred <- predict(
  lasso_model,
  newx = x_test
)

# Test Mean Squared Error
lasso_mse <- mean((lasso_pred - y_test)^2)
lasso_mse

# Refit the model on the complete dataset
lasso_final <- glmnet(
  x,
  y,
  alpha = 1,
  lambda = lambda_lasso
)

coef(lasso_final)

# Ridge has a lower test MSE than Lasso for this train-test split.
# Unlike Ridge, Lasso also performs variable selection by shrinking
# some coefficients exactly to zero.


# PRINCIPAL COMPONENT REGRESSION -----------------------------------------

# Use the same training-test split
train_data <- prostate_data[train, ]
test_data  <- prostate_data[-train, ]

# Fit PCR using 10-fold cross-validation
set.seed(1234)

pcr_model <- pcr(
  lpsa ~ .,
  data = train_data,
  scale = TRUE,
  validation = "CV",
  segments = 10
)

validationplot(pcr_model)

summary(pcr_model)

# Cross-validation error
pcr_rmsep <- RMSEP(
  pcr_model,
  estimate = "CV"
)

pcr_rmsep

# Select the optimal number of principal components
# The first position corresponds to 0 components
optimal_components <- which.min(pcr_rmsep$val) - 1
optimal_components

# Predictions on the test set
pcr_pred <- predict(
  pcr_model,
  newdata = test_data,
  ncomp = optimal_components
)

# Test Mean Squared Error
pcr_mse <- mean((pcr_pred - test_data$lpsa)^2)
pcr_mse