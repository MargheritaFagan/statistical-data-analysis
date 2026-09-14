# Tree-Based Methods for Regression and Classification
# Datasets: prostate and SAheart (ElemStatLearn)
#
# Methods:
# - Regression and classification trees
# - Tree pruning with cross-validation
# - Bagging
# - Random forests
# - Test-set performance evaluation
# - Variable importance

library(ElemStatLearn)
library(tree)
library(randomForest)


# =============================================================================
# REGRESSION
# =============================================================================

data("prostate")

# Remove the original training-set indicator
prostate_data <- prostate[, -10]

n <- nrow(prostate_data)

# Split the data into training and test sets
set.seed(1234)

train_index <- sample(1:n, ceiling(n / 2))

train_data <- prostate_data[train_index, ]
test_data  <- prostate_data[-train_index, ]


# Regression tree ---------------------------------------------------------------

reg_tree <- tree(
  lpsa ~ .,
  data = train_data
)

plot(reg_tree)
text(reg_tree, pretty = 0, digits = 3)

# Predictions and test MSE
reg_tree_pred <- predict(
  reg_tree,
  newdata = test_data
)

reg_tree_mse <- mean((test_data$lpsa - reg_tree_pred)^2)
reg_tree_mse


# Pruned regression tree --------------------------------------------------------

# Select the optimal tree size using 5-fold cross-validation
set.seed(1234)

cv_reg_tree <- cv.tree(
  reg_tree,
  FUN = prune.tree,
  K = 5
)

best_size_reg <- cv_reg_tree$size[which.min(cv_reg_tree$dev)]
best_size_reg

pruned_reg_tree <- prune.tree(
  reg_tree,
  best = best_size_reg
)

summary(pruned_reg_tree)

plot(pruned_reg_tree)
text(pruned_reg_tree, pretty = 0, digits = 3)

# Predictions and test MSE
pruned_reg_pred <- predict(
  pruned_reg_tree,
  newdata = test_data
)

pruned_reg_mse <- mean((pruned_reg_pred - test_data$lpsa)^2)
pruned_reg_mse


# Bagging -----------------------------------------------------------------------

# Use all predictors at each split
p <- ncol(prostate_data) - 1

set.seed(1234)

bagging_reg <- randomForest(
  lpsa ~ .,
  data = train_data,
  mtry = p,
  importance = TRUE
)

importance(bagging_reg)
varImpPlot(bagging_reg)

bagging_reg_pred <- predict(
  bagging_reg,
  newdata = test_data
)

bagging_reg_mse <- mean((bagging_reg_pred - test_data$lpsa)^2)
bagging_reg_mse


# Random forest -----------------------------------------------------------------

set.seed(1234)

rf_reg <- randomForest(
  lpsa ~ .,
  data = train_data,
  importance = TRUE
)

importance(rf_reg)
varImpPlot(rf_reg)

rf_reg_pred <- predict(
  rf_reg,
  newdata = test_data
)

rf_reg_mse <- mean((rf_reg_pred - test_data$lpsa)^2)
rf_reg_mse


# Compare regression models
regression_results <- data.frame(
  Model = c(
    "Regression Tree",
    "Pruned Regression Tree",
    "Bagging",
    "Random Forest"
  ),
  Test_MSE = c(
    reg_tree_mse,
    pruned_reg_mse,
    bagging_reg_mse,
    rf_reg_mse
  )
)

regression_results


# =============================================================================
# CLASSIFICATION
# =============================================================================

data("SAheart")

# Define predictors and binary response
x <- SAheart[, -10]
y <- as.factor(SAheart[, 10])

heart_data <- data.frame(
  y = y,
  x
)

n <- nrow(heart_data)

# Split the data into training and test sets
set.seed(1234)

train_index <- sample(1:n, ceiling(n / 2))

train_data <- heart_data[train_index, ]
test_data  <- heart_data[-train_index, ]


# Classification tree -----------------------------------------------------------

class_tree <- tree(
  y ~ .,
  data = train_data
)

plot(class_tree)
text(class_tree, pretty = 0)

class_tree_pred <- predict(
  class_tree,
  newdata = test_data,
  type = "class"
)

class_tree_error <- mean(class_tree_pred != test_data$y)
class_tree_accuracy <- 1 - class_tree_error

class_tree_error
class_tree_accuracy


# Pruned classification tree ----------------------------------------------------

set.seed(1234)

cv_class_tree <- cv.tree(
  class_tree,
  FUN = prune.misclass
)

best_size_class <- cv_class_tree$size[which.min(cv_class_tree$dev)]
best_size_class

pruned_class_tree <- prune.misclass(
  class_tree,
  best = best_size_class
)

summary(pruned_class_tree)

pruned_class_pred <- predict(
  pruned_class_tree,
  newdata = test_data,
  type = "class"
)

pruned_class_error <- mean(pruned_class_pred != test_data$y)
pruned_class_accuracy <- 1 - pruned_class_error

pruned_class_error
pruned_class_accuracy


# Bagging -----------------------------------------------------------------------

p <- ncol(heart_data) - 1

set.seed(1234)

bagging_class <- randomForest(
  y ~ .,
  data = train_data,
  mtry = p,
  importance = TRUE
)

importance(bagging_class)
varImpPlot(bagging_class)

bagging_class_pred <- predict(
  bagging_class,
  newdata = test_data,
  type = "class"
)

bagging_class_error <- mean(bagging_class_pred != test_data$y)
bagging_class_error


# Random forest -----------------------------------------------------------------

set.seed(1234)

rf_class <- randomForest(
  y ~ .,
  data = train_data,
  importance = TRUE
)

importance(rf_class)
varImpPlot(rf_class)

rf_class_pred <- predict(
  rf_class,
  newdata = test_data,
  type = "class"
)

rf_class_error <- mean(rf_class_pred != test_data$y)
rf_class_error


# Compare classification models
classification_results <- data.frame(
  Model = c(
    "Classification Tree",
    "Pruned Classification Tree",
    "Bagging",
    "Random Forest"
  ),
  Test_Error = c(
    class_tree_error,
    pruned_class_error,
    bagging_class_error,
    rf_class_error
  )
)

classification_results
