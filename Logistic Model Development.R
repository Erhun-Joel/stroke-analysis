# This R script is a discovery of the best logistic fit for the stroke model 

# Loading required libraries
library(tidyverse)
library(tidymodels)

# Read data into file
# Kindly add the directory of this file to continue
# stroke.data <- read.csv("") %>% as_tibble()
stroke.data

# Create train samples
set.seed(123)
train <- sample(1:dim(stroke.data)[1], size = dim(stroke.data)[1]*0.8)
length(train)

# Create folds for resampling
set.seed(124)
folds <- vfold_cv(stroke.data[train,], v = 10, strata = stroke)
folds

# Declare the preprocessing recipe
stroke.recipe <-
recipe(
  stroke ~ .,
  data = stroke.data[train,]
) %>%
  step_mutate(
    bmi = parse_number(as.character(bmi), na = c("", "NA", "N/A"))
  ) %>%
  step_impute_median(bmi) %>%
  update_role(id, new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors())
stroke.recipe

# Check out preprocessing results
stroke.recipe %>%
  prep %>%
  juice

# Load library to implement class imbalance correction
library(themis)

# Side note, lets try logrithmic solutions
log.spec <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

# Fiting log specification into resamples
log.result <- workflow(
  preprocessor = stroke.recipe %>% step_smote(stroke),
  spec = log.spec
) %>%
  fit_resamples(
    resamples = folds,
    control = control_resamples(save_pred = TRUE),
    metrics = metric_set(sensitivity, specificity, roc_auc)
  )
log.result

# Check out average results
log.result %>%
  collect_metrics()

# We have a couple of decisions to make:
# 1. Class imbalance solutions
# 3. Regularisation terms.

# First, lets start with class imbalance by implementing two methodologies with the control being none

# create an metric function with your metrics
evaluation.metrics = metric_set(roc_auc, sensitivity, specificity, precision)
evaluation.metrics

# Making a workflow map with different specs
class.imbalance.workflow.set <- workflow_set(
  preproc = list(
    none = stroke.recipe,
    downsample = step_downsample(stroke.recipe, stroke),
    smote = step_smote(stroke.recipe, stroke)
  ),
  models = list(
    logistic.spec = log.spec
  )
)
class.imbalance.workflow.set

# Now fit the workflow set to the folds
class.imbalance.results <-
workflow_map(
  object = class.imbalance.workflow.set,
  fn = "fit_resamples",
  resamples = folds,
  metrics = evaluation.metrics,
  control = control_resamples(save_pred = TRUE)
)
class.imbalance.results

# Check out results
class.imbalance.results %>%
  collect_metrics()

# Give precidence to roc_auc
class.imbalance.results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  arrange(-mean)

# Ok then. Lets now perform regularization on the logistic model

# Redefining the recipe to keep near zero variance variables knowing that regularisation will deal with them
stroke.recipe <-
recipe(
  stroke ~ .,
  data = stroke.data[train,]
) %>%
  step_mutate(
    bmi = parse_number(as.character(bmi), na = c("", "NA", "N/A"))
  ) %>%
  step_impute_median(bmi) %>%
  update_role(id, new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Defining initial grid
log.grid <- grid_regular(penalty(range = c(0, 1.5), trans = NULL), mixture(), levels = c(10, 4))
log.grid

# Redefine specifications
log.spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Fit grid and spec to resamples
log.grid.results <- tune_grid(
  object = log.spec,
  preprocessor = stroke.recipe,
  resamples = folds,
  grid = log.grid,
  metrics = evaluation.metrics,
  control = control_resamples(save_pred = TRUE)
)
log.grid.results

# Plot results
autoplot(log.grid.results)
# It seems any form of regularisation reduces its predicting power.

# Lets now look at the specificity of the model with the hope of increasing it
class.imbalance.results[1,] %>%
  collect_predictions() %>%
  evaluation.metrics(
    truth = stroke,
    estimate = .pred_class,
    .pred_No
  )

# Lets now alter the prediction cutoff using a variable
cutoff.probability = 0.0175
cutoff.probability

# Modify .pred_class and evaluate again
class.imbalance.results[1,] %>%
  collect_predictions() %>%
  mutate(
    .pred_class = as.factor(if_else(.pred_Yes >= cutoff.probability, "Yes", "No"))
  ) %>%
  evaluation.metrics(
    truth = stroke,
    estimate = .pred_class,
    .pred_No
  )
# By altering the cutoff probability, we have been able to increase the models ability to predict who gets stroke at the expense of accuracy
# Now, lets train the final model

# Defining workflow
log.workflow <- workflow(
  preprocessor = stroke.recipe,
  spec = logistic_reg()
)
log.workflow

# Fitting workflow to train data
log.fit <- fit(
  object = log.workflow,
  data = stroke.data[train,]
)
log.fit

# Testing it
bind_cols(
  stroke.data[-train,]["stroke"],
  predict(log.fit, stroke.data[-train,], type = "prob")
) %>%
  mutate(
    .pred_class = as.factor(if_else(.pred_Yes >= cutoff.probability, "Yes", "No")),
    stroke = as.factor(stroke)
  ) %>%
  evaluation.metrics(
    truth = stroke,
    estimate = .pred_class,
    .pred_No
  )
# Does as well as can be

# Save model
saveRDS(
  object = log.fit,
  file = "C:/Users/Erhun/Desktop/Datasets to Complete/Stroke prediction/Data and objects/log_fit.rds"
)
