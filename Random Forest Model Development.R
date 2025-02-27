# This script is made to find the best random forest mode to go with this

# Loading required packages
library(tidyverse)
library(tidymodels)
library(themis)
library(readr)

# Loading data
# KIndly place location directory in the command below
stroke.data <- read.csv("https://raw.githubusercontent.com/Erhun-Joel/stroke-analysis/refs/heads/main/Data%20and%20objects/healthcare-dataset-stroke-data.csv") %>%
  as_tibble() %>%
  filter(gender != "Other")
stroke.data

# Making sure stroke variable is a factor and parse bmi variable
stroke.data <- stroke.data %>%
  mutate(
    stroke = as.factor(stroke),
    bmi = parse_number(bmi, na = c("", "NA", "N/A"))
  )
stroke.data

# Set out a training series for the stroke data
set.seed(124)
train = sample(1:dim(stroke.data)[1], dim(stroke.data)[1]*0.8)
train

# Creating recipe to prepare stroke
stroke.recipe <-
recipe(
  stroke ~ .,
  data = stroke.data[train,]
) %>%
  step_impute_median(bmi) %>%
  update_role(id, new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())
stroke.recipe

# Prep out data for recipe inspection
stroke.recipe %>%
  prep %>%
  juice

# Create 10 folds for re-sampling
folds <- vfold_cv(
  data = stroke.data[train,],
  strata = stroke
)
folds

# Create a metric specification for sensitivity and specificity
evaluation.metrics <- metric_set(roc_auc, sensitivity, specificity)

# Lets try a random forest to get base performance
rf.spec <- rand_forest(trees = 1500) %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf.spec

# Create workflow
rf.workflow <- workflow(
  preprocessor = stroke.recipe,
  spec = rf.spec
)

# Fitting workflow to resamples
set.seed(100)
rf.results <- rf.workflow %>%
  fit_resamples(
    resamples = folds,
    control = control_resamples(save_pred = TRUE),
    metrics = evaluation.metrics
  )
rf.results

# Check resulting metrics
rf.results %>%
  collect_metrics()

# Ok then. Now we deal with two problems:
# 1. Class imbalance
# 2. Regularisation

# Lets start with class imbalance
# Defining workflow set with different class imbalance techniques
rf.wflow.set <-
workflow_set(
  preproc = list(
    none = stroke.recipe,
    downsample = step_downsample(stroke.recipe, stroke),
    smote = step_smote(stroke.recipe, stroke)
  ),
  models = list(
    random.forests = rf.spec
  )
)
rf.wflow.set

# Fitting workflow set
set.seed(101)
rf.wflow.results <-
workflow_map(
  object = rf.wflow.set,
  resamples = folds,
  control = control_resamples(save_pred = TRUE),
  metrics = evaluation.metrics
)
rf.wflow.results

# Check metrics, giving greater weight to roc_auc
rf.wflow.results %>%
  collect_metrics %>%
  filter(.metric == "roc_auc") %>%
  arrange(-mean)
# Downsample works best

# Lets see if proportions improve prediction

# Declaring downsampling workflow set
downsample.wflow.set <-
workflow_set(
  preproc = list(
    none = step_downsample(stroke.recipe, stroke),
    downsample1 = step_downsample(stroke.recipe, stroke, under_ratio = 1.5),
    downsample2 = step_downsample(stroke.recipe, stroke, under_ratio = 2),
    downsample3 = step_downsample(stroke.recipe, stroke, under_ratio = 2.5),
    downsample4 = step_downsample(stroke.recipe, stroke, under_ratio = 3)
  ),
  models = list(
    random.forests = rf.spec
  )
)
downsample.wflow.set

# Fitting workflow set
set.seed(123)
downsample.wflow.results <-
workflow_map(
  object = downsample.wflow.set,
  resamples = folds,
  control = control_resamples(save_pred = TRUE),
  metrics = evaluation.metrics
)
downsample.wflow.results

# Check metrics, giving greater weight to roc_auc
downsample.wflow.results %>%
  collect_metrics %>%
  filter(.metric == "roc_auc") %>%
  arrange(-mean)

# It seems downsampling with the under_ratio 3 is optimal

# Now lets turn to regularisation

# Create rf.grid
rf.grid = grid_regular(finalize(mtry(range = c(5, 20)), stroke.data), min_n(range = c(20, 70)), levels = 6)
rf.grid

# Declaring tunning spec
rf.spec <-
rand_forest(mtry = tune(), min_n = tune(), trees = 1500) %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf.spec

# Fiting grid and spec to resamples
doParallel::registerDoParallel()
set.seed(124)
rf.tune.results <-
tune_grid(
  preprocessor = step_downsample(stroke.recipe, stroke, under_ratio = 3),
  object = rf.spec,
  resamples = folds,
  grid = rf.grid,
  control = control_resamples(save_pred = TRUE),
  metrics = evaluation.metrics
)

# Plotting out tunning results
rf.tune.results %>%
  autoplot()

# Get out highest performing parameters
rf.parameters <-
select_best(rf.tune.results, metric = "roc_auc")
rf.parameters

# Now train final random fit
set.seed(1144)
rf.fit <- workflow(
  preprocessor = step_downsample(stroke.recipe, under_ratio = 3),
  spec = set_mode(rand_forest(mtry = rf.parameters$mtry, min_n = rf.parameters$min_n, trees = 1500), mode = "classification") %>%
    set_engine(engine = "ranger", importance = "impurity")
) %>%
  fit(
    data = stroke.data[train,]
  )
rf.fit

# Testing fit on test data
predict(rf.fit, new_data = stroke.data[-train,], type = "prob") %>%
  bind_cols(
    stroke.data[-train,]["stroke"]
  ) %>%
  mutate(
    .pred_class = as.factor(if_else(.pred_1 >= 0.015, 1, 0))
  ) %>%
  evaluation.metrics(
    truth = stroke,
    estimate = .pred_class,
    .pred_0
  )
# !!!
# The roc_auc is suprisingly high!
# In the test, the specificity is increased at the expense of sensitivity to predict the number of people who are likely to have stroke

# Save fitted model
saveRDS(rf.fit, file = "C:/Users/Erhun/Desktop/Datasets to Complete/Stroke prediction/Data and objects/rand_fit.rds")

