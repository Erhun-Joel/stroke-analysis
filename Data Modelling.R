# Load necessary libraries
library(tidyverse)
library(tidymodels)

# Loading stroke data to environment. Fill the empty string below with file location to continue
# stroke.data <- read.csv("") %>% as_tibble()
stroke.data

# Prepare data for modeling

# Input proper NA values in bmi column
stroke.data <-
stroke.data %>%
  mutate(
    bmi = case_when(
      bmi == "N/A" ~ NA,
      TRUE ~ bmi
    ),
    bmi = as.numeric(bmi),
    stroke = if_else(stroke == 1, "Yes", "No")
  )
stroke.data

# Data Budgeting

# Create validation split
set.seed(1233)
stroke.split <-
stroke.data %>%
  initial_split(strata = stroke)
stroke.split

# Take out testing and training data from validation split
stroke.train <- training(stroke.split)
stroke.test <- testing(stroke.split)
stroke.train

# Creating re-sample folds of training and testing data
set.seed(1223)
stroke.folds <- vfold_cv(stroke.train, strata = stroke)
stroke.folds

# Using recipe to define basic pre-processing steps
basic.recipe <-
recipe(stroke ~ ., data = stroke.train) %>%
  update_role(id, new_role = "id") %>%
  step_impute_median(bmi) %>%
  step_log(bmi) %>%
  step_dummy(all_nominal_predictors())
basic.recipe

# Lets test run a simple random forest and define its specifications
rf.spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf.spec

# Fitting with resamples
rf.simple.result <-
fit_resamples(
  preprocessor = basic.recipe,
  object = rf.spec,
  resamples = stroke.folds,
  control = control_resamples(save_pred = TRUE)
)
rf.simple.result

# Checking Overall Model performance
rf.simple.result %>%
  collect_metrics()

# Visualizing its ROC by fold instances
rf.simple.result %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(stroke, .pred_No) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_path(show.legend = FALSE) +
  theme_minimal()
# It seems do do quite well in explaining the variations between those who have stroke and those who don't

# Lets try a simple logistic mode
# Define a logistic specification
logistic.spec <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")
logistic.spec

# Since the pre-processing steps can be the same, lets simply compute the logistic result
log.result <- fit_resamples(
  preprocessor = basic.recipe,
  object = logistic.spec,
  resamples = stroke.folds,
  control = control_resamples(save_pred = TRUE)
)
log.result

# Check overall model performance
log.result %>%
  collect_metrics()

# Visualizing the ROC for each fold
log.result %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(stroke, .pred_No) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_path()

# Seems that logistic regression performs slightly better than random forests. Lets see if regression increases performance
# Lets make up a grid for mixture and penalty
logistic.grid <- grid_regular(penalty(), mixture(), levels = 5)
logistic.grid

# Creating a logistic specification with tuning instructions for penalty or mixture
logistic.spec <- logistic_reg(mixture = tune(), penalty = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
logistic.spec

# The pre-processing steps remains the same. Therefore, immediate computations can be made
# Compute logistic tuning results
logistic.tune.result <-
tune_grid(
  preprocessor = basic.recipe,
  object = logistic.spec,
  resamples = stroke.folds,
  grid = logistic.grid,
  control = control_resamples(save_pred = TRUE, verbose = FALSE)
)
logistic.tune.result

# Check out roc_auc metric
logistic.tune.result %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  arrange(-mean)

# Lets viaualize this
logistic.tune.result %>%
  autoplot() +
  theme_minimal()
# It seems any form of regularization simply reduces the predictive power of the model

# I believe the problem with these models lies with the class imbalance experienced
# Lets solve this using nearest neigbours
library(themis)

# Declaring recipe that simulates new observations using smote with an overall no stroke to stroke ratio of 0.6
balanced.recipe <-
recipe(stroke ~ ., data = stroke.train) %>%
  update_role(id, new_role = "id") %>%
  step_impute_median(bmi) %>%
  step_log(bmi) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(stroke, over_ratio = 0.6)
balanced.recipe

# Lets place this recipe in random forest again
set.seed(126)
balanced.rf.results <- fit_resamples(
  preprocessor = balanced.recipe,
  object = rf.spec,
  resamples = stroke.folds,
  metrics = metric_set(accuracy, roc_auc, sensitivity, specificity),
  control = control_resamples(save_pred = TRUE)
)
balanced.rf.results

# Check out results
balanced.rf.results %>%
  collect_metrics()

# Lets plot the roc curve
balanced.rf.results %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(stroke, .pred_No) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_path()
# A little dissapointing to see this didn't improve results substaintially

# Perhaps boosted models would work more better

# Defining boosted tree spec using the xgboost engine
boosted.spec <- boost_tree() %>%
  set_mode("classification") %>%
  set_engine("xgboost")
boosted.spec

# Computing results of boosted tree
boost.results <- fit_resamples(
  preprocessor = balanced.recipe,
  object = boosted.spec,
  resamples = stroke.folds,
  metrics = metric_set(accuracy, roc_auc, sensitivity, specificity),
  control = control_resamples(save_pred = TRUE)
)
boost.results

# Checking out the results
boost.results %>%
  collect_metrics()
# Seems to do perform slightly worst

# Lets try log again, this time with a balanced data to train against

# Define logistic specifications
logistic.spec <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")
logistic.spec

# Compute results
log.balanced.results <- fit_resamples(
  preprocessor = balanced.recipe,
  object = logistic.spec,
  resamples = stroke.folds,
  metrics = metric_set(accuracy, roc_auc, sensitivity, specificity),
  control = control_resamples(save_pred = TRUE)
)
log.balanced.results

# Checking out results
log.balanced.results %>%
  collect_metrics()

# Now lets see if performing reguarisation helps in any way

# Defining logistic tunning specifications
logistic.spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
logistic.spec

# Computing results
log.balanced.tuned <- tune_grid(
  object = logistic.spec,
  preprocessor = balanced.recipe,
  resamples = stroke.folds,
  grid = logistic.grid,
  control = control_resamples(save_pred = TRUE, verbose = FALSE),
  metrics = metric_set(accuracy, roc_auc, sensitivity, specificity)
)
log.balanced.tuned

# Checking out results
log.balanced.tuned %>%
  collect_metrics()

# Plotting results to derive insights
log.balanced.tuned %>%
  autoplot()
# Again, it seems any form of regularisation cause depreciation in roc_auc value

# Select the best model by specificity ensuring resonable roc_auc

# Collect parameters for best specificity
log.parameters <-
log.balanced.tuned %>%
  select_best(metric = "specificity")
log.parameters

# Checking the roc_auc
log.balanced.tuned %>%
  collect_metrics() %>%
  filter(.config == log.parameters$.config)
# The roc_auc is seemly reasonable

# Lets train the best log model

# Declaring spec
logistic.spec <- logistic_reg(penalty = log.parameters$penalty, mixture = log.parameters$mixture) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
logistic.spec

# Training the fit 
logistic.fit <- workflow() %>%
  add_recipe(balanced.recipe) %>%
  add_model(logistic.spec) %>%
  fit(
    data = stroke.train
  )
logistic.fit

# Lets test on testing data
logistic.fit %>%
  predict(stroke.test, type = "prob") %>%
  mutate(.pred_class = if_else(.pred_Yes >= 0.4, "Yes", "No")) %>%
  select(.pred_class) %>%
  bind_cols(
    stroke.test %>%
      select(stroke)
  ) %>%
  table()

# Creating a metric set to automatically calculate metrics
evaluation.metrics <- metric_set(roc_auc, sensitivity, specificity)
evaluation.metrics

# Lets now see the metrics on test set
logistic.fit %>%
  predict(stroke.test, type = "prob") %>%
  mutate(.pred_class = as.factor(if_else(.pred_Yes >= 0.4, "Yes", "No"))) %>%
  bind_cols(
    stroke.test %>%
      select(stroke) %>%
      mutate(stroke = as.factor(stroke))
  ) %>%
  evaluation.metrics(
    truth = stroke,
    estimate = .pred_class,
    .pred_No
  )
# About 80% of variations is explained by this finalized model

# There are too many categorical variables in the data for LDA to be considered optimal.

# The models don't seem to improve well on the matter of prediction because of the class imbalance
# Smote resampling seems to be poor at solving this problem

# Therefore, I have come up with my own methodology:
# Randomly selecting 'no stroke' occurances and pass that through a random forest model.
# I mean every time a resample runs, let the model down sample by taking all stroke occurance and random data points of no stroke occurance.
# Find a way to create a fold list that takes all the sample that has stroke and random samples of those that do not.
custom.folds <-
function(x, yes.prop = 0.75, no.prop = 0.2, folds.n = 100){
  
  # Define function data
  data = x

  # Add row tracking column
  data$numerics = 1:dim(x)[1]

  # Get out the row numbers that corressponds to both stroke values in the dataset
  stroke.yes <- data[data$stroke == "Yes",]$numerics
  stroke.no <- data[data$stroke == "No",]$numerics

  # Drop numerics column
  data$numerics = NULL

  # Create output list for rsplit containment
  split.list <- list()

  # Creating for-loop to fill defined split list
  for(i in 1:folds.n){

    # Creating split information by taking random amounts of numbers from stroke.yes and stroke.no
    # Stroke instances are varied less than no stroke instances
    # This hopefully helps the model get a sense of the entire variation

    # Defining analysis and assessment vectors
    analysis.vector = c(sample(stroke.yes, size = length(stroke.yes)*yes.prop), sample(stroke.no, size = length(stroke.no)*no.prop))
    assess.vector = c(stroke.yes, sample(stroke.no, size = length(stroke.yes)))

    # Create actual split specifications
    y <- make_splits(
      list(
        analysis = analysis.vector,
        assessment = assess.vector
      ),
      data = data
    )

    # Save created split to predeclared split list
    split.list <- append(split.list, list(y))

  }

  # Create split dataframe
  split.tibble = new_rset(
    splits = split.list,
    ids = ifelse(1:folds.n < 10, paste0("id_0", 1:folds.n), paste0("id_", 1:folds.n)),
    attrib = list(note = "custom splits"), subclass = "rset"
  )

  # Output split data frame
  return(split.tibble)

}
special.folds <- custom.folds(stroke.train, yes.prop = 0.9, no.prop = 0.1, folds.n = 250)
special.folds

# Lets try the random forest model again using this fold. It should work better now.
rf.spec <- rand_forest(trees = 1500) %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf.spec

# Create another recipe
new.recipe <-
recipe(stroke ~ ., data = stroke.train) %>%
  update_role(id, new_role = "id") %>%
  step_impute_median(bmi) %>%
  step_log(bmi) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())
new.recipe

# Lets compute the results of custom folds using random specifications
custom.folds.result <- fit_resamples(
  workflow(
    preprocessor = new.recipe,
    spec = rf.spec
  ),
  resamples = special.folds,
  control = control_resamples(save_pred = TRUE, verbose = FALSE, save_workflow = TRUE, extract = function(x){
    extract_fit_parsnip(x)
  }),
  metrics = metric_set(accuracy, roc_auc, sensitivity, specificity)
)
custom.folds.result

# Check the metrics of the computations
custom.folds.result %>%
  collect_metrics()
# Quite impressive, noting that parameter tunning hasn't been done yet

# We saved the workflows in the custom report. Lets make a function to use the report to make predictions
custom.random.forest.prediction <- function(results, data, recipe = new.recipe){

  # Apply recipe to data frame for preping and baking
  prediction.data = bake(object = prep(new.recipe), data)

  # Define dataframe to output
  outcome = tibble(placeholder = 1:dim(prediction.data)[1])

  # Create for-loop to collect predictions for the number of extracted fits available
  for(i in 1:length(results[[".extracts"]])){

    # Predicting with individual fits
    x = predict(
      object = results$.extracts[[i]]$.extracts[[1]],
      new_data = prediction.data,
      type = "prob"
    )

    # Bindling results to the outcome tibble
    outcome <- bind_cols(
      outcome,
      tibble(
        !!paste0("model_", i) := x$.pred_No
      )
    )
  }

  outcome$placeholder = NULL

  return(outcome)

}
special.rf.outcome <- custom.random.forest.prediction(results = custom.folds.result, data = stroke.test)
special.rf.outcome

# We have the output probability of no stroke for all the fits in the custom.fold.results
# Lets view it by observations to determine how to aggregate
# We can vary the row number to see different observation outputs across each model
tibble(
  prob = unname(as_vector(special.rf.outcome[132,]))
) %>%
  ggplot(aes(prob)) +
  geom_boxplot()
# By selecting different rows, we see that the mean is an accurate description of the distribution

# Again, we can verify to see if the mean exist in the range of standard deviation
stats.calc <- function(vector){
  return(
    tibble(
      vector.mean = mean(vector),
      vector.median = median(vector),
      vector.sd = sd(vector),
      range = range(vector)[2] - range(vector)[1]
    )
  )
}
stats.calc(unname(as_vector(special.rf.outcome[132,])))

# By observing these, it can be said that mean represents a good approximation of the distribution

# Now lets remake the function to actually predict classes
custom.random.forest.prediction <- function(results, data, recipe = new.recipe){

  # Apply recipe to data frame for preping and baking
  prediction.data = bake(object = prep(recipe), data)

  # Define dataframe to output
  outcome = tibble(placeholder = 1:dim(prediction.data)[1])

  # Create for-loop to collect predictions for the number of extracted fits available
  for(i in 1:length(results[[".extracts"]])){

    # Predicting with individual fits
    x = predict(
      object = results$.extracts[[i]]$.extracts[[1]],
      new_data = prediction.data,
      type = "prob"
    )

    # Bindling results to the outcome tibble
    outcome <- bind_cols(
      outcome,
      tibble(
        !!paste0("model_", i) := x$.pred_No
      )
    )
  }

  # Remove tibble placeholder
  outcome$placeholder = NULL

  # Create a tibble of average probabilities
  probabilities = tibble(
    .pred_No = rowMeans(outcome),
    .pred_Yes = 1 - .pred_No
  )

  # Output results
  return(probabilities)

}

# Save results
custom.final.result <-
custom.random.forest.prediction(results = custom.folds.result, data = stroke.test)
custom.final.result

# Moment of truth!
# Combine with actual results and give out metrics
bind_cols(
  stroke = as.factor(stroke.test[["stroke"]]),
  custom.final.result
) %>%
  mutate(
    .pred_class = as.factor(if_else(.pred_Yes >= 0.25, "Yes", "No"))
  ) %>%
  evaluation.metrics(
    truth = stroke,
    estimate = .pred_class,
    .pred_No
  )
# Thats quite disappointing. The roc_auc metric dropped by more than 10%.
# Probably, the entire model ensemble was overfitted. Lets prevent that by two things:
# 1. Regularising on one random forest model and retraining on the spec
# 2. Reducing the number of folds used to create the model ensemble

# Define tunning sepc for random forest
rf.spec <- rand_forest(mtry = tune(), trees = 1500, min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf.spec

# Set up grid to search against
rf.grid = grid_regular(finalize(mtry(), stroke.train), min_n(), levels = 5)
rf.grid

# Now, lets train the models
rf.grid.results <- workflow(
  preprocessor = new.recipe,
  spec = rf.spec
) %>%
  tune_grid(
    resamples = stroke.folds,
    grid = rf.grid,
    metrics = evaluation.metrics
  )
rf.grid.results

# Check out plot progression
rf.grid.results %>%
  autoplot()

# Get best acting parameters
best.parameters <-
rf.grid.results %>%
  select_best(metric = "roc_auc")
best.parameters

# Now redefine the spec to be used
rf.spec <- rand_forest(mtry = best.parameters$mtry, trees = 1500, min_n = best.parameters$min_n) %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf.spec

# Creating another folds with smaller fold instances
another.special.folds <- custom.folds(stroke.train, yes.prop = 0.9, no.prop = 0.15, folds.n = 20)
another.special.folds

# Fiting random forests on this new folds
another.custom.folds.result <- fit_resamples(
  workflow(
    preprocessor = new.recipe,
    spec = rf.spec
  ),
  resamples = another.special.folds,
  control = control_resamples(save_pred = TRUE, verbose = FALSE, save_workflow = TRUE, extract = function(x){
    extract_fit_parsnip(x)
  }),
  metrics = evaluation.metrics
)
another.custom.folds.result

# Check out the metrics
another.custom.folds.result %>%
  collect_metrics()

# Perhaps, this would translate to better test results. Lets try it.

# Create predictions from test data
another.custom.predictions <- custom.random.forest.prediction(
  results = another.custom.folds.result,
  data = stroke.test
)
another.custom.predictions

# Lets test results
bind_cols(
  stroke = as.factor(stroke.test[["stroke"]]),
  another.custom.predictions
) %>%
  mutate(
    .pred_class = as.factor(if_else(.pred_Yes >= 0.25, "Yes", "No"))
  ) %>%
  evaluation.metrics(
    truth = stroke,
    estimate = .pred_class,
    .pred_No
  )
# Still seems to be overfitiing no matter. More experimenting needed.









