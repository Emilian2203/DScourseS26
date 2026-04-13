library(tidyverse)
library(tidymodels)
library(glmnet)

# Step 4 - load housing data
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", 
                      col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# Step 5 - set seed
set.seed(123456)

# Step 6 - split data
housing_split <- initial_split(housing)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# Step 7 - create recipe
housing_recipe <- recipe(medv ~ ., data = housing) %>%
                    step_log(all_outcomes()) %>%
                    step_mutate(chas = factor(chas)) %>%
                    step_dummy(chas) %>%
                    step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                      ptratio:b:lstat:dis:nox) %>%
                    step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b,
                      lstat,dis,nox, degree=6)

housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped  <- housing_prep %>% bake(new_data = housing_test)

housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y  <- housing_test_prepped  %>% select(medv)

# Step 8 - LASSO with 6-fold CV
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

housing_folds <- vfold_cv(housing_train, v = 6)
lambda_grid <- grid_regular(penalty(), levels = 50)

lasso_workflow <- workflow() %>%
  add_recipe(housing_recipe) %>%
  add_model(lasso_spec)

lasso_tune <- tune_grid(
  lasso_workflow,
  resamples = housing_folds,
  grid = lambda_grid
)

best_lambda_lasso <- lasso_tune %>% select_best(metric = "rmse")
best_lambda_lasso

lasso_final <- lasso_workflow %>%
  finalize_workflow(best_lambda_lasso) %>%
  fit(data = housing_train)

# LASSO In-sample RMSE
tibble(
  truth = housing_train_prepped$medv,
  pred = predict(lasso_final %>% extract_fit_parsnip(),
                 new_data = housing_train_x)$.pred
) %>%
  rmse(truth, pred) %>%
  print()

# LASSO Out-of-sample RMSE
tibble(
  truth = housing_test_prepped$medv,
  pred = predict(lasso_final %>% extract_fit_parsnip(),
                 new_data = housing_test_x)$.pred
) %>%
  rmse(truth, pred) %>%
  print()

# Step 9 - Ridge with 6-fold CV
ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

ridge_workflow <- workflow() %>%
  add_recipe(housing_recipe) %>%
  add_model(ridge_spec)

ridge_tune <- tune_grid(
  ridge_workflow,
  resamples = housing_folds,
  grid = lambda_grid
)

best_lambda_ridge <- ridge_tune %>% select_best(metric = "rmse")
best_lambda_ridge

ridge_final <- ridge_workflow %>%
  finalize_workflow(best_lambda_ridge) %>%
  fit(data = housing_train)

# Ridge In-sample RMSE
tibble(
  truth = housing_train_prepped$medv,
  pred = predict(ridge_final %>% extract_fit_parsnip(),
                 new_data = housing_train_x)$.pred
) %>%
  rmse(truth, pred) %>%
  print()

# Ridge Out-of-sample RMSE
tibble(
  truth = housing_test_prepped$medv,
  pred = predict(ridge_final %>% extract_fit_parsnip(),
                 new_data = housing_test_x)$.pred
) %>%
  rmse(truth, pred) %>%
  print()
