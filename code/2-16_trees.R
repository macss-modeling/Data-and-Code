# Perspectives on computational modeling (30100)
# Philip Waggoner


# Random forests, Bagging, Boosting

# load some libraries
library(tidyverse)
library(tidymodels)
library(randomForest)
library(patchwork)
library(rcfss)
library(rpart)
library(rpart.plot)
library(ranger)

set.seed(1234)
theme_set(theme_minimal())

# set up the Ames housing data

set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

# A default RF model

m1 <- randomForest(
  formula = Sale_Price ~ .,
  data    = ames_train
)

m1

# How many trees?

tibble(
  mse = m1$mse
) %>%
  mutate(
    ntrees = row_number()
  ) %>%
  ggplot(aes(ntrees, mse)) +
  geom_line() +
  geom_vline(xintercept = which.min(m1$mse), linetype = 2) +
  labs(x = "Number of trees grown",
       y = "OOB MSE")

# Compare with the validation set error
set.seed(123)
valid_split <- initial_split(ames_train, .8)

# training data
ames_train_v2 <- analysis(valid_split)

# validation data
ames_valid <- assessment(valid_split)
x_test <- select(ames_valid, -Sale_Price)
y_test <- ames_valid$Sale_Price

rf_oob_comp <- randomForest(
  formula = Sale_Price ~ .,
  data = ames_train_v2,
  xtest = x_test,
  ytest = y_test
)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$mse)
validation <- sqrt(rf_oob_comp$test$mse)

# compare error rates
tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation
) %>%
  mutate(ntrees = row_number()) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_brewer(type = "qual") +
  xlab("Number of trees")

#
# Tuning hyperparameters

#
# CAUTION: this section takes about 3 minutes to run
#

# First, set up and search a grid
hyper_grid <- expand.grid(
  mtry = seq(20, 30, by = 2),
  node_size = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE = 0
)

for(i in 1:nrow(hyper_grid)) {
  # train
  model <- ranger(
    formula = Sale_Price ~ ., 
    data = ames_train, 
    num.trees = 500,
    mtry = hyper_grid$mtry[i],
    min.node.size = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

# inspect
hyper_grid <- hyper_grid %>% 
  arrange(OOB_RMSE)

hyper_grid %>%
  head(10)

# Repeat 100 times

#
# CAUTION: this loop takes about 3 minutes to run
#

OOB_RMSE <- vector(mode = "numeric", length = 100)
OOB_RMSE

set.seed(123)

for(i in seq_along(OOB_RMSE)) {
  optimal_ranger <- ranger(
    formula = Sale_Price ~ ., 
    data = ames_train, 
    num.trees = 500,
    mtry = hyper_grid$mtry[[1]],
    min.node.size = hyper_grid$node_size[[1]],
    sample.fraction = hyper_grid$sampe_size[[1]]
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

# visualize error
tibble(RMSE = OOB_RMSE) %>%
  ggplot(aes(RMSE)) +
  geom_histogram() +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = "Distribution of OOB RMSE",
       subtitle = "Repeats = 100",
       x = "OOB RMSE",
       y = "Frequency")

#
# Interpretation via feature importance

# helper function to pass to the Predictor$new function from the iml library
pred_ranger <- function(model, newdata){
  results <- predict(model, newdata)
  return(results$prediction)
}

library(iml)

predictor_rf <- Predictor$new(
  model = optimal_ranger,
  data = select(ames_train, - Sale_Price),
  y = ames_train$Sale_Price,
  predict.fun = pred_ranger
)

#
# CAUTION: the following line takes about 3 minutes to run
#

feat_imp_rf <- FeatureImp$new(predictor_rf, loss = "mse")

# now viz
plot(feat_imp_rf) +
  ggtitle("Random forest")


#
# Comparison to bagging

#
# CAUTION: each of these functions take about 1.5 minutes a piece to run 
#

# bag (with oob)
bag_oob <- randomForest(
  formula = Sale_Price ~ .,
  data = ames_train,
  num.trees = 500,
  mtry = ncol(ames_train) - 1,
  nodesize = hyper_grid$node_size[[1]],
  replace = FALSE,
  sampsize = ceiling(hyper_grid$sampe_size[[1]] * nrow(ames_train))
)

# rf (with oob)
rf_oob <- randomForest(
  formula = Sale_Price ~ .,
  data = ames_train,
  num.trees = 500,
  mtry = hyper_grid$mtry[[1]],
  nodesize = hyper_grid$node_size[[1]],
  replace = FALSE,
  sampsize = ceiling(hyper_grid$sampe_size[[1]] * nrow(ames_train))
)

# viz
tibble(
  Bagging = sqrt(bag_oob$mse),
  `Random Forest` = sqrt(rf_oob$mse)
) %>%
  dplyr::mutate(ntrees = row_number()) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_brewer(type = "qual") +
  xlab("Number of trees")

#
# Boosting

# Basic boosting implementation
library(gbm)

set.seed(254)
ANES <- read_csv("nes2008.csv")
lambda_range <- seq(0.0001, 0.04, by = 0.001)

# train/test
n <- seq(1:nrow(ANES))
train <- sample(n, (0.8*length(n)))
test <- n[-train]

# create empty vectors to store error rates
training_set_mse <- rep(NA, length(lambda_range))
test_set_mse <- rep(NA, length(lambda_range))

# loop over lambda 
for(lmi in 1:length(lambda_range)){
  lm <- lambda_range[lmi]
  
  boost2_ANES <- gbm(biden ~ ., data = ANES[train, ], 
                     distribution = "gaussian", 
                     n.trees = 1000, 
                     shrinkage = lm)
  
  # training error
  y_hat <- predict(boost2_ANES, 
                   newdata = ANES[train, ], 
                   n.trees = 1000)
  
  training_set_mse[lmi] <- mean((y_hat - ANES[train, ]$biden)^2)
  
  # testing error
  y_hat <- predict(boost2_ANES, 
                   newdata = ANES[test, ], 
                   n.trees = 1000)
  
  test_set_mse[lmi] <- mean((y_hat - ANES[test, ]$biden)^2)
}

# viz both curves
{
  plot(lambda_range, training_set_mse, 
     type = 'l', 
     pch = 19, 
     col = amerika::amerika_palette("Republican", 1), 
     xlab = 'Lambda', 
     ylab = 'MSE',
     main = 'MSE Over Range of Learning Rate')
lines(lambda_range, test_set_mse, 
      type = 'l', 
      pch = 19, 
      col = amerika::amerika_palette("Democrat", 1))
legend(0.02, 515, legend=c("Training MSE", "Test MSE"),
       col = c(amerika::amerika_palette("Republican", 1), 
               amerika::amerika_palette("Democrat", 1)), 
       lty = 1)
}

# in case you're interested: GBM via caret (takes about 2 minutes to run)
# library(caret)

#boost1_Ames <- train(Sale_Price ~ .,
#                          data = ames_train,
#                          method = "gbm",
#                          trControl = trainControl(method = "cv", 
#                                                   number = 10, 
#                                                   verboseIter = FALSE),
#                          verbose = 0)


# inspect output
#boost1_Ames
#boost1_Ames$results

#
# XGBoost via tidymodels

set.seed(1234)

# load in biden data (if not already done)
ANES <- read_csv("nes2008.csv")
n <- seq(1:nrow(ANES))
train <- sample(n, (0.8*length(n)))
test <- n[-train]

# model
library(tidymodels)

xgb_spec <- boost_tree(
  trees = 500, 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                  
  sample_size = tune(),
  mtry = tune(), 
  learn_rate = tune(),
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")


xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), ANES[train, ]),
  learn_rate(),
  size = 30
)

xgb_wf <- workflow() %>%
  add_formula(biden ~ .) %>%
  add_model(xgb_spec)

folds <- vfold_cv(ANES[train, ])

# tune
doParallel::registerDoParallel()

xgb_res <- tune_grid( # takes a while (~5 minutes) to run...
  xgb_wf,
  resamples = folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

# inspect
collect_metrics(xgb_res)

# viz tuning
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  dplyr::select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(title = "XGBoost Tuning Results",
       x = NULL, y = "RMSE") + 
  theme_bw()

# show top best models
xgb_res %>%
  show_best(metric = "rmse") %>%
  knitr::kable()

# single best model
xgboost_best_params <- xgb_res %>%
  select_best("rmse")

knitr::kable(xgboost_best_params)

# finalize
params_xgboost_best <- xgb_res %>%
  select_best("rmse")

# fit final model, based on best tuned version
final_fit <- boost_tree(
  trees = 500, 
  tree_depth = params_xgboost_best$tree_depth,
  min_n = params_xgboost_best$min_n, 
  loss_reduction = params_xgboost_best$loss_reduction, 
  sample_size = params_xgboost_best$sample_size,
  mtry = params_xgboost_best$mtry,
  learn_rate = params_xgboost_best$learn_rate, 
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

xgboost_wf <- 
  workflows::workflow() %>%
  add_model(final_fit) %>% 
  add_formula(biden ~ .)

xgboost_model_final <- final_fit %>% 
  finalize_model(xgboost_best_params)

# make preds
test_prediction <- xgboost_model_final %>%
  # fit the model on all the training data
  fit(
    formula = biden ~ ., 
    data = ANES[train, ]
  ) %>%
  # use the training model fit to predict the test data
  predict(new_data = ANES[test, ]) 

# append preds to test df
anes_test_set <- ANES[test, ]
anes_test_set$preds <- test_prediction$.pred

# collect error metrics
anes_test_set %>% 
  metrics(truth = biden, 
          estimate = preds)

# viz preds by feature
library(ggpubr)
library(patchwork)

fem <- anes_test_set %>% 
  ggline(x = "female", 
         y = "preds", 
         add = "mean_se",
         color = "female",
         point.size = 1,
         ggtheme = theme_minimal(),
         ylab = "Feelings toward Biden", 
         xlab = "Gender",
         title = "Gender") +
  scale_color_discrete(name="Female",
                       breaks=c("0", "1"),
                       labels=c("Male", "Female")) +
  scale_x_discrete(labels=c("0" = "Male", 
                            "1" = "Female")) +
  coord_flip() + 
  theme(axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

dem <- anes_test_set %>% 
  ggline(x = "dem", 
         y = "preds", 
         add = "mean_se",
         color = "dem",
         point.size = 1,
         ggtheme = theme_minimal(),
         ylab = "Feelings toward Biden", 
         xlab = "Democrat",
         title = "Party") +
  scale_color_discrete(name="Female",
                       breaks=c("0", "1"),
                       labels=c("Non-Democrat", "Democrat")) +
  scale_x_discrete(labels=c("0" = "Non-Democrat", 
                            "1" = "Democrat")) +
  coord_flip() + 
  theme(axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# side by side
xgb_plots <- (fem + dem)

xgb_plots + plot_annotation(
  title = 'Feature Impacts on Feelings toward Biden',
  caption = 'Predictions on test set from tuned XGBoost regression.'
)
