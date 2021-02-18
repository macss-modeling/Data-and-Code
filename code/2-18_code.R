# Perspectives on Computational Modeling (30100)
# Philip Waggoner

# Code for class


# NOTE: No on your own code for today due to the storm/internet access issues still persisting in the US


# SVM 
## theoretical first

# load libs and set seed
library(tidyverse)
library(e1071)

set.seed(2345)

# create the data for classification
x <- matrix(rnorm(20*2), ncol=2)
class <- c(rep(-1,10), rep(1,10))
x[class == 1, ] = x[class == 1, ] + 1

train <- data.frame(x = x, class = as.factor(class)) 

ggplot(train, 
       aes(x.1, x.2, 
           color = class)) +
  geom_point(size = 2) +
  theme_minimal()

# fit
svmfit <- svm(class ~ ., 
              data = train, 
              kernel = "linear", 
              cost = 10); summary(svmfit)

# now plot
plot(svmfit, train)

# Which obs were the support vectors, determining the SVC? 
svmfit$index

#
# what about a smaller cost value
svmfit <- svm(class ~ ., 
              data = train, 
              kernel = "linear", 
              cost = 0.1); summary(svmfit)

plot(svmfit, train)

svmfit$index

# now we get a larger number of support vectors, because the margin is now wider given the lesser penalty which allowed for more observations to be considered in the range of the margin and threshold placement, i.e., more support vectors with a wider margin (smaller cost to widening the margin)


# CV
## Instead of manually searching, though, let's use CV to find the best cost value
set.seed(2345)

tune_c <- tune(svm, 
               class ~ ., 
               data = train, 
               kernel = "linear", 
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

# best?
tuned_model <- tune_c$best.model
summary(tuned_model) # same *results* as high cost/penalty at beginning


#
# Now we can predict the class label on a set of test observations based on our trained SVM
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1
test <- data.frame(x = xtest, class = as.factor(ytest))

# predict class labels
# Here we use the best model obtained through cross-validation in order to make predictions:
class_pred <- predict(tuned_model, 
                      test)

table(predicted = class_pred, 
      true = test$class)

# Based on our tuned SVM, *around* 14 test observations are correctly classified

# Some suggest a general "rule of thumb" value for cost is 0.01. So, what if we had instead used this value of cost instead? Would results change?

svmfit_01 <- svm(class ~ ., 
                 data = train, 
                 kernel = "linear", 
                 cost = .01)

class_01 <- predict(svmfit_01, 
                    test)

table(predicted = class_01, 
      true = test$class)

# not a huge change, but a little worse due to a lesser penalty (wider margin)...


# 
# Let's now increase the overlap and test the strength of the SVM
set.seed(2345)

x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,]+2
x[101:150,] = x[101:150,]-2
class <- c(rep(1,150),rep(2,50))
overlap_data <- data.frame(x = x, class = as.factor(class))

ggplot(overlap_data, 
       aes(x.1, x.2, color = factor(class))) +
  geom_point(size = 1) +
  theme_minimal()

# let's split 70/30, train/test
train_overlap <- overlap_data %>%
  sample_frac(0.7)

test_overlap <- overlap_data %>%
  setdiff(train_overlap)

# Now fit the SVM
# Recall, re: gamma, it controls the influence that each case has on the position of the hyperplane and is used by all the kernel functions except the linear kernel.

# The larger gamma, the more granular the contours of the decision boundary will be (potentially leading to overfitting).

# The smaller gamma, the less granular the contours of the decision boundary will be (potentially leading to underfitting)

svmfit_overlap <- svm(class ~ ., 
                      data = train_overlap, 
                      kernel = "radial",  
                      gamma = 1, 
                      cost = 1)

plot(svmfit_overlap, train_overlap)


# Let's see what happens when we dramatically increase cost to reduce training errors
svmfit_overlap2 <- svm(class ~ ., 
                       data = train_overlap, 
                       kernel = "radial", 
                       gamma = 1, 
                       cost = 10000)

plot(svmfit_overlap2, train_overlap)

# Significantly increased variance of the fit to the training data, and thus a threat of....?



# Let's return to CV to tune the SVM in this overlapping case
# We can perform cross-validation using tune() to select the best choice of gamma() and cost for an SVM with a radial kernel:
set.seed(2345)

tune_c <- tune(svm, 
               class ~ ., 
               data = train_overlap, 
               kernel = "radial",
               ranges = list(cost = c(0.1, 1, 10, 100, 1000), 
                             gamma = c(0.5, 1, 2, 3)
               ))

tuned_overlap_model <- tune_c$best.model

# viz
plot(tuned_overlap_model, train_overlap)

table(true = test_overlap$class, 
      pred = predict(tuned_overlap_model, 
                     newdata = test_overlap))

# how did we do?


## Application with real data
library(tidyverse)
library(here)
library(caret)
library(tictoc)
library(tidymodels)

set.seed(1234)
theme_set(theme_minimal())

# first (and focus): Herron's study/Krehbiel's data
herron <- read_csv(here("data", "Herron.csv")) 


# OUTCOME: "House members who chose to cosponsor this bill, formally known as H.R. 3266" (1 = yes, 0 = no); From Herron's replication of Krehbiel 1995, on p. 94 of Herron 

# By using a geometric solution (via SVM), instead of a probability-based solution, we not only bypass these issues that Herron rightly points out in probability-based classification, which are namely overstating predictive accuracy (i.e., not accounting for uncertainty in postestimation, which gives the traditional PCP that Herron was updating), but we ALSO get a much more accurate solution as we will see.

# set up response for using caret to fit the models
herron <- herron %>%
  mutate(cosp_fact = factor(cosp, 
                            levels = c(0, 1), 
                            labels = c("No", "Yes")))


# Approach 1: linear kernel (SVC)
cv_ctrl <- trainControl(method = "cv",
                        number = 10,
                        savePredictions = "final",
                        classProbs = TRUE)

# fit model with linear kernel 
{
  tic()
  svm_linear <- train(
    cosp_fact ~ ada + ntu + democrat + firstelected + 
      margin + appromember + budgetmember, 
    data = herron, 
    method = "svmLinear",
    trControl = cv_ctrl,
    tuneLength = 10)
  toc()
}

# can draw indiv ROC curve and calc AUC
#svm_linear_roc <- roc(predictor = svm_linear$pred$Yes,
#                      response = svm_linear$pred$obs,
#                      levels = rev(levels(herron$cosp_fact)))

#plot(svm_linear_roc)
#auc(svm_linear_roc)

# polynomial kernel
{
  tic()
  svm_poly <- train(
    cosp_fact ~ ada + ntu + democrat + firstelected + 
      margin + appromember + budgetmember, 
    data = herron, 
    method = "svmPoly",
    trControl = cv_ctrl)
  toc()
}

# radial kernel
{
  tic()
  svm_radial <- train(
    cosp_fact ~ ada + ntu + democrat + firstelected + 
      margin + appromember + budgetmember, 
    data = herron, 
    method = "svmRadial",
    trControl = cv_ctrl)
  toc()
}

# Plot ROC for all kernels, and overlay curves
bind_rows(
  Linear = svm_linear$pred,
  Polynomial = svm_poly$pred,
  Radial = svm_radial$pred,
  .id = "kernel"
) %>%
  group_by(kernel) %>%
  roc_curve(truth = obs, 
            estimate = Yes) %>%
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = kernel)) +
  geom_path() +
  geom_abline(lty = 3) +
  scale_color_brewer(type = "qual") +
  coord_flip() +
  labs(title = "Comparison of ROC Curves by Kernel",
       subtitle = "10-fold CV",
       x = "Specificity",
       y = "Sensitivity",
       color = NULL) +
  theme(legend.position = "bottom")

# Plot AUC for all kernels and directly compare (note the consistency across each; but note linear is the best, with the smallest amount (by a hair) under the curve left unexplained)
bind_rows(
  Linear = svm_linear$pred,
  Polynomial = svm_poly$pred,
  Radial = svm_radial$pred,
  .id = "kernel"
) %>%
  group_by(kernel) %>%
  roc_auc(truth = obs, Yes) %>%
  group_by(kernel) %>%
  summarize(.estimate = mean(.estimate)) %>%
  ggplot(aes(fct_reorder(kernel, -.estimate), .estimate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Comparison of Area Under the Curve (AUC) by Kernel",
       subtitle = "10-fold CV",
       x = "Algorithm",
       y = "1 - AUC")

# These are cross-validated measures, so it's not as if they should be heavily biased. However they are all really close to each other, so the differences across kernels are likely not that substantial, suggesting the SVM performs extremely well on these data. 

# But what about in comparison to other classifiers we have learned about? Let's compare to the logistic regression from last week. 
{
  tic()
  herron_glm <- train(
    cosp_fact ~ ada + ntu + democrat + firstelected + 
      margin + appromember + budgetmember, 
    data = herron, 
    method = "glm",
    family = "binomial",
    trControl = cv_ctrl)
  toc()
}

## Now, let's compare fit across all
# ROC
bind_rows(
  `SVM (linear)` = svm_linear$pred,
  `SVM (polynomial)` = svm_poly$pred,
  `SVM (radial)` = svm_radial$pred,
  `Logistic regression` = herron_glm$pred,
  .id = "kernel"
) %>%
  group_by(kernel) %>%
  roc_curve(truth = obs, estimate = Yes) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = kernel)) +
  geom_path() +
  geom_abline(lty = 3) +
  scale_color_brewer(type = "qual") +
  coord_flip() +
  labs(title = "Comparison of ROC Curves by Classifer",
       subtitle = "10-fold CV",
       x = "Specificity",
       y = "Sensitivity",
       color = NULL)

# AUC
bind_rows(
  `SVM (linear)` = svm_linear$pred,
  `SVM (polynomial)` = svm_poly$pred,
  `SVM (radial)` = svm_radial$pred,
  `Logistic regression` = herron_glm$pred,
  .id = "kernel"
) %>%
  group_by(kernel) %>%
  roc_auc(truth = obs, Yes) %>%
  group_by(kernel) %>%
  summarize(.estimate = mean(.estimate)) %>%
  ggplot(aes(fct_reorder(kernel, -.estimate), .estimate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Comparison of Area Under the Curve (AUC) by Classifier",
       subtitle = "10-fold CV",
       x = "Algorithm",
       y = "1 - AUC")
