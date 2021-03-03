# Perspectives on Computational Modeling (30100)

# Interpretable Machine Learning

# First, a quick look at MFX via margins package as mentioned in the lecture notes
library(tidyverse)
library(titanic)
library(margins)

titanic <- titanic_train %>%
  as_tibble() %>%
  drop_na()

age_fare_x <- glm(Survived ~ Age * Fare, 
                  data = titanic,
                  family = binomial); summary(age_fare_x)

par(mfrow = c(1,2))
cplot(age_fare_x, "Fare")
cplot(age_fare_x, "Fare", what = "effect")
par(mfrow = c(1,1))

library(plotly)

# interactive 3D predicted probs
surface <- margins:::calculate_surface(x = age_fare_x,
                                       xvar = "Age",
                                       yvar = "Fare",
                                       dx = "Age",
                                       nx = 25L,
                                       ny = 25L,
                                       type = "response",
                                       vcov = stats::vcov(age_fare_x),
                                       what = "prediction")

outcome <- surface[["outcome"]]
xvals <- surface[["xvals"]]
yvals <- surface[["yvals"]]

plot_ly(x = ~xvals, y = ~yvals, z = ~outcome) %>%
  add_surface() %>%
  layout(
    title = "Predicted probability of survival",
    scene = list(
      xaxis = list(title = "Age"),
      yaxis = list(title = "Fare"),
      zaxis = list(title = "Predicted probability of survival")
    ))

# interactive 3D marginal effects
surface <- margins:::calculate_surface(x = age_fare_x,
                                       xvar = "Age",
                                       yvar = "Fare",
                                       dx = "Age",
                                       nx = 25L,
                                       ny = 25L,
                                       type = "response",
                                       vcov = stats::vcov(age_fare_x),
                                       what = "effect")

outcome <- surface[["outcome"]]
xvals <- surface[["xvals"]]
yvals <- surface[["yvals"]]

plot_ly(x = ~xvals, y = ~yvals, z = ~outcome) %>%
  add_surface() %>%
  layout(
    title = "Marginal effect on survival",
    scene = list(
      xaxis = list(title = "Age"),
      yaxis = list(title = "Fare"),
      zaxis = list(title = "AME")
    ))
#


# Next, IML

# load some libraries
library(iml) # for PDP and ICE functions and plots
library(randomForest) # to fit the model
library(MASS) # for the Boston housing data
library(amerika) # for some colors

# Task: Predict median value of owner-occupied homes (in \$1000s; the medv feature)
# Feature of focus: average number of rooms per dwelling (as on the assumption the home value should increase as rooms increase)
# Consider trying other features instead of `rm`, to see differences:
names(Boston)

# train a random forest
set.seed(1234)

rf <- randomForest(medv ~ ., 
                   data = Boston, 
                   ntree = 100)

rf_mod_info <- Predictor$new(rf, 
                             data = Boston)

# PDP and ICE
feature_effect <- FeatureEffect$new(rf_mod_info,
                                    feature = "rm", 
                                    method = "pdp+ice")

# viz
plot(feature_effect) + 
  theme_minimal()

# viz with original predictions overlaid
plot(feature_effect) +
  geom_point(data = Boston, aes(y = rf_mod_info$predict(Boston)[[1]], 
                                x = rm),
             color = amerika_palettes$Democrat[3],
             alpha = 0.4) + 
  theme_minimal()

# aggregated partial dependence
agg_partial_dep <- FeatureEffect$new(rf_mod_info, 
                                     feature = "rm", 
                                     method = "pdp")

plot(agg_partial_dep) + 
  theme_minimal()

# ICE
feature_effect_ice <- FeatureEffect$new(rf_mod_info, 
                                        feature = "rm", 
                                        method = "ice")

plot(feature_effect_ice) + 
  theme_minimal()
