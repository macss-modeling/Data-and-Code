# Nonlinear estimation, pt. 1
# Perspectives on Computational Modeling
# Philip Waggoner, pdwaggoner@uchicago.edu

# Load some libraries and set up
library(tidyverse)
library(tidymodels)
library(rcfss)
library(knitr)
library(splines)
library(lattice)
library(here)
library(patchwork)
library(margins)

set.seed(1234)
theme_set(theme_minimal())

# read the data; drop NAs
mh <- read_csv(file.choose()) %>%
  na.omit

#
## Basis vs. natural splines (mental health / voting data)

# One drawback to the previously address "basis splines" is that they have high variance at the outer range of the predictors. 
# Consider a basis spline with 3 knots applied to the voting and age logistic regression model using the mental health data again

# estimate model
glm(vote96 ~ bs(age, df = 6), data = mh, family = binomial) %>%
  cplot("age", what = "prediction", n = 101, draw = FALSE) %>%
  ggplot(aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_rug(data = filter(mh, vote96 == 0), aes(age), alpha = .02, sides = "b") +
  geom_rug(data = filter(mh, vote96 == 1), aes(age), alpha = .02, sides = "t") +
  labs(title = "Predicted probability of voting",
       subtitle = "Basis spline with 3 knots",
       x = "Age",
       y = "Predicted probability of voting")

# Notice the 95% confidence interval balloons at both low and high values for age. 
# To control for this, we impose boundary constraints: the function is required to be linear at the boundary 
# (the region where $X$ is smaller than the smallest knot, or larger than the largest knot). 
# This type of spline is known as a natural spline. This leads to more stable estimates at the boundaries.

# estimate basis spline model
vote_age_basis <- glm(vote96 ~ bs(age, df = 6), data = mh, family = binomial) %>%
  cplot("age", what = "prediction", n = 101, draw = FALSE)

vote_age_natural <- glm(vote96 ~ ns(age, df = 6), data = mh, family = binomial) %>%
  cplot("age", what = "prediction", n = 101, draw = FALSE)

bind_rows(
  Basis = vote_age_basis,
  Natural = vote_age_natural,
  .id = "model"
) %>%
  ggplot(aes(x = xvals)) + 
  geom_line(aes(y = yvals, color = model)) +
  geom_line(aes(y = upper, color = model), linetype = 2) +
  geom_line(aes(y = lower, color = model), linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_rug(data = filter(mh, vote96 == 0), aes(age), alpha = .02, sides = "b") +
  geom_rug(data = filter(mh, vote96 == 1), aes(age), alpha = .02, sides = "t") +
  scale_color_brewer(type = "qual") +
  labs(title = "Predicted probability of voting",
       x = "Age",
       y = "Predicted probability of voting",
       color = "Spline") +
  theme(legend.position = "bottom")


# Typically knots are placed in a uniform fashion; that is, the cutpoints $c$ are determined by first identifying the 
# number of knots $K$ for the model, then partitioning $X$ into uniform quantiles. 
# So if we fit a cubic regression spline with 5 knots on the voting and age logistic regression, it would divide 
# `age` into 5 equal quantiles. That is, each interval contains the same number of observations rather than each 
# interval having an equal width. and estimate the cubic spline function for each quantile:

# estimate model
glm(vote96 ~ ns(age, df = 8), data = mh, family = binomial) %>%
  cplot("age", what = "prediction", n = 101, draw = FALSE) %>%
  ggplot(aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_vline(xintercept = attr(bs(mh$age, df = 8), "knots"),
             linetype = 2, color = "blue") +
  geom_rug(data = filter(mh, vote96 == 0), aes(age), alpha = .02, sides = "b") +
  geom_rug(data = filter(mh, vote96 == 1), aes(age), alpha = .02, sides = "t") +
  labs(title = "Predicted probability of voting",
       subtitle = "Natural spline with 5 knots",
       x = "Age",
       y = "Predicted probability of voting")


# But this still leaves unaddressed the matter of how many knots should we use? 
# Or how many degrees should each polynomial be? While theory should still be our guide, we can also use CV to determine the 
# optimal number of knots and/or polynomial degrees. 
# For our voting and age model, we can estimate $10$-fold CV MSE for varying numbers of knots for a cubic natural spline:

# function to simplify things
vote_spline <- function(splits, df = NULL){
  # estimate the model on each fold
  model <- glm(vote96 ~ ns(age, df = df),
               data = analysis(splits))
  
  model_acc <- augment(model, newdata = assessment(splits)) %>%
    accuracy(truth = factor(vote96), estimate = factor(round(.fitted)))
  
  mean(model_acc$.estimate)
}

tune_over_knots <- function(splits, knots){
  vote_spline(splits, df = knots + 3)
}

# estimate CV error for knots in 0:25
results <- vfold_cv(mh, v = 10)

expand(results, id, knots = 1:25) %>%
  left_join(results) %>%
  mutate(acc = map2_dbl(splits, knots, tune_over_knots)) %>%
  group_by(knots) %>%
  summarize(acc = mean(acc)) %>%
  mutate(err = 1 - acc) %>%
  ggplot(aes(knots, err)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Optimal number of knots for natural cubic spline regression",
       x = "Knots",
       y = "10-fold CV error")


# These results (weakly) suggest the optimal number of knots is around 7, though this is subjective to a degree. The resulting model produced by these parameters is:

glm(vote96 ~ ns(age, df = 10), data = mh, family = binomial) %>%
  cplot("age", what = "prediction", n = 101, draw = FALSE) %>%
  ggplot(aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_vline(xintercept = attr(bs(mh$age, df = 10), "knots"),
             linetype = 2, color = "blue") +
  geom_rug(data = filter(mh, vote96 == 0), aes(age), alpha = .02, sides = "b") +
  geom_rug(data = filter(mh, vote96 == 1), aes(age), alpha = .02, sides = "t") +
  labs(title = "Predicted probability of voting",
       subtitle = "Natural spline with 7 knots",
       x = "Age",
       y = "Predicted probability of voting")

# 
# Comparison to polynomial regression

# estimate natural spline model with df = 15
vote_age_spline <- glm(vote96 ~ ns(age, df = 15), data = mh, family = binomial) %>%
  cplot("age", what = "prediction", n = 101, draw = FALSE)

vote_age_poly <- glm(vote96 ~ poly(age, degree = 15), data = mh, family = binomial) %>%
  cplot("age", what = "prediction", n = 101, draw = FALSE)

bind_rows(
  `Natural cubic spline` = vote_age_spline,
  `Polynomial` = vote_age_poly,
  .id = "model"
) %>%
  ggplot(aes(x = xvals)) + 
  geom_line(aes(y = yvals, color = model)) +
   geom_line(aes(y = upper, color = model), linetype = 2) +
   geom_line(aes(y = lower, color = model), linetype = 2) +
   geom_hline(yintercept = 0, linetype = 1) +
  geom_rug(data = filter(mh, vote96 == 0), aes(age), alpha = .02, sides = "b") +
  geom_rug(data = filter(mh, vote96 == 1), aes(age), alpha = .02, sides = "t") +
  scale_color_brewer(type = "qual") +
  labs(title = "Predicted probability of voting",
       x = "Age",
       y = "Predicted probability of voting",
       color = NULL) +
  theme(legend.position = "bottom")


## IN SUM: Splines are generally superior to polynomial regression because their flexibility is defined primarily in terms 
## of the number of knots in the model, rather than the degree of the highest polynomial term (e.g. $X^{15}$), 
## whereas polynomial regression models must use high degree polynomials to achieve similar flexibility. 
## This leads to more overfitting and wilder behavior, especially in the boundary regions. In the comparison above, 
## we fit a natural cubic spline with 15 degrees of freedom (aka 12 knots) and compared it to a d=15 polynomial regression. 
## Notice the undesirable behavior of the polynomial regression model at the boundaries relative to the natural cubic spline.



#
# Smoothing splines (with simulated data)
# Smoothing splines take a different approach to producing a spline; 
# The goal is to fit a function $g(x)$ to the observed data well

sim_spline_data <- tibble(
  x = runif(100, min = -1, max = 1),
  y = poly(x, degree = 15, raw = TRUE) %>%
    rowSums() + rnorm(100)
)

ggplot(sim_spline_data, aes(x, y)) +
  geom_point() +
  geom_line() + # hypothetical "function" fit to the points in the data; not a real function that we estimated; just making a point
  labs(x = expression(X),
       y = expression(Y))

# Clearly such a function would overfit the data. Instead, we want a function $g$ that makes $\text{RSS}$ small, 
# but that is also smooth. The approach of a smoothing spline is to find the function that minimizes,
# $$\sum_{i=1}^n (y_i - g(x_i))^2 + \lambda \int g''(t)^2 dt$$, where $\lambda$ is a non-negative tuning parameter. 
# The function that $g$ minimizes is the smoothing spline.


# smooth spline data points
sim_smooth <- smooth.spline(sim_spline_data$x, sim_spline_data$y, df = 6) %>%
  predict %>%
  as_tibble

ggplot(sim_spline_data, aes(x, y)) +
  geom_point() +
  geom_smooth(aes(color = "Cubic spline"),
              method = "lm", formula = y ~ bs(x, df = 6), se = FALSE) +
  geom_smooth(aes(color = "Natural spline"),
              method = "lm", formula = y ~ ns(x, df = 6), se = FALSE) +
  geom_line(data = sim_smooth, aes(color = "Smoothing spline"), size = 1) +
  scale_color_brewer(type = "qual") +
  labs(x = expression(X),
       y = expression(Y),
       color = NULL) +
  theme(legend.position = "bottom")

## choosing lambda
# Recall: since the tuning parameter $\lambda$ dictates the roughness of the smoothing spline, 
# it also controls the effective degrees of freedom. As $\lambda$ INCREASES from 0 to $\infty$, 
# the effective degrees of freedom DECREASES from $n$ to $2$.

list(
  `50` = smooth.spline(sim_spline_data$x, sim_spline_data$y, df = 50),
  `20` = smooth.spline(sim_spline_data$x, sim_spline_data$y, df = 20),
  `2` = smooth.spline(sim_spline_data$x, sim_spline_data$y, df = 2),
  `11` = smooth.spline(sim_spline_data$x, sim_spline_data$y)
) %>%
  map_df(predict, .id = "df") %>%
  mutate(df = factor(df, levels = c(2, 11, 20, 50),
                     labels = c("2", "11", "20", "50"))) %>%
  ggplot(aes(x, y)) +
  geom_point(data = sim_spline_data, alpha = .2) +
  geom_line(aes(color = df)) +
  scale_color_brewer(type = "qual", palette = "Dark2", guide = FALSE) +
  facet_wrap(~ df) +
  labs(title = "Effective degrees of freedom",
       x = expression(X),
       y = expression(Y))


# Compare piecewise polynomials and splines (with simulated data)

# Each function is estimated using least squares applied to the individual functions and subsets of data. 
# The more knots $K$ used, the more flexible the piecewise polynomial.

# While the example here uses a cubic (third-order) polynomial, we don't have to use it. 
# We could select any higher-order degree for the polynomial.

# Of course this leads to a somewhat odd and discontinuous function:

# simulate data
sim_piece <- tibble(
  x = runif(100, 0, 10),
  y = ifelse(x < 5,
             .05 * x + .05 * x^2 + .05 * x^3 + rnorm(100, 0, 3),
             .1 * x + .1 * x^2 - .05 * x^3 + rnorm(100, 0, 3))
)

# estimate models
sim_piece_mod1 <- glm(y ~ poly(x, 3, raw = TRUE), data = sim_piece, subset = x < 5)
sim_piece_mod2 <- glm(y ~ poly(x, 3, raw = TRUE), data = sim_piece, subset = x >= 5)

# draw the plot
sim_piece_grid <- tibble(
  x = seq(0, 10, by = .001)
)

bind_rows(
  sim_piece_mod1 = augment(sim_piece_mod1, newdata = sim_piece_grid),
  sim_piece_mod2 = augment(sim_piece_mod2, newdata = sim_piece_grid),
  .id = "model"
) %>%
  filter((x < 5 & model == "sim_piece_mod1") |
           (x >=5 & model == "sim_piece_mod2")) %>%
  ggplot(aes(x, y)) +
  geom_point(data = sim_piece) +
  geom_line(aes(y = .fitted, color = model), size = 1) +
  geom_vline(xintercept = 5, linetype = 2, color = "grey") +
  scale_color_brewer(type = "qual", guide = FALSE) +
  labs(title = "Piecewise cubic regression",
       x = "X",
       y = "Y")

# Instead of a discontinuous piecewise function, we'd rather have something continuous. 
# That is, we impose the constraint that the fitted curve must be continuous.

# simulate data
sim_piece_cont <- sim_piece %>%
  mutate(y = ifelse(x < 5,
                    15 + .05 * x - .5 * x^2 - .05 * x^3,
                    .05 * x + .1 * x^2 - .05 * x^3))

# estimate models
sim_piece_cont_mod1 <- glm(y ~ poly(x, 3, raw = TRUE), data = sim_piece_cont, subset = x < 5)
sim_piece_cont_mod2 <- glm(y ~ poly(x, 3, raw = TRUE), data = sim_piece_cont, subset = x >= 5)

# draw the plot
bind_rows(
  sim_piece_cont_mod1 = augment(sim_piece_cont_mod1, newdata = sim_piece_grid),
  sim_piece_cont_mod2 = augment(sim_piece_cont_mod2, newdata = sim_piece_grid),
  .id = "model"
) %>%
  filter((x < 5 & model == "sim_piece_cont_mod1") |
           (x >=5 & model == "sim_piece_cont_mod2")) %>%
  ggplot() +
  geom_point(data = sim_piece %>%
               mutate(y = ifelse(x < 5,
                                 15 + .05 * x - .5 * x^2 - .05 * x^3 + rnorm(100, 0, 3),
                                 .05 * x + .1 * x^2 - .05 * x^3) + rnorm(100, 0, 3)), aes(x, y)) +
  geom_line(aes(x, .fitted, color = model), size = 1) +
  geom_vline(xintercept = 5, linetype = 2, color = "grey") +
  scale_color_brewer(type = "qual", guide = FALSE) +
  labs(title = "Continuous piecewise cubic regression",
       x = "X",
       y = "Y")

# But notice that this constraint is insufficient. 
# Now the function is continuous, but still looks unnatural because of the V-shaped join. 
# We should add two additional constraints: not only should the fitted curve be continuous, 
# but the first and second derivatives should also be continuous at the knot. 
# This will generate a fitted curve that is continuous and smooth

# estimate models
sim_piece_smooth <- glm(y ~ bs(x, knots = c(5)), data = sim_piece)

# plot
augment(sim_piece_smooth, newdata = sim_piece) %>%
  ggplot(aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = .fitted), size = 1) +
  geom_vline(xintercept = 5, linetype = 2, color = "grey") +
  labs(title = "Cubic spline",
       x = "X",
       y = "Y")

# By increasing the number of knots, we will increase the flexibility of the resulting spline:

tibble(
  terms = c(1, 5, 10),
  models = map(terms, ~ glm(y ~ bs(x, df = . + 3), data = sim_piece)),
  pred = map(models, augment, newdata = sim_piece)
) %>%
  unnest(pred) %>%
  ggplot(aes(x, y)) +
  geom_point(data = sim_piece, alpha = .2) +
  geom_line(aes(y = .fitted, color = factor(terms))) +
  scale_color_brewer(type = "qual") +
  labs(title = "Cubic spline",
       x = "X",
       y = "Y",
       color = "Knots")

