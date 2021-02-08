# Nonlinear estimation, pt. 2
# Perspectives on Computational Modeling
# Philip Waggoner, pdwaggoner@uchicago.edu

# load some libs
library(tidyverse)
library(tidymodels)
library(rcfss)
library(titanic)
library(knitr)
library(splines)
library(ISLR)
library(lattice)
library(gam)
library(here)
library(patchwork)
library(margins)
library(gganimate)

set.seed(1234)
theme_set(theme_minimal())



## Kernel functions

#Consider a truly one-dimensional problem. Here, we have a dataset of infant mortality per country.

infant <- read_csv(file.choose())
  
infant <- infant %>%
  # remove non-countries
  filter(is.na(`Value Footnotes`) | `Value Footnotes` != 1) %>%
  select(`Country or Area`, Year, Value) %>%
  rename(country = `Country or Area`,
         year = Year,
         mortal = Value)

ggplot(infant, aes(mortal)) +
  geom_histogram() +
  labs(title = "Infant mortality rate for 195 nations",
       x = "Infant mortality rate (per 1,000)",
       y = "Frequency")


# Say we want to use kernel smoothing to visualize this distribution using a continuous, rather than a discrete, function. 

# How does the choice of kernel function influence the appearance of the distribution? (not a ton...)
ggplot(infant, aes(mortal)) +
  geom_density(aes(color = "Gaussian"), kernel = "gaussian") +
  geom_density(aes(color = "Epanechnikov"), kernel = "epanechnikov") +
  geom_density(aes(color = "Rectangular"), kernel = "rectangular") +
  geom_density(aes(color = "Triangular"), kernel = "triangular") +
  scale_color_brewer(type = "qual", palette = "Paired") +
  labs(title = "Density estimators of infant mortality rate for 195 nations",
       x = "Infant mortality rate (per 1,000)",
       y = "Density",
       color = "Kernel") +
  theme(legend.position = c(0.96, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white"))

# Once you select a kernel, remember you also need to select the bandwidth, $b$ or $\lambda$
# Take the infant mortality data and the Epanechnikov kernel. 
# How does varying $\lambda$ influence the resulting smoothing line?

# let's animatethe process to see how smoothness changes over a range of b (takes a couple minutes)
kernel_bandwidth <- tibble(
  bandwidth = seq(from = 1, to = 15, by = .1)
) %>%
  mutate(model = map(bandwidth, ~ density(x = infant$mortal, 
                                          bw = .x, 
                                          kernel = "epanechnikov")),
         pred = map(model, ~ tibble(x = .x$x,
                                    y = .x$y))) %>%
  unnest(pred) %>%
  ggplot(aes(x = x, y = y, group = bandwidth)) +
  geom_line() +
  xlim(0, NA) +
  labs(title = "Epanechnikov kernel",
       subtitle = "Bandwidth = {closest_state}",
       x = "Infant mortality rate (per 1,000)",
       y = "Density") +
  transition_states(bandwidth,
                    transition_length = .05,
                    state_length = .05) + 
  ease_aes("cubic-in-out")

# animate to see differences for Ep kernel over many bandwidths in real time; takes a couple minutes to run, but it's worth it!
animate(kernel_bandwidth, nframes = length(seq(from = 1, to = 15, by = .1)) * 2)


# Fortunately R (and Python) determine the optimal bandwidth automatically...
ggplot(infant, aes(mortal)) +
  geom_density(kernel = "epanechnikov") +
  labs(title = "Epanechnikov kernel function",
       subtitle = str_c("Optimal bandwidth = ",
                        round(density(x = infant$mortal, kernel = "epanechnikov")$bw,
                              digits = 2)),
       x = "Infant mortality rate (per 1,000)",
       y = "Density")




#
## Local regression

# Here is an example of a local linear regression on the `ethanol` dataset in the `lattice` package (which, recall, we saw in the first week)

library(broom)
library(lattice)

mod <- loess(NOx ~ E, 
             data = ethanol, 
             degree = 1, # polynomial degree (default is quadratic)  
             span = .75) # bandwidth (sometimes called span, "s", "b", or "lambda")
fit <- augment(mod)

ggplot(fit, aes(E, NOx)) +
  geom_point() +
  geom_line(aes(y = .fitted), color = "red") +
  labs(title = "Local linear regression",
       x = "Equivalence ratio",
       y = "Concentration of nitrogen oxides in micrograms/J")



# One important argument you can control with LOESS is the bandwidth, 
# as this dictates how smooth the LOESS function will become. 
# A larger span will result in a smoother curve, but may not be as accurate.
spans <- c(.25, .5, .75, 1)

# create loess fits, one for each span
fits <- tibble(span = spans) %>%
  group_by(span) %>%
  do(augment(loess(NOx ~ E, ethanol, degree = 1, span = .$span)))



# calculate weights to reproduce this with local weighted fits
dat <- ethanol %>%
  crossing(span = spans, center = unique(ethanol$E)) %>%
  as_tibble %>%
  group_by(span, center) %>%
  mutate(dist = abs(E - center)) %>%
  filter(rank(dist) / n() <= span) %>%
  mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

# create faceted plot with changing points, local linear fits, and vertical lines,
# and constant hollow points and loess fit
dat_spans <- ggplot(dat, aes(E, NOx)) +
  geom_point(aes(alpha = weight)) +
  geom_smooth(aes(group = center, weight = weight), method = "lm", se = FALSE) +
  geom_vline(aes(xintercept = center), lty = 2) +
  geom_point(shape = 1, data = ethanol, alpha = .25) +
  geom_line(aes(y = .fitted), data = fits, color = "red") +
  facet_wrap(~span) +
  ylim(0, 5) +
  ggtitle("x0 = ") +
  labs(title = "Centered over {closest_state}",
      # x = "Equivalence ratio",
       y = "Concentration of nitrogen oxides in micrograms/J") +
  transition_states(center,
                    transition_length = 2,
                    state_length = 1) + 
  ease_aes("cubic-in-out")

# as before, animate difference over span size and weights for local points, but the extra time to run is worth it! Stand by...
animate(dat_spans, nframes = length(unique(ethanol$E)) * 2)


# polynomial local reg
# The downside is that to decrease bias, local polynomial regression will also increase the variance of the estimates
ggplot(ethanol, aes(E, NOx)) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess", se = FALSE, method.args = list(degree = 0),
              aes(color = "Zero-order")) +
  geom_smooth(method = "loess", se = FALSE, method.args = list(degree = 1),
              aes(color = "First-order")) +
  geom_smooth(method = "loess", se = FALSE, method.args = list(degree = 2),
              aes(color = "Second-order")) +
  scale_color_brewer(type = "qual", breaks = c("Zero-order", "First-order", "Second-order")) +
  labs(title = "Local linear regression (polynomial)",
       y = "Concentration of nitrogen oxides in micrograms/J",
       color = "Degree") +
  theme(legend.position = "bottom")




#
## GAMs
# Note, I included all packages for each section to be executed independently; thus I am reproducing some of the earlier packages so this section could stand on it's own if you want to come back here, but forget to load the packages at the beginning of the script

library(tidyverse)
library(tidymodels)
library(rcfss)
library(splines)
library(gam)
library(here)
library(patchwork)
library(margins)

set.seed(1234)
theme_set(theme_minimal())


# Let's estimate a GAM for the Biden dataset using the model, predicting feelings toward Joe Biden

# $$\text{Biden} = \beta_0 + f_1(\text{Age}) + f_2(\text{Education}) + f_3(\text{Gender}) + \epsilon$$

# load data
biden <- read_csv(file.choose())

# estimate model for splines on age and education plus dichotomous female
biden_gam <- gam(biden ~ bs(age) + bs(educ) + female, 
                 data = biden)

# get graphs of each term
biden_gam_terms <- preplot(biden_gam, 
                           se = TRUE, 
                           rug = FALSE)

## plot the effect of age on ftbiden
tibble(x = biden_gam_terms$`bs(age)`$x,
       y = biden_gam_terms$`bs(age)`$y,
       se.fit = biden_gam_terms$`bs(age)`$se.y) %>%
  mutate(y_low = y - 1.96 * se.fit,
         y_high = y + 1.96 * se.fit) %>%
  ggplot(aes(x, y)) +
  geom_line() +
  geom_line(aes(y = y_low), linetype = 2) +
  geom_line(aes(y = y_high), linetype = 2) +
  labs(title = "GAM of Biden feeling thermometer",
       x = "Age",
       y = expression(f[1](age)))

## plot the effect of education on ftbiden
tibble(x = biden_gam_terms$`bs(educ)`$x,
       y = biden_gam_terms$`bs(educ)`$y,
       se.fit = biden_gam_terms$`bs(educ)`$se.y) %>%
  mutate(y_low = y - 1.96 * se.fit,
         y_high = y + 1.96 * se.fit) %>%
  ggplot(aes(x, y)) +
  geom_line() +
  geom_line(aes(y = y_low), linetype = 2) +
  geom_line(aes(y = y_high), linetype = 2) +
  labs(title = "GAM of Biden feeling thermometer",
       x = "Education",
       y = expression(f[2](education)))

## ## plot the effect of gender on ftbiden
tibble(x = biden_gam_terms$female$x,
       y = biden_gam_terms$female$y,
       se.fit = biden_gam_terms$female$se.y) %>%
  unique %>%
  mutate(y_low = y - 1.96 * se.fit,
         y_high = y + 1.96 * se.fit,
         x = factor(x, levels = 0:1, labels = c("Male", "Female"))) %>%
  ggplot(aes(x, y, ymin = y_low, ymax = y_high)) +
  geom_errorbar() +
  geom_point() +
  labs(title = "GAM of Biden feeling thermometer",
       x = NULL,
       y = expression(f[3](gender)))


# Of note: for age, there does not appear to be a substantial relationship with Biden feelings after controlling for education and gender

# For education, as education increases, predicted Biden feeling thermometer ratings decrease until approximately 15 years of formal education, then increase again for those with college or post-graduate degrees. 

# Finally, for gender the difference between males and females is substantial and statistically distinguishable from 0.



# Instead of cubic splines, we could use local regression
biden_gam_local <- gam(biden ~ lo(age) + lo(educ) + female, data = biden)

# get graphs of each term
biden_gam_local_terms <- preplot(biden_gam_local, 
                                 se = TRUE, 
                                 rug = FALSE)

## age
tibble(x = biden_gam_local_terms$`lo(age)`$x,
       y = biden_gam_local_terms$`lo(age)`$y,
       se.fit = biden_gam_local_terms$`lo(age)`$se.y) %>%
  mutate(y_low = y - 1.96 * se.fit,
         y_high = y + 1.96 * se.fit) %>%
  ggplot(aes(x, y)) +
  geom_line() +
  geom_line(aes(y = y_low), linetype = 2) +
  geom_line(aes(y = y_high), linetype = 2) +
  labs(title = "GAM of Biden feeling thermometer",
       subtitle = "Local regression",
       x = "Age",
       y = expression(f[1](age)))

## education
tibble(x = biden_gam_local_terms$`lo(educ)`$x,
       y = biden_gam_local_terms$`lo(educ)`$y,
       se.fit = biden_gam_local_terms$`lo(educ)`$se.y) %>%
  mutate(y_low = y - 1.96 * se.fit,
         y_high = y + 1.96 * se.fit) %>%
  ggplot(aes(x, y)) +
  geom_line() +
  geom_line(aes(y = y_low), linetype = 2) +
  geom_line(aes(y = y_high), linetype = 2) +
  labs(title = "GAM of Biden feeling thermometer",
       subtitle = "Local regression",
       x = "Education",
       y = expression(f[2](education)))

## gender
tibble(x = biden_gam_local_terms$female$x,
       y = biden_gam_local_terms$female$y,
       se.fit = biden_gam_local_terms$female$se.y) %>%
  unique %>%
  mutate(y_low = y - 1.96 * se.fit,
         y_high = y + 1.96 * se.fit,
         x = factor(x, levels = 0:1, labels = c("Male", "Female"))) %>%
  ggplot(aes(x, y, ymin = y_low, ymax = y_high)) +
  geom_errorbar() +
  geom_point() +
  labs(title = "GAM of Biden feeling thermometer",
       x = NULL,
       y = expression(f[3](gender)))

# The results are pretty similar to the cubic splines


#
## MARS

# For implementation in Python, see "py-earth", https://github.com/scikit-learn-contrib/py-earth

library(tidyverse)
library(tidymodels)
library(rcfss)
library(titanic)
library(knitr)
library(splines)
library(ISLR)
library(lattice)
library(gam)
library(here)
library(patchwork)
library(margins)
library(earth)

set.seed(1234)
theme_set(theme_minimal())


## Application to Ames housing data (note: these data come from the AmesHousing package; no need to load external data)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7, strata = "Sale_Price")
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

# create base plot (updated next)
ames_base <- ggplot(ames_train, aes(Year_Built, Sale_Price)) +
  geom_point(alpha = .05) +
  scale_y_continuous(labels = scales::dollar)


# the prior approach of setting poly orders and steps
{
  ames_base +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle("Linear regression")
} + {
  ames_base +
    geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), se = FALSE) +
    ggtitle("Degree-2 polynomial")
} + {
  ames_base +
    geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = FALSE) +
    ggtitle("Degree-3 polynomial")
} + {
  ames_base +
    geom_smooth(method = "lm", formula = y ~ cut_interval(x, 3), se = FALSE) +
    ggtitle("Step function")
}


# Consider now the MARS approach to this problem
# Here we have a simple model `Sale_Price ~ Year_Built`

# The MARS procedure first looks for the single point across the range of `Year_Built` values where two different linear relationships between `Sale_Price` and `Year_Built` achieve the smallest error

# For a single knot, the hinge function results in the linear model:

set.seed(1234)

year_built_k1 <- earth(
  Sale_Price ~ Year_Built,
  nk = 3,
  pmethod = "none",
  data = ames_train   
)

year_built_k2 <- earth(
  Sale_Price ~ Year_Built,  
  nk = 4,
  pmethod = "none",
  data = ames_train   
)

year_built_k3 <- earth(
  Sale_Price ~ Year_Built,  
  nk = 5,
  pmethod = "none",
  data = ames_train   
)

year_built_k4 <- earth(
  Sale_Price ~ Year_Built,  
  nk = 6,
  pmethod = "none",
  data = ames_train   
)

# FIRST KNOT
k1_cuts <- year_built_k1$cuts
k1_coefs <- year_built_k1$coefficients

ames_base +
  geom_line(data = prediction(year_built_k1), aes(y = fitted), size = 1, color = "blue") +
  ggtitle("MARS: One knot")


# Once the first knot has been found, the search continues for a second knot
k2_cuts <- year_built_k2$cuts
k2_coefs <- year_built_k2$coefficients

ames_base +
  geom_line(data = prediction(year_built_k2), aes(y = fitted), size = 1, color = "blue") +
  ggtitle("MARS: Two knots")

{
  ames_base +
    geom_line(data = prediction(year_built_k1), aes(y = fitted), size = 1, color = "blue") +
    ggtitle("One knot")
} + {
  ames_base +
    geom_line(data = prediction(year_built_k2), aes(y = fitted), size = 1, color = "blue") +
    ggtitle("Two knots")
} + {
  ames_base +
    geom_line(data = prediction(year_built_k3), aes(y = fitted), size = 1, color = "blue") +
    ggtitle("Three knots")
} + {
  ames_base +
    geom_line(data = prediction(year_built_k4), aes(y = fitted), size = 1, color = "blue") +
    ggtitle("Four knots")
}

# This procedure continues until many knots are found. The result is a highly non-linear pattern, but also a highly overfit model. Consider if we increase the number of knots to 9:
  
year_built_k9 <- earth(
  Sale_Price ~ Year_Built,  
  nk = 11,
  pmethod = "none",
  data = ames_train   
)

ames_base +
  geom_line(data = prediction(year_built_k9), aes(y = fitted), size = 1, color = "blue") +
  ggtitle("MARS: Nine knots")

# At this point, we have a large model that overfits the training data, so we might use CV, e.g., to prune the model to remove terms that cause the smallest increase in residual squared error (RMSE at the model level)


#
## Extra code for fun 

# Recall, we can also use GAMs for classification problems, like the Titanic example.
library(titanic)

# estimate model for splines on age and education plus dichotomous female
titanic_gam <- gam(Survived ~ bs(Age) + bs(Fare) + Sex, data = titanic_train,
                   family = binomial)

# get graphs of each term
titanic_gam_terms <- preplot(titanic_gam, se = TRUE, rug = FALSE)

## age
tibble(x = titanic_gam_terms$`bs(Age)`$x,
       y = titanic_gam_terms$`bs(Age)`$y,
       se.fit = titanic_gam_terms$`bs(Age)`$se.y) %>%
  mutate(y_low = y - 1.96 * se.fit,
         y_high = y + 1.96 * se.fit) %>%
  ggplot(aes(x, y)) +
  geom_line() +
  geom_line(aes(y = y_low), linetype = 2) +
  geom_line(aes(y = y_high), linetype = 2) +
  labs(title = "GAM of Titanic survival",
       subtitle = "Cubic spline",
       x = "Age",
       y = expression(f[1](age)))

## fare
tibble(x = titanic_gam_terms$`bs(Fare)`$x,
       y = titanic_gam_terms$`bs(Fare)`$y,
       se.fit = titanic_gam_terms$`bs(Fare)`$se.y) %>%
  mutate(y_low = y - 1.96 * se.fit,
         y_high = y + 1.96 * se.fit) %>%
  ggplot(aes(x, y)) +
  geom_line() +
  geom_line(aes(y = y_low), linetype = 2) +
  geom_line(aes(y = y_high), linetype = 2) +
  labs(title = "GAM of Titanic survival",
       subtitle = "Cubic spline",
       x = "Fare",
       y = expression(f[2](fare)))

## gender
tibble(x = titanic_gam_terms$Sex$x,
       y = titanic_gam_terms$Sex$y,
       se.fit = titanic_gam_terms$Sex$se.y) %>%
  unique %>%
  mutate(y_low = y - 1.96 * se.fit,
         y_high = y + 1.96 * se.fit,
         x = factor(x, levels = c("male", "female"), labels = c("Male", "Female"))) %>%
  ggplot(aes(x, y, ymin = y_low, ymax = y_high)) +
  geom_errorbar() +
  geom_point() +
  labs(title = "GAM of Titanic survival",
       x = NULL,
       y = expression(f[3](gender)))

# Using CV to tune MARS
set.seed(1234) 

# create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:2, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

# train the model with caret
library(caret)

tuning_mars <- train(
  x = subset(ames_train, select = -Sale_Price),
  y = ames_train$Sale_Price,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl("cv", 10),
  tuneGrid = hyper_grid
)

# which is best?
tuning_mars$bestTune

tuning_mars$results %>%
  filter(nprune == tuning_mars$bestTune$nprune, 
         degree == tuning_mars$bestTune$degree)

# plot all iterations
ggplot(tuning_mars)
