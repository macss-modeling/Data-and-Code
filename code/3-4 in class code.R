# Autoencoders (30100)
# NOTE: As before, much of this is taken from my newest book under contract with Cambridge University Press; so, please don't share the code beyond this class
# Philip Waggoner, pdwaggoner@uchicago.edu

# Load libraries
library(tidyverse)
library(here)
library(amerika)
library(tictoc)
library(h2o)
library(bit64) # speeds up some h2o computation

# read in ANES 2019
anes <- read_rds(here("Data", "anes.rds"))

# fitting
set.seed(1234)

anes$democrat <- factor(anes$democrat)

# initializing the h2o cluster
my_h2o <- h2o.init()

# h2o df
anes_h2o <- anes %>% 
  as.h2o()

# train, val, test
split_frame <- h2o.splitFrame(anes_h2o, 
                              ratios = c(0.6, 0.2), 
                              seed = 1234)   

split_frame %>% 
  str()

train <- split_frame[[1]]
validation <- split_frame[[2]]
test <- split_frame[[3]]

# pull out party feature (for later use)
response <- "democrat"

predictors <- setdiff(colnames(train), response)

# vanilla AE
{
  tic()
autoencoder <- h2o.deeplearning(x = predictors,
                                training_frame = train, 
                                autoencoder = TRUE, 
                                reproducible = TRUE,
                                seed = 1234, 
                                hidden = c(16), 
                                epochs = 100, 
                                activation = "Tanh")
  toc()
} # ~ 4.5 seconds

# save model, if desired
#h2o.saveModel(autoencoder, 
#              path = "autoencoder", 
#              force = TRUE)

# load the model directly, if desired
#autoencoder <- h2o.loadModel(".../file/path/here")

# feature extraction
codings_train <- h2o.deepfeatures(autoencoder, 
                                  data = train, 
                                  layer = 1) %>% 
  as.data.frame() %>%
  mutate(democrat = as.vector(train[ , 36]))

# Numeric inspection of the "scores"
codings_train %>% 
  head(10)


# viz inspection of deep features
{
p1 <- ggplot(codings_train, aes(x = DF.L1.C1, 
                                y = DF.L1.C2, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 1 & 2",
       color = "Democrat") + 
  theme_minimal()

# (3 and 4)
p2 <- ggplot(codings_train, aes(x = DF.L1.C3, 
                                y = DF.L1.C4, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 3 & 4",
       color = "Democrat") + 
  theme_minimal()

# 5 & 6
p3 <- ggplot(codings_train, aes(x = DF.L1.C5, 
                                y = DF.L1.C6, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 5 & 6",
       color = "Democrat") + 
  theme_minimal()

# 7 & 8
p4 <- ggplot(codings_train, aes(x = DF.L1.C7, 
                                y = DF.L1.C8, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 7 & 8",
       color = "Democrat") + 
  theme_minimal()

# 9 & 10
p5 <- ggplot(codings_train, aes(x = DF.L1.C9, 
                                y = DF.L1.C10, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 9 & 10",
       color = "Democrat") + 
  theme_minimal()

# 11 & 12
p6 <- ggplot(codings_train, aes(x = DF.L1.C11, 
                                y = DF.L1.C12, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 11 & 12",
       color = "Democrat") + 
  theme_minimal()

# 13 & 14
p7 <- ggplot(codings_train, aes(x = DF.L1.C13, 
                                y = DF.L1.C14, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 13 & 14",
       color = "Democrat") + 
  theme_minimal()

# 15 & 16
p8 <- ggplot(codings_train, aes(x = DF.L1.C15, 
                                y = DF.L1.C16, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 15 & 16",
       color = "Democrat") + 
  theme_minimal()

# view together
library(patchwork)

(p1 + p2 + p3 + p4) / 
  (p5 + p6 + p7 + p8)
}

# Let's go deeper (pun)

## predict party afiliation
codings_val <- h2o.deepfeatures(object = autoencoder, 
                                data = validation, 
                                layer = 1) %>%
  as.data.frame() %>%
  mutate(democrat = as.factor(as.vector(validation[ , 36]))) %>%
  as.h2o()

deep_features <- setdiff(colnames(codings_val), response)

deep_net <- h2o.deeplearning(y = response,
                             x = deep_features,
                             training_frame = codings_val,
                             activation = "Tanh",
                             hidden = c(8, 8), 
                             epochs = 100)

## preds on test set
test_3 <- h2o.deepfeatures(object = autoencoder, 
                           data = test, 
                           layer = 1)

test_pred <- h2o.predict(deep_net, test_3, type = "response") %>%
  as.data.frame() %>%
  mutate(truth = as.vector(test[, 36]))

# conf mat
print(h2o.predict(deep_net, test_3) %>%
        as.data.frame() %>%
        mutate(truth = as.vector(test[, 36])) %>%
        group_by(truth, predict) %>%
        summarise(n = n()) %>%
        mutate(freq = n / sum(n)))

## Feature importance
fimp <- as.data.frame(h2o.varimp(deep_net)) %>% 
  arrange(desc(relative_importance))

# viz
fimp %>% 
  ggplot(aes(x = relative_importance, 
             y = reorder(variable, -relative_importance))) +
  geom_point(color = "dark red", 
             fill = "dark red", 
             alpha = 0.5,
             size = 5) +
  labs(title = "Relative Feature Importance",
       subtitle = "Deep Neural Network (2 hidden layers with 16 total neurons)",
       x = "Relative Importance",
       y = "Feature") + 
  theme_minimal()

# viz most important deep features for train and val sets
codings_val2 <- h2o.deepfeatures(object = autoencoder, 
                                 data = validation, 
                                 layer = 1) %>%
  as.data.frame() %>%
  mutate(democrat = as.factor(as.vector(validation[ , 36]))) 

# training plot
tr <- ggplot(codings_train, aes(x = DF.L1.C8, 
                               y = DF.L1.C6, 
                               color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Training Set",
       color = "Democrat") + 
  theme_minimal()

# validation plot
val <- ggplot(codings_val2, aes(x = DF.L1.C8, 
                          y = DF.L1.C6, 
                          color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Validation Set",
       color = "Democrat") + 
  theme_minimal()

# now side by side
(tr + val)

# Shut down h2o 
h2o.shutdown()
