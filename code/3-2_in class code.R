# Self-organizing maps (30100)
# NOTE: Again, much of this is taken from my newest book under contract with Cambridge University Press; so, please don't share the code beyond this class
# Philip Waggoner, pdwaggoner@uchicago.edu

# Load libraries
library(tidyverse)
library(here)
library(amerika)
library(tictoc)
library(kohonen)

# read in cleaned and preprocessed 2019 ANES Pilot Data
anes <- read_rds(here("Data", "anes.rds"))

# begin
set.seed(1234)

# standardize FTs
anes_scaled <- anes[ ,1:35] %>% 
  scale()

# create the structure of the output layer
search_grid <- somgrid(xdim = 10, 
                       ydim = 10, 
                       topo = "rectangular",
                       neighbourhood.fct = "gaussian") 

# fit
{
  tic()
som_fit <- som(anes_scaled,
               grid = search_grid,
               alpha = c(0.1, 0.001), 
               radius = 1, 
               rlen = 500, 
               dist.fcts = "euclidean", 
               mode = "batch") 
  toc()
} # ~12 seconds

# plot
som_fit$changes %>% 
  as_tibble() %>% 
  mutate(changes = V1,
         iteration = seq(1:length(changes))) %>% 
  ggplot(aes(iteration, changes)) +
  geom_line() +
  labs(x = "Training Iteration",
       y = "Mean Distance to Closest Node") +
  theme_minimal()


# clustering from SOM via k-means (hard), FCM (soft), and HAC (tree)
point_colors <- c(amerika_palettes$Republican[2], 
                  amerika_palettes$Democrat[2])

neuron_colors <- c(amerika_palettes$Republican[3], 
                   amerika_palettes$Democrat[3])

## k-means
kmeans_clusters <- som_fit$codes[[1]] %>% 
  kmeans(., centers = 2)

class_assign_km <- map_dbl(kmeans_clusters$cluster, ~{
  if(. == 1) 2
  else 1
}
)

plot(som_fit, 
     type = "mapping", 
     pch = 21, 
     bg = point_colors[as.factor(anes$democrat)],
     shape = "straight",
     bgcol = neuron_colors[as.integer(class_assign_km)],
     main = "2 clusters via k-means"); add.cluster.boundaries(x = som_fit, clustering = class_assign_km, 
                                                      lwd = 5, lty = 5)

## FCM
fcm_clusters <- som_fit$codes[[1]] %>% 
  ppclust::fcm(., centers = 2)

class_assign_fcm <- map_dbl(fcm_clusters$cluster, ~{
  if(. == 1) 2
  else 1
}
)

plot(som_fit, 
     type = "mapping", 
     pch = 21, 
     bg = point_colors[as.factor(anes$democrat)],
     shape = "straight",
     bgcol = neuron_colors[as.integer(class_assign_fcm)],
     main = "2 clusters via FCM"); add.cluster.boundaries(x = som_fit, clustering = class_assign_fcm, 
                                                      lwd = 5, lty = 5)


## HAC
hac_clusters <- som_fit$codes[[1]] %>% 
  dist() %>% 
  hclust() %>% 
  cutree(., k = 2)

class_assign <- map_dbl(hac_clusters, ~{
  if(. == 1) 2
  else 1
}
)

plot(som_fit, type = "mapping", pch = 21, 
     bg = point_colors[as.factor(anes$democrat)],
     shape = "straight",
     bgcol = neuron_colors[as.integer(class_assign)],
     main = "2 clusters via HAC"); add.cluster.boundaries(x = som_fit, 
                                                  clustering = class_assign, 
                                                  lwd = 5, 
                                                  lty = 5)

# 

# now, a feature-level comparison

# trump and obama 
som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(Trump, Obama)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "SOM Codes for Feelings toward Trump and Obama") +
  theme_minimal()

# sanders and obama 
som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(Sanders, Obama)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "SOM Codes for Feelings toward Sanders and Obama") +
  theme_minimal()

# UN and NRA
som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(UN, NRA)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "SOM Codes for Feelings toward the UN and NRA") +
  theme_minimal()

# UN and NRA
som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(ICE, NRA)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "SOM Codes for Feelings toward ICE and the NRA") +
  theme_minimal()
