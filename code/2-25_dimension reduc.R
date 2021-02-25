# Dimension reduction (30100)
# NOTE: Much of this is taken from my newest book under contract with Cambridge University Press; so, please don't share the code beyond this class
# Philip Waggoner, pdwaggoner@uchicago.edu

# Techniques: PCA, LLE, t-SNE, UMAP

# Load libraries for this section
library(tidyverse)
library(here)
library(corrr)
library(amerika)
library(factoextra)
library(patchwork)
library(ggrepel)

# Read in cleaned and preprocessed 2019 ANES Pilot Data (35 FTs + democrat party feature)
anes <- read_rds(here("Data", "anes.rds"))

# correlations
## fttrump vs. all others
anes %>%
  select(-democrat) %>% 
  correlate(use = "pairwise.complete.obs",
            method = "pearson",
            quiet = TRUE) %>% 
  focus(Trump) %>%
  mutate(rowname = reorder(rowname, Trump)) %>%
  ggplot(aes(rowname, Trump)) +
  geom_col() + 
  coord_flip() + 
  labs(y = "Feelings Toward Trump", 
       x = "All Other Feeling Thermometers") +
  theme_minimal()

## ftjapan vs. all others
anes %>%
  select(-democrat) %>% 
  correlate(use = "pairwise.complete.obs",
            method = "pearson",
            quiet = TRUE) %>% 
  focus(Japan) %>%
  mutate(rowname = reorder(rowname, Japan)) %>%
  ggplot(aes(rowname, Japan)) +
  geom_col() + 
  coord_flip() + 
  labs(y = "Feelings Toward Japan", 
       x = "All Other Feeling Thermometers") + 
  theme_minimal()

## network viz
anes %>%
  select(-democrat) %>% 
  correlate(use = "pairwise.complete.obs",
            method = "pearson",
            quiet = TRUE) %>% 
  network_plot(colors = c(amerika_palettes$Democrat[1], 
                          amerika_palettes$Republican[1]),
               curved = FALSE) 

# fit
pca_fit <- anes[,-36] %>%
  scale() %>% 
  prcomp(); summary(pca_fit)


# viz via factoextra
scree1 <- fviz_screeplot(pca_fit, main = "", addlabels = TRUE, choice = "variance")
scree2 <- fviz_screeplot(pca_fit, main = "", addlabels = TRUE, choice = "eigenvalue")

scree1 + scree2 

# biplot
pca_fit %>% 
  fviz_pca_biplot(label = "var",
                  col.var = amerika_palettes$Republican[2],
                  col.ind = amerika_palettes$Democrat[3]) +
  labs(title = "") +
  theme_minimal()

# feature loadings/contributions ("contrib")
pca_fit %>% 
  fviz_pca_var(col.var = "contrib") +
  scale_color_gradient(high = amerika_palettes$Democrat[1], 
                       low = amerika_palettes$Republican[1]) +
  labs(color = "Contribution",
       title = "") +
  theme_minimal()

# custom, full viz
anes %>% 
  ggplot(aes(pca_fit$x[, 1],
             pca_fit$x[, 2], 
             col = factor(democrat))) +
  geom_point() +
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                      name="Party",
                      breaks=c("0", "1"),
                      labels=c("Non-Democrat", "Democrat")) +
  labs(x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()


#
# LLE
# libraries needed for this section
library(tidyverse)
library(lle)
library(amerika)
library(parallel)
library(ggrepel)
library(tictoc)
library(patchwork)

# scale the data first
anes_scaled <- anes[, 1:35] %>% 
  scale() %>% 
  as_tibble() 

cores <- detectCores() - 1 

# find optimal k
tic() 
find_k <- calc_k(anes_scaled,
                 m = 2, 
                 parallel = TRUE,
                 cpus = cores) 
toc() # ~ 10.9 minutes on 3 cores; ~ 9.2 minutes on 7 cores

# inspect -- what is the optimal value for k? (a couple options...)
## option 1: manually by arranging
find_k %>% 
  arrange(rho) # looks like k = 19 is optimal

## option 2: extracting via which.min()
find_k[which.min(find_k$rho), ] 

# Extract based on min \rho
optimal_k_rho <- find_k %>% 
  arrange(rho) %>% 
  filter(rho == min(.))

## viz
find_k %>% 
  arrange(rho) %>% 
  ggplot(aes(k, rho)) +
  geom_line() +
  geom_point(color = ifelse(find_k$k == min(find_k$k), 
                            "red", 
                            "black")) +
  geom_vline(xintercept = optimal_k_rho$k, 
             linetype = "dashed", 
             color = "red") +
  geom_label_repel(aes(label = k),
                   box.padding = unit(0.5, 'lines')) +
  labs(x = "Neighborhood Size (k)",
       y = expression(rho)) +
  theme_minimal()

# fit 
{
  tic() 
  lle_fit <- lle(anes_scaled,
                 m = 2,
                 nnk = TRUE,
                 k = optimal_k_rho$k)
  toc() # ~ 1.5 minutes on 3 cores; ~ 1.4 minutes on 7 cores
  }

# full LLE viz
anes %>% 
  ggplot(aes(x = lle_fit$Y[,1], 
             y = lle_fit$Y[,2], 
             col = factor(democrat))) +
  geom_point() +
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", 
                              "Democrat")) +
  labs(x = "First Dimension",
       y = "Second Dimension",
       title = "LLE") + 
  theme_minimal()


# Compare with raw inputs
p1 <- anes %>% 
  ggplot(aes(Trump, Obama, 
             color = factor(democrat))) +
  geom_density_2d() + 
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", 
                              "Democrat")) +
  labs(x = "Feelings Toward Trump",
       y = "Feelings Toward Obama") +
  theme_minimal()

p2 <- anes %>% 
  ggplot(aes(ICE, Illegal, 
             color = factor(democrat))) +
  geom_density_2d() + 
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", 
                              "Democrat")) +
  labs(x = "Feelings Toward ICE",
       y = "Feelings Toward Illegal Immigrants") +
  theme_minimal()

p3 <- anes %>% 
  ggplot(aes(UN, NATO, 
             color = factor(democrat))) +
  geom_density_2d() + 
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", 
                              "Democrat")) +
  labs(x = "Feelings Toward the United Nations",
       y = "Feelings Toward NATO") +
  theme_minimal()

p4 <- anes %>% 
  ggplot(aes(Palestine, Israel, 
             color = factor(democrat))) +
  geom_density_2d() + 
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", 
                              "Democrat")) +
  labs(x = "Feelings Toward Palestine",
       y = "Feelings Toward Israel") +
  theme_minimal()

# viz together
(p1 + p2) /
  (p3 + p4)

#
# t-SNE

# libraries needed for this section
library(tidyverse)
library(amerika)
library(tictoc)
library(patchwork)
library(Rtsne)
library(umap)
library(tidymodels)
library(embed)

set.seed(1234)

{
  tic()
  
  # perplexity = 2
  tsne_2 <- Rtsne(as.matrix(anes[ ,1:35]), 
                  perplexity = 2)
  
  perp_2 <- anes %>%
    ggplot(aes(tsne_2$Y[,1], tsne_2$Y[,2], 
               col = factor(democrat))) +
    geom_point() +
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Democrat",
                       breaks=c("0", "1"),
                       labels=c("No", 
                                "Yes")) +
    ylim(-100, 100) +
    xlim(-100, 100) +
    labs(x = "First dimension",
         y = "Second dimension",
         subtitle = "Perplexity = 2") +
    theme_minimal()
  
  # perplexity = 25
  tsne_25 <- Rtsne(as.matrix(anes[ ,1:35]), 
                   perplexity = 25) 
  
  perp_25 <- anes %>%
    ggplot(aes(tsne_25$Y[,1], tsne_25$Y[,2], 
               col = factor(democrat))) +
    geom_point() +
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Democrat",
                       breaks=c("0", "1"),
                       labels=c("No", 
                                "Yes")) +
    ylim(-100, 100) +
    xlim(-100, 100) +
    labs(x = "First dimension",
         y = "Second dimension",
         subtitle = "Perplexity = 25") +
    theme_minimal()


  # perplexity = 50
  tsne_50 <- Rtsne(as.matrix(anes[ ,1:35]), 
                    perplexity = 50) 
  
  perp_50 <- anes %>%
    ggplot(aes(tsne_50$Y[,1], tsne_50$Y[,2], 
               col = factor(democrat))) +
    geom_point() +
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Democrat",
                       breaks=c("0", "1"),
                       labels=c("No", 
                                "Yes")) +
    ylim(-100, 100) +
    xlim(-100, 100) +
    labs(x = "First dimension",
         y = "Second dimension",
         subtitle = "Perplexity = 50") +
    theme_minimal()
  
  
  # perplexity = 500
  tsne_500 <- Rtsne(as.matrix(anes[ ,1:35]), 
                    perplexity = 500) 
  
  perp_500 <- anes %>%
    ggplot(aes(tsne_500$Y[,1], tsne_500$Y[,2], 
               col = factor(democrat))) +
    geom_point() +
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Democrat",
                       breaks=c("0", "1"),
                       labels=c("No", 
                                "Yes")) +
    ylim(-100, 100) +
    xlim(-100, 100) +
    labs(x = "First dimension",
         y = "Second dimension",
         subtitle = "Perplexity = 500") +
    theme_minimal()
  
  toc()
} # ~1 minute


# Visualize
tsne_plots <- (perp_2 + perp_25) /
  (perp_50 + perp_500)

tsne_plots

## with annotation if desired
#tsne_plots + plot_annotation(title = "t-SNE Results Across a Range of Perplexity",
#                             subtitle = "Color conditional on Party Affiliation")


#
# UMAP

# finally, let's take a look at UMAP

# epochs = 500
umap_fit_5 <- anes[,1:35] %>% 
  umap(n_neighbors = 5,
       metric = "euclidean",
       n_epochs = 500)
  
umap_fit_5 <- anes %>% 
  mutate_if(.funs = scale,
            .predicate = is.numeric,
            scale = FALSE) %>% 
  mutate(First_Dimension = umap_fit_5$layout[,1],
         Second_Dimension = umap_fit_5$layout[,2]) %>% 
  gather(key = "Variable",
         value = "Value",
         c(-First_Dimension, -Second_Dimension, -democrat))

k_5 <- ggplot(umap_fit_5, aes(First_Dimension, Second_Dimension, 
                              col = factor(democrat))) + 
  geom_point(alpha = 0.6) +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Democrat",
                     breaks=c("-0.418325434439179", 
                              "0.581674565560822"),
                     labels=c("No", 
                              "Yes")) +
  labs(title = " ",
       subtitle = "Neighborhood size: 5; Epochs = 500",
       x = "First Dimension",
       y = "Second Dimension") +
  theme_minimal()


# epochs = 20 
umap_fit_e_20 <- anes[,1:35] %>% 
  umap(n_neighbors = 5,
       metric = "euclidean",
       n_epochs = 20)

umap_fit_e_20 <- anes %>% 
  mutate_if(.funs = scale,
            .predicate = is.numeric,
            scale = FALSE) %>% 
  mutate(First_Dimension = umap_fit_e_20$layout[,1],
         Second_Dimension = umap_fit_e_20$layout[,2]) %>% 
  gather(key = "Variable",
         value = "Value",
         c(-First_Dimension, -Second_Dimension, -democrat))

e_20 <- ggplot(umap_fit_e_20, aes(First_Dimension, Second_Dimension, 
                                  col = factor(democrat))) + 
  geom_point(alpha = 0.6) +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Democrat",
                     breaks=c("-0.418325434439179", 
                              "0.581674565560822"),
                     labels=c("No", 
                              "Yes")) +
  labs(title = " ",
       subtitle = "Neighborhood size: 5; Epochs = 20",
       x = "First Dimension",
       y = "Second Dimension") +
  theme_minimal()

# side by side
k_5 + e_20


##
## ON YOUR OWN
## 

# For today's "on your own" section, I want you to compare the global vs. local tradeoff across the t-SNE and UMAP algorithms. We might expect similar structure to be revealed as we are working with the same data. But recall, these are different approaches to data representation. So there's room to also expect differences in the projection space (that is, the lower dimensional representation of the data). Explore this tradeoff by answering the few questions below, which are based on versions of the algorithms we have fit together today in class. 

# REMEMBER: submit your code by 5:00 pm CDT tomorrow on Canvas to receive credit for this exercise. 

# 1. The global vs. local representation of data in a t-SNE fit is controlled by the perplexity hyperparameter, where larger values mean a more global version, compared to smaller values which mean a more local version. Crank up perplexity to 1000 for a t-SNE fit to the ANES data. Plot the results, colored by party. *Caution*: this will take ~5-7 minutes to run.


# 2. The tradeoff in UMAP between global and local behavior is controlled by the n_neighbors hyperparameter, where larger values mean more neighbors to include in the fuzzy search region, versus fewer neighbors with smaller values for this hyperparameter. Fit a similarly global version of UMAP to the ANES data by cranking up the n_neighbors hyperparameter to 1000. Plot the results, colored by party. *Caution*: this will take about 5-7 minutes to run.


# 3. Do these global version of the algorithms reveal similar structure in the projection space or not? Give just a couple sentences describing your thoughts on global vs. local behavior, and also in comparing the t-SNE and UMAP algorithms. 
