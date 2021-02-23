# Clustering
# Perspectives on Computational Modeling
# pdwaggoner@uchicago.edu

# Techniques covered: k-means, hierarchical clustering, GMM

# load libraries and set some stuff up
library(tidyverse)
library(tidymodels)
library(patchwork)
library(here)
library(tictoc)
library(ggdendro)
library(cluster)
library(factoextra)
library(skimr)
library(dbscan)

set.seed(1234)
theme_set(theme_minimal())

# k-means will give you however many clusters you search for, whether these are meaningful or not, e.g...

# create some data
x <- list(
  `1` = MASS::mvrnorm(n = 300, c(-4,10), matrix(c(1.5,1,1,1.5),2)),
  `2` = MASS::mvrnorm(n = 300, c(5,7), matrix(c(1,2,2,6),2)),
  `3` = MASS::mvrnorm(n = 300, c(-1,1), matrix(c(4,0,0,4),2)),
  `4` = MASS::mvrnorm(n = 300, c(10,-10), matrix(c(4,0,0,4),2)),
  `5` = MASS::mvrnorm(n = 300, c(3,-3), matrix(c(4,0,0,4),2))
) %>%
  map_df(as_tibble, .id = "cluster") %>%
  rename(x = V1,
         y = V2)

# estimate k clusters
x.out <- x %>%
  select(-cluster) %>%
  mutate(k2 = kmeans(x, 2, nstart = 20)$cluster,
         k3 = kmeans(x, 3, nstart = 20)$cluster,
         k4 = kmeans(x, 4, nstart = 20)$cluster,
         k5 = kmeans(x, 5, nstart = 20)$cluster,
         k6 = kmeans(x, 6, nstart = 20)$cluster,
         k7 = kmeans(x, 7, nstart = 20)$cluster)

# plot clusters
x.out %>%
  gather(K, pred, k2:k7) %>%
  mutate(K = parse_number(K),
         pred = factor(pred)) %>%
  ggplot(aes(x, y, color = pred)) +
  facet_wrap(~ K, labeller = label_both) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme(legend.position = "none")


# Now, re-run multiple versions with different set of random initial centroids; colors don't matter here, rather the actual point assignments are what matters along with WSS changes. This is basically like starting the algorithm with a different random seed each time.

# First with k = 5
kmean.out <- rerun(6, kmeans(x %>%
                               select(-cluster), 
                             5, 
                             nstart = 1))

withinss <- rep(map_chr(kmean.out, ~ .$tot.withinss), each = nrow(x))

kmean.out %>%
  map_df(~ enframe(.$cluster, name = NULL), .id = "id") %>%
  bind_cols(bind_rows(x, x, x, x, x, x)) %>%
  mutate(withinss = str_c("Within SS = ", withinss),
         id = str_c("Attempt #", id),
         value = factor(value)) %>%
  ggplot(aes(x, y, color = value)) +
  facet_wrap(~ id + withinss, ncol = 3, 
             labeller = label_wrap_gen(multi_line = TRUE)) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Dark2", guide = FALSE) +
  labs(title = "Convergence of k-means cluster algorithm",
       x = expression(X[1]),
       y = expression(X[2])) +
  theme(legend.position = "none")

# not much change? 

# now, with 2
kmean.out <- rerun(6, kmeans(x %>%
                               select(-cluster), 
                             3, 
                             nstart = 1))

withinss <- rep(map_chr(kmean.out, ~ .$tot.withinss), each = nrow(x))

kmean.out %>%
  map_df(~ enframe(.$cluster, name = NULL), .id = "id") %>%
  bind_cols(bind_rows(x, x, x, x, x, x)) %>%
  mutate(withinss = str_c("Within SS = ", withinss),
         id = str_c("Attempt #", id),
         value = factor(value)) %>%
  ggplot(aes(x, y, color = value)) +
  facet_wrap(~ id + withinss, ncol = 3, labeller = label_wrap_gen(multi_line = TRUE)) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Dark2", guide = FALSE) +
  labs(title = "Convergence of k-means cluster algorithm",
       x = expression(X[1]),
       y = expression(X[2])) +
  theme(legend.position = "none")

# now a bit more; but still not a ton


# Another look at k-means with real data
df <- USArrests %>%
  na.omit %>%
  scale()

head(df)

# inspect reduction in within-cluster variation (sums of squares) over a range of values for k
wss <- function(k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}

tibble(
  k = 1:10
) %>%
  mutate(wss = map_dbl(k, wss)) %>%
  ggplot(aes(k, wss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "USArrests",
       x = "Number of clusters K",
       y = "Total within-clusters sum of squares")


# Now inspect k-means quality using average silhouette width

# RECALL: The silhouette value is a measure of how similar an object is to its own cluster (cohesion) compared to other clusters (separation). The silhouette ranges from −1 to +1, where a high value indicates that the object is well matched to its own cluster and poorly matched to neighboring clusters. If most objects have a high value, then the clustering configuration is appropriate. If many points have a low or negative value, then the clustering configuration may have too many or too few clusters.

# do it all with a helper function to compute average silhouette value for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

tibble(
  k = 2:10
) %>%
  mutate(avg_sil = map_dbl(k, avg_sil)) %>%
  ggplot(aes(k, avg_sil)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "US Arrests Data",
       x = "Number of clusters K",
       y = "Average silhouette value")


#
# Hierarchical clustering

# simulated data
# generate data
x <- tibble(x1 = rnorm(50) + 3,
            x2 = rnorm(50) - 4,
            y = ifelse(x1 < 3, "1",
                       ifelse(x2 > -4, "2", "3")))

ggplot(x, aes(x1, x2, color = y)) +
  geom_point() +
  scale_color_brewer(type = "qual", 
                     palette = "Dark2") +
  labs(title = "Simulated data",
       x = expression(X[1]),
       y = expression(X[2])) +
  theme(legend.position = "none")


# compare linkage methods
complete <- hclust(dist(x), 
                   method = "complete")

single <- hclust(dist(x), 
                 method = "single")

average <- hclust(dist(x), 
                  method = "average")

# plot
ggdendrogram(complete) +
  labs(title = "Complete linkage")

ggdendrogram(single) +
  labs(title = "Single linkage")

ggdendrogram(average) +
  labs(title = "Average linkage")

#
# A little deeper exploration focusing on HAC with complete linkage for tree cutting
# fit HAC with complete linkage 
hc_complete <- hclust(dist(x), 
                      method = "complete")

# plot
ggdendrogram(hc_complete)

# Recall, that to generate clusters, we make a horizontal cut somewhere on the dendrogram, severing the tree into multiple subtrees. 
# The height of the cut will dictate how many clusters are formed. 

# First, try cutting the tree at a height of 4 splits the dendrogram into two subtrees, and therefore two clusters:
h <- 4

# extract dendro data
hcdata <- dendro_data(hc_complete)

hclabs <- label(hcdata) %>%
  left_join(tibble(label = as.factor(seq.int(nrow(x))),
                   cl = as.factor(cutree(hc_complete, h = h))))

# plot dendrogram
ggdendrogram(hc_complete, labels = FALSE) +
  geom_text(data = hclabs,
            aes(label = label, x = x, y = 0, color = cl),
            vjust = .5, angle = 90) +
  geom_hline(yintercept = h, linetype = 2) +
  scale_color_brewer(type = "qual", palette = "Dark2", guide = FALSE) +
  theme(axis.text.x = element_blank(),
        legend.position = "none")

# now try for k = 3
h <- 3

# extract dendro data
hcdata <- dendro_data(hc_complete)

hclabs <- label(hcdata) %>%
  left_join(tibble(label = as.factor(seq.int(nrow(x))),
                   cl = as.factor(cutree(hc_complete, h = h))))

# plot dendrogram
ggdendrogram(hc_complete, labels = FALSE) +
  geom_text(data = hclabs,
            aes(label = label, x = x, y = 0, color = cl),
            vjust = .5, angle = 90) +
  geom_hline(yintercept = h, linetype = 2) +
  scale_color_brewer(type = "qual", palette = "Dark2", guide = FALSE) +
  theme(axis.text.x = element_blank(),
        legend.position = "none")

# 
# GMM

# load libraries needed for this section
library(tidyverse)
library(mixtools)
library(plotGMM)
library(amerika)

pres <- read_csv("2012_DVS.csv") %>% 
  mutate(State = X1,
         DVS = dem_vs) %>% 
  select(-c(X1, dem_vs))

head(pres)

# Take a look at the density
ggplot(pres, aes(x = DVS)) +
  geom_density() + 
  xlim(min(pres$DVS) - 10, 
       max(pres$DVS) + 10) +
  theme_minimal() +
  labs(x = "Democratic Vote Share")

# best guess at component means (fig from lecture)
ggplot(pres, aes(x = DVS)) +
  geom_density() + 
  xlim(min(pres$DVS) - 10, 
       max(pres$DVS) + 10) +
  theme_minimal() +
  labs(x = "Democratic Vote Share") +
  geom_vline(xintercept = 41, 
             col = amerika_palettes$Republican[3]) + 
  geom_vline(xintercept = 53, 
             col = amerika_palettes$Democrat[3])

# Start by fitting a two component (cluster) gmm
set.seed(7355) 

gmm1 <- normalmixEM(pres$DVS, k = 2) 

ggplot(data.frame(x = gmm1$x)) +
  geom_histogram(aes(x, ..density..), alpha = 0.4, fill = "darkgray") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm1$mu[1], gmm1$sigma[1], lam = gmm1$lambda[1]),
                colour = amerika_palettes$Republican[3]) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm1$mu[2], gmm1$sigma[2], lam = gmm1$lambda[2]),
                colour = amerika_palettes$Democrat[3]) +
  xlab("Democratic Vote Shares") +
  ylab("Density") + 
  theme_minimal()


# next attempt
set.seed(7355)
gmm2 <- normalmixEM(pres$DVS, k = 3)

ggplot(data.frame(x = gmm2$x)) +
  geom_histogram(aes(x, ..density..), alpha = 0.4, fill = "darkgray") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm2$mu[1], gmm2$sigma[1], lam = gmm2$lambda[1]),
                colour = amerika_palettes$Republican[3]) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm2$mu[2], gmm2$sigma[2], lam = gmm2$lambda[2]),
                colour = amerika_palettes$Democrat[3]) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm2$mu[3], gmm2$sigma[3], lam = gmm2$lambda[3]),
                colour = "black") +
  xlab("Democratic Vote Shares") +
  ylab("Density") + 
  theme_minimal()


# Searching for (potentially) problematic observation, given poor fit of GMM
which(pres$DVS > 80)

# now we can try again without outlier
pres2 <- pres[-c(17), ]

# quickly compare to make sure it worked
withDC <- head(pres$DVS, 20)
withoutDC <- head(pres2$DVS, 20)

head(data.frame(cbind(withDC, withoutDC)), 20)


# now on with the GMM
set.seed(1)

dvs.nodc <- pres2$DVS
gmm.nodc <- normalmixEM(dvs.nodc, k = 2)

ggplot(data.frame(x = gmm.nodc$x)) +
  geom_histogram(aes(x, ..density..), alpha = 0.4, fill = "darkgray", bins = 20) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm.nodc$mu[1], gmm.nodc$sigma[1], lam = gmm.nodc$lambda[1]),
                colour = amerika_palettes$Republican[3]) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(gmm.nodc$mu[2], gmm.nodc$sigma[2], lam = gmm.nodc$lambda[2]),
                colour = amerika_palettes$Democrat[3]) +
  xlab("Democratic Vote Shares") +
  ylab("Density") + 
  theme_minimal()

# Call specific values from the output
# means
gmm.nodc$mu

# sd's
gmm.nodc$sigma

# weights
gmm.nodc$lambda


## Explore component densities a bit
# Table for viz
posterior <- data.frame(cbind(gmm.nodc$x, gmm.nodc$posterior))
rownames(posterior) <- pres2$State
round(head(posterior, 10), 3)

# get counts for each component 
posterior$component <- ifelse(posterior$comp.1 > 0.3, 1, 2)
table(posterior$component) 

# View the DVS by component (again, saying nothing of parties explicitly)
ggplot(posterior, aes(x = V1)) + 
  geom_histogram(aes(fill = factor(component)), alpha = 0.4, stat ="bin", binwidth = 3) +
  labs(x = "Democratic Vote Share",
       y = "Count of States",
       title = "Gaussian Mixture Model") +
  scale_fill_manual(values=c(amerika_palettes$Republican[3], amerika_palettes$Democrat[3]),
                    name="Component",
                    breaks=c("1", "2"),
                    labels=c("1", "2")) +
  geom_vline(xintercept = 50, linetype="solid", 
             color = "black", size=1.2) +
  theme_minimal()


## 
## On your own: k-means and 2012 pres vote shares

# For this section, you may either stay in the class and work independently or you may log off and work independently. 

# Either way, don't forget that this code you're working on here is due to the appropriate Canvas module prior to 5:00 pm CDT tomorrow. You need only submit a *single* file/script to be considered for credit. Recall, I don't care whether you got things right. I only care that attempts to each question have been made.

# Load data and update the pres vote data for 2012
library(tidyverse)

pres <- read_csv("2012_DVS.csv") %>% 
  mutate(State = X1,
         DVS = dem_vs) %>% 
  select(-c(X1, dem_vs))


# 1. Fit a k-means algorithm to the presidential vote shares data we used today, initialized at k=2. 


# 2. Plot the cluster assignments for all states, varying color for each cluster. Consider using a histogram as we did for the GMM case.


# 3. Put a vertical cut point/line at 50. Given that in a normal election, it requires 50+1 shares of the votes to win, the idea here is if the k-means solution is good, then all states clustered together should lie on their respective sides of the line.


# 4. Is any state on the "wrong" side of the line? If so, which one?




##
##
## EXTRA CODE (if you're interested): Using the DBSCAN algorithm to explore state legislative professionalism

# load professionalism data
load("legprof-components.v1.0.RData")

# First, set up scaled df
st <- x %>% 
  filter(sessid == "2009/10") %>% 
  select(-c(fips, stateabv, sessid, mds1, mds2, year)) %>%
  na.omit(st); skim(st)

st_scale <- data.frame(scale(st[,2:5]))

states <- st$state # save state names for plot

# read as matrix for dbscan fit
st_scale <- data.frame(scale(st[,2:5])) %>% 
  rename(`Total Length` = t_slength,
         `Regular Length` = slength,
         `Salary` = salary_real,
         `Expenditures` = expend) %>% 
  as.matrix()


# first, determine the optimal epsilon value

# Epsilon is the maximum distance between two points, which informs clustering

# the kNNdistplot() function plots the average distance between every point and its nearest neighbors, and are arranged in ascending order

# Though a bit ambiguous, we are looking for an "elbow"/"knee" or a sharp change in average distance, suggesting distance between points is growing, 

# thus signaling the possibility of outliers, which informs the threshold for the neighborhood size

kNNdistplot(st_scale, 
            k = 4) # number of nearest neighbors; try altering this to see differences
abline(h = 1.2, # looks like around 1.2
       col = "red")

# run the algorithm
dbscan_fit <- dbscan(st_scale, 
                     eps = 1.2, 
                     minPts = 4)

# Visualize all features: visualize across all raw values
pairs(st_scale, 
      col = ifelse(dbscan_fit$cluster == 1, "#F8766D", "#00BFC4"), 
      pch = 19)

# note: blue points/states are treated as outliers/noise - this is at the heart of the difference in the DBSCAN approach to clustering

# Visualize cluster assignment
rownames(st_scale) <- st$state

fviz_cluster(dbscan_fit, st_scale, 
             repel = TRUE,
             show.clust.cent = FALSE,
             outlier.color = "#00BFC4", 
             labelsize = 7,
             pointsize = 1.5, 
             main = "Cluster Assignments from DBSCAN Algorithm") +
  theme_minimal()
