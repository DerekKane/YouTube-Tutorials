#########################################################################
# Clsuter Analysis in R Tutorial
#########################################################################


#########################################################################
# Lets create some data for the analysis
#########################################################################

n = 100
g = 6 
set.seed(g)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d)


#########################################################################
# Method 1. Look for a bend or elbow in the sum of squared error (SSE) scree plot.
#########################################################################

mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
title('Scree Plot')

# We might conclude that 4 clusters would be indicated by this method 


#########################################################################
# Method 2. partitioning around medoids to estimate the number of clusters 
#########################################################################

# install.packages("fpc")

library(fpc)
library(cluster)

pamk.best <- pamk(d)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(d, pamk.best$nc))

# we could also do:

library(fpc)
asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- pam(d, k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")

# still 4

#########################################################################
# Method 3. Calinsky criterion
#########################################################################

# In this case we try between 1 and 10 groups.

# install.packages("vegan")


require(vegan)
fit <- cascadeKM(scale(d, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# 5 clusters!

#########################################################################
# Method 4. Determine the optimal model and number of clusters according to 
# the Bayesian Information Criterion for expectation-maximization
#########################################################################

# See http://www.jstatsoft.org/v18/i06/paper
# http://www.stat.washington.edu/fraley/mclust/tr504.pdf

library(mclust)

# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.

d_clust <- Mclust(as.matrix(d), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")

# 4 clusters
plot(d_clust)


#########################################################################
# Method 5. Affinity propagation (AP) clustering
#########################################################################

library(apcluster)
d.apclus <- apcluster(negDistMat(r=2), d)
cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")

# 4 clusters

heatmap(d.apclus)
plot(d.apclus, d)

#########################################################################
# Method 6. Hierarchical Clustering
#########################################################################

# Hierarchical Clustering Approach. 

d_dist <- dist(as.matrix(d))   # find distance matrix 
plot(hclust(d_dist))           # apply hirarchical clustering and plot


# a Bayesian clustering method, good for high-dimension data, more details:
# http://vahid.probstat.ca/paper/2012-bclust.pdf


library(bclust)
x <- as.matrix(d)
d.bclus <- bclust(x, transformed.par = c(0, -50, log(16), 0, 0, 0))
viplot(imp(d.bclus)$var); plot(d.bclus); ditplot(d.bclus)
dptplot(d.bclus, scale = 20, horizbar.plot = TRUE,varimp = imp(d.bclus)$var, horizbar.distance = 0, dendrogram.lwd = 2)
# I just include the dendrogram here

# Also for high-dimension data is the pvclust library which calculates p-values for hierarchical 
# clustering via multiscale bootstrap resampling. Here's the example from 
# the documentation (wont work on such low dimensional data as in my example): 


library(pvclust)
library(MASS)

attach(Boston)
boston.pv <- pvclust(Boston)
plot(boston.pv)