#########################################################################
# Cluster Analysis in R Tutorial
#########################################################################


# Load some data

data(ruspini, package="cluster")

# Shuffle the rows

ruspini <- ruspini[sample(1:nrow(ruspini)),]

# Scale each column in the data to zero mean and unit standard deviation (z-scores). 
# This prevents one attribute with a large range to dominate the others for the distance calculation.

ruspini <- scale(ruspini)
plot(ruspini)
title('Ruspini Data')

###################################################################
# Cluster Techniques - kmeans
###################################################################

# Assumes Euclidean distances. We use k=10 clusters and run the algorithm 10 times 
# with random initialized centroids. The best result is returned.

km <- kmeans(ruspini, centers=4, nstart=10)
km

plot(ruspini, col=km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID
title('Kmeans Cluster Plot')

# Alternative plot from package cluster (uses principal components analysis for >2 dimensions)

library(cluster)
clusplot(ruspini, km$cluster)

km$centers

def.par <- par(no.readonly = TRUE) # save default, for resetting...
layout(t(1:4)) # 4 plots in one
for(i in 1:4) barplot(km$centers[i,], ylim=c(-2,2), main=paste("Cluster", i))

par(def.par)  #- reset to default

# Find data for a single cluster

cluster1 <- ruspini[km$cluster==1,]
head(cluster1)

plot(cluster1)
title('Plot of Cluster 1')

###################################################################
# Cluster Techniques - Hierarchical Clustering
###################################################################

# dist defaults to method="Euclidean"

d <- dist(ruspini)

# we cluster using a complete link

hc <- hclust(d, method="complete")

# Dendrogram

plot(hc)
rect.hclust(hc, k=4)

plot(as.dendrogram(hc), leaflab="none") # plot dendrogram without leaf labels
title('Dendrogram with No Labels')

cluster_complete <- cutree(hc, k=4)
plot(ruspini, col=cluster_complete)
title('Hierarchical Clustering')

# Clustering with single link

hc_single <- hclust(d, method="single")
plot(hc_single)
rect.hclust(hc_single, k=4)

cluster_single <- cutree(hc_single, k=4)
plot(ruspini, col=cluster_single)
title('Hierarchical Clustering with Single Link')

###################################################################
# Cluster Techniques - Density-based clustering with DBSCAN
###################################################################

library(fpc)

db <- dbscan(ruspini, eps=.3, MinPts=5)
db

str(db)

plot(ruspini, col=db$cluster+1L)
title('Density based cluster')

# Note: 0 is not a color so we add 1 to cluster.

###################################################################
# Cluster Techniques - Gaussian Mixture Models
###################################################################

library(mclust)

# Mclust uses BIC to find the number of clusters

m <- Mclust(ruspini)
summary(m)

plot(m)

# Rerun with 4 clusters

m <- Mclust(ruspini, G=4)
summary(m)

plot(m)

###################################################################
# Cluster Techniques - Internal Cluster Validation - Compare the Clustering Quality
###################################################################

library(fpc)

cluster.stats(d, km$cluster)


# Silhouette plot

plot(silhouette(km$cluster, d))

###################################################################
# Cluster Techniques - Find Optimal Clusters 
###################################################################

ks <- 2:10

# Use within sum of squares (look for the knee)

WSS <- sapply(ks, FUN=function(k) {
  kmeans(ruspini, centers=k, nstart=5)$tot.withinss
})
plot(ks, WSS, type="l")
title("Scree Plot")

# Use average silhouette width (look for the max)

ASW <- sapply(ks, FUN=function(k) {
  cluster.stats(d, kmeans(ruspini, centers=k, nstart=5)$cluster)$avg.silwidth
})
plot(ks, ASW, type="l")
title("Average Silhouette Width Plot")

# Find the max

ks[which.max(ASW)]

###################################################################
# Cluster Techniques - Visualize the Distance Matrix
###################################################################

# Heatmap of the data

image(as.matrix(d))

# Data organized in Clusters

library(seriation)

pimage(d, colorkey=TRUE)

# reorder using cluster labels

pimage(d, order=order(km$cluster), colorkey=TRUE)

# use displot

dissplot(d, labels=km$cluster, options=list(main="k-means with k=4"))

# spot the data problems for dbscan

dissplot(d, labels=db$cluster+1L, options=list(main="DBSCAN"))

# Here is a mispecified k = 3 instead of k = 4

dissplot(d, labels=kmeans(ruspini, centers=3)$cluster)

# Other versions etc...

dissplot(d, labels=kmeans(ruspini, centers=9)$cluster)

dissplot(d)

###################################################################
# Cluster Techniques - External Cluster Validation
###################################################################

library(mlbench)
shapes <- mlbench.smiley(n=500, sd1 = 0.1, sd2 = 0.05)
plot(shapes)
title("Happy Data")


# Prepare the data

truth <- as.integer(shapes$class)
shapes <- scale(shapes$x)

plot(shapes)
title("Happy Data")


# k-means approach

ks <- 2:20

# scree plot

WSS <- sapply(ks, FUN=function(k) {
  kmeans(shapes, centers=k, nstart=10)$tot.withinss
})
plot(ks, WSS, type="l")
title("Scree Plot")

# looks like 6 clusters

km <- kmeans(shapes, centers=6, nstart = 10)
plot(shapes, col=km$cluster)
title("Happy Data with KMeans")


##################################################################
# Hierarchical clustering
##################################################################

d <- dist(shapes)
hc <- hclust(d, method="single")

# (single-link because of the mouth)

ASW <- sapply(ks, FUN=function(k) {
  cluster.stats(d, cutree(hc, k))$avg.silwidth
})
plot(ks, ASW, type="l")
title("Average Silhouette Width Plot")


# 4 Clusters

hc_4 <- cutree(hc, 4)
plot(shapes, col=hc_4)
title("Happy Data with Hierarchical Clustering")

# Compare with ground truth (corrected.rand and vi)

cbind(
  kmeans = cluster.stats(d, km$cluster, truth, compareonly = TRUE),
  hc = cluster.stats(d, hc_4, truth, compareonly = TRUE)
)

