######################################################################
# Genetic Algorithm Tutorial - Traveling Salesman Problem
######################################################################


# Read csv as a dataframe

mydata <- read.csv("TSPdata.csv")
mydata2 <- mydata

# Load Libraries

library(GA)

######################################################################
# Perform K-Means to identify the clusters
######################################################################

# Remove unneeded data columns   

mydata2$CustomerID<-NULL
mydata2$Address<-NULL

# perform the KMeans clsuter for 5 number of clusters.

set.seed(14)

results<-kmeans(mydata2, 5)

# this will plot the original dataset with the coloring of the kmeans
# cluster results.

plot(mydata[c("Latitude", "Longitude")], col= results$cluster)

# Combine the results to original dataset

mydata <- cbind(mydata, results$cluster)

# rename the 5th column

colnames(mydata)[5] <- "Cluster"

######################################################################
# Subset to identify a specific cluster
######################################################################

TSPdata <- subset(mydata, Cluster == "3")

# Plot the cluster

plot(TSPdata[c("Latitude", "Longitude")], col= "green")


# Remove unecessary clusters

TSPdata$Address <- NULL
TSPdata$Cluster <- NULL
# TSPdata$CustomerID <- NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The Distance Matrix Function for GeoGraphic Points
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run this code below.


ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$name
  colnames(mat.distances) <- df.geopoints$name
  
  return(mat.distances)
}




# This section require the GeoGraphic Function to be executed.

colnames(TSPdata)[1] <- "name"
colnames(TSPdata)[2] <- "lat"
colnames(TSPdata)[3] <- "lon"

# Run the GeoGraphic Distance Function first!!! 

Distmatrix <- GeoDistanceInMetresMatrix(TSPdata)
D <- as.matrix(Distmatrix)
V <- as.vector(D)


######################################################################
# Run the Genetic Algorithm to identify the best route
######################################################################

# The fitness function to me maximized can be definied as the reciproical 
# of the tour length.

tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour,2)[,2:1]
  sum(distMatrix[route])
}

tspFitness <- function(tour, ...) 1/tourLength(tour, ...)

GA <- ga(type= "permutation", fitness= tspFitness, distMatrix=D, min=1, max=nrow(D), popSize=50, maxiter = 5000, run=500, pmutation=0.2)

summary(GA)

plot(GA)

# The solutions correspond to a unique path equal to:

apply(GA@solution, 1, tourLength, D)

# This solution will show the map of the ideal route produced by the GA.

x <- TSPdata[, 2]
y <- TSPdata[, 3]
plot(x,y, type= "n", asp=1, xlab="Latitude", ylab="Longitude", col="green")
tour <- GA@solution[1,]
tour <- c(tour, tour[1])
n <- length(tour)
abline(arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length=0.15, angle=25, col = "steelblue", lwd=2))
text(x,y, labels(TSPdata$name), cex=0.8)


# Plot the first solution

GA1 <- ga(type= "permutation", fitness= tspFitness, distMatrix=D, min=1, max=nrow(D), popSize=50, maxiter = 1, run=1, pmutation=0.2)

plot(TSPdata$lat,TSPdata$lon, type="n", asp=1, xlab="Latitude", ylab="Longitude", col="green")
tour <- GA1@solution[1,]
tour <- c(tour, tour[1])
n <- length(tour)
abline(arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length=0.15, angle=25, col = "steelblue", lwd=2))
text(x,y, labels(TSPdata$name), cex=0.8)
