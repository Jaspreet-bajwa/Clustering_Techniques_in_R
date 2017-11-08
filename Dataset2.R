#loading packages
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(dbscan)
library(fpc)
library(class)

# Initial variable declaration
#change variable path value to folder path of dataset2.csv
dataset2path <- "C:/Users/iaman/Desktop/Jaspreet/Jaspreet/dataset2.csv"

readdata <- function(path){
  #dataset2.csv
  set.seed(9850)
  mydata <- read.csv(path,header = TRUE)
  mydata <- mydata[,c(1,2,3,4)]
  gp <- runif(nrow(mydata))
  mydata <- mydata[order(gp),]
  #normalize the data so that features with higher values should not undue influence the results
  mydata <- as.data.frame(lapply(mydata, normalize))
}

normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

hclustfunc <- function(d, method = "complete", dmeth = "euclidean") {
  cat("agglomerative method")
  hc1 <- hclust(dist(d, method = dmeth), method = method)
}

hclustfun <- function(d, method = "ward", dmeth = "euclidean") {
  cat("agglomerative method")
  hc2 <- agnes(dist(d, method = dmeth), method = method)
}

optimalnumofclusters <- function(d){
  # Determine number of clusters
  wss <- (nrow(d)-1)*sum(apply(d,2,var))
  for (i in 2:50) wss[i] <- sum(kmeans(d,centers=i)$withinss)
  plot(1:50, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

myHierarchical <- function(d){
  fit <- hclustfunc(d)
  #fit <- hclustfun(d)
}

#different methods -> "hybrid", "raw", "dist"
myDensityBased <- function(d, meth = "raw", k1 = 17){
  kNNdistplot(d, k = 4)
  set.seed(123)
  # fpc package
  res.fpc <- fpc::dbscan(d, eps = 0.10, MinPts = k1)
  print(res.fpc)
  # dbscan package
  cat("results")
  res.db <- dbscan::dbscan(d, 0.10, k1)
  print(res.db)
  #all(res.fpc$cluster == res.db)
  plot(res.fpc, d, main = "DBSCAN", frame = FALSE)
  fviz_cluster(res.db, d, stand = FALSE, frame = FALSE, geom = "point")
}

myGraphBased <- function(d){
  c1 <- jpclust(d, k = 20, kt=8)
  plot(d, col=c1$cluster+1L, cex = 0.5)
  nn <- kNN(d, k=30)
  c1 <- jpclust(nn, k = 20, kt = 8)
  d1 <- pointdensity(d, eps=25)
  #hist(d, breaks=20)
  d_noiseless <- d[d>110,]
  c1 <- jpclust(d_noiseless, k=20, kt=10)
  print(c1)
  plot(d_noiseless, col=c1$cluster+1L, cex=0.5)
}

plotHierarchical <- function(fit){
  plot(fit, labels = NULL, hang = 0.1, check = TRUE,
       axes = TRUE, frame.plot = FALSE, ann = TRUE,
       main = "Cluster Dendrogram",
       sub = NULL, xlab = NULL, ylab = "cluster")
}

myKMeans <- function(d){
  set.seed(2017)
  x = c(rnorm(250000, 0,0.9), rnorm(350000, 4,1), rnorm(500000, -5,1.1))
  y = c(rnorm(250000, 0,0.9), rnorm(350000, 5.5,1), rnorm(500000,  5,1.1))
  z = c(rnorm(250000, 0,0.9), rnorm(350000, 4,1), rnorm(500000,  -5,1.1))
  w = c(rnorm(250000, 0,0.9), rnorm(350000, 5.5,1), rnorm(500000,  5,1.1))
  XYZW = data.frame(x,y,z,w)
  Sample5K = sample(length(x), 5000)     ## Downsample
  ## Cluster the sample
  d5000 = dist(XYZW[Sample5K,])
  h5000 = hclust(d5000, method="single")
  str(h5000)
  Groups = cutree(h5000, 8)
  Groups[Groups>4] = 4
  plot(XYZW[Sample5K,], pch=20, col=rainbow(4, alpha=c(0.2,0.2,0.2,1))[Groups])
}

clusteringResults <- function(algo, d){
  cat(paste("Clustering for ", algo))
  fit <- switch (algo, 
                 Hierarchical = myHierarchical(d),
                 KMeans = myKMeans(d),
                 Density = myDensityBased(d),
                 Graph = myGraphBased(d)
  )
  
  if(algo == "Hierarchical"){
    plotHierarchical(fit);
  }
  
}

cat("\n Dataset : Dataset2\n\n")
d <- readdata(dataset2path)
optimalnumofclusters(d)
clusteringResults("KMeans",d)
#clusteringResults("Hierarchical",d)
#clusteringResults("Density",d)
#clusteringResults("Graph",d)
