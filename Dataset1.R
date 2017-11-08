#loading packages
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(dbscan)
library(fpc)

# Initial variable declaration
#change variable path value to folder path of dataset1.csv
dataset1path <- "C:/Users/iaman/Desktop/Jaspreet/Jaspreet/dataset1.csv"

readdata <- function(path){
  #dataset1.csv
  set.seed(9850)
  mydata <- read.csv(path,header = TRUE)
  mydata <- mydata[,c(1,2,3)]
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

optimalK <- function(df){
  cat("\nelbow method")
  fviz_nbclust(df, FUN = hcut, method = "wss")
  cat("\naverage silhoutte")
  fviz_nbclust(df, FUN = hcut, method = "silhouette")
  cat("\ngraph")
  gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
  fviz_gap_stat(gap_stat)
}

myKMeans <- function(d){
  fit <- kmeans(d, centers = 8, nstart = 25, algorithm = "Hartigan-Wong")
  str(fit)
  fviz_cluster(fit, data = d)
}

myHierarchical <- function(d){
  fit <- hclustfunc(d)
  #fit <- hclustfun(d)
}

#different methods -> "hybrid", "raw", "dist"
myDensityBased <- function(d, meth = "raw", k1 = 17){
  kNNdistplot(d, k = 3)
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

clusteringResults <- function(algo, d){
  cat(paste("\n Clustering for ", algo))
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

cat("\n Dataset : Dataset1\n\n")
d <- readdata(dataset1path)
cat("\n Clustering Results\n")
clusteringResults("KMeans",d)
clusteringResults("Hierarchical",d)
clusteringResults("Density",d)
clusteringResults("Graph",d)

