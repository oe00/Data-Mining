# Oguzhan Ergun

# load data

#setwd("/Users/oe/Desktop/assignment3")

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(fastcluster)
library(Hmisc)
library(GGally)

student_evaluation = read.csv(file="student-evaluation.csv", header = T, sep =",")

dataset = student_evaluation[,c(6:33)]

str(dataset)

summary(dataset)

ggcorr(dataset)

clusterRange = c(1:15)
maxCluster = max(clusterRange)

### K-Means Clustering

kMeansResult = kmeans(dataset, centers = 3, nstart = 20)
kMeansResult
kMeansResult = kmeans(dataset, centers = 5, nstart = 20)
kMeansResult
kMeansResult = kmeans(dataset, centers = 7, nstart = 20)
kMeansResult
kMeansResult = kmeans(dataset, centers = 9, nstart = 20)
kMeansResult

## Normal

# Elbow Method
fviz_nbclust(dataset, kmeans, method = "wss",k.max = maxCluster)
elbow = 3 ## elbow located at 3.

# Silhouette Method
fviz_nbclust(dataset, kmeans , method = "silhouette",k.max = maxCluster)
silhouette = 3 ## silhouette located at 3.

# K-Means with Optimal Cluster Size
kMeansResult = kmeans(dataset, centers = 5, nstart = 20)
kMeansResult # K-means clustering with 3 clusters of sizes 1239, 2358, 2223

## PCA

datasetPCA = prcomp(dataset)
str(datasetPCA)
principleComps = c("Q1","Q2")

datasetPCA = dataset[,principleComps]
summary(prcomp(datasetPCA))

fviz_nbclust(datasetPCA, kmeans, method = "wss",k.max = maxCluster)
elbow = 3 ## elbow located at 3.

kMeansResult = kmeans(datasetPCA, centers = 3, nstart = 20)
kMeansResult # K-means clustering with 3 clusters of sizes 1767, 2043, 2010

# K-Means with PCA and Optimal Cluster Size

fviz_cluster(kMeansResult,dataset,ellipse = TRUE,
             main="Clusters of Student Evaluation (K-Means)",
             geom="point")



### Agglomerative Clustering

visualiseAgglomerative = function(ds){
  clusplot(dataset,
           ds,
           lines = 0,
           shade = TRUE,
           color = TRUE,
           labels= 0,
           plotchar = FALSE,
           span = TRUE,
           main = paste('Clusters of Student Evaluation (Agglomerative)'),
           xlab = "Question 1 (PC 1)",
           ylab = "Question 2 (PC 2)")
  
}

d = dist(dataset)
hc = hclust(d,method="ward.D2")
plot(hc)


hc_2_cluster = cutree(hc,k=2)
plot(hc)
rect.hclust(hc, k=2, border="red")
visualiseAgglomerative(hc_2_cluster)

hc_3_cluster = cutree(hc,k=3)
plot(hc)
rect.hclust(hc, k=3, border="red")
visualiseAgglomerative(hc_3_cluster)


