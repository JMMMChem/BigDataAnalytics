rm(list = ls()); 
gc();

####################DIRECTORY SETTING
setwd("~/Big Data Analytics - UC3M/T1/Technological fundamentals in the Big Data world/Progetto/lab2/")

####################LIBRARIES INSTALLING
#install.packages("dplyr")
#install.packages("akmedoids")
#install.packages("fpc")
#install.packages("cluster")
#install.packages("RColorBrewer")
library(dplyr)
library(tidyverse)
library(akmedoids)
library(fpc)
library(cluster)
library(RColorBrewer)
library(parallel)
library(doParallel)
library(factoextra)
library(gplots)

####################DATASET UPLOAD
time_0 <- Sys.time()
Original <- read.csv("computers.csv", sep=";")
CP <- Original
attach(CP)

####################DATA CLEANING AND DUMMY VARIABLES
CP$X.ÈÀid <- NULL
CP$cd <- ifelse(CP$cd=="yes",1,0)
CP$multi <- ifelse(CP$multi=="yes",1,0)
CP$premium <- ifelse(CP$premium=="yes",1,0)
CPscaled <- CP %>% mutate_each(funs(scale))

###################PARALLEL VERSION OF THE SERIAL PROGRAM
kmeansfunc <- function(k) {
  cluster <- kmeans(CPscaled, k)
  return(cluster$tot.withinss)
}

k_max <- 20
clust <- makeCluster(16) 
clusterExport(clust,varlist=c("kmeansfunc", "CPscaled"))

start_time <- Sys.time()
ssd <- parLapply(clust, 2:k_max, kmeansfunc)
stopCluster(clust)
end_time <- Sys.time()
time_taken <- end_time-start_time
sprintf("The time that it took to build the k-ssd matrix in parallel is: %f seconds", time_taken)

ssd <- unlist(ssd)
elbow <- elbowPoint(seq(2:k_max),ssd)
k_optimus <- round(elbow[[1]],0)
least_ssd <- round(elbow[[2]],2)
sprintf("The best number of k clusters is: %d", k_optimus)
sprintf("With %d clusters, the sum of squared distances is %f", k_optimus, least_ssd)

###################PLOT THE RESULTS OF THE ELBOW GRAPH
elbowmatrix <-data.frame(2:k_max, ssd)
ggplot(elbowmatrix, aes(x = X2.k_max, y = ssd))+geom_point()+geom_line()+geom_vline(xintercept=k_optimus,linetype="dashed", color="red")+scale_x_continuous(breaks=seq(1,20,by=1))+ggtitle("Elbow method for optimal K")+labs(x="k", y="Sum of squared distances")+theme(plot.title = element_text(hjust=0.5))

###################CLUSTER THE DATA USING THE OPTIMUM K
set.seed(123)
start_time <- Sys.time()
km_results <- kmeans(CPscaled, k_optimus, nstart = 25)
end_time <- Sys.time()
time_taken <- end_time-start_time
sprintf("The time that it took to run the kmeans function is: %f seconds", time_taken)

###################PLOT THE FIRST 2 DIMENSIONS OF THE CLUSTERS

fviz_cluster(km_results, CPscaled, geom='point', ellipse=FALSE)

###################FIND THE CLUSTER WITH THE HIGHEST AVERAGE PRICE

centroids <- km_results$centers
clus_max_price <- which.max(centroids[,1])
sprintf("The cluster with the highest avarage price is the number %d", clus_max_price)

###################PRINT A HEATMAP OF THE CLUSTERS CENTROIDS

heatmap.2(centroids, Rowv=FALSE, Colv=FALSE)
#image(t(CPscaled)[, nrow(CPscaled):1], yaxt = "n", main = "Original Data")
#image(t(CPscaled)[, order(km_results$cluster)], yaxt = "n", main = "Clustered Data")
time_final <-Sys.time()
time_total <- time_final-time_0
sprintf("The time that it took to run the whole program is: %f seconds", time_total)



