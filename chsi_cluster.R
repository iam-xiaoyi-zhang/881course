#Silhouette analysis for determining the number of clusters 
library(fpc)
asw <- numeric(20)
for (k in 2:20){
  print(k)
  asw[[k]] <- pam(mydata, k) $ silinfo $ avg.width
}
k.best <- which.max(asw)

cat("silhouette-optimal number of clusters:", k.best, "\n")
d <- dist(mydata, method = "euclidean") 
plot(pam(d, k.best))
plot(1:20,asw)



# cluster of chsi: health in MA
View(MA[[3]])
mydata <- MA[[3]][,c(3,7,11,17,23)]

# possible clean
mydata[is.na(mydata)] <- 0

# standardization
for(i in c(2:5)){
  mydata[[i]] <- (max(mydata[[i]]) - mydata[[i]])/(max(mydata[[i]] - min(mydata[[i]])))
}

# decide the number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata[,2:5],2,var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(mydata[,2:5],centers=i)$withinss)
}   
wss
plot(1:10, wss, xlab="Number of Clusters",ylab="Within groups sum of squares")

# do clustering
cluout <- kmeans(mydata[,2:5],center=5)
mydata$cluster <- cluout$cluster
View(mydata)

