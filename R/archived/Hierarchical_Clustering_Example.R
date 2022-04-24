library(MASS)
library(dplyr)

mu1 <- c(2,2)
sigma1 <- diag(2)/2

mu2 <- c(3,3)
sigma2 <- diag(2)/2

set.seed(2)
dat1 <- mvrnorm(4, mu1, sigma1) %>% as.data.frame()
colnames(dat1) <- c("X", "Y")

dat2 <- mvrnorm(3, mu2, sigma2) %>% as.data.frame()
colnames(dat2) <- c("X", "Y")

dat1$Cluster = "C1"
dat2$Cluster = "C2"

dat <- rbind(dat1, dat2)
dat$index <- seq(1, 7)

# distance matrix
distmat <- matrix(0, 7, 7)
for (i in seq(1,7)){
  for (j in seq(1,7)){
    distmat[i, j] <- sqrt((dat[i, 1] - dat[j, 1])^2 + (dat[i, 2] - dat[j, 2])^2)
  }
}

# calculate average distance between two groups of points
calcdist <- function(mat, entity1, entity2){
  sumval <- 0
  for (i1 in entity1){
    for (i2 in entity2){
      sumval <- sumval + mat[i1, i2]
    }
  }
  return(sumval / length(entity1) / length(entity2))
}

# hierarchical clustering
distmat <- dist(dat[,c(1,2)])
hc.average <- hclust(distmat, method="average")
plot(hc.average, xlab="")
agglo_prediction <- cutree(hc.average, 2)
dat$aggloresult <- paste0("R", agglo_prediction)

# plotting
library(ggplot2)
ggplot(dat, aes(X, Y, label=index)) + geom_point() + 
  xlim(0, 5) + ylim(0, 5)+ geom_text(hjust=-0.1, vjust=-0.1)+
  theme_bw(base_size=12)


ggplot(dat, aes(X, Y, color=aggloresult, label=index)) + geom_point() + 
  geom_text(hjust=-0.1, vjust=-0.1)+ xlim(0, 5) + ylim(0, 5)+
  theme_bw(base_size=12) + theme(legend.position = "none") +
  scale_color_manual(values=c("red", "blue"))

dat$divresult <- paste0("R", c(1,1,2,1,2,1,1))

ggplot(dat, aes(X, Y, color=divresult, label=index)) + geom_point() + 
  geom_text(hjust=-0.1, vjust=-0.1)+ xlim(0, 5) + ylim(0, 5)+
  theme_bw(base_size=12) + theme(legend.position = "none") +
  scale_color_manual(values=c("red", "blue"))

library(mixtools)
cluster1 <- ellipse(mu1, sigma1, alpha=0.3) %>% as.data.frame()
colnames(cluster1) <- c("X", "Y")
cluster1 <- rbind(cluster1, cluster1[1,])
cluster2 <- ellipse(mu2, sigma2, alpha=0.3) %>% as.data.frame()
colnames(cluster2) <- c("X", "Y")
cluster2 <- rbind(cluster2, cluster2[1,])


ggplot(data = dat, aes(X, Y)) + geom_point(aes(color = Cluster)) + 
  scale_color_manual(values=c("red", "blue"))+
  geom_path(data = cluster1, col='red')+
  geom_path(data = cluster2, col='blue') +
  xlim(0, 5) + ylim(0, 5)+
  theme_bw(base_size=12) + theme(legend.position = "none")
