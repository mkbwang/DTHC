library(MASS)
library(dplyr)
library(hclust)

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
  xlim(0, 5) + ylim(0, 5)+
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

circles <- c(0.2*exp(pi * 0.2i * seq(0, 2, length.out = Npoints+1)[-1]),
             0.4*exp(pi * 0.2i * seq(0, 2, length.out = Npoints+1)[-1]),
             0.6*exp(pi * 0.2i * seq(0, 2, length.out = Npoints+1)[-1]),
             0.8*exp(pi * 0.2i * seq(0, 2, length.out = Npoints+1)[-1]),
             exp(pi * 0.2i * seq(0, 2, length.out = Npoints+1)[-1]))

coordinates_2D_1 <- data.frame(X=Re(circles), Y=Im(circles))
coordinates_2D_1$X <- coordinates_2D_1$X + 2
coordinates_2D_1$Y <- coordinates_2D_1$Y + 2

coordinates_2D_2 <- data.frame(X=Re(circles), Y=Im(circles))
coordinates_2D_2$X <- coordinates_2D_1$X + 3
coordinates_2D_2$Y <- coordinates_2D_1$Y + 3

data.grid.1 <- expand.grid(X = seq(1, 3, length.out=50), Y = seq(1, 3, length.out=50))
data.grid.1$radiussq <- (data.grid.1$X - 2)^2 + (data.grid.1$Y - 2)^2
data.grid.1 <- data.grid.1 %>% filter(radiussq < 1) %>% select(X, Y)
q.samp_1 <- cbind(data.grid.1, prob = mvtnorm::dmvnorm(data.grid.1, mean = mu1, sigma = sigma1))
q.samp_1$Cluster <- "C1"

data.grid.2 <- expand.grid(X = seq(2, 4, length.out=50), Y = seq(2, 4, length.out=50))
data.grid.2$radiussq <- (data.grid.2$X - 3)^2 + (data.grid.2$Y - 3)^2
data.grid.2 <- data.grid.2 %>% filter(radiussq < 1) %>% select(X, Y)
q.samp_2 <- cbind(data.grid.2, prob = mvtnorm::dmvnorm(data.grid.2, mean = mu2, sigma = sigma2))
q.samp_2$Cluster <- "C2"

q.samps <- rbind(q.samp_1, q.samp_2)

ggplot(data = dat, aes(X, Y)) + geom_point(aes(color = Cluster)) + 
  scale_color_manual(values=c("red", "blue"))+
  geom_contour(data = q.samp_1, aes(z=prob), col='red')+
  geom_contour(data = q.samp_2, aes(z=prob), col='blue') +
  xlim(0, 5) + ylim(0, 5)+
  theme_bw(base_size=12) + theme(legend.position = "none")
