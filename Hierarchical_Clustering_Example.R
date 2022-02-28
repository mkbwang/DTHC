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

distmat <- dist(dat[,c(1,2)])
hc.average <- hclust(distmat, method="average")
plot(hc.average)
prediction <- cutree(hc.average, 2)

library(ggplot2)
ggplot(dat, aes(X, Y, color=Cluster)) + geom_point() + theme_bw(base_size=12)

ggplot(dat, aes(X, Y, color=result)) + geom_point() + theme_bw(base_size=12)
dat$result <- paste0("R", prediction)


