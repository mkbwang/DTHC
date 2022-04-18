library(MASS)
library(dplyr)

rm(list=ls())

mu1 <- c(2, 2)
sigma1 <- diag(2)

mu2 <- c(-2, 2)
sigma2 <- diag(2)

mu3 <- c(-2, -2)
sigma3 <- diag(2)

mu4 <- c(2, -2)
sigma4 <- diag(2)

set.seed(2022)
dat1 <- cbind(mvrnorm(40, mu1, sigma1), 1) %>% round(digits=2)
dat2 <- cbind(mvrnorm(40, mu2, sigma2), 2) %>% round(digits=2)
dat3 <- cbind(mvrnorm(40, mu3, sigma3), 3) %>% round(digits=2)
dat4 <- cbind(mvrnorm(40, mu4, sigma4), 4) %>% round(digits=2)

alldata <- as.data.frame(rbind(dat1, dat2, dat3, dat4))
write.table(alldata, file='testdatavar1', sep='\t', col.names = FALSE, row.names=FALSE)

alldata$V3 <- as.factor(alldata$V3)
library(ggplot2)
ggplot(alldata, aes(x=V1, y=V2, color=V3)) + geom_point()


