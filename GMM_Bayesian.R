library(dplyr)
library(dirichletprocess)
library(bayesm)
library(mixtools)

data(iris)
iris_subset <- iris %>% select(Petal.Length, Petal.Width, Species)

library(ggplot2)
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point() + 
  xlab("Petal Length") + ylab("Petal Width")+theme_bw(base_size=12)
ggsave("iris.pdf", width = 5, height = 4)


features <- iris_subset[,c(1,2)] %>% as.matrix()
mubar <- colMeans(features) %>% unname()
var1 <- sd(features[, 1])*3
var2 <- sd(features[, 2])*3



# set up prior
covariance_mat = matrix(c(var1, 0, 0, var2), nrow=2, ncol=2)


prior = list(Mubar = mubar, A=matrix(0.01), nu=5, V = covariance_mat, a=c(1, 1, 1), ncomp=3)

mcmc <- list(R=10000, keep=1, nprint=100, LogLike=FALSE)

# gibbs sampling
mixture_result <- rnmixGibbs(list(y=features), Prior=prior, Mcmc=mcmc)

# calculate posterior mean for cluster mean and covariance matrix
mean1 <- matrix(0, 1, 2)
invchol1 <- matrix(0, 2, 2)

mean2 <- matrix(0, 1, 2)
invchol2 <- matrix(0, 2, 2)

mean3 <- matrix(0, 1, 2)
invchol3 <- matrix(0, 2, 2)

for (drawid in 9901:10000){
  draws <- mixture_result$nmix$compdraw[[drawid]]
  mean1 <- mean1 + draws[[1]]$mu
  invchol1 <- invchol1 + draws[[1]]$rooti
  mean2 <- mean2 + draws[[2]]$mu
  invchol2 <- invchol2 + draws[[2]]$rooti
  mean3 <- mean3 + draws[[3]]$mu
  invchol3 <- invchol3 + draws[[3]]$rooti
}

mean1 <- mean1 / 100
invchol1 <- invchol1 / 100
mean2 <- mean2 / 100
invchol2 <- invchol2 / 100
mean3 <- mean3 / 100
invchol3 <- invchol3 / 100

var1 <- t(solve(invchol1)) %*% solve(invchol1)
var2 <- t(solve(invchol2)) %*% solve(invchol2)
var3 <- t(solve(invchol3)) %*% solve(invchol3)

# check density
c1d <- dmvnorm(features, mean1, var1)
c2d <- dmvnorm(features, mean2, var2)
c3d <- dmvnorm(features, mean3, var3)

dev.off()
traj1 <- ellipse(as.vector(mean1), var1, alpha=0.3, draw=FALSE) %>% as.data.frame()
traj1 <- rbind(traj1, traj1[1,])
traj2 <- ellipse(as.vector(mean2), var2, alpha=0.3, draw=FALSE) %>% as.data.frame()
traj2 <- rbind(traj2, traj2[1,])
traj3 <- ellipse(as.vector(mean3), var3, alpha=0.3, draw=FALSE) %>% as.data.frame()
traJ3 <- rbind(traj3, traj3[1,])

colnames(traj1) <- c("Petal.Length", "Petal.Width")
colnames(traj2) <- c("Petal.Length", "Petal.Width")
colnames(traj3) <- c("Petal.Length", "Petal.Width")

ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + geom_point(aes(color=Species)) + 
  geom_path(data=traj1) + geom_path(data=traj2) + geom_path(data=traj3)+
  xlab("Petal Length") + ylab("Petal Width") + theme(legend.position = "none")+
  theme_bw(base_size=12)
ggsave('iris_postmean.pdf', width=5, height=4)

combined_density <- cbind(c1d, c2d, c3d)
predicted_label <- apply(combined_density, 1, which.max)


iris$predict <- as.factor(predicted_label)

ggplot(iris, aes(x=Petal.Length, y=Petal.Width, shape=predict)) + geom_point(size=2) + 
  xlab("Petal Length") + ylab("Petal Width") + theme(legend.position = "none")+
  theme_bw(base_size=12)
ggsave('iris_pred.pdf', width=5, height=4)


# apply dirichlet process

iris_subset %>% select(Petal.Length, Petal.Width) %>% scale -> iris_scaled


dp <- DirichletProcessMvnormal(iris_scaled)
dp <- Fit(dp, 2000, progressBar = TRUE)

num_clusters <- rep(0, 1000)

for (iter in 1001:2000){
  num_clusters[iter - 1000] <- length(table(dp$labelsChain[[iter]]))
}

twocluster <- cbind(iris_subset, as.factor(dp$labelsChain[[2000]]))
colnames(twocluster)[4] <- 'Label'
twocluster$Sample <- "Two Cluster"

threecluster <- cbind(iris_subset, as.factor(dp$labelsChain[[1910]]))
colnames(threecluster)[4] <- 'Label'
threecluster$Sample <- "Three Cluster"

fourcluster <- cbind(iris_subset, as.factor(dp$labelsChain[[1351]]))
colnames(fourcluster)[4] <- 'Label'
fourcluster$Sample <- "Four Cluster"

fivecluster <- cbind(iris_subset, as.factor(dp$labelsChain[[1991]]))
colnames(fivecluster)[4] <- 'Label'
fivecluster$Sample <- "Five Cluster"

result <- rbind(twocluster, threecluster, fourcluster, fivecluster)


library(ggplot2)
ggplot(result, aes(x=Petal.Length, y=Petal.Width, shape=Label)) + geom_point(size=2) + 
  facet_wrap(~Sample, nrow=2) + theme_bw(base_size=12)+
  xlab("Petal Length") + ylab("Petal Width") + theme(legend.position = "none")
ggsave('iris_dir_pred.pdf', width=6, height=5)

