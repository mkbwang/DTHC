library(dplyr)

directory = "/Users/wangmk/Documents/Research/Wen_group/raw_data/Luke_Suspension"
alldata = readRDS(file.path(directory, 'singlecell_sheet.rds'))
# count cell type counts
table(alldata$Phenotype)

## subsample data
set.seed(2022)
Bcells <- alldata %>% filter(Phenotype %in% c("B")) %>% slice_sample(prop=0.005) %>%
  select(CD19, CD45R, CD3, CD4, CD8) %>% as.matrix()
CD4T <- alldata %>% filter(Phenotype %in% c("CD4+")) %>% slice_sample(prop=0.005) %>%
  select(CD19, CD45R, CD3, CD4, CD8) %>% as.matrix()
CD8T <- alldata %>% filter(Phenotype %in% c("CD8+")) %>% slice_sample(prop=0.005) %>%
  select(CD19, CD45R, CD3, CD4, CD8) %>% as.matrix()

majorcells <- rbind(Bcells, CD4T, CD8T)



library(bayesm)

# set up prior
covariance_mat = matrix(1, 5, 5) * -10000
diag(covariance_mat) <- 40000
prior = list(Mubar = c(10, 10, 60, 60, 60), A=matrix(0.00001), nu=5+3, V = covariance_mat, a=c(1, 1.5, 5), ncomp=3)

mcmc <- list(R=10000, keep=1, nprint=100, LogLike=FALSE)

mixture_result <- rnmixGibbs(list(y=as.matrix(majorcells)), Prior=prior, Mcmc=mcmc)

# calculate posterior mean for cluster mean and covariance matrix
mean1 <- matrix(0, 1, 5)
invchol1 <- matrix(0, 5, 5)

mean2 <- matrix(0, 1, 5)
invchol2 <- matrix(0, 5, 5)

mean3 <- matrix(0, 1, 5)
invchol3 <- matrix(0, 5, 5)

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

# check performance
original_labels <- c(rep('B', 3424), rep('CD4T', 10024), rep('CD8T', 3812))
predicted_labels <- mixture_result$nmix$zdraw[10000,]
label_strings <- c("B", "CD8T", "CD4T")
predicted_labels <- label_strings[predicted_labels]

table(original_labels, predicted_labels)

# visualize performance
majorcells <- as.data.frame(majorcells)
majorcells$pheno <- original_labels
majorcells$pred <- predicted_labels
library(ggplot2)

original_plot <- ggplot(majorcells, aes(x=CD3, y=CD4, color=pheno)) + geom_point(size=0.1) + 
  scale_color_manual(values = c("B" = "black", "CD4T" = "red", "CD8T" = "blue"))+
  ggtitle("Original Label")

new_plot <- ggplot(majorcells, aes(x=CD3, y=CD4, color=pred)) + geom_point(size=0.1) + 
  scale_color_manual(values = c("B" = "black", "CD4T" = "red", "CD8T" = "blue"))+
  ggtitle("Predicted Label")

library(cowplot)
combined_plot <- plot_grid(original_plot, new_plot)


ellipse(mu, sigma, npoints = 200, newplot = TRUE)

# TODO: try visualizing distribution of clusters using 
calculate_ellipse <- function(center, shape, radius, segments){
  # Adapted from https://github.com/tidyverse/ggplot2/blob/master/R/stat-ellipse.R
  chol_decomp <- chol(shape)
  angles <- (0:segments) * 2 * pi/segments
  unit.circle <- cbind(cos(angles), sin(angles))
  ellipse <- t(center + radius * t(unit.circle %*% chol_decomp))
  colnames(ellipse) <- c("CD3","CD4")
  as.data.frame(ellipse)
}

B_ellipse <- 
  
saveRDS(mixture_result, file = file.path(directory, 'BayesM_result.rds'))
