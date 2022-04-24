# check out the likelihood trace


rm(list=ls())
test_liks <- read.table('extdata/test_lik.txt', sep='', header=FALSE)[, 6]
test_lik_avg <- rep(0, 4000)
for (i in 1:4000){
  split_result <- strsplit(test_liks[i], '[+]')
  test_lik_avg[i] <- as.numeric(split_result[[1]][1])
}

test_lik_avg <- unlist(test_lik_avg)
selected_test_lik <- test_lik_avg[seq(1500, 4000, 25)]
acf(selected_test_lik)
# plot(1:4000, test_lik_avg, type='l')


cytof_liks <- read.table('extdata/cytof_lik.txt', sep='', header=FALSE)[, 6]
cytof_lik_avg <- rep(0, 4000)
for (i in 1:4000){
  split_result <- strsplit(cytof_liks[i], '[+]')
  cytof_lik_avg[i] <- as.numeric(split_result[[1]][1])
}

cytof_lik_avg <- unlist(cytof_lik_avg)
selected_cytof_lik <- cytof_lik_avg[seq(1500, 4000, 25)]

# acf(cytof_lik_avg)
acf(selected_cytof_lik)
# plot(seq(1, 4000), cytof_lik_avg, type='l')


# check out the actual labels

testdata <- read.table('extdata/testdata', sep='', header=FALSE)
cytofdata <- read.table('extdata/CyTOFdata', sep='', header=FALSE)

test_features <- testdata[,c(1,2)]
test_reallabels <- testdata[, 3]

cytof_features <- cytofdata[, c(1,2,3)]
cytof_reallabels <- cytofdata[, 4]

# hierarchical clustering using complete linkage

dist_mat <- dist(test_features, method="euclidean")
hierclust_result <- hclust(dist_mat, method = 'complete')
test_hclust <- cutree(hierclust_result, k=4)

dist_mat <- dist(cytof_features, method="euclidean")
hierclust_result <- hclust(dist_mat, method = 'complete')
cytof_hclust <- cutree(hierclust_result, k=3)


# hierarchical clustering from diffusion trees

source('R/get_cluster.R')


## simulated data
tree_ids <- seq(1500, 4000, 25)
test_tree_samples <- matrix(0, nrow=160, ncol=length(tree_ids))
for (i in 1:length(tree_ids)){
  id <- tree_ids[i] 
  filename = sprintf('extdata/test_tree_structure/tree_%d_trimmed.txt', id)
  current_structure <- read.table(filename, header=FALSE, sep='')
  clusters <- find_cluster(current_structure, num_cluster=4)
  labels <- rep(0, 160)
  for (j in 1:4){
    labels[clusters[[j]]] <- j
  }
  test_tree_samples[, i] <- labels
}

rand_test_real_hclust <- adjustedRandIndex(test_reallabels, test_hclust)
rand_test_DFT_real <- rep(0, length(tree_ids))
rand_test_DFT_hclust <- rep(0, length(tree_ids))
for (i in 1:length(tree_ids)){
  rand_DFT_real[i] <- adjustedRandIndex(test_reallabels, test_tree_samples[, i])
  rand_test_DFT_hclust[i] <- adjustedRandIndex(test_hclust, test_tree_samples[, i])
}



## simulated data

cytof_tree_samples <- matrix(0, nrow=796, ncol=length(tree_ids))
for (i in 1:length(tree_ids)){
  id <- tree_ids[i] 
  filename = sprintf('extdata/cytof_tree_structure/tree_%d_trimmed.txt', id)
  current_structure <- read.table(filename, header=FALSE, sep='')
  clusters <- find_cluster(current_structure, num_cluster=3)
  labels <- rep(0, 796)
  for (j in 1:3){
    labels[clusters[[j]]] <- j
  }
  cytof_tree_samples[, i] <- labels
}

rand_cytof_real_hclust <- adjustedRandIndex(cytof_reallabels, cytof_hclust)
rand_cytof_DFT_real <- rep(0, length(tree_ids))
rand_cytof_hclust_DFT <- rep(0, length(tree_ids))
for (i in 1:length(tree_ids)){
  rand_cytof_DFT_real[i] <- adjustedRandIndex(cytof_reallabels, cytof_tree_samples[, i])
  rand_cytof_hclust_DFT[i] <- adjustedRandIndex(cytof_hclust, cytof_tree_samples[, i])  
}

