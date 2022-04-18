# summarize the sample trees

rm(list=ls())

testcase1 <- read.table('testdatavar1', header=FALSE, sep='')
testcase3 <- read.table('testdatavar3', header=FALSE, sep='')
colnames(testcase1) <- c("X", "Y", "Label")
colnames(testcase3) <- c("X", "Y", "Label")

tree_ids <- seq(5000, 7000, 20)

source('R/get_cluster.R')

accuracies_1 <- c()
samples_1 <- matrix(0, nrow=160, ncol=length(tree_ids))
for (i in 1:length(tree_ids)){
  id <- tree_ids[i]  
  filename = sprintf('tree_structure1/tree_%d_trimmed.txt', id)
  current_structure <- read.table(filename, header=FALSE, sep='')
  clusters <- find_cluster(current_structure, num_cluster=4)
  sample_labels <- print_labels(clusters)
  samples_1[, i] <- sample_labels
  accuracies_1 <- c(accuracies_1, mean(sample_labels == testcase$Label))
}


accuracies_3 <- c()
samples_3 <- matrix(0, nrow=160, ncol=length(tree_ids))
for (i in 1:length(tree_ids)){
  id <- tree_ids[i]
  filename = sprintf('tree_structure3/tree_%d_trimmed.txt', id)
  current_structure <- read.table(filename, header=FALSE, sep='')
  clusters <- find_cluster(current_structure, num_cluster=4)
  sample_labels <- print_labels(clusters)
  samples_3[, i] <- sample_labels
  accuracies_3 <- c(accuracies_3, mean(sample_labels == testcase$Label))
}


# check out hierarchical clustering result

d1 <- dist(testcase1[,c(1,2)], method = "euclidean")
d3 <- dist(testcase3[,c(1,2)], method = "euclidean")



hc1 <- hclust(d1, method="complete")
hc3 <- hclust(d3, method="complete")


hclabel <- function(tree, num=4){
  agglomerative_result <- cutree(tree, k=num)
  median_index <- foreach(i=1:num, .combine=c) %do%
    median(which(agglomerative_result == i))
  new_label <- rank(median_index)
  final_labels <- new_label[as.factor(agglomerative_result)]
  return(final_labels)
}

hclabel1 <- hclabel(hc1)
hclabel3 <- hclabel(hc3)

mean(hclabel1 == testcase1$Label)
mean(hclabel3 == testcase3$Label)


accuracies1 <- as.data.frame(accuracies_1)
accuracies1$Variance <- "1"
colnames(accuracies1)[1] <- "Accuracy"

accuracies3 <- as.data.frame(accuracies_3)
accuracies3$Variance <- "1/3"
colnames(accuracies3)[1] <- "Accuracy"

accuracies <- rbind(accuracies1, accuracies3)

ggplot(accuracies, aes(x=Variance, y=Accuracy)) + 
  geom_violin() +geom_jitter(shape=16, position=position_jitter(0.2))

library(ggplot2)
testcase1$Label <- as.factor(testcase1$Label)
testcase3$Label <- as.factor(testcase3$Label)
allpoints1 <- ggplot(testcase1, aes(x=X, y=Y)) + geom_point() + theme_bw()
allpoints_colored1 <- ggplot(testcase1, aes(x=X, y=Y, color=Label)) + geom_point() + theme_bw() +
  theme(legend.position = "none")

allpoints3 <- ggplot(testcase3, aes(x=X, y=Y)) + geom_point() + theme_bw()
allpoints_colored3 <- ggplot(testcase3, aes(x=X, y=Y, color=Label)) + geom_point() + theme_bw() +
  theme(legend.position = "none")

library(cowplot)
plot_grid(allpoints1, allpoints_colored1, allpoints3, allpoints_colored3, align="hv")


treesample1 <- cbind(testcase1[,c(1,2)], samples_1[, 35])
colnames(treesample1)[3] <- "Label"
testcase1$Source <- "Real"
treesample1$Source <- "Sample"
df <- rbind(testcase1, treesample1)

ggplot(df, aes(x=X, y=Y, color=Label)) + geom_point()+ 
  facet_grid(~Source) + theme_bw() +theme(legend.position = "none")
  


treesample3 <- cbind(testcase3[,c(1,2)], samples_3[, 13])
colnames(treesample3)[3] <- "Label"
testcase3$Source <- "Real"
treesample3$Source <- "Sample"
df <- rbind(testcase3, treesample3)

ggplot(df, aes(x=X, y=Y, color=Label)) + geom_point()+ 
  facet_grid(~Source) + theme_bw() +theme(legend.position = "none")

