library(dplyr)
rm(list=ls())
directory = "~/UM/Research/Wen_Lab/data"
alldata = readRDS(file.path(directory, 'singlecell_sheet.rds'))

# all proteins
colnames(alldata)

# count cell type counts
table(alldata$Phenotype)

# select samples
set.seed(2022)

NKcells <- alldata %>% dplyr::filter(Phenotype %in% c("NK")) %>% dplyr::slice_sample(prop=0.008) %>%
  dplyr::select(CD3, CD8, CD49b) %>% as.data.frame()
NKcells$Type <- "NK"

CD8T <- alldata %>% dplyr::filter(Phenotype %in% c("CD8+")) %>% dplyr::slice_sample(prop=0.0005) %>%
  dplyr::select(CD3, CD8, CD49b) %>% as.data.frame()
CD8T$Type <- "CD8T"

NKT <- alldata %>% dplyr::filter(Phenotype %in% c("NKT")) %>% dplyr::slice_sample(prop=0.008) %>%
  dplyr::select(CD3, CD8, CD49b) %>% as.data.frame()
NKT$Type <- "NKT"

sample_data <- rbind(CD8T, NKcells, NKT)

quantile(sample_data$CD3, 0.995)
quantile(sample_data$CD8, 0.995)
quantile(sample_data$CD49b, 0.995)

# scale the samples
scaled_sample <- sample_data
scaled_sample$CD3 <- scaled_sample$CD3 / quantile(sample_data$CD3, 0.995)
scaled_sample$CD3[scaled_sample$CD3 > 1] = 1
scaled_sample$CD8 <- scaled_sample$CD8 / quantile(sample_data$CD8, 0.995)
scaled_sample$CD8[scaled_sample$CD8 > 1] = 1
scaled_sample$CD49b <- scaled_sample$CD49b / quantile(sample_data$CD49b, 0.995)
scaled_sample$CD49b[scaled_sample$CD49b > 1] = 1

# normalize them to have mean zero and standard deviation of 1
normalized_sample <- scaled_sample %>% mutate_at(c("CD3", "CD8", "CD49b"), ~(scale(.) %>% as.vector))
normalized_sample$Type <- as.numeric(as.factor(normalized_sample$Type))

library(ggplot2)
biplot1 <- ggplot(normalized_sample, aes(x=CD3, y=CD8, color=Type)) + 
  geom_jitter(height=0.02, width=0.02, size=0.7)
biplot2 <- ggplot(normalized_sample, aes(x=CD3, y=CD49b, color=Type)) + 
  geom_jitter(height=0.02, width=0.02, size=0.7)
biplot3 <- ggplot(normalized_sample, aes(x=CD8, y=CD49b, color=Type)) + 
  geom_jitter(height=0.02, width=0.02, size=0.7)

library(cowplot)

plot_grid(biplot1, biplot2, biplot3, align="v", ncol=1)

# hierarchical clustering
expressions <- normalized_sample[, c(1,2,3)] %>% as.matrix()
dist_mat <- dist(expressions, method="euclidean")

hierclust_result <- hclust(dist_mat, method = 'complete')


sample_data$hierclust <- cutree(hierclust_result, k=3)
sample_data %>% group_by(hierclust) %>% summarise(meanCD3 = mean(CD3),
                                                  meanCD8 = mean(CD8),
                                                  meanCD49 = mean(CD49b))


normalized_expmat <- normalized_sample %>% as.matrix() %>%  round(digits=2)


write.table(normalized_expmat, file='extdata/CyTOFdata', sep='\t', col.names = FALSE, row.names=FALSE)


