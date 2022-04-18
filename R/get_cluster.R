# get cluster information from the tree structure output
library(dplyr)
library(foreach)

trackdown <- function(structure, nodeid){
  if (nodeid > 0){
    return(nodeid)
  } else {
    parentvec <- structure$Parent
    targetrow <- which(parentvec == nodeid)
    child1 <- structure$Child1[targetrow]
    child2 <- structure$Child2[targetrow]
    descendents1 <- trackdown(structure, child1)
    descendents2 <- trackdown(structure, child2)
    return(c(descendents1, descendents2))
  }
}


find_cluster <- function(structure, num_cluster = 4){
  
  colnames(structure) <- c("Parent", "Child1", "Child2", "Descendent", "Time")
  sorted_structure <- structure %>% arrange(Time) # sort based on time
  childrennodes <- c(sorted_structure$Child1[1:(num_cluster - 1)],
                     sorted_structure$Child2[1:(num_cluster - 1)])
  parentnodes <- sorted_structure$Parent[1:(num_cluster - 1)]
  nodes_of_interest <- setdiff(childrennodes, parentnodes)
  
  result <- list()
  for (j in nodes_of_interest){
    descendents <- trackdown(sorted_structure, j)
    result <- append(result, list(descendents))
  }
  return(result)
}


print_labels <- function(clusters){

  num_clusters <- length(clusters)
  
  # individual ID
  indvs <- foreach(i=1:num_clusters, .combine=c) %do% clusters[[i]]
  
  # median value
  mean_values <- foreach(i=1:num_clusters, .combine=c) %do% mean(clusters[[i]])
  
  
  distinct_labels <- rank(mean_values)
  complete_labels <- foreach(i=1:num_clusters, .combine=c) %do% 
    rep(distinct_labels[i], length(clusters[[i]]))
  
  outcome <- cbind(indvs, complete_labels)
  return(outcome[order(outcome[, 1]), 2])
}

