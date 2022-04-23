# check out the likelihood trace


rm(list=ls())
test_liks <- read.table('extdata/test_lik.txt', sep='', header=FALSE)[, 6]
test_lik_avg <- rep(0, 4000)
for (i in 1:4000){
  split_result <- strsplit(test_liks[i], '[+]')
  test_lik_avg[i] <- as.numeric(split_result[[1]][1])
}

test_lik_avg <- unlist(test_lik_avg)
selected_test_lik <- test_lik_avg[seq(2000, 4000, 20)]
acf(selected_test_lik)
plot(1:4000, test_lik_avg, type='l')


cytof_liks <- read.table('extdata/cytof_lik.txt', sep='', header=FALSE)[, 6]
cytof_lik_avg <- rep(0, 4000)
for (i in 1:4000){
  split_result <- strsplit(cytof_liks[i], '[+]')
  cytof_lik_avg[i] <- as.numeric(split_result[[1]][1])
}

cytof_lik_avg <- unlist(cytof_lik_avg)
selected_cytof_lik <- cytof_lik_avg[seq(1500, 4000, 25)]
acf(selected_cytof_lik)
plot(seq(1, 4000), cytof_lik_avg, type='l')
