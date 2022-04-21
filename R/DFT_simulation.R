library(Rcpp)
library(dplyr)

sourceCpp('source/DFTsimulate.cpp')

result <- simulate_DFT(num=5, interval=1e-4, stdev=1)
time_steps <- seq(0, 1, 1e-4)

complete_data <- rbind(time_steps, result$data) %>% t() %>% as.data.frame()
colnames(complete_data) <- c("Time", paste0('X', seq(1,6)))

library(ggplot2)

traces_plot <- ggplot(complete_data, aes(Time)) +  
  geom_line(aes(y = X1)) +
  geom_line(aes(y = X2)) +
  geom_line(aes(y = X3)) +
  geom_line(aes(y = X4)) +
  geom_line(aes(y = X5))+
  geom_line(aes(y = X6)) +
  xlab('Time') 
  

