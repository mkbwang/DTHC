library(Rcpp)
library(dplyr)

sourceCpp('source/DFTsimulate.cpp')

set.seed(234567)
result <- simulate_DFT(num=5, interval=1e-4, stdev=2)
time_steps <- seq(0, 1, 1e-4)

complete_data <- rbind(time_steps, result$data) %>% t() %>% as.data.frame()
colnames(complete_data) <- c("Time", paste0('X', seq(1,5)))

end_points <- matrix(0, nrow=5, ncol=2)
end_points[, 1] <- 1
end_points[, 2] <- result$data[1:5, 10001]

split_points <- matrix(0, nrow=4, ncol=2)
split_points[, 1] <- result$SplitTime
split_points[, 2] <- result$SplitValue

start_point <- matrix(0, nrow=1, ncol=2)

highlight_points <- as.data.frame(rbind(start_point, split_points, end_points))
colnames(highlight_points) <- c("Time", "Value")
highlight_points$Label <- c("0", "a", "b", "c", "d", "1", "2", "3", "4", "5")


library(ggplot2)

# full trace plot
traces_plot <- ggplot() +  
  geom_line(data=complete_data, aes(x=Time, y = X1), size=0.1) +
  geom_line(data=complete_data, aes(x=Time, y = X2), size=0.1) +
  geom_line(data=complete_data, aes(x=Time, y = X3), size=0.1) +
  geom_line(data=complete_data, aes(x=Time, y = X4), size=0.1) +
  geom_line(data=complete_data, aes(x=Time, y = X5), size=0.1)+
  geom_point(data=highlight_points, aes(x=Time, y=Value), size=1.5, colour="#BD4700")+
  geom_text(data=highlight_points, aes(x=Time, y=Value, label=Label), 
            size=5, nudge_x=0.03, nudge_y=0.03, colour="#BD4700")+
  xlab('Time') +ylab('X') + theme_bw()
 


track1 <- highlight_points[c(1, 4, 3, 2, 6), ]
track2 <- highlight_points[c(1, 4, 3, 2, 5, 7),]
track3 <- highlight_points[c(1, 4, 3, 8), ]
track4 <- highlight_points[c(1, 4, 9), ]
track5 <- highlight_points[c(1, 4, 3, 2, 5, 10), ]

simplified_traces_plot <- ggplot() + 
  geom_line(data=track1, aes(x=Time, y=Value), size=0.7)+
  geom_line(data=track2, aes(x=Time, y=Value), size=0.7)+
  geom_line(data=track3, aes(x=Time, y=Value), size=0.7)+
  geom_line(data=track4, aes(x=Time, y=Value), size=0.7)+
  geom_line(data=track5, aes(x=Time, y=Value), size=0.7)+
  geom_point(data=highlight_points, aes(x=Time, y=Value), size=1.5, colour="#BD4700")+
  geom_text(data=highlight_points, aes(x=Time, y=Value, label=Label), 
            size=5, nudge_x=0.03, nudge_y=0.03, colour="#BD4700")+
  xlab('Time') +ylab('X')+ theme_bw()



library(cowplot)

plot_grid(traces_plot, simplified_traces_plot, labels = "AUTO",
          ncol=1)

