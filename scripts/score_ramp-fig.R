# building figure to showcase score ramp
# what do I need:
# scores - don't need to be from an actual analysis
# abs diffs - absolute differences of x and y values.

set.seed(1)
data <- data.frame(x = runif(45, min = 1, max = 20),
                   y = runif(45, min = 1, max = 20))
data$Differences <- abs(data$x - data$y)

data$Scores <- score_ramp(data$x, data$y, w1 = 5, w2 = 10)

score_ramp <- ggplot(data = data, aes(x = Differences, y = Scores)) +
  geom_line(color = "lightblue",
            linewidth = 1) +
  geom_point(color = "black",
             shape = 1,
             size = 3) +
  theme_light() +
  ggtitle("Example of Score Ramp 
w1 = 5 and w2 = 10")
score_ramp
ggsave("score_ramp-plot.png",
       device = "png",
       path = "figure_output/",
       width = 10,
       height = 8,
       units = c("cm"),
       dpi = 300)
