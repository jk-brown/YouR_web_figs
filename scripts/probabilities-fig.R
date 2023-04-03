#' Calculating probabilities 
#'
#' @param h_result A hector result
#' @param metric A metric to filter results of interest
#' @param criterionA criterion used to score Hector runs
#' @param bins Bins for computing probabilities - defaults for global_tas
#'
#' @return A data frame of probabilities 
probabilities <- function(h_result,
                          metric,
                          crit,
                          bins = c(1, 1.5, 2, 3, 4, Inf)) {
  # calculating metrics
  metrics = metric_calc(h_result, metric)
  # calculating scores
  scores = score_hruns(h_result, crit, score_ramp, w1 = 2, w2 = 20)
  # merging metrics and scores
  metric_scores = merge(metrics, scores, by = "run_number")
  # calculating probability
  probability = prob_calc(metric_scores$metric_result, bins,
                          metric_scores$scores)
  # coercing probability to data frame for plotting
  probs = as.data.frame(probability)
  # return
  return(probs)
}

# defining metric of mean global_tas for 1990:2100
metric_global_tas <- new_metric(GLOBAL_TAS(), years = 1990:2100, op = mean)

# Calculating probabilities for each hector run
probs_126 <- probabilities(hector_126, metric_global_tas, criterion_co2_obs())
probs_126$scenario <- rep("SSP1-2.6")
probs_245 <- probabilities(hector_245, metric_global_tas, criterion_co2_obs())
probs_245$scenario <- rep("SSP2-4.5")
probs_370 <- probabilities(hector_370, metric_global_tas, criterion_co2_obs())
probs_370$scenario <- rep("SSP3-7.0")
probs_585 <- probabilities(hector_585, metric_global_tas, criterion_co2_obs())
probs_585$scenario <- rep("SSP5-8.5")

#combining dfs with scenario types and probs
results_all <- rbind(probs_126,
                     probs_245,
                     probs_370,
                     probs_585)
colnames(results_all) <- c("Warming", "Score" ,"Probability", "Scenario")

# Plotting probabilities as stacked bar graph
ggplot(results_all, aes(fill = Warming, y = Probability, x = Scenario)) +
  geom_bar(position = position_fill(reverse = T), 
           stat = "identity",
           width = 0.6) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.1)) +
  scale_fill_manual(values = c("dark grey", "light coral", "dark red", "black"),
                    labels = c("1 to 1.5 C", "1.5 to 2 C", "2 to 3 C", "3 to 4 C")) +
  coord_flip() +
  theme_light()
