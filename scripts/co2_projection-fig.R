# building a figure showing the basic function - entire simulation?
# what do I need:
# Run a hector iteration simulations of CO2 into future.
# Show example of probabilistic output for temperature anomaly.
# Use code from vignette to make is easy on yourself. 

#' Scored Hector runs
#'
#' @param inifile a path to ini file of emissions pathway
#' @param draws number of random draws to generate model parameters
#' @param metric a metric defining data to filter from Hector output
#' @param crit scoring criterion to use
#' @param ssp_name An optional name to identify the core (string)
#'
#' @return Hector results with added column scoring each run.
scored_hector_runs <- function(inifile, ssp_name, draws, crit) {
  # initiate and core
  core = newcore(inifile, name = ssp_name)
  # generate parameters
  params = generate_params(core, draws)
  # running Hector
  h_result = iterate_hector(core, params)
  # score Hector runs 
  scores = score_hruns(h_result, crit, score_ramp, w1 = 2, w2 = 20)
  # merge scores with Hector Results
  scored_hector = merge(h_result, scores, "run_number")
  # return
  return(scored_hector)  
}

# Establishing ini files for each scenario
ini_126 <- system.file("input/hector_ssp126.ini", package = "hector")
ini_245 <- system.file("input/hector_ssp245.ini", package = "hector")
ini_370 <- system.file("input/hector_ssp370.ini", package = "hector")
ini_585 <- system.file("input/hector_ssp585.ini", package = "hector")

# Running hector for each scenario
start <- Sys.time()
set.seed(2)
hector_126 <- scored_hector_runs(ini_126, ssp_name = "ssp126", 50, criterion_co2_obs())
hector_245 <- scored_hector_runs(ini_245, ssp_name = "ssp245", 50, criterion_co2_obs())
hector_370 <- scored_hector_runs(ini_370, ssp_name = "ssp370", 50, criterion_co2_obs())
hector_585 <- scored_hector_runs(ini_585, ssp_name = "ssp585", 50, criterion_co2_obs())
print( Sys.time() - start )
# Merging hector results for plotting
hector_merge <- rbind(hector_126,
                      hector_245,
                      hector_370,
                      hector_585)


# Plotting CO2 projections
plot_co2 <- ggplot(subset(hector_merge,
                          year > 1990 & year < 2100
                          & variable == CONCENTRATIONS_CO2())) +
  geom_line(aes(x = year, y = value,
                group = run_number,
                color = scores, 
                alpha = scores),
            linewidth = 1) +
  scale_color_gradient(high = "dodgerblue4", low = "lightblue1") +
  scale_alpha_continuous(range = c(0.1, 1)) +
  geom_line(data = matilda:::metricdata_co2,
            aes(year, co2_ppm),
            color = "red",
            linewidth = 1) +
  facet_wrap(~variable + scenario) +
  ylab(expression(CO[2]~Concentration~(ppm))) +
  theme_light() +
  guides(alpha = "none") +
  ggtitle(expression("CO"[2]~"concentration projections 1990 to 2100"))
plot_co2
ggsave("co2_projection.png",
       device = "png",
       path = "figure_output/",
       width = 15,
       height = 12,
       units = c("cm"),
       dpi = 300)
