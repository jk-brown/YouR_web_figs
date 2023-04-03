# build distribution figure for NPP parameter
# 
# Set-up a data frame of NPP values using mean from Hector and the sd defined in matilda 

# What is the NPP value set to in the Hector?

library(hector)
n_core <- newcore(ini_245)

NPP <- fetchvars(n_core, NA, NPP_FLUX0())
NPP

NPP_dat <- data.frame(variable = rep(c("NPP"), each = 2000),
                      value = c(rnorm(100000, mean = 56.2, sd = 14.3)))

ggplot(NPP_dat, aes(x = value)) +
  geom_density(linewidth = 0.7,
               color = "dodgerblue4",
               fill = "lightblue1",
               alpha = 0.3) +
  geom_vline(aes(xintercept = mean(value)),
             color = "darkred",
             linetype = "dashed",
             linewidth = 0.7) +
  scale_y_continuous(expand = c(0,0)) +
  theme_light()
ggsave("npp_distribution.png",
       device = "png",
       path = "figure_output/",
       width = 15,
       height = 12,
       units = c("cm"),
       dpi = 300)
