# Testing new things...

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MAB notes ------------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plots want: 
#   1) High AR & 2) Low AR (probably directly estimate with AR model, to show ? And can show not necessarily stable...)
# probably okay if not smooth !!
#   3) High AR with extremes, + windows
# Do sthg very similar to Janne's for this one, with a smoother plot etc (no beeps, but the thresholds/gray )
# OR MAYBE?? Beeps and then one with window, to show both??


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R packages -----------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(here) # for relative paths within project

library(dplyr) # pre-processing
library(tidyr) # pre-processing
library(ggplot2) # plots
library(patchwork) # combine plots

library(grid) # save plots as EMFs
library(devEMF) # save plots as EMFs


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotting presets------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

col_dashed <- "#edae49"
col_secondary <- "#00798c"
col_bg <- "#5F5F5F"
col_line <- "#69b3a2"

ggtheme_formatting <- list(
  labs(x = "Time", y = "Mental state"),
  theme_bw(),
  theme(axis.text.y = element_blank(), 
           axis.text.x = element_blank(), 
           axis.ticks.x = element_blank(), 
           axis.ticks.y = element_blank(), 
           axis.ticks.length.x = unit(0, "cm"),
           axis.ticks.length.y = unit(0, "cm"),
           plot.margin = unit(c(0,0,0,0), "cm"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
)
  
  



ggline_formatting <- list(size=2, alpha=0.9, linetype=2)

ggtheme_removemargin <- theme(axis.title = element_blank(), 
                              axis.text.y = element_blank(), 
                              axis.text.x = element_blank(), 
                              axis.ticks.x = element_blank(), 
                              axis.ticks.y = element_blank(), 
                              axis.ticks.length.x = unit(0, "cm"),
                              axis.ticks.length.y = unit(0, "cm"),
                              plot.margin = unit(c(0,0,0,0), "cm"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Fig 1: High/Low AR plot-----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Generating data ------------------------------------------------------------

# Set the parameters for the ARIMA process
phi_high <- 0.8 # Autoregressive coefficient
phi_low <- 0.2
n <- 130   # Number of data points

set.seed(245)

# Simulate data from an AR(1) process, both high & low
simAR_high <- arima.sim(model = list(ar = phi_high), n = n) %>%
  as.numeric() %>%
  data.frame(AR_high = .) %>%
  mutate(time = row_number(), .before = AR_high)

simAR_low <- arima.sim(model = list(ar = phi_low), n = n) %>%
  as.numeric() %>%
  data.frame(AR_low = .)

# Joining high & low datasets
simAR <- cbind(simAR_high, simAR_low)


# Make long (for ggplot)
simAR <- simAR %>% 
  pivot_longer(!time, names_to = "AR", values_to = "value" )


### Plotting data ------------------------------------------------------------

# Facet labels
AR_labels <- as_labeller(c(`AR_high` = "High AR (0.8)", `AR_low` = "Low AR (0.2)"))


AR_highlow_plot <- ggplot() +
  geom_line(simAR, mapping = aes(x = time, y = value), color = col_line, alpha = .9) +
  ggtheme_formatting +  # specified above
  scale_x_continuous(expand=c(0,1)) + # so plot starts at/near y-axis 
  facet_wrap(~factor(AR, levels = c('AR_low', 'AR_high')), # Plotting in increasing order
             nrow = 2, 
             labeller = AR_labels) # Labels specified right above

#devEMF::emf(file = here::here("figures", "explanatory", "Fig_AR_highlow.emf"), 
#                              width = 12, height = 8, units = "cm", emfPlus = T, emfPlusFont = TRUE)
AR_highlow_plot
#dev.off()



# Fig 2: Continuous vs discrete ------------------------------------------------------------

### Generating data ------------------------------------------------------------

# Adapted from figures by Janne Adolf, C1 project proposal


#### Time series AR data ------------------------------------------------------------
set.seed(10) 

timeseries <- arima.sim(n = 1000,
                    model = list(ar = {AR <- .99}),
                    sd = sqrt({var_z <- .5})) %>% 
  as.numeric() %>%
  data.frame(y = .) %>%
  mutate(time = row_number(), .before = y)

# Smoothing
timeseries %<>% mutate(y = (loess(y ~ time, span = .05)) %>% .$fitted, y = y/sd(y))

# rescale to make last episode more extreme
timeseries %<>% mutate(y_org = y,
                   scale_y = c(rep(1,600), 1+seq(1, 400)*(0.4/(400))),
                   y = y*scale_y)


#### ID'ing extreme parts of data (for later highlighting) ------------------------------------------------------------

# SD of time series data
sdy <- sd(timeseries$y)

# Data above threshold of 1.5 * SD
timeseries <- timeseries %>%
  mutate(extremes = case_when(
    y > 1.5*sdy ~ 1,
    y < -1.5*sdy ~ 1, 
    .default = 0))

# Getting starts & ends of these regions
# Code adapted from: https://stackoverflow.com/questions/32543176/highlight-areas-within-certain-x-range-in-ggplot2
extreme_regions <- diff(c(0, timeseries$extremes))
start <- which(extreme_regions == 1) 
end <- which(extreme_regions == -1) 
rects <- data.frame(start = start, end = end)

#### Beep times ------------------------------------------------------------

timeseries_beeps <- timeseries %>%
  select(time, y) %>%
  # selecting rows, ~every 10 timepoints
  slice(round(jitter(seq(5, 1000, 30))))


### Plotting data ------------------------------------------------------------

# Simple time series 
Fig_timeseries <- ggplot() +
  geom_line(timeseries, mapping = aes(x = time, y = y), linetype = "solid") +
  ggtheme_formatting + # specified above
  coord_cartesian(ylim = c(-3*sdy, 3*sdy), expand = FALSE) +
  ggtitle("A. Trajectory")
  
# Time series with dots for ESM prompts (signal contingent)
Fig_timeseries_beeps <- Fig_timeseries + 
  geom_point(timeseries_beeps, mapping = aes(x = time, y = y), color = col_secondary) +
  ggtitle("B. Trajectory, with signal-contingent ESM prompts")

# Time series with "extreme" areas 
Fig_timeseries_extremes <- Fig_timeseries + 
    # add dashed line at 1.5 * SD
  geom_hline(yintercept = c(-1.5*sdy, 1.5*sdy), color = col_dashed, linetype = "dashed") +
  geom_rect(data = rects, aes(xmin=start, xmax=end, ymin=-3*sdy, ymax = 3*sdy), fill = col_dashed, alpha = .2) +
  theme(legend.position = "none") +
  ggtitle("C. Extreme ranges (+/- 1.5 SD) of a trajectory")

# Putting figures together (via Patchwork)
Figs_joint <- Fig_timeseries / plot_spacer() / Fig_timeseries_beeps / plot_spacer() / Fig_timeseries_extremes +
  plot_layout(axis_titles = "collect") + plot_layout(heights = c(4, .3, 4, .3, 4))
#+ plot_annotation(tag_levels = 'A') 

# Saving figure
devEMF::emf(file = here::here("figures", "explanatory", "Fig_trajectory_extremes.emf"), 
                              width = 18, height = 12, units = "cm", emfPlus = T, emfPlusFont = TRUE)
Figs_joint
dev.off()

