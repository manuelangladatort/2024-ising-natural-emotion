################################################################################
# Analysis iterated singing experiment
################################################################################

# Structure: (1) global, (2) error, (3) distribution intervals


################################################################################
# GLOBAL PARAMETERS
################################################################################
# import packages
library(tidyverse) # for data manipulation
library(cowplot) # for plots


# import raw data: target melodies and vocal productions (sung melodies)
data_melodies <- read_csv("data/data-sing-scales-v2_full_tonality.csv")

head(data_melodies)
length(unique(data_melodies$participant_id)) # N participants
length(unique(data_melodies$network_id)) # N transmission chains


################################################################################
# ANALALYSIS: ERROR
################################################################################
# calculate the mean interval error over generations
error_data = data_melodies %>% 
  group_by(degree) %>% 
  summarise(
    n = n(), 
    mean_pitch_interval_error = mean(root_mean_squared_interval, na.rm = T),
    sd_pitch_interval_error = sd(root_mean_squared_interval, na.rm = T),
    se_pitch_interval_error = sd_pitch_interval_error/sqrt(n),
    mean_ISI_error = mean(ISI_rms_error, na.rm = T),
    sd_ISI_error = sd(ISI_rms_error, na.rm = T),
    se_ISI_error = sd_ISI_error/sqrt(n)
  )

# plot pitch interval error
plot_pitch_interval_error = error_data %>%
  ggplot(aes(x= degree, y = mean_pitch_interval_error)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_pitch_interval_error - se_pitch_interval_error, 
                  ymax=mean_pitch_interval_error + se_pitch_interval_error),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10, 11)) +
  ylab("Interval error (semitones)") +
  xlab("Iteration") +
  ggtitle("Pitch (interval)") +
  theme_classic() +
  theme(legend.position = "none")

plot_pitch_interval_error


# plot note duration error
plot_ISI_error = error_data %>%
  ggplot(aes(x= degree, y = mean_ISI_error)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_ISI_error - se_ISI_error, 
                  ymax=mean_ISI_error + se_ISI_error),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10, 11)) +
  ylab("IOI error (seconds)") +
  xlab("Iteration") +
  ggtitle("Rhythm (Inter-Onset-Interval)") +
  theme_classic() +
  theme(legend.position = "none")

plot_ISI_error

plot_grid(plot_pitch_interval_error, plot_ISI_error, nrow = 1)


ggsave("evolution_error.png", width = 16, height = 8, units = "cm")


################################################################################
# ANALALYSIS: DISTRIBUTION OF INTERVALS
################################################################################
# parameters for the analysis
NBOOT = 1000 # number of iterations for the KDE
BW = 0.25 # bandwidth for the KDE

# plotting
MAX_INTERVAL_SIZE = 15 # max interval size in semitones
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE) # range of intervals 
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1) # vertical lines indicating integer semitones

# We need to import a custom function to run the KDE (Kernel density estimation) over the distribution of intervals
source("methods/kde_bootstrap_1d.R")


# function to calculate the KDE
make_marginals_kde = function(data, sung_intervals, n_samples, bw,  title, boot_over = "chains"){
  
  # select last 3 iterations + create a long format
  data_last3_long = data %>%
    filter(degree %in% 8:10)  %>%
    select(degree, network_id, sung_interval1:sung_interval4) %>% 
    pivot_longer(sung_interval1:sung_interval4, names_to = "interval", values_to = "sung_intervals")
  
  # Apply KDE bootrapping over transsmision chains to obtain the denbsity distribution of intervals
  data_kde_last3 = kde_bootstrap_1d(data_last3_long, 
                                       100, 
                                       "sung_intervals", 
                                       bw, 
                                       boot_over, 
                                       interval_range
  )
  
  # Plot the distribution of intervals with standard deviations around the average density value.
  # It also draws vertical reference lines at integer interval values.
  plot = data_kde_last3 %>% 
    ggplot(aes(x)) +
    scale_x_continuous(
      breaks = seq(min(vertical.lines), max(vertical.lines), by = 1),
      limits = c(min(vertical.lines), max(vertical.lines))
    ) +
    geom_vline(
      xintercept = vertical.lines,
      colour = "lightgrey", 
      linetype = "dashed"
    ) +
    geom_ribbon(
      aes(ymin = avg - sdev, ymax = avg + sdev),
      fill = "grey",
      alpha = 0.4
    ) +
    geom_line(
      aes(y = avg),
      color = "black",
      size = 0.7
    ) +
    ylab("density") +
    ggtitle("Distribution of intervals") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6),
      title = element_text(size = 8),
      axis.title.x = element_blank()
    )
  
  return(plot)
}


# plot the distribution of intervals
plot_kde_melodies_intervals = make_marginals_kde(data_melodies, c("interval"), NBOOT, BW, "marginals") 

# see
plot_kde_melodies_intervals

# save
ggsave("distribution_intervals.png", width = 14, height = 7, units = "cm")

