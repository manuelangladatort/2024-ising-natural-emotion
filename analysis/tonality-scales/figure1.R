################################################################################
# Iterated singing (tonality & scales)
# Script: figure 1
################################################################################
# TODO: implement distribution of IOIs
# TODO: implement entropy og pitch and IOIs


# import 
library(tidyverse)
library(egg)
library(ggpubr)
library(cowplot)

source("utils/plots.R")
source("utils/features.R")


# global 
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 13
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

# data
data_melodies <- read_csv("data/data-clean/sing-scales-v2/data-sing-scales-v2_full.csv")

length(table(data_melodies$participant_id))
length(table(data_melodies$network_id))
length(table(data_melodies$degree))


################################################################################
# Distribution of intervals
################################################################################
NBOOT = 1000
BW = 0.25

# marginals intervals
data_melodies_long_intervals =  data_melodies %>% 
  select(id:participant_id, sung_interval1:sung_interval4) %>%
  pivot_longer(cols = starts_with("sung_interval"),
               names_to = "interval_pos",
               values_to = "interval")

marginals_melodies_intervals = make_marginals_kde(data_melodies_long_intervals, c("interval"), NBOOT, BW, "marginals") 

marginals_melodies_intervals

ggsave("results/figure1/marginals_intervals.png", width = 14, height = 7, units = "cm")


# separate
marginals_melodies_intervals_separate = make_marginals_kde_separate_4ints(
  data_melodies, list_4int_sung.intervals, NBOOT, BW, "separate marginals"
)

marginals_melodies_intervals_separate

# ggsave("results/figure1/marginals_intervals_separate_column.png", width = 12, height = 15, units = "cm")
ggsave("results/figure1/marginals_intervals_separate_row.png", width = 35, height = 5, units = "cm")


################################################################################
# Distribution of IOIs
################################################################################
# TODO: make sure analysis is correct

NBOOT = 100
BW = 0.005
interval_range = c(0, 0.5)
vertical.lines = c(0.17, 0.25, 0.33)

# marginals IOIs
data_melodies_IOIs = data_melodies %>% 
  # select(id:participant_id, sung_IOI1:sung_IOI4) %>%
  mutate(TOT = as.numeric(sung_IOI1) + as.numeric(sung_IOI2) + as.numeric(sung_IOI3) + as.numeric(sung_IOI4)) %>% 
  mutate(
    sung_IOI1 = as.numeric(sung_IOI1)/ TOT,
    sung_IOI2 = as.numeric(sung_IOI2)/ TOT,
    sung_IOI3 = as.numeric(sung_IOI3)/ TOT,
    sung_IOI4 = as.numeric(sung_IOI4)/ TOT
  ) 

data_melodies_long_IOIs = data_melodies_IOIs %>% 
  select(id:participant_id, sung_IOI1:sung_IOI4) %>%
  pivot_longer(cols = starts_with("sung_IOI"),
               names_to = "interval_pos",
               values_to = "interval") %>% 
    mutate(interval = as.numeric(interval)) %>% 
  drop_na(interval)

marginals_melodies_IOIs = make_marginals_IOI_kde(data_melodies_long_IOIs, c("interval"), NBOOT, BW, "marginals") 

marginals_melodies_IOIs

ggsave("results/figure1/marginals_IOIs_separate_row.png", width = 14, height = 7, units = "cm")


# marginals IOI ratios
NBOOT = 1000
BW = 0.025
interval_range = c(0, 3.5)

#  1:4, 1:2, 1:1, 2:1 and 4:1
vertical.lines = c(0.25, 0.5, 1, 1.25, 1.5)

data_melodies_ratios <- data_melodies_IOIs %>% 
  # select(id:participant_id, sung_IOI1:sung_IOI4) %>% 
  mutate(
    ratio_1_2 = as.numeric(sung_IOI1) / as.numeric(sung_IOI2),
    ratio_2_3 = as.numeric(sung_IOI2) / as.numeric(sung_IOI3),
    ratio_3_4 = as.numeric(sung_IOI3) / as.numeric(sung_IOI4)
  )

data_melodies_ratios_long_ratios = data_melodies_ratios %>% 
  select(id:participant_id, ratio_1_2:ratio_3_4) %>%
  pivot_longer(cols = starts_with("ratio_"),
               names_to = "interval_pos",
               values_to = "interval") %>% 
  mutate(interval = as.numeric(interval)) %>% 
  drop_na(interval)

marginals_melodies_IOIratios = make_marginals_IOI_kde(data_melodies_ratios_long_ratios, c("interval"), NBOOT, BW, "marginals") 

marginals_melodies_IOIratios

ggsave("results/figure1/marginals_IOIratios_separate_row.png", width = 14, height = 7, units = "cm")

################################################################################
# Trends: Error & Entropy (bootstrapping)
################################################################################
NBOOT = 100
vars_sung_intervals = c("sung_interval1", "sung_interval2", "sung_interval3", "sung_interval4")
vars_sung_IOIs = c("sung_IOI1", "sung_IOI2", "sung_IOI3", "sung_IOI4")
vars_sung_IOIratios = c("ratio_1_2", "ratio_2_3", "ratio_3_4")

# get error and entropy via bootstrap
data_melodies_features_boot = get_bootstrapped_features_melody(
  data_melodies_ratios, 
  vars_sung_intervals,
  vars_sung_IOIratios,
    NBOOT
    )


data_features_aggregate = data_melodies_features_boot %>% 
  group_by(degree) %>%
  summarise(
    n = n(),
    m_interval_error = mean(mean_interval_error),
    sd_error = sd(mean_interval_error, na.rm = T),
    m_ISI_error = mean(mean_ISI_error),
    sd_ISI_error = sd(mean_ISI_error, na.rm = T),
    m_entropy_interval = mean(entropy_interval),
    sd_entropy_interval = sd(entropy_interval, na.rm = T),
    m_entropy_IOI = mean(entropy_IOI),
    sd_entropy_IOI = sd(entropy_IOI, na.rm = T)
  ) 

# write_csv(data_features_aggregate, "data_features_aggregate.csv")

# error
data_error_aggregate_long <- data_features_aggregate %>%
  pivot_longer(cols = c(m_interval_error, m_ISI_error), 
               names_to = "Metric", 
               values_to = "Error") %>%
  mutate(SD = ifelse(Metric == "m_interval_error", sd_error, sd_ISI_error),
         ymin = Error - SD,
         ymax = Error + SD)

# Plot with facet_wrap
plot_error <- ggplot(data_error_aggregate_long, aes(x = degree, y = Error)) +
  geom_line(aes(color = Metric), size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Metric), alpha = 0.2) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "Degree", y = "Error") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10, 11)) +
  theme_classic() +
  scale_color_manual(values = c("m_interval_error" = "blue", "m_ISI_error" = "red")) +
  scale_fill_manual(values = c("m_interval_error" = "blue", "m_ISI_error" = "red")) +
  theme(legend.position = "none")

plot_error

ggsave("results/figure1/error2.png", width = 11, height = 6,  units = "cm", bg = "white")


# error
data_entropy_aggregate_long <- data_features_aggregate %>%
  pivot_longer(cols = c(m_entropy_interval, m_entropy_IOI), 
               names_to = "Metric", 
               values_to = "Error") %>%
  mutate(SD = ifelse(Metric == "m_entropy_interval", sd_entropy_interval, sd_entropy_IOI),
         ymin = Error - SD,
         ymax = Error + SD)


# Plot with facet_wrap
plot_entropy <- ggplot(data_entropy_aggregate_long, aes(x = degree, y = Error)) +
  geom_line(aes(color = Metric), size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Metric), alpha = 0.2) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "Degree", y = "Error") +
  theme_classic() +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10, 11)) +
  scale_color_manual(values = c("m_entropy_interval" = "blue", "m_entropy_IOI" = "red")) +
  scale_fill_manual(values = c("m_entropy_interval" = "blue", "m_entropy_IOI" = "red")) +
  theme(legend.position = "none")

plot_entropy

ggsave("results/figure1/entropy2.png", width = 11, height = 6, units = "cm", bg = "white")

################################################################################
# Normalized
################################################################################
# error
data_error_norm <- data_features_aggregate %>%
  filter(degree != 0) %>% 
  mutate(
    z_pitch_interval_error = (m_interval_error - mean(m_interval_error)) / sd(m_interval_error),
    z_pitch_interval_se = sd_error / sd(m_interval_error),
    z_ISI_error = (m_ISI_error - mean(m_ISI_error)) / sd(m_ISI_error),
    z_ISI_se = sd_ISI_error / sd(m_ISI_error)
  )

data_error_norm1 <- data_error_norm %>%
  mutate(
    z_pitch_interval_error = z_pitch_interval_error - z_pitch_interval_error[degree==1][1L],
    z_ISI_error = z_ISI_error - z_ISI_error[degree==1][1L]
  )

# Reshape the data to long format for easier plotting
error_data_long <- data_error_norm1 %>%
  select(degree, z_pitch_interval_error, z_pitch_interval_se, z_ISI_error, z_ISI_se) %>%
  pivot_longer(
    cols = c(z_pitch_interval_error, z_ISI_error),
    names_to = "error_type",
    values_to = "z_error"
  ) %>%
  mutate(
    z_se = ifelse(error_type == "z_pitch_interval_error", z_pitch_interval_se, z_ISI_se)
  )

combined_plot <- error_data_long %>%
  ggplot(aes(x = degree, y = z_error, group = error_type, fill = error_type)) +
  geom_line() +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = z_error - z_se, ymax = z_error + z_se), alpha = 0.3) +
  labs(
    x = "Iteration",
    y = "Normalized Copying Error (z-score)",
    color = "Error Type",
    fill = "Error Type",
  ) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10, 11)) +
  scale_color_manual(
    values = c("z_pitch_interval_error" = "blue", "z_ISI_error" = "red"),
    labels = c("Pitch Interval", "Inter-Onset-Interval")
  ) +
  scale_fill_manual(
    values = c("z_pitch_interval_error" = "blue", "z_ISI_error" = "red"),
    labels = c("Pitch Interval", "Inter-Onset-Interval")
  ) +
  theme_classic()

combined_plot

ggsave("results/figure1/evolution_error_norm.png", width = 12, height = 8, units = "cm")


# entropy
data_entropy_norm <- data_features_aggregate %>%
  mutate(
    z_interval_entropy = (m_entropy_interval - mean(m_entropy_interval)) / sd(m_entropy_interval),
    z_interval_entropy_se = sd_entropy_interval / sd(m_entropy_interval),
    z_IOI_entropy = (m_entropy_IOI - mean(m_entropy_IOI)) / sd(m_entropy_IOI),
    z_IOI_entropy_se = sd_entropy_IOI / sd(m_entropy_IOI)
  )

data_entropy_norm1 <- data_entropy_norm %>%
  mutate(
    z_interval_entropy = z_interval_entropy - z_interval_entropy[degree==0][1L],
    z_IOI_entropy = z_IOI_entropy - z_IOI_entropy[degree==0][1L]
  )

# Reshape the data to long format for easier plotting
entropy_data_long <- data_entropy_norm1 %>%
  select(degree, z_interval_entropy, z_interval_entropy_se, z_IOI_entropy, z_IOI_entropy_se) %>%
  pivot_longer(
    cols = c(z_interval_entropy, z_IOI_entropy),
    names_to = "error_type",
    values_to = "z_error"
  ) %>%
  mutate(
    z_se = ifelse(error_type == "z_interval_entropy", z_interval_entropy_se, z_IOI_entropy_se)
  )

combined_plot_entropy <- entropy_data_long %>%
  ggplot(aes(x = degree, y = z_error, group = error_type, fill = error_type)) +
  geom_line() +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = z_error - z_se, ymax = z_error + z_se), alpha = 0.3) +
  labs(
    x = "Iteration",
    y = "Normalized Copying Error (z-score)",
    color = "Error Type",
    fill = "Error Type",
  ) +
  scale_x_continuous(breaks=c(0, 1,2,3,4,5,6,7,8,9,10, 11)) +
  scale_color_manual(
    values = c("z_interval_entropy" = "blue", "z_IOI_entropy" = "red"),
    labels = c("Pitch Interval", "Inter-Onset-Interval")
  ) +
  scale_fill_manual(
    values = c("z_interval_entropy" = "blue", "z_IOI_entropy" = "red"),
    labels = c("Pitch Interval", "Inter-Onset-Interval")
  ) +
  theme_classic()

combined_plot_entropy

ggsave("results/figure1/evolution_entropy_norm.png", width = 12, height = 8, units = "cm")

