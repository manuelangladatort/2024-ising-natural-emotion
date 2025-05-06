################################################################################
# Iterated singing (tonality & scales)
# Script: figure 1 - Emergence of melodic and rhythmic structures increase learnability. 
################################################################################
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

# data
data_melodies <- read_csv("data/final-data/sing-scales-v2/data-sing-scales-v2_full.csv")

length(table(data_melodies$participant_id))
length(table(data_melodies$network_id))
length(table(data_melodies$degree))

table(data_melodies$degree)


################################################################################
# Distribution of pitch intervals
################################################################################
NBOOT = 1000
BW = 0.25
MAX_INTERVAL_SIZE = 13
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)


# pitch classess
data_melodies_pc = data_melodies %>% 
  select(id, degree, network_id, vars_sung_pitches) %>% 
  mutate(row_id = row_number()) %>% 
  pivot_longer(vars_sung_pitches, names_to = "index", values_to = "pitch") %>% 
  # convert ti pitch class
  mutate(pc = round(pitch) %% 12)

ggplot(data_melodies_pc, aes(x = pc)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Estimated Key", y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ degree)

ggsave("results/figure1/marginals_pitch_classess_degree.png", 
       width = 20, height = 15, units = "cm", bg = "white")


# pitch intervals
data_melodies_interval = data_melodies %>% 
  select(id, degree, network_id, vars_sung_intervals) %>% 
  mutate(row_id = row_number()) %>% 
  pivot_longer(vars_sung_intervals, names_to = "index", values_to = "interval") %>% 
  # convert ti pitch class
  mutate(interval = round(interval))

ggplot(data_melodies_interval, aes(x = interval)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Estimated Key", y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ degree)

ggsave("results/figure1/marginals_pitch_intervals_degree.png", 
       width = 20, height = 15, units = "cm", bg = "white")


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

ggsave("results/figure1/marginals_intervals_separate_column.png", width = 12, height = 15, units = "cm")


# evolution
BW = 0.25
vertical.lines = c(-12, -5, 0,  5, 12)

marginals_intervals_evolution = data_melodies_long_intervals %>% 
  # select few iterations for visualization
  filter(degree == 0 | degree == 2 | degree == 4 | degree == 6 | degree == 8 | degree == 10) %>%
  ggplot(aes(x = interval)) +
  geom_density(bw=BW)  +
  scale_x_continuous(breaks=vertical.lines,
                     limits = c(min(-13), max(13)))  + 
  geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") + 
  facet_wrap(~ degree, nrow = 1) +
  theme(axis.text.x = element_text(size=7), 
        axis.text.y=element_text(size=7), 
        axis.title.x = element_blank(), 
        axis.title.y =element_blank())
  
marginals_intervals_evolution

ggsave("results/figure1/evolution_marginals_intervals.png", width = 20, height = 4, units = "cm")


################################################################################
# Distribution of IOIs
################################################################################
NBOOT = 1000
BW = 0.005
interval_range = c(0.2, 0.8)
vertical.lines = c(0.2, 0.333, 0.5, 0.666, 0.8)
vertical.labels <- c("1/5", "1/3", "1/2", "2/3", "4/5")

# marginals of ratios between adjacent IOIs
data_melodies_ratios = data_melodies %>% 
  select(id:participant_id, sung_pitch1:sung_pitch5, 
         sung_IOI1:sung_IOI4, sung_interval1:sung_interval4,
         target_IOI1:target_IOI4,root_mean_squared_interval, ISI_rms_error) %>% 
  mutate(
    sung_ratio1 = sung_IOI1/(sung_IOI1+sung_IOI2),
    sung_ratio2 = sung_IOI2/(sung_IOI2+sung_IOI3),
    sung_ratio3 = sung_IOI3/(sung_IOI3+sung_IOI4),
    target_ratio1 = target_IOI1/(target_IOI1+target_IOI2),
    target_ratio2 = target_IOI2/(target_IOI2+target_IOI3),
    target_ratio3 = target_IOI3/(target_IOI3+target_IOI4)
  )

data_melodies_ratios_long = data_melodies_ratios %>% 
  select(id:participant_id, sung_ratio1:sung_ratio3) %>%
  pivot_longer(cols = starts_with("sung_ratio"),
               names_to = "ratio_pos",
               values_to = "ratio")

marginals_melodies_ratio_IOIs = make_marginals_ratio_IOI_kde(data_melodies_ratios_long, c("ratio"), NBOOT, BW, "marginals") 

marginals_melodies_ratio_IOIs

ggsave("results/figure1/marginals_IOIs_separate_row.png", width = 14, height = 7, units = "cm")


# separate
vars_sung_IOIratios <- c("sung_ratio1", "sung_ratio2", "sung_ratio3")

marginals_melodies_ratio_IOI_separate = make_marginals_kde_separate_3ints(
  data_melodies_ratios, vars_sung_IOIratios, NBOOT, BW, "separate marginals"
)

marginals_melodies_ratio_IOI_separate

ggsave("results/figure1/marginals_melodies_ratio_IOI_separate_column.png", width = 12, height = 15, units = "cm")


# evolution
BW = 0.005
vertical.lines = c(0.333, 0.5, 0.666)
vertical.labels <- c("1/3", "1/2", "2/3")

marginals_ratios_evolution = data_melodies_ratios_long %>% 
  # select few iterations for visualization
  filter(degree == 0 | degree == 2 | degree == 4 | degree == 6 | degree == 8 | degree == 10) %>%
  ggplot(aes(x = ratio)) +
  geom_density(bw=BW)  +
  scale_x_continuous(breaks = vertical.lines,
                     labels = vertical.labels,
                     limits = c(min(0.2), max(0.8)))  + 
  geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") + 
  facet_wrap(~ degree, nrow = 1) +
  theme(axis.text.x = element_text(size=7), 
        axis.text.y=element_text(size=7), 
        axis.title.x = element_blank(), 
        axis.title.y =element_blank())

marginals_ratios_evolution

ggsave("results/figure1/evolution_marginals_ratios.png", width = 20, height = 4, units = "cm")


################################################################################
# Trends: Error & Entropy (bootstrapping)
################################################################################
NBOOT = 100
vars_sung_pitches = c("sung_pitch1", "sung_pitch2", "sung_pitch3", "sung_pitch4", "sung_pitch5")
vars_sung_intervals = c("sung_interval1", "sung_interval2", "sung_interval3", "sung_interval4")
vars_sung_IOIratios = c("sung_ratio1", "sung_ratio2", "sung_ratio3")
vars_target_IOIratios = c("target_ratio1", "target_ratio2", "target_ratio3")


# get error and entropy via bootstrap
data_melodies_features_boot = get_bootstrapped_features_melody(
  data_melodies_ratios, 
  vars_sung_pitches,
  vars_sung_intervals,
  vars_sung_IOIratios,
  NBOOT
  )

# save
# write_csv(data_melodies_features_boot, "data_melodies_features_boot.csv")


# plot
data_features_aggregate = data_melodies_features_boot %>% 
  group_by(degree) %>%
  summarise(
    n = n(),
    m_interval_error = mean(mean_interval_error),
    sd_error = sd(mean_interval_error, na.rm = T),
    m_rms_error_ratios = mean(rms_error_ratios),
    sd_rms_error_ratios = sd(rms_error_ratios, na.rm = T),
    m_entropy_interval = mean(entropy_interval),
    sd_entropy_interval = sd(entropy_interval, na.rm = T),
    m_entropy_ratio_IOI = mean(entropy_IOI_ratios),
    sd_entropy_ratio_IOI = sd(entropy_IOI_ratios, na.rm = T)
  ) 


# error
data_error_aggregate_long <- data_features_aggregate %>%
  filter(degree < 11) %>%
  pivot_longer(cols = c(m_interval_error, m_rms_error_ratios), 
               names_to = "Metric", 
               values_to = "Error") %>%
  mutate(SD = ifelse(Metric == "m_interval_error", sd_error, sd_rms_error_ratios),
         ymin = Error - SD,
         ymax = Error + SD) %>% 
  mutate(Metric = factor(Metric, levels = c("m_interval_error", "m_rms_error_ratios")))

# Plot with facet_wrap
plot_error <- ggplot(data_error_aggregate_long, aes(x = degree, y = Error)) +
  geom_line(aes(color = Metric), size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Metric), alpha = 0.2) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "Iteration", y = "Error") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  theme_classic() +
  scale_color_manual(values = c("m_interval_error" = "blue", "m_rms_error_ratios" = "red")) +
  scale_fill_manual(values = c("m_interval_error" = "blue", "m_rms_error_ratios" = "red")) +
  theme(legend.position = "none")

plot_error

ggsave("results/figure1-structures/evolution_error.png", width = 11, height = 6,  units = "cm", bg = "white")


# entropy interval and error
data_entropy_aggregate_long <- data_features_aggregate %>%
  filter(degree < 11) %>%
  pivot_longer(cols = c(m_entropy_interval, m_entropy_ratio_IOI), 
               names_to = "Metric", 
               values_to = "Error") %>%
  mutate(SD = ifelse(Metric == "m_entropy_interval", sd_entropy_interval, sd_entropy_ratio_IOI),
         ymin = Error - SD,
         ymax = Error + SD)


# Plot with facet_wrap
plot_entropy <- ggplot(data_entropy_aggregate_long, aes(x = degree, y = Error)) +
  geom_line(aes(color = Metric), size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Metric), alpha = 0.2) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "Iteration", y = "Entropy") +
  theme_classic() +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  scale_color_manual(values = c("m_entropy_interval" = "blue", "m_entropy_ratio_IOI" = "red")) +
  scale_fill_manual(values = c("m_entropy_interval" = "blue", "m_entropy_ratio_IOI" = "red")) +
  theme(legend.position = "none")

plot_entropy

ggsave("results/figure1-structures/evolution_entropy.png", width = 11, height = 6,  units = "cm", bg = "white")


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
    z_IOI_entropy = (m_entropy_ratio_IOI - mean(m_entropy_ratio_IOI)) / sd(m_entropy_ratio_IOI),
    z_IOI_entropy_se = sd_entropy_ratio_IOI / sd(m_entropy_ratio_IOI)
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

