################################################################################
# Iterated singing (tonality & scales)
# Script: figure 3 - Emergence of music scales
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
data_melodies_free <- read_csv("data/final-data/sing-varnotes-v1/data-sing-varnotes-v1_full.csv")

length(table(data_melodies_free$participant_id))
length(table(data_melodies_free$network_id))
length(table(data_melodies_free$degree))
table(data_melodies_free$degree)


################################################################################
# Distribution of pitch intervals
################################################################################
NBOOT = 1000
BW = 0.25
MAX_INTERVAL_SIZE = 13
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

# marginals intervals
data_melodies_long_intervals =  data_melodies_free %>% 
  select(id:participant_id, num_sung_pitches, sung_intervals) %>%
  mutate(sung_intervals = str_split(sung_intervals, ",")) %>%
  unnest_longer(sung_intervals) %>%
  mutate(sung_intervals = as.numeric(sung_intervals))


marginals_melodies_intervals = make_marginals_kde(
  data_melodies_long_intervals,
  c("sung_intervals"), NBOOT, BW, "marginals"
  ) 

marginals_melodies_intervals

ggsave("results/figure3/marginals_intervals.png", width = 14, height = 7, units = "cm")


# Separate by melody length
marginals_intervals_length = data_melodies_long_intervals %>% 
  filter(degree %in% 8:10) %>%
  # classify short melodies within 3 to 5 pitches, medium to 6 to 8 pitches, and long melodies with 9 to 12 pitches
  mutate(melody_length = case_when(
    num_sung_pitches %in% 3:4 ~ "short",
    num_sung_pitches %in% 5:8 ~ "medium",
    num_sung_pitches %in% 9:15 ~ "long")) %>%
  mutate(melody_length = factor(melody_length, levels = c("short", "medium", "long"))) %>%
  ggplot(aes(x = sung_intervals)) +
  geom_density(bw=BW)  +
  scale_x_continuous(breaks=vertical.lines,
                     limits = c(min(-13), max(13)))  + 
  geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") + 
  facet_wrap(~ melody_length, nrow = 1) +
  theme(axis.text.x = element_text(size=7), 
        axis.text.y=element_text(size=7), 
        axis.title.x = element_blank(), 
        axis.title.y =element_blank())

marginals_intervals_length


# evolution
BW = 0.25
vertical.lines = c(-12, -5, 0,  5, 12)

marginals_intervals_evolution = data_melodies_long_intervals %>% 
  # select few iterations for visualization
  filter(degree == 0 | degree == 2 | degree == 4 | degree == 6 | degree == 8 | degree == 10) %>%
  ggplot(aes(x = sung_intervals)) +
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

# ggsave("results/figure3/evolution_marginals_intervals.png", width = 20, height = 4, units = "cm")


################################################################################
# Distribution of IOIs
################################################################################
NBOOT = 1000
BW = 0.005
interval_range = c(0.2, 0.8)
vertical.lines = c(0.2, 0.333, 0.5, 0.666, 0.8)
vertical.labels <- c("1/5", "1/3", "1/2", "2/3", "4/5")


intervals_to_ratios <- function(intervals) {
  sums <- head(intervals, -1) + tail(intervals, -1)
  ratios <- head(intervals, -1) / sums
  return(ratios)
}

# get ratios
data_melodies_free_ratios <- data_melodies_free %>% 
  select(id:participant_id, sung_intervals, num_sung_pitches, sung_ISIs, target_ISIs,
         root_mean_squared_interval) %>%
  mutate(sung_ISIs = str_split(sung_ISIs, ",")) %>%
  mutate(target_ISIs = str_split(target_ISIs, ",")) %>%
  mutate(sung_ratios = map(sung_ISIs, function(x) intervals_to_ratios(as.numeric(x)))) %>% 
  mutate(target_ratios = map(target_ISIs, function(x) intervals_to_ratios(as.numeric(x)))) 

# long format
data_melodies_ratios_long =  data_melodies_free_ratios %>% 
  unnest_longer(sung_ratios) 

marginals_melodies_ratios = make_marginals_ratio_IOI_kde(data_melodies_ratios_long, c("sung_ratios"), NBOOT, BW, "marginals") 

marginals_melodies_ratios

ggsave("results/figure3/marginals_ratios.png", width = 14, height = 7, units = "cm")

# evolution
marginals_ratios_evolution = data_melodies_ratios_long %>% 
  # select few iterations for visualization
  filter(degree == 0 | degree == 2 | degree == 4 | degree == 6 | degree == 8 | degree == 10) %>%
  ggplot(aes(x = sung_ratios)) +
  geom_density(bw=BW)  +
  scale_x_continuous(breaks=vertical.lines,
                     labels=vertical.labels,
                     limits = c(min(interval_range), max(interval_range)))  +
  geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
  facet_wrap(~ degree, nrow = 1) +
  theme(axis.text.x = element_text(size=7), 
        axis.text.y=element_text(size=7), 
        axis.title.x = element_blank(), 
        axis.title.y =element_blank())

marginals_ratios_evolution



# Melody length
marginals_ratios_length = data_melodies_ratios_long %>% 
  filter(degree %in% 8:10) %>%
  # classify short melodies within 3 to 5 pitches, medium to 6 to 8 pitches, and long melodies with 9 to 12 pitches
  mutate(melody_length = case_when(
    num_sung_pitches %in% 3:4 ~ "short",
    num_sung_pitches %in% 5:8 ~ "medium",
    num_sung_pitches %in% 9:15 ~ "long")) %>%
  mutate(melody_length = factor(melody_length, levels = c("short", "medium", "long"))) %>%
  ggplot(aes(x = sung_ratios)) +
  geom_density(bw=BW)  +
  scale_x_continuous(breaks=vertical.lines,
                     limits = c(min(0.2), max(0.8)))  + 
  geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") + 
  facet_wrap(~ melody_length, nrow = 1) +
  theme(axis.text.x = element_text(size=7), 
        axis.text.y=element_text(size=7), 
        axis.title.x = element_blank(), 
        axis.title.y =element_blank())

marginals_ratios_length

# ggsave("results/figure3/lenght_marginals_ratios.png", width = 20, height = 5, units = "cm")


################################################################################
# Tonality analysis
################################################################################

source("utils/key-operations.R") # methods for key analysis

tonal_features_list <- apply_key_finding_free_notes(data_melodies_free, kk_all_key_profiles)

tonal_features = do.call(rbind, tonal_features_list)

# match based on ids
matching_indices <- match(tonal_features$melody_id, data_melodies_free$id)
data_melodies_free_filtered <- data_melodies_free[matching_indices, ]

data_melodies_free_filtered$mode <- tonal_features$mode
data_melodies_free_filtered$estimated_key <- tonal_features$estimated_key
data_melodies_free_filtered$estimated_tonic <- tonal_features$estimated_tonic
data_melodies_free_filtered$tonalness <- tonal_features$tonalness
data_melodies_free_filtered$tonal.clarity <- tonal_features$tonal.clarity
data_melodies_free_filtered$tonal.spike <- tonal_features$tonal.spike

# save data
# write_csv(data_melodies_free_filtered, "data_melodies_free_filtered.csv")


# tonality features over time
tonality_data_long <- data_melodies_free_filtered %>%
  pivot_longer(cols = c(tonalness, tonal.clarity, tonal.spike),
               names_to = "feature", values_to = "value") %>%
  group_by(degree, feature) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  )

plot_tonalness <- plot_tonality_feature(tonality_data_long, "tonalness", "Tonalness", "Tonalness")

ggsave("results/figure3/tonalness.png", 
       plot_tonalness, width = 7, height = 7, units = "cm", bg = "white")


################################################################################
# Trends: Error & Entropy (bootstrapping)
################################################################################
NBOOT = 100

# get error and entropy via bootstrap
data_melodies_features_boot = get_bootstrapped_features_freenote_melody(
  data_melodies_free_ratios, 
  NBOOT
)


data_features_aggregate = data_melodies_features_boot %>% 
  group_by(degree) %>%
  summarise(
    n = n(),
    m_interval_error = mean(mean_interval_error),
    sd_error = sd(mean_interval_error, na.rm = T),
    m_IOI_ratio_error = mean(rms_error_ratios),
    sd_IOI_ratio_error = sd(rms_error_ratios, na.rm = T),
    m_entropy_interval = mean(entropy_interval),
    sd_entropy_interval = sd(entropy_interval, na.rm = T),
    m_entropy_ratio_IOI = mean(entropy_IOI),
    sd_entropy_ratio_IOI = sd(entropy_IOI, na.rm = T)
  ) 


# error
data_error_aggregate_long <- data_features_aggregate %>%
  filter(degree < 11) %>%
  pivot_longer(cols = c(m_interval_error, m_IOI_ratio_error), 
               names_to = "Metric", 
               values_to = "Error") %>%
  mutate(SD = ifelse(Metric == "m_interval_error", sd_error, sd_IOI_ratio_error),
         ymin = Error - SD,
         ymax = Error + SD)

# Plot with facet_wrap
plot_error <- ggplot(data_error_aggregate_long, aes(x = degree, y = Error)) +
  geom_line(aes(color = Metric), size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Metric), alpha = 0.2) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "Degree", y = "Error") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  theme_classic() +
  scale_color_manual(values = c("m_interval_error" = "blue", "m_IOI_ratio_error" = "red")) +
  scale_fill_manual(values = c("m_interval_error" = "blue", "m_IOI_ratio_error" = "red")) +
  theme(legend.position = "none")

plot_error

ggsave("results/figure3/evolution_error.png", width = 12, height = 6,  units = "cm", bg = "white")


# entropy
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
  labs(x = "Degree", y = "Error") +
  theme_classic() +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  scale_color_manual(values = c("m_entropy_interval" = "blue", "m_entropy_ratio_IOI" = "red")) +
  scale_fill_manual(values = c("m_entropy_interval" = "blue", "m_entropy_ratio_IOI" = "red")) +
  theme(legend.position = "none")

plot_entropy

ggsave("results/figure3/evolution_entropy.png", width = 12, height = 6, units = "cm", bg = "white")



################################################################################
# Note length
################################################################################

# mean number noters over time
btw_free_all_sum = data_melodies_free %>%
  group_by(degree) %>%
  dplyr::summarise(
    n=n(),
    mean_num_pitches = mean(num_sung_pitches, na.rm = T),
    median_num_pitches = median(num_sung_pitches, na.rm = T),
    sd_num_pitches = sd(num_sung_pitches, na.rm = T),
    se = sd_num_pitches/sqrt(n)
  ) 


plot_mean_notes = btw_free_all_sum %>% 
  filter(degree < 11) %>%
  mutate(degree=as.factor(degree)) %>% 
  ggplot(aes(degree, mean_num_pitches, group=1)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean_num_pitches-se, 
                  ymax=mean_num_pitches+se), 
              fill = "#0072B2", alpha = 0.3) +
  geom_point(fill = "white", size = 2, shape=21) +
  # scale_y_continuous(breaks=seq(6,9,1),limits=c(6,8.5)) +
  xlab("Iteration") +
  ylab("Mean number of notes") +
  geom_hline(aes(yintercept = 8), linetype="dashed")  +
  theme(aspect.ratio = 1,
    axis.text.x = element_text(size=10),
        axis.text.y=element_text(size=10),
        legend.position = "none",
        legend.title = element_blank())

plot_mean_notes

ggsave("results/figure3/mean_notes.png", width = 7, height = 7, units = "cm", bg = "white")


# histogram of notes
btw_free_all_sum = data_melodies_free %>% 
  group_by(degree, num_sung_pitches) %>% 
  dplyr::summarise(
    n =n()
  ) %>% 
  filter(degree != 0 )

seed = tibble(
  degree= rep(0, 9),
  num_sung_pitches = seq(4,12,1),
  n = rep(24, 9)  # all melodies started with 24 chains
)

btw_free_all_sum_all = rbind(btw_free_all_sum, seed)

btw_free_all_sum_sum = btw_free_all_sum_all %>% 
  mutate(
    gg = case_when(
      degree == 0 ~ "Iteration 0 (seed)",
      degree < 4 ~ "Iteration 1-3",
      degree < 7 ~ "Iteration 4-7",
      degree < 11 ~ "Iteration 8-10"
    )
  ) %>% 
  group_by(num_sung_pitches, gg) %>% 
  dplyr::summarise(mean_n = mean(n), sd_n = sd(n), se_n = sd_n/sqrt(n()))

hist_lenght = btw_free_all_sum_sum %>% 
  drop_na(gg) %>% 
  filter(num_sung_pitches < 13) %>% 
  ggplot(aes(x=as.factor(num_sung_pitches), y=mean_n,
             fill=as.factor(num_sung_pitches)))+
  geom_bar(stat="identity", color="black") +
  geom_errorbar(aes(ymin=mean_n-se_n, ymax=mean_n+se_n), width=.2, 
                position=position_dodge(.9)) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "D", direction = 1) +
  xlab("Number of notes") +
  ylab("Frequency")  +
  geom_hline(aes(yintercept = 24), linetype="dashed",color="black") +
  theme(axis.text.x = element_text(size=8),
        axis.text.y=element_text(size=8),
        legend.position = "none",
        legend.title = element_blank()) +
  facet_wrap(~gg, nrow = 1) 

hist_lenght

ggsave("results/figure3/histogram_notes.png", width = 15, height = 5, units = "cm", bg = "white")


################################################################################
# Distributions of individual scales
################################################################################
pitch_classes_ref <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")

# Transpose so that the first MIDI note becomes 60 (Middle C)
transpose_midi_to_C <- function(midi_vec) {
  if (length(midi_vec) == 0 || any(is.na(midi_vec))) return(NA)
  shift <- 60 - midi_vec[1]
  midi_vec + shift
}

# Convert MIDI to pitch class
midi_to_pitch_class <- function(midi_note) {
  pitch_classes_ref[(midi_note %% 12) + 1]
}

# Create signature from sorted pitch classes
create_scale_signature <- function(pitch_classes) {
  pcs_unique <- unique(pitch_classes)
  ordered <- pitch_classes_ref[pitch_classes_ref %in% pcs_unique]
  paste(ordered, collapse = "-")
}

# Main pipeline
hist_scales_data <- data_melodies_free %>%
  filter(degree %in% 8:10) %>%
  select(id:participant_id, register, num_sung_pitches, sung_pitches) %>%
  mutate(sung_pitches = str_split(sung_pitches, ",")) %>%
  mutate(sung_pitches_round = map(sung_pitches, ~ round(as.numeric(.x)))) %>%
  mutate(transposed_midi = map(sung_pitches_round, transpose_midi_to_C)) %>%
  mutate(transposed_pc = map(transposed_midi, midi_to_pitch_class)) %>%
  mutate(scale_signature = map_chr(transposed_pc, create_scale_signature)) %>%
  count(scale_signature) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

# Plot
ggplot(hist_scales_data, aes(x = reorder(scale_signature, -n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Scale Signature", y = "Number of Melodies") +
  theme(
    axis.text.x = element_text(size= 8, angle = 90, hjust = 1),
    axis.text.y = element_text(size= 8),
    axis.title.x = element_text(size= 10),
    axis.title.y = element_text(size= 10),
    )


ggsave("results/figure3/distribution_scales.png", width = 10, height = 7, units = "cm", bg = "white")


################################################################################
# Entropy of scales
################################################################################
compute_entropy <- function(freqs) {
  probs <- freqs / sum(freqs)
  entropy::entropy(probs, unit = "log2")
}

# Bootstrap entropy function
bootstrap_entropy <- function(scale_counts, n_boot = 1000) {
  total <- sum(scale_counts)
  boot_entropies <- replicate(n_boot, {
    boot_sample <- sample(rep(names(scale_counts), scale_counts), total, replace = TRUE)
    boot_freqs <- table(boot_sample)
    compute_entropy(boot_freqs)
  })
  tibble(
    mean_entropy = mean(boot_entropies),
    sd_entropy = sd(boot_entropies),
    lower_ci = quantile(boot_entropies, 0.025),
    upper_ci = quantile(boot_entropies, 0.975)
  )
}


# Main pipeline with bootstrap + entropy + CI
entropy_over_iterations <- data_melodies_free %>%
  select(degree, sung_pitches) %>%
  mutate(
    sung_pitches_vec = map(sung_pitches, ~ round(as.numeric(str_split(.x, ",")[[1]]))),
    transposed_midi = map(sung_pitches_vec, transpose_midi_to_C),
    transposed_pc = map(transposed_midi, midi_to_pitch_class),
    scale_signature = map_chr(transposed_pc, create_scale_signature)
  ) %>%
  group_by(degree, scale_signature) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(degree) %>%
  summarise(
    entropy_ci = list(bootstrap_entropy(setNames(n, scale_signature))),
    .groups = "drop"
  ) %>%
  unnest(entropy_ci)

# plot
entropy_over_iterations %>% 
  filter(degree < 11) %>% 
  ggplot(aes(x = degree, y = mean_entropy)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean_entropy-sd_entropy, 
                  ymax=mean_entropy+sd_entropy), 
              fill = "#0072B2", alpha = 0.3) +
  geom_point(fill = "white", size = 2, shape=21) +
  labs(x = "Iteration", y = "Shannon Entropy") +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 10, 1))  +
  theme(
    axis.text.x = element_text(size= 8),
    axis.text.y = element_text(size= 8),
    axis.title.x = element_text(size= 10),
    axis.title.y = element_text(size= 10),
  )


ggsave("results/figure3/entropy_scales.png", width = 7, height = 7, units = "cm", bg = "white")


################################################################################
# Distribution of numbers of scale degrees
################################################################################
scale_size_distribution <- data_melodies_free %>%
  filter(degree %in% 8:10) %>%
  select(degree, sung_pitches) %>%
  mutate(
    sung_pitches_vec = map(sung_pitches, ~ round(as.numeric(str_split(.x, ",")[[1]]))),
    transposed_midi = map(sung_pitches_vec, transpose_midi_to_C),
    transposed_pc = map(transposed_midi, midi_to_pitch_class),
    scale_size = map_int(transposed_pc, ~ length(unique(.x)))
  ) %>%
  count(scale_size)


scale_size_distribution %>% 
  ggplot(aes(x = scale_size, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of Scale Degrees", y = "Number of Melodies") +
  scale_x_continuous(breaks = seq(2, 11, 1))  +
  theme_classic() +
  theme(
    axis.text.x = element_text(size= 8),
    axis.text.y = element_text(size= 8),
    axis.title.x = element_text(size= 10),
    axis.title.y = element_text(size= 10),
  )

ggsave("results/figure3/distribution_scale_size.png", width = 7, height = 7, units = "cm", bg = "white")  


# over time
scale_size_distribution <- data_melodies_free %>%
  # filter(degree %in% 8:10) %>%
  select(degree, sung_pitches) %>%
  mutate(
    sung_pitches_vec = map(sung_pitches, ~ round(as.numeric(str_split(.x, ",")[[1]]))),
    transposed_midi = map(sung_pitches_vec, transpose_midi_to_C),
    transposed_pc = map(transposed_midi, midi_to_pitch_class),
    scale_size = map_int(transposed_pc, ~ length(unique(.x)))
  ) %>%
  count(degree, scale_size)


scale_size_distribution %>% 
  filter(degree %in% c(0,2,4,6,8,10)) %>%
  ggplot(aes(x = scale_size, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of Scale Degrees", y = "Number of Melodies") +
  scale_x_continuous(breaks = seq(2, 11, 1))  +
  theme_classic() +
  theme(
    axis.text.x = element_text(size= 8),
    axis.text.y = element_text(size= 8),
    axis.title.x = element_text(size= 10),
    axis.title.y = element_text(size= 10),
  ) +
  facet_wrap(~ degree, nrow = 1) 
