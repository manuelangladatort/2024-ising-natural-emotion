################################################################################
# Iterated singing (tonality & scales)
# Script: figure 2
################################################################################

# import 
library(tidyverse)
library(egg)
library(ggpubr)
library(cowplot)

source("utils/plots.R")
source("utils/key-operations.R") # methods for key analysis


# global 
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 13
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

# data
data_melodies <- read_csv("data/final-data/sing-scales-v2/data-sing-scales-v2_full.csv")


################################################################################
# Tonality analysis
################################################################################
tonal_features_list <- apply_key_finding(data_melodies, kk_all_key_profiles)
# tonal_features_list <- apply_key_finding_fantastic(data_melodies, major_profile_ALbrecht2013, minor_profile_ALbrecht2013)

tonal_features = do.call(rbind, tonal_features_list)

data_melodies$mode <- tonal_features$mode
data_melodies$estimated_key <- tonal_features$estimated_key
data_melodies$estimated_tonic <- tonal_features$estimated_tonic
data_melodies$tonalness <- tonal_features$tonalness
data_melodies$tonal.clarity <- tonal_features$tonal.clarity
data_melodies$tonal.spike <- tonal_features$tonal.spike

# save data
# write_csv(data_melodies, "data/final-data/sing-scales-v2/data-sing-scales-v2_full_tonality.csv")


################################################################################
# Key profiles
################################################################################
NBOOT = 1000
BW = 0.25
interval_range = c(0, 15)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)


diff_data <-  data_melodies %>%
  select(id:degree, register, sung_pitch1:sung_pitch5, estimated_key, estimated_tonic, mode)  %>%
  mutate(estimated_key_midi = note_to_midi_converter(estimated_key)) %>%
  mutate(sung_pitch1 = ifelse(register == "low", (sung_pitch1+12), sung_pitch1),
         sung_pitch2 = ifelse(register == "low", (sung_pitch2+12), sung_pitch2),
         sung_pitch3 = ifelse(register == "low", (sung_pitch3+12), sung_pitch3),
         sung_pitch4 = ifelse(register == "low", (sung_pitch4+12), sung_pitch4),
         sung_pitch5 = ifelse(register == "low", (sung_pitch5+12), sung_pitch5)) %>%
  mutate(diff_key_pitch1 = abs(sung_pitch1 - estimated_key_midi),
         diff_key_pitch2 = abs(sung_pitch2 - estimated_key_midi),
         diff_key_pitch3 = abs(sung_pitch3 - estimated_key_midi),
         diff_key_pitch4 = abs(sung_pitch4 - estimated_key_midi),
         diff_key_pitch5 = abs(sung_pitch5 - estimated_key_midi)) %>%
  select(id:degree, estimated_key_midi, mode, diff_key_pitch1:diff_key_pitch5)


diff_data_long <-  diff_data %>%
  pivot_longer(cols = starts_with("diff_key_pitch"),
               names_to = "interval_pos",
               values_to = "interval")

marginals_interval_all_key = make_marginals_kde(diff_data_long, c("interval"), NBOOT, BW, "Note to Key interval") 

ggsave("results/figure2/notes_to_key_intervals_all.png", width = 14, height = 7, units = "cm")

diff_data_long_major <-  diff_data %>%
  filter(mode == "major")   %>% 
  pivot_longer(cols = starts_with("diff_key_pitch"),
               names_to = "interval_pos",
               values_to = "interval")

diff_data_long_minor<-  diff_data %>%
  filter(mode == "minor")   %>% 
  pivot_longer(cols = starts_with("diff_key_pitch"),
               names_to = "interval_pos",
               values_to = "interval")


marginals_interval_major_key = make_marginals_kde(diff_data_long_major, c("interval"), NBOOT, BW, "Major mode: Note to Key interval") 
marginals_interval_minor_key = make_marginals_kde(diff_data_long_minor, c("interval"), NBOOT, BW, "Minor mode: Note to Key interval") 

plot_grid(marginals_interval_major_key, marginals_interval_minor_key, nrow = 2)

ggsave("results/figure2/notes_to_key_intervals_mode.png", width = 14, height = 12, units = "cm")


# separate
marginals_melodies_intervals_separate = make_marginals_kde_separate_notes_to_key_intervals(
  diff_data, 
  c("diff_key_pitch1", "diff_key_pitch2", "diff_key_pitch3", "diff_key_pitch4", "diff_key_pitch5"), 
  NBOOT, BW)  


marginals_melodies_intervals_separate

ggsave("results/figure2/notes_to_key_intervals_minus_trans_continous.png", width = 12, height = 15, units = "cm")


################################################################################
# tonality features over time
################################################################################
# calculate summary stats tonal features
tonality_data = data_melodies %>% 
  group_by(degree) %>% 
  summarise(
    n = n(), 
    mean_tonalness = mean(tonalness, na.rm = T),
    sd_tonalness = sd(tonalness, na.rm = T),
    se_tonalness = sd_tonalness/sqrt(n),
    mean_tonal.clarity = mean(tonal.clarity, na.rm = T),
    sd_tonal.clarity = sd(tonal.clarity, na.rm = T),
    se_sd_tonal.clarity = sd_tonal.clarity/sqrt(n),
    mean_tonal.spike = mean(tonal.spike, na.rm = T),
    sd_tonal.spike = sd(tonal.spike, na.rm = T),
    se_tonal.spike = sd_tonal.spike/sqrt(n)
  )

# plot tonal features
plot_tonalness = tonality_data %>%
  filter(degree < 11) %>% 
  ggplot(aes(x= degree, y = mean_tonalness)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_tonalness - se_tonalness, 
                  ymax=mean_tonalness + se_tonalness),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  # add labels
  ylab("Tonalness") +
  xlab("Iteration") +
  ggtitle("Tonalness") +
  theme_classic() +
  theme(legend.position = "none")

plot_tona.clarity = tonality_data %>%
  filter(degree < 11) %>% 
  ggplot(aes(x= degree, y = mean_tonal.clarity)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_tonal.clarity - se_sd_tonal.clarity, 
                  ymax=mean_tonal.clarity + se_sd_tonal.clarity),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  ylab("Tonal Clarity") +
  xlab("Iteration") +
  ggtitle("Tonal Clarity") +
  theme_classic() +
  theme(legend.position = "none")

plot_tona.spike = tonality_data %>%
  filter(degree < 11) %>% 
  ggplot(aes(x= degree, y = mean_tonal.spike)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_tonal.spike - se_tonal.spike, 
                  ymax=mean_tonal.spike + se_tonal.spike),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  ylab("Tonal Spike") +
  xlab("Iteration") +
  ggtitle("Tonal Spike") +
  theme_classic() +
  theme(legend.position = "none")


tonal.results = plot_grid(plot_tonalness, plot_tona.clarity, plot_tona.spike, nrow = 1)

tonal.results

ggsave("results/figure2/evolution_tonal.results_Albrecht2013.png", tonal.results, width = 15, height = 5, units = "cm")


################################################################################
# test recency
################################################################################
# TODO: do first and last notes tend to finish with tonic?
# TODO: mean normalized difference between key and notes over time


# old analysis
data_recency = data_melodies %>% 
  select(id:degree, round_sung_pitch1:round_sung_pitch5, estimated_key, mode)  %>%
  mutate(estimated_key_midi = note_to_midi_converter(estimated_key)) %>%
  mutate(end_in_tonic = ifelse(round_sung_pitch5==estimated_key_midi, 1, 0)) %>% 
  mutate(start_in_tonic = ifelse(round_sung_pitch1==estimated_key_midi, 1, 0)) %>% 
  mutate(diff_key_pitch1 = abs(round_sung_pitch1 - estimated_key_midi),
         diff_key_pitch2 = abs(round_sung_pitch2 - estimated_key_midi),
         diff_key_pitch3 = abs(round_sung_pitch3 - estimated_key_midi),
         diff_key_pitch4 = abs(round_sung_pitch4 - estimated_key_midi),
         diff_key_pitch5 = abs(round_sung_pitch5 - estimated_key_midi))

data_summary_end <- data_recency %>%
  count(degree, end_in_tonic) %>%
  filter(end_in_tonic == 1) %>%
  group_by(degree) %>%
  summarize(
    mean_percent = mean((n/100)*100),
    se_percent = sd((n/100)*100) / sqrt(n())
  )

data_summary_start <- data_recency %>%
  count(degree, start_in_tonic) %>%
  filter(start_in_tonic == 1) %>%
  group_by(degree) %>%
  summarize(
    mean_percent = mean((n/100)*100),
    se_percent = sd((n/100)*100) / sqrt(n())
  )


plot_end_with_key = ggplot(data_summary_end, aes(x = degree, y = mean_percent)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = mean_percent - se_percent, ymax = mean_percent + se_percent), alpha = 0.2) +
  ylim(0, 100) +
  labs(
    title = "Tonic in last note",
    x = "Degree",
    y = "Percentage"
  ) +
  theme_minimal()

plot_start_with_key = ggplot(data_summary_start, aes(x = degree, y = mean_percent)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = mean_percent - se_percent, ymax = mean_percent + se_percent), alpha = 0.2) +
  ylim(0, 100) +
  labs(
    title = "Tonic in first note",
    x = "Degree",
    y = "Percentage"
  ) +
  theme_minimal()


plot_grid(plot_end_with_key, plot_start_with_key, nrow = 1)


# difference between key and pitch
data_summary <- data_recency %>%
  group_by(degree) %>%
  summarise(
    mean_diff_key_pitch1 = mean(diff_key_pitch1),
    mean_diff_key_pitch2 = mean(diff_key_pitch2),
    mean_diff_key_pitch3 = mean(diff_key_pitch3),
    mean_diff_key_pitch4 = mean(diff_key_pitch4),
    mean_diff_key_pitch5 = mean(diff_key_pitch5)
  )

# Normalize each pitch column to start from the same value (subtract first value of each pitch)
data_normalized <- data_summary %>%
  mutate(
    mean_diff_key_pitch1 = mean_diff_key_pitch1 - mean_diff_key_pitch1[1],
    mean_diff_key_pitch2 = mean_diff_key_pitch2 - mean_diff_key_pitch2[1],
    mean_diff_key_pitch3 = mean_diff_key_pitch3 - mean_diff_key_pitch3[1],
    mean_diff_key_pitch4 = mean_diff_key_pitch4 - mean_diff_key_pitch4[1],
    mean_diff_key_pitch5 = mean_diff_key_pitch5 - mean_diff_key_pitch5[1]
  )

# Reshape the data into long format for ggplot
data_long <- data_normalized %>%
  tidyr::pivot_longer(
    cols = starts_with("mean_diff_key_pitch"),
    names_to = "pitch",
    values_to = "mean_diff"
  )

# Adjust legend labels by removing 'mean_diff_key_' prefix
data_long <- data_long %>%
  mutate(pitch = gsub("mean_diff_key_pitch", "note", pitch))


# Create the line plot
ggplot(data_long, aes(x = degree, y = mean_diff, color = pitch, group = pitch)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    # title = "Normalized Mean Difference from Key by Degree",
    x = "Degree",
    y = "Mean difference (normalized)",
    color = "Pitch"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

ggsave("results/figure2/mean_diff_key_pitch_normalized.png", width = 12, height = 7, units = "cm")



