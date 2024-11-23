# import libraries
library(tidyverse)
library(egg)
library(ggpubr)
library(cowplot)

source("utils/plots.R")

# global parameters
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 12
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

# import data
# data_melodies <- read_csv("data/data-clean/sing-scales-v1/data-sing-scales-v1_full.csv")
data_melodies <- read_csv("data/data-clean/sing-scales-v2/data-sing-scales-v2_full.csv")

source("utils/key-operations.R")


################################################################################
# Tonality analysis
################################################################################
tonal_features_list <- apply_key_finding(
  data_melodies, 
  major_profile_ALbrecht2013,
  minor_profile_ALbrecht2013
  # major_profile_KS,
  # minor_profile_KS
  )
tonal_features = do.call(rbind, tonal_features_list)

data_melodies$tonalness <- tonal_features$tonalness
data_melodies$tonal.clarity <- tonal_features$tonal.clarity
data_melodies$tonal.spike <- tonal_features$tonal.spike
data_melodies$mode <- tonal_features$mode
data_melodies$estimated_key <- tonal_features$estimated_key

# save data
write_csv(data_melodies, "data/data-clean/sing-scales-v2/data-sing-scales-v2_full_tonality.csv")

################################################################################
# histogram of estimated key
################################################################################
hist_key <- data_melodies %>%
  select(id:degree, estimated_key) %>% 
  count(estimated_key) %>%  
  arrange(desc(n))

ggplot(hist_key, aes(x = reorder(estimated_key, -n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Estimated Key", y = "Number of Melodies", title = "Histogram of Estimated Keys") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("results/hist_estimated_keys_Albrecht2013.png", width = 20, height = 15, units = "cm")

# over time
hist_key <- data_melodies %>%
  select(id:degree, estimated_key) %>% 
  count(degree, estimated_key) %>%  
  arrange(desc(n))

hist_key_degree <- data_melodies %>%
  select(id:degree, estimated_key) %>% 
  count(degree, estimated_key) %>%  
  arrange(desc(n))

ggplot(hist_key_degree, aes(x = reorder(estimated_key, -n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Estimated Key", y = "Number of Melodies", title = "Histogram of Melody Sequences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  facet_wrap(~degree) 
  
ggsave("results/hist_estimated_keys_Albrecht2013_evolution.png", width = 20, height = 15, units = "cm")


################################################################################
# interval between notes and key
################################################################################
diff_data <-  data_melodies %>%
  select(id:degree, round_sung_pitch1:round_sung_pitch5, estimated_key, mode)  %>%
  mutate(estimated_key_midi = note_to_midi_converter(estimated_key)) %>%
  mutate(diff_key_pitch1 = abs(round_sung_pitch1 - estimated_key_midi),
         diff_key_pitch2 = abs(round_sung_pitch2 - estimated_key_midi),
         diff_key_pitch3 = abs(round_sung_pitch3 - estimated_key_midi),
         diff_key_pitch4 = abs(round_sung_pitch4 - estimated_key_midi),
         diff_key_pitch5 = abs(round_sung_pitch5 - estimated_key_midi)) %>%
  # mutate(diff_key_pitch1 = estimated_key_midi - round_sung_pitch1,
  #        diff_key_pitch2 = estimated_key_midi - round_sung_pitch2,
  #        diff_key_pitch3 = estimated_key_midi - round_sung_pitch3,
  #        diff_key_pitch4 = estimated_key_midi - round_sung_pitch4,
  #        diff_key_pitch5 = estimated_key_midi - round_sung_pitch5) %>%
  select(id:degree, estimated_key_midi, diff_key_pitch1:diff_key_pitch5)

diff_data_lonf <-  diff_data %>%
  pivot_longer(cols = starts_with("diff_key_pitch"),
               names_to = "interval_pos",
               values_to = "interval")


NBOOT = 1000
BW = 0.25
interval_range = c(0, 15)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

marginals_interval_key = make_marginals_kde(diff_data_lonf, c("interval"), NBOOT, BW, "Note to Key interval") 

marginals_interval_key

ggsave("results/notes_to_key_intervals_all_minus_trams.png", width = 14, height = 7, units = "cm")


marginals_melodies_intervals_separate = make_marginals_kde_separate_notes_to_key_intervals(
  diff_data, 
  c("diff_key_pitch1", "diff_key_pitch2", "diff_key_pitch3", "diff_key_pitch4", "diff_key_pitch5"), 
  NBOOT, BW)  


marginals_melodies_intervals_separate

ggsave("results/notes_to_key_intervals_minus_trans.png",  dpi = 300)


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

ggsave("results/evolution_tonal.results_Albrecht2013.png", tonal.results, width = 20, height = 8, units = "cm")


################################################################################
# test recency
################################################################################
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

data_recency %>% 
  count(degree, end_in_tonic) %>% 
  filter(end_in_tonic == 1) %>% 
  mutate(percent = (n/100)*100)

data_recency %>% 
  group_by(degree) %>%
  summarise(
    mean_diff_key_pitch1 = mean(diff_key_pitch1),
    mean_diff_key_pitch2 = mean(diff_key_pitch2),
    mean_diff_key_pitch3 = mean(diff_key_pitch3),
    mean_diff_key_pitch4 = mean(diff_key_pitch4),
    mean_diff_key_pitch5 = mean(diff_key_pitch5)
  )



