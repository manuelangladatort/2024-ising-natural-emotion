################################################################################
# Iterated singing (tonality & scales)
# Script: figure 3 - Emergence of tonality
################################################################################

# import 
library(tidyverse)
library(egg)
library(ggpubr)
library(cowplot)

source("utils/plots.R")
source("utils/features.R")
source("utils/key-operations.R") # methods for key analysis


# global 
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 12
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

key_to_pc <- c("C"=0, "C#"=1, "D"=2, "D#"=3, "E"=4, "F"=5, "F#"=6, "G"=7, "G#"=8, "A"=9, "A#"=10, "B"=11)


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

# --------------------------------------------------------------------------------
# DISCRETE ANALYSIS
# --------------------------------------------------------------------------------

interval_range = c(-12, 12)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

diff_data <-  data_melodies %>%
  select(id:degree, estimated_key, estimated_tonic, mode, sung_pitch1:sung_pitch5) %>% 
  mutate(
    key_pc = key_to_pc[toupper(estimated_key)]  # Make sure this is tonic pitch class
  ) %>% 
  relocate(key_pc, .after = estimated_key) %>%
  mutate(across(starts_with("sung_pitch"), ~ round(.) %% 12, .names = "pc_{.col}")) %>% #
  mutate(across(starts_with("pc_sung_pitch"), ~ . - key_pc, .names = "diff_from_key_{.col}"))

  
diff_data_long <-  diff_data %>%
  pivot_longer(cols = starts_with("diff_from_key_pc_sung_"),
               names_to = "interval_pos",
               values_to = "interval")

marginals_interval_all_key = make_marginals_kde(diff_data_long, c("interval"), NBOOT, BW, "Note to Key interval") 
marginals_interval_all_key

ggsave("results/figure2-tonality/key_profile_discrete_intervals_all.png", width = 14, height = 7, units = "cm")


marginals_interval_diff_all_key = make_kde_difference_plot(diff_data_long, c("interval"), NBOOT, BW, "Note to Key interval (diff)") 
marginals_interval_diff_all_key

ggsave("results/figure2-tonality/key_profile_diff_discrete_intervals_all.png", width = 14, height = 7, units = "cm")


# --------------------------------------------------------------------------------
# CONTINUOUS VERSION
# --------------------------------------------------------------------------------

interval_range = c(0, 12)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

diff_data <-  data_melodies %>%
  select(id:degree, estimated_key, estimated_tonic, mode, sung_pitch1:sung_pitch5) %>%
  mutate(key_pc = key_to_pc[toupper(estimated_key)]) %>%
  relocate(key_pc, .after = estimated_key) %>%
  # mutate(across(starts_with("sung_pitch"), ~ (. - key_pc) %% 12, .names = "diff_from_key_pc_{.col}"))
  mutate(across(
    starts_with("sung_pitch"),
    ~ ((. %% 12 - key_pc) %% 12),  # do everything in one go
    .names = "diff_from_key_pc_{.col}"
  ))

diff_data_long <-  diff_data %>%
  pivot_longer(cols = starts_with("diff_from_key_pc_sung_"),
               names_to = "interval_pos",
               values_to = "interval")


marginals_interval_diff_all_key = make_kde_difference_plot(diff_data_long, c("interval"), NBOOT, BW, "Note to Key interval (diff)") 
marginals_interval_diff_all_key

ggsave("results/figure2-tonality/key_profile_diff_continous_intervals_all.png", width = 14, height = 7, units = "cm")


# --------------------------------------------------------------------------------
# SPLIT BY MODE 
# --------------------------------------------------------------------------------

diff_data_long_major <-  diff_data_long %>%  filter(mode == "Major")
diff_data_long_minor <-  diff_data_long %>%  filter(mode == "Minor")

marginals_interval_all_key_major = make_kde_difference_plot(diff_data_long_major, c("interval"), NBOOT, BW, "Note to Key interval (Major)") 
marginals_interval_all_key_minor = make_kde_difference_plot(diff_data_long_minor, c("interval"), NBOOT, BW, "Note to Key interval (Minor)") 

plot_grid(marginals_interval_all_key_major, marginals_interval_all_key_minor, nrow = 2)

ggsave("results/figure2-tonality/key_profile_diff_continous_intervals_mode.png", width = 12, height = 10, units = "cm")


# --------------------------------------------------------------------------------
# DIFFERENCE BETWEEN SEED AND ITERATION 1, 3, 5, 7 AND 10
# --------------------------------------------------------------------------------

result = make_kde_difference_by_degree(
  data = diff_data, 
  sung_intervals = c("diff_from_key_pc_sung_pitch1", "diff_from_key_pc_sung_pitch2", 
                     "diff_from_key_pc_sung_pitch3", "diff_from_key_pc_sung_pitch4", 
                     "diff_from_key_pc_sung_pitch5"), 
  n_samples = NBOOT, 
  BW = BW, 
  title = "Difference from Seed (Degree 0)"
)

result$plot  # plot

ggsave("results/figure2-tonality/key_profile_diff_continous_evolution.png", width = 20, height = 7, units = "cm")


# --------------------------------------------------------------------------------
# DISCRETE CIRCULAR VERSION
# --------------------------------------------------------------------------------

# Long-format the pitch class differences
diff_data_long <- diff_data %>%
  pivot_longer(
    cols = starts_with("diff_from_key_pc_sung_pitch"),
    names_to = "note_position",
    values_to = "pc_diff"
  ) %>%
  mutate(pc_diff = round(pc_diff) %% 12)  # Ensure [0–11] pitch class

seed_profile <- diff_data_long %>%
  filter(degree == 0) %>%
  count(pc_diff) %>%
  mutate(proportion_seed = n / sum(n)) %>%
  select(pc_diff, proportion_seed)

# Loop over selected degrees and compute diffs vs seed
target_degrees <- c(1, 3, 5, 7, 10)

profile_diffs <- lapply(target_degrees, function(deg) {
  prof <- diff_data_long %>%
    filter(degree == deg) %>%
    count(pc_diff) %>%
    mutate(proportion_gen = n / sum(n)) %>%
    select(pc_diff, proportion_gen)
  
  # Join with seed and compute difference
  full_join(seed_profile, prof, by = "pc_diff") %>%
    mutate(
      proportion_seed = replace_na(proportion_seed, 0),
      proportion_gen = replace_na(proportion_gen, 0),
      diff = proportion_gen - proportion_seed,
      degree = deg
    )
}) %>%
  bind_rows()

# plot
ggplot(profile_diffs, aes(x = pc_diff, y = diff, fill = diff > 0)) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  coord_polar(start = -pi/12) +
  scale_x_continuous(
    breaks = 0:11,
    labels = c("C", "C♯", "D", "D♯", "E", "F", 
               "F♯", "G", "G♯", "A", "A♯", "B")
  ) +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue"), guide = "none") +
  facet_wrap(~degree, nrow = 1) +
  labs(
    # title = "Key Profile Evolution: Difference vs Seed",
    x = "Pitch Class Interval (0 = Tonic)",
    y = "Proportion Difference"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10),
    plot.title = element_text()
  )


ggsave("results/figure2-tonality/key_profile_diff_ciruclar_evolution.png", width = 15, height = 5, units = "cm", bg = "white")


# --------------------------------------------------------------------------------
# NOW BY MODE
# --------------------------------------------------------------------------------
# Long-format the pitch class differences
diff_data_long <- diff_data %>%
  pivot_longer(
    cols = starts_with("diff_from_key_pc_sung_pitch"),
    names_to = "note_position",
    values_to = "pc_diff"
  ) %>%
  mutate(
    pc_diff = round(pc_diff) %% 12,  # ensure values are [0–11]
    mode = as.factor(mode)           # ensure mode is categorical
  )

# compute profile in seed
seed_profile <- diff_data_long %>%
  filter(degree == 0) %>%
  group_by(mode, pc_diff) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(mode) %>%
  mutate(proportion_seed = count / sum(count)) %>%
  ungroup() %>%
  select(mode, pc_diff, proportion_seed)

# loop over desired iterations
target_degrees <- c(1, 3, 5, 7, 10)

profile_diffs <- lapply(target_degrees, function(deg) {
  # Current generation profile by mode
  prof <- diff_data_long %>%
    filter(degree == deg) %>%
    group_by(mode, pc_diff) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(mode) %>%
    mutate(proportion_gen = count / sum(count)) %>%
    ungroup()
  
  # Join with seed profile and compute difference
  full_join(seed_profile, prof, by = c("mode", "pc_diff")) %>%
    mutate(
      proportion_seed = replace_na(proportion_seed, 0),
      proportion_gen = replace_na(proportion_gen, 0),
      diff = proportion_gen - proportion_seed,
      degree = deg
    )
}) %>%
  bind_rows()


# plot
ggplot(profile_diffs, aes(x = pc_diff, y = diff, fill = diff > 0)) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  coord_polar(start = -pi/12) +
  scale_x_continuous(
    breaks = 0:11,
    labels = c("C", "C♯", "D", "D♯", "E", "F", 
               "F♯", "G", "G♯", "A", "A♯", "B")
  ) +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue"), guide = "none") +
  facet_grid(mode ~ degree) +
  labs(
    # title = "Evolution of Key Profiles vs Seed (by Mode)",
    x = "Note-to-Key Interval (Pitch Class)",
    y = "Proportion Difference"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10),
    plot.title = element_text()
  )


ggsave("results/figure2-tonality/key_profile_diff_ciruclar_evolution_mode.png", width = 15, height = 10, units = "cm", bg = "white")


################################################################################
# tonality features over time
################################################################################
# add tonal confidence from m21 (Bayesian key-finding; Temperley, 2007)
data_key_m21 <- read_csv("tonality-explore/key_estimation_results.csv") %>% 
  select(id:degree, m21_confidence)

# Join and summarise in long format
tonality_data_long <- data_melodies %>%
  left_join(data_key_m21, by = c("id", "degree")) %>%
  pivot_longer(cols = c(tonalness, tonal.clarity, tonal.spike, m21_confidence),
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
plot_tonal_clarity <- plot_tonality_feature(tonality_data_long, "tonal.clarity", "Tonal Clarity", "Tonal Clarity")
plot_tonal_spike <- plot_tonality_feature(tonality_data_long, "tonal.spike", "Tonal Spike", "Tonal Spike")
plot_m21_conf <- plot_tonality_feature(tonality_data_long, "m21_confidence", "Tonal Confidence", "Tonal Confidence")

tonal.results = plot_grid(
  plot_tonalness, 
  plot_tonal_clarity, 
  plot_tonal_spike, 
  plot_m21_conf, nrow = 2)

tonal.results

ggsave("results/figure2-tonality/tonal_features.png", 
       tonal.results, width = 12, height = 12, units = "cm", bg = "white")



################################################################################
# First-Last Interval
################################################################################
NBOOT = 1000
BW = 0.25

data_melodies_firstlast_interval = data_melodies %>% 
  select(id:degree, sung_pitch1,sung_pitch5) %>% 
  mutate(first_last_interval = sung_pitch1 - sung_pitch5) 
  

data_melodies_firstlast_interval %>% 
  group_by(degree) %>%
  summarise(
    n = n(),
    mean = mean(first_last_interval, na.rm = TRUE),
    sd = sd(first_last_interval, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  ) 


marginals_melodies_firstlast_interval = make_marginals_kde(data_melodies_firstlast_interval, c("first_last_interval"), NBOOT, BW, "marginals") 

marginals_melodies_firstlast_interval

ggsave("results/figure2-tonality/first_last_interval_marginals.png", width = 14, height = 7, units = "cm")


# evolution
vertical.lines = c(-12, -5, 0,  5, 12)

marginals_firstlast_interval_evolution = data_melodies_firstlast_interval %>% 
  filter(degree == 0 | degree == 2 | degree == 4 | degree == 6 | degree == 8 | degree == 10) %>%
  ggplot(aes(x = first_last_interval)) +
  geom_density(bw=BW)  +
  scale_x_continuous(breaks=vertical.lines,
                     limits = c(min(-12), max(12)))  + 
  geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") + 
  facet_wrap(~ degree, nrow = 1) +
  theme(axis.text.x = element_text(size=7), 
        axis.text.y=element_text(size=7), 
        axis.title.x = element_blank(), 
        axis.title.y =element_blank())

marginals_firstlast_interval_evolution

ggsave("results/figure2-tonality/first_last_interval_evolution.png", width = 20, height = 4, units = "cm")


# --------------------------------------------------------------------------------
# CALCULATE ENTROPY VIA BOOTSTEAP
# --------------------------------------------------------------------------------

# Bootstrap entropy for each degree
NBOOT <- 100
degrees <- sort(unique(data_melodies_firstlast_interval$degree))
results <- vector("list", NBOOT)  

# Bootstrap loop
for (i in seq_len(NBOOT)) {
  message(sprintf("Bootstrap %d of %d", i, NBOOT))
  
  # Sample networks with replacement
  data_sample <- data_melodies_firstlast_interval %>%
    group_by(network_id) %>%
    group_split() %>%
    sample(length(.), replace = TRUE) %>%
    bind_rows()
  
  # Compute entropy per degree
  entropy_vec <- map_dbl(degrees, function(d) {
    data_sample %>%
      filter(degree == d) %>%
      rename(interval = first_last_interval) %>%
      get_entropy_0.25()
  })
  
  # Store result
  results[[i]] <- tibble(
    boot = i,
    degree = degrees,
    entropy = entropy_vec
  )
}

# Combine results
entropy_boot_df <- bind_rows(results)


# sum
sum_entropy_boot_df <- entropy_boot_df %>% 
  group_by(degree) %>%
  summarise(
    mean_entropy = mean(entropy, na.rm = TRUE),
    sd_entropy = sd(entropy, na.rm = TRUE),
    se_entropy = sd_entropy / sqrt(n()),
    lower = mean_entropy - qt(0.975, df = n() - 1) * se_entropy,
    upper = mean_entropy + qt(0.975, df = n() - 1) * se_entropy,
    .groups = "drop"
  )

plot_entropy = sum_entropy_boot_df %>% 
  filter(degree < 11) %>%
  mutate(degree=as.factor(degree)) %>% 
  ggplot(aes(degree, mean_entropy, group=1)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean_entropy-sd_entropy, 
                  ymax=mean_entropy+sd_entropy), 
              fill = "#D55E00", alpha = 0.3) +
  geom_point(fill = "white", size = 2, shape=21) +
  ggtitle("Entropy of closest-to-note key") +
  xlab("Iteration") +
  ylab("Entropy (bits)") +
  theme(aspect.ratio = 1,
        axis.text.x = element_text(size=8),
        axis.text.y=element_text(size=8),
        title = element_text(size = 10),
        legend.position = "none",
        legend.title = element_blank())

plot_entropy

ggsave("results/figure2-tonality/first_last_interva_entropy2.png", width = 8, height = 8, units = "cm", bg = "white")


################################################################################
# Recency and Primacy
################################################################################

# For each melody in the data, look at the estimated key and find who of the 5 notes in the melody is closest to the estimated key
# random melodies (degree = 0) should have almost a uniform distribution with lo2(5) entropy
# but in the lat iterations (degree 8-10), note 1 and 5 should have a higher probability to be closer to the estimated key
# The entropy of the distribution should be lower for the last iterations

# Calculate pitch class difference
data_melodies_recency <- data_melodies %>% 
  select(id:degree, sung_pitch1:sung_pitch5, estimated_key, estimated_tonic, mode) %>%
  
  # # rerun analysis using m21 results
  # left_join(data_key_m21, by = c("id", "degree")) %>%
  # select(-m21_key, -estimated_key) %>%
  # rename(estimated_key = m21_key_converted) %>%
  
  mutate(key_pc = key_to_pc[toupper(estimated_key)]) %>%
  rowwise() %>%
  mutate(
    pc_sung_pitch1 = round(sung_pitch1) %% 12,
    pc_sung_pitch2 = round(sung_pitch2) %% 12,
    pc_sung_pitch3 = round(sung_pitch3) %% 12,
    pc_sung_pitch4 = round(sung_pitch4) %% 12,
    pc_sung_pitch5 = round(sung_pitch5) %% 12
  ) %>%
  ungroup()


# Create long format
data_melodies_recency_long <- data_melodies_recency %>%
  select(id, degree, network_id, key_pc, starts_with("pc_sung")) %>%
  pivot_longer(cols = starts_with("pc_sung"),
               names_to = "note_position",
               names_prefix = "sung_pitch",
               values_to = "pitch_pc") %>%
  mutate(
    note_idx = as.integer(str_extract(note_position, "[1-5]")),
    interval = (pitch_pc - key_pc) %% 12,
    distance_to_key = pmin(interval, 12 - interval)  # circular distance
  )

# For each melody and degree, select which note_idx is closest to the key
closest_note_per_melody <- data_melodies_recency_long %>%
  group_by(id, degree) %>%
  slice_min(distance_to_key, n = 1, with_ties = FALSE) %>%
  ungroup()

#  Count how many times each note is closest per degree
note_closest_counts <- closest_note_per_melody %>%
  group_by(degree, note_idx) %>%
  summarise(count = n(), .groups = "drop")

# Compute proportions and standard errors via a binomial model
note_counts_with_total <- note_closest_counts %>%
  group_by(degree) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(
    prop = count / total,
    se = sqrt(prop * (1 - prop) / total)
  )

# Normalize by baseline
baseline <- note_counts_with_total %>%
  filter(degree == 0) %>%
  select(note_idx, baseline_prop = prop)

# Plot with error bars
note_counts_norm <- note_counts_with_total %>%
  left_join(baseline, by = "note_idx") %>%
  mutate(
    prop_diff = prop - baseline_prop
  ) %>% 
  filter(degree < 11)

# plot
plot_change_closest_to_key_note <- ggplot(
  note_counts_norm, 
  aes(x = degree, y = prop_diff, 
      group = factor(note_idx),
      fill = factor(note_idx))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line()+
  geom_ribbon(aes(ymin = prop_diff - se, ymax = prop_diff + se, fill = factor(note_idx)), alpha = 0.3) +
  geom_point(aes(fill = factor(note_idx)), size = 2, shape=21) +
  labs(
    x = "Iteration",
    y = "Normalized proportion (from seed)",
    color = "Note"
  ) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 8), 
    legend.title = element_blank(),
    title = element_text(size = 10)
  )

plot_change_closest_to_key_note

ggsave("results/figure2-tonality/closest_note_to_key_evolution.png", width = 12, height = 8, units = "cm", bg = "white")


# plot distribution of closest note to key over iterations
plot_distribution_closest_to_key_note <-ggplot(note_position_counts, aes(x = factor(note_idx), y = prop, fill = factor(degree))) +
  geom_col(position = "dodge") +
  labs(title = "Distribution of closest note to key",
       x = "Note Position in Melody",
       y = "Proportion",
       fill = "Iteration") +
  theme_classic() +
  facet_wrap(~degree) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 8), 
    # axis.text.y = element_text(size = 8),
    title = element_text(size = 10)
  )


plot_distribution_closest_to_key_note

# ggsave("results/figure2-tonality/closest_note_to_key.png", width = 12, height = 8, units = "cm", bg = "white")



# --------------------------------------------------------------------------------
# CALCULATE ENTROPY VIA BOOTSTEAP
# --------------------------------------------------------------------------------

# Bootstrap entropy for each degree
NBOOT = 100
degrees <- sort(unique(data_melodies_recency_long$degree))
results <- list()

for (i in 1:NBOOT) {
  message(paste("Bootstrap", i, "of", NBOOT))
  
  # Sample networks with replacement
  data_sample <- data_melodies_recency_long %>%
    group_by(network_id) %>%
    group_split() %>%
    sample(length(.), replace = TRUE) %>%
    bind_rows()
  
  # Find which note is closest to the key
  closest_note <- data_sample %>%
    group_by(id) %>%
    slice_min(distance_to_key, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Prepare storage for entropy values per degree
  boot.entropies <- numeric(length(degrees))
  
  for (d in seq_along(degrees)) {
    degree.d <- degrees[d]
    
    closest_note_degree <- closest_note %>%
      filter(degree == degree.d)
    
    if (nrow(closest_note_degree) > 0) {
      note_position_counts <- closest_note_degree %>%
        group_by(note_idx) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(prop = count / sum(count))
      
      entropy_val <- -sum(note_position_counts$prop * log2(note_position_counts$prop))
    } else {
      entropy_val <- NA  # Handle empty case
    }
    
    boot.entropies[d] <- entropy_val
  }
  
  results[[i]] <- tibble(
    boot = i,
    degree = degrees,
    entropy = boot.entropies
  )
}
  
entropy_boot_df <- bind_rows(results)

# sum
sum_entropy_boot_df <- entropy_boot_df %>% 
  group_by(degree) %>%
  summarise(
    mean_entropy = mean(entropy, na.rm = TRUE),
    sd_entropy = sd(entropy, na.rm = TRUE),
    se_entropy = sd_entropy / sqrt(n()),
    lower = mean_entropy - qt(0.975, df = n() - 1) * se_entropy,
    upper = mean_entropy + qt(0.975, df = n() - 1) * se_entropy,
    .groups = "drop"
  )

plot_entropy = sum_entropy_boot_df %>% 
  filter(degree < 11) %>%
  mutate(degree=as.factor(degree)) %>% 
  ggplot(aes(degree, mean_entropy, group=1)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean_entropy-sd_entropy, 
                  ymax=mean_entropy+sd_entropy), 
              fill = "#0072B2", alpha = 0.3) +
  geom_point(fill = "white", size = 2, shape=21) +
  ggtitle("Entropy of closest-to-note key") +
  xlab("Iteration") +
  ylab("Entropy (bits)") +
  theme(aspect.ratio = 1,
        axis.text.x = element_text(size=8),
        axis.text.y=element_text(size=8),
        title = element_text(size = 10),
        legend.position = "none",
        legend.title = element_blank())

plot_entropy

ggsave("results/figure2-tonality/closest_note_to_key_entropy.png", width = 8, height = 8, units = "cm", bg = "white")


################################################################################
# Last note duration
################################################################################
NBOOT = 1000
BW = 0.25

data_melodies_durations = data_melodies %>% 
  select(id:degree, target_note_durations) %>% 
  mutate(target_note_durations = map(target_note_durations, ~ as.numeric(str_split(.x, ",")[[1]]))) %>% 
  # separate target_note_duraitons into columns
  unnest_wider(target_note_durations, names_sep = "_") %>% 
  mutate(tot = rowSums(across(starts_with("target_note_durations_")), na.rm = TRUE)) %>%
  mutate(across(starts_with("target_note_durations_"), ~ .x / tot, .names = "norm_{.col}"))

# plot target_note_durations_norm columns over degree
data_melodies_durations_long <- data_melodies_durations %>%
  select(id, degree, network_id, tot, starts_with("norm_target_note_durations_")) %>%
  pivot_longer(cols = starts_with("norm_target_note_durations_"),
               names_to = "note_position",
               names_prefix = "norm_target_note_durations_",
               values_to = "duration")

# aggregate duration note position over degree
data_melodies_durations_sum = data_melodies_durations_long %>% 
  group_by(degree, note_position) %>%
  summarise(
    n = n(),
    mean = mean(duration, na.rm = TRUE),
    sd = sd(duration, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  ) 


# plot duration note position over degree
plot_durations <- ggplot(data_melodies_durations_sum, aes(x = degree, y = mean, group = note_position, fill = note_position)) +
  geom_line()+
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = note_position), alpha = 0.3) +
  geom_point(aes(fill = note_position), size = 2, shape=21) +
  labs(x = "Iteration",
       y = "Duration (normalized)",
       color = "Note Position") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 8), 
    # axis.text.y = element_text(size = 8),
    title = element_text(size = 10)
  )

plot_durations

ggsave("results/figure2-tonality/last_note_duration.png", width = 12, height = 8, units = "cm", bg = "white")

