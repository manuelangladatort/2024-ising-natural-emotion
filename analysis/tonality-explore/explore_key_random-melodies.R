# import 
library(tidyverse)
source("utils/key-operations.R") # methods for key analysis

# data
data_melodies <- read_csv("data/final-data/sing-scales-v2/data-sing-scales-v2_full.csv")


################################################################################
# Tonality analysis
################################################################################
# only random melodies
data_melodies_tonality = data_melodies %>% 
  select(id:participant_id, sung_pitch1:sung_pitch5, sung_note_durations)


tonal_features_list_peter <- apply_key_finding(data_melodies_tonality, kk_all_key_profiles)
tonal_features_list_daniel <- apply_key_finding_fantastic(data_melodies_tonality, major_profile_KS, minor_profile_KS)

tonal_features_peter = do.call(rbind, tonal_features_list_peter)
tonal_features_daniel = do.call(rbind, tonal_features_list_daniel)


data_melodies_tonality$peter_tonalness = tonal_features_peter$tonalness
data_melodies_tonality$peter_mode = tonal_features_peter$mode
data_melodies_tonality$peter_estimated_key = tonal_features_peter$estimated_key
data_melodies_tonality$peter_estimated_tonic = tonal_features_peter$estimated_tonic
data_melodies_tonality$pter_tonal.clarity = tonal_features_peter$tonal.clarity
data_melodies_tonality$pter_tonal.spike = tonal_features_peter$tonal.spike

data_melodies_tonality$daniel_tonalness = tonal_features_daniel$tonalness
data_melodies_tonality$daniel_mode = tonal_features_daniel$mode
data_melodies_tonality$daniel_estimated_key = tonal_features_daniel$estimated_key

# write_csv(data_melodies_tonality, "data-sing-scales-v2_full-tonality.csv")

# plot peter
pitch_class_order <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
tonal_features_peter <- data_melodies_tonality %>%
  mutate(peter_estimated_key = toupper(peter_estimated_key),
         peter_estimated_key = factor(peter_estimated_key, levels = pitch_class_order))

tonal_features_peter %>% 
  filter(degree == 0) %>% 
  ggplot(aes(x = peter_estimated_key)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Estimated Keys (Peter)",
       x = "Estimated Key",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ peter_mode)

ggsave("tonality-explore/key-profile-degree0-hist-peter.png", width = 10, height = 6, bg = "white")


# plot daniel
tonal_features_daniel <- data_melodies_tonality %>%
  mutate(daniel_estimated_key = toupper(daniel_estimated_key),
         daniel_estimated_key = factor(daniel_estimated_key, levels = pitch_class_order))

tonal_features_daniel %>% 
  filter(degree == 0) %>% 
  ggplot(aes(x = daniel_estimated_key)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Estimated Keys (Daniel)",
       x = "Estimated Key",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ daniel_mode)

ggsave("tonality-explore/key-profile-degree0-hist-daniel.png", width = 10, height = 6, bg = "white")


################################################################################
# Histograms
################################################################################

# calculate key profile for each melody (ROUNDING)

# Step 1: Define a mapping from key name to pitch class (C=0, ..., B=11)
key_to_pc <- c("C"=0, "C#"=1, "D"=2, "D#"=3, "E"=4, "F"=5, "F#"=6, "G"=7, "G#"=8, "A"=9, "A#"=10, "B"=11)

# Step 2: Add a column with the pitch class of the estimated key
data_melodies_tonality_degree0 <- data_melodies_tonality %>% 
  filter(degree == 0) %>% 
  mutate(key_pc = key_to_pc[toupper(peter_estimated_key)])

# Step 3: Convert sung pitches to pitch class (rounded MIDI notes), then compute (note_pc - key_pc) %% 12
note_cols <- paste0("sung_pitch", 1:5)

for (col in note_cols) {
  data_melodies_tonality_degree0 <- data_melodies_tonality_degree0 %>%
    mutate(
      !!paste0(col, "_pc") := round(.data[[col]]) %% 12,
      !!paste0(col, "_diff_from_key") := (.data[[paste0(col, "_pc")]] - key_pc) %% 12
    )
}


# Step 4: Gather all pitch class differences into a single vector
diff_cols <- paste0("sung_pitch", 1:5, "_diff_from_key")

key_profile <- data_melodies_tonality_degree0 %>%
  select(peter_mode, all_of(diff_cols)) %>%
  pivot_longer(cols = starts_with("sung_pitch"), names_to = "note", values_to = "pc_diff") %>%
  mutate(pc_diff = factor(pc_diff, levels = 0:11))

# Normalize frequency counts within each mode
key_profile_norm <- key_profile %>%
  group_by(peter_mode, pc_diff) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(peter_mode) %>%
  mutate(proportion = count / sum(count))

# Step 5: Plot histogram of pitch class differences
ggplot(key_profile, aes(x = factor(pc_diff, levels = 0:11))) +
  geom_bar(fill = "skyblue", color = "black") +
  scale_x_discrete(name = "Pitch Class Difference (Note - Key) mod 12") +
  labs(title = "Estimated Key Profile Across All Melodies",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ peter_mode) 

# save
ggsave("tonality-explore/key-profile-degree0-hist-peter.png", width = 10, height = 6, bg = "white")


# Step 5: Plot as circular bar chart
ggplot(key_profile_norm, aes(x = pc_diff, y = proportion, fill = peter_mode)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  coord_polar(start = -pi/12) +
  facet_wrap(~ peter_mode) +
  labs(title = "Circular Estimated Key Profile by Mode",
       x = "Pitch Class Difference (mod 12)",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))



# calculate key profile for each melody (CONTINOUS)

# Step 1: Define a mapping from key name to pitch class (C=0, ..., B=11)
key_to_pc <- c("C"=0, "C#"=1, "D"=2, "D#"=3, "E"=4, "F"=5, "F#"=6,
               "G"=7, "G#"=8, "A"=9, "A#"=10, "B"=11)

# Step 2: Add pitch class of estimated key
data_melodies_degree0 <- data_melodies_degree0 %>%
  mutate(key_pc = key_to_pc[toupper(estimated_key_peter)])

# Step 3: Calculate continuous pitch class difference for each note
note_cols <- paste0("sung_pitch", 1:5)

for (col in note_cols) {
  data_melodies_degree0 <- data_melodies_degree0 %>%
    #  continuous pitch class difference between the sung pitch and the estimated key
    mutate(!!paste0(col, "_diff_from_key") := (.data[[col]] - key_pc) %% 12)
}

# Step 4: Gather continuous pitch class differences into long format
diff_cols <- paste0(note_cols, "_diff_from_key")

key_profile_cont <- data_melodies_degree0 %>%
  select(mode_peter, all_of(diff_cols)) %>%
  pivot_longer(cols = all_of(diff_cols), names_to = "note", values_to = "pc_diff")

# Step 5: Plot density instead of histogram
ggplot(key_profile_cont, aes(x = pc_diff, color = mode_peter, fill = mode_peter)) +
  geom_density(alpha = 0.3, bw = 0.25) +  # Set bandwidth explicitly here
  scale_x_continuous(name = "Pitch Class Difference (Note - Key) mod 12",
                     breaks = 0:12, limits = c(0, 12)) +
  labs(title = "Continuous Estimated Key Profile",
       y = "Density") +
  # add vertical lines at 3, 5, 7 and 12
  geom_vline(xintercept = c(0, 3, 4, 5, 6, 7, 12), linetype = "dashed", color = "lightgrey") +
  theme_classic() +
  facet_wrap(~ mode_peter) 


# Circular density plot
ggplot(key_profile_cont, aes(x = pc_diff, fill = mode_peter, color = mode_peter)) +
  geom_density(alpha = 0.4, bw = 0.2) +
  scale_x_continuous(
    name = "Pitch Class Difference (mod 12)",
    breaks = 0:11,
    limits = c(0, 12),
    expand = c(0, 0)
  ) +
  coord_polar(start = -pi/2, direction = 1) +
  labs(
    title = "Circular Density of Pitch Class Differences",
    y = "Density (Radial Axis)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  facet_wrap(~ mode_peter)



################################################################################
# Closest-to-key note analysis
################################################################################
data_melodies_tonality_note <- data_melodies_tonality %>% 
  select(id:degree, sung_pitch1:sung_pitch5, 
         peter_mode, peter_estimated_key, daniel_mode, daniel_estimated_key) %>%
  mutate(peter_key_pc = key_to_pc[toupper(peter_estimated_key)]) %>% 
  mutate(daniel_key_pc = key_to_pc[toupper(daniel_estimated_key)]) %>% 
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
data_melodies_tonality_note_long <- data_melodies_tonality_note %>%
  select(id, degree, network_id, daniel_key_pc, starts_with("pc_sung")) %>%
  pivot_longer(cols = starts_with("pc_sung"),
               names_to = "note_position",
               names_prefix = "sung_pitch",
               values_to = "pitch_pc") %>%
  mutate(
    note_idx = as.integer(str_extract(note_position, "[1-5]")),
    interval = (pitch_pc - daniel_key_pc) %% 12,
    distance_to_key = pmin(interval, 12 - interval)  # circular distance
  )

# Find which note is closest to the key
closest_note <- data_melodies_tonality_note_long %>%
  group_by(id) %>%
  slice_min(distance_to_key, n = 1, with_ties = FALSE) %>%
  ungroup()


# Count how often each position is closest to the key
note_position_counts <- closest_note %>%
  group_by(degree, note_idx) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(degree) %>%
  mutate(prop = count / sum(count))

# Compute entropy
entropy_df <- note_position_counts %>%
  group_by(degree) %>%
  summarise(entropy = -sum(prop * log2(prop)), .groups = "drop")

# plot 1: distribution of closest note to key over iterations
ggplot(note_position_counts, aes(x = factor(note_idx), y = prop, fill = factor(degree))) +
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
