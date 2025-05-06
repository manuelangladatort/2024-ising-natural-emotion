# explore key finding using bayseian approach fr m21

# try the basyian results
key_estimation_results <- read_csv("tonality-explore/key_estimation_results.csv") %>% 
  select(id:participant_id, peter_mode, peter_estimated_key, daniel_mode, daniel_estimated_key,
         m21_key:m21_key_full, sung_pitch1:sung_pitch5) 

table(key_estimation_results$m21_key)

# Step 1: Define enharmonic mapping
enharmonic_map <- c(
  "C" = "C", "B#" = "C",
  "C#" = "C#", "D-" = "C#",
  "D" = "D",
  "D#" = "D#", "E-" = "D#",
  "E" = "E", "F-" = "E",
  "F" = "F", "E#" = "F",
  "F#" = "F#", "G-" = "F#",
  "G" = "G",
  "G#" = "G#", "A-" = "G#",
  "A" = "A",
  "A#" = "A#", "B-" = "A#",
  "B" = "B", "C-" = "B"
)

# Step 2: Apply mapping to normalize key names
key_estimation_results <- key_estimation_results %>%
  mutate(m21_key_normalized = recode(m21_key, !!!enharmonic_map))

table(key_estimation_results$m21_key_normalized)

# plot m21
pitch_class_order <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")

key_estimation_results <- key_estimation_results %>%
  mutate(m21_key_normalized = toupper(m21_key_normalized),
         m21_key_normalized = factor(m21_key_normalized, levels = pitch_class_order))

key_estimation_results %>% 
  filter(degree == 0) %>% 
  ggplot(aes(x = m21_key_normalized)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Estimated Keys (m21)",
       x = "Estimated Key",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ m21_mode)

key_estimation_results %>% 
  ggplot(aes(x = m21_key)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Estimated Keys (Daniel)",
       x = "Estimated Key",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ m21_mode) +
  facet_wrap(~ degree)


################################################################################
# Histograms
################################################################################

# calculate key profile for each melody (ROUNDING)

# Step 1: Define a mapping from key name to pitch class (C=0, ..., B=11)
key_to_pc <- c("C"=0, "C#"=1, "D"=2, "D#"=3, "E"=4, "F"=5, "F#"=6, "G"=7, "G#"=8, "A"=9, "A#"=10, "B"=11)

# Step 2: Add a column with the pitch class of the estimated key
data_melodies_tonality_degree0 <- key_estimation_results %>% 
  filter(degree == 0) %>% 
  mutate(key_pc = key_to_pc[toupper(m21_key_normalized)])

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
  select(m21_mode, all_of(diff_cols)) %>%
  pivot_longer(cols = starts_with("sung_pitch"), names_to = "note", values_to = "pc_diff") %>%
  mutate(pc_diff = factor(pc_diff, levels = 0:11))

# Normalize frequency counts within each mode
key_profile_norm <- key_profile %>%
  group_by(m21_mode, pc_diff) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(m21_mode) %>%
  mutate(proportion = count / sum(count))

# Step 5: Plot histogram of pitch class differences
ggplot(key_profile, aes(x = factor(pc_diff, levels = 0:11))) +
  geom_bar(fill = "skyblue", color = "black") +
  scale_x_discrete(name = "Pitch Class Difference (Note - Key) mod 12") +
  labs(title = "Estimated Key Profile Across All Melodies",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ m21_mode) 

# save
ggsave("tonality-explore/key-profile-degree0-hist-m21.png", width = 10, height = 6, bg = "white")


################################################################################
# Key profiles
################################################################################
NBOOT = 1000
BW = 0.25
MAX_INTERVAL_SIZE = 12
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

key_to_pc <- c("C"=0, "C#"=1, "D"=2, "D#"=3, "E"=4, "F"=5, "F#"=6, "G"=7, "G#"=8, "A"=9, "A#"=10, "B"=11)

hist(key_estimation_results$m21_confidence)

# discrete
diff_data <-  key_estimation_results %>%
  filter(m21_confidence > 0.6) %>% 
  select(id:degree, m21_key_normalized, m21_mode, sung_pitch1:sung_pitch5) %>% 
  mutate(key_pc = key_to_pc[toupper(m21_key_normalized)]) %>% 
  relocate(key_pc, .after = m21_key_normalized) %>%
  mutate(across(starts_with("sung_pitch"), ~ round(.) %% 12, .names = "pc_{.col}")) %>% 
  mutate(across(starts_with("pc_sung_pitch"), ~ . - key_pc, .names = "diff_from_key_{.col}")) 

# continuous
# diff_data <-  data_melodies %>%
#   select(id:degree, estimated_key, estimated_tonic, mode, sung_pitch1:sung_pitch5) %>% 
#   mutate(key_pc = key_to_pc[toupper(estimated_key)]) %>% 
#   relocate(key_pc, .after = estimated_key) %>%
#   mutate(across(starts_with("sung_pitch"), ~ (. - key_pc) %% 12, .names = "diff_from_key_pc_{.col}"))


diff_data_long <-  diff_data %>%
  pivot_longer(cols = starts_with("diff_from_key_pc_sung_"),
               names_to = "interval_pos",
               values_to = "interval")

marginals_interval_all_key = make_marginals_kde(diff_data_long, c("interval"), NBOOT, BW, "Note to Key interval") 

marginals_interval_all_key

ggsave("tonality-explore/m21_notes_to_key_intervals_all.png", width = 14, height = 7, units = "cm")


# evolution
vertical.lines = c(-12, -7, -5, -3, 0, 3, 5, 7, 12)

marginals_key_profiles_evolution = diff_data_long %>% 
  # select few iterations for visualization
  filter(degree == 0 | degree == 2 | degree == 4 | degree == 6 | degree == 8 | degree == 10) %>%
  ggplot(aes(x = interval)) +
  geom_density(bw=BW)  +
  scale_x_continuous(breaks=vertical.lines,
                     limits = c(min(vertical.lines), max(vertical.lines)))  + 
  geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
  facet_wrap(~ degree, nrow = 1) +
  theme(axis.text.x = element_text(size=7), 
        axis.text.y=element_text(size=7), 
        axis.title.x = element_blank(), 
        axis.title.y =element_blank())

marginals_key_profiles_evolution



################################################################################
# tonal confidence
################################################################################

tonality_data = key_estimation_results %>% 
  group_by(degree) %>% 
  summarise(
    n = n(), 
    mean_confidence = mean(m21_confidence, na.rm = T),
    sd_confidence = sd(m21_confidence, na.rm = T),
    se_confidence = sd_confidence/sqrt(n),
  )

# plot tonal features
plot_tonalness = tonality_data %>%
  filter(degree < 11) %>% 
  ggplot(aes(x= degree, y = mean_confidence)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_confidence - se_confidence, 
                  ymax=mean_confidence + se_confidence),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  # add labels
  ylab("Tonal Confidence (M21)") +
  xlab("Iteration") +
  ggtitle("Tonal Confidence") +
  theme_classic() +
  theme(legend.position = "none") 

plot_tonalness

ggsave("tonality-explore/m21_confidence_overtime.png", width = 7, height = 7, units = "cm")
