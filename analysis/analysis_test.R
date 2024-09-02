################################################################################
# Iterated singing (naturalistic)
# Script: Analysis
################################################################################
# import libraries
library(tidyverse)
library(egg)
library(ggpubr)
library(cowplot)

# global parameters
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 12
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

# import data
data_v1 <- read_csv("data/data-clean/sing-scales-v1/sing-scales-v1_full.csv")


################################################################################
# Analysis - error
################################################################################

# calculate the mean interval error over generations
error_data = data_v1 %>% 
  group_by(degree) %>% 
  summarise(
    n = n(), 
    mean_pitch_interval_error = mean(root_mean_squared_interval, na.rm = T),
    sd_pitch_interval_error = sd(root_mean_squared_interval, na.rm = T),
    se_pitch_interval_error = sd_pitch_interval_error/sqrt(n),
    mean_note_duration_error = mean(note_duration_root_mean_squared, na.rm = T),
    sd_note_duration_error = sd(note_duration_root_mean_squared, na.rm = T),
    se_note_duration_error = sd_note_duration_error/sqrt(n)
  )

# plot pitch interval error
plot_pitch_interval_error = error_data %>%
  ggplot(aes(x= degree, y = mean_pitch_interval_error)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_pitch_interval_error - se_pitch_interval_error, 
                  ymax=mean_pitch_interval_error + se_pitch_interval_error),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  # add labels
  ylab("Copying Error (Pitch Interval)") +
  xlab("Iteration") +
  ggtitle("Copying Error (pitch interval)") +
  theme_classic() +
  theme(legend.position = "none")

plot_pitch_interval_error


# plot note duration error
plot_note_duration_error = error_data %>%
  ggplot(aes(x= degree, y = mean_note_duration_error)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_note_duration_error - se_note_duration_error, 
                  ymax=mean_note_duration_error + se_note_duration_error),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  # add labels
  ylab("Copying Error (Note Duration)") +
  xlab("Iteration") +
  ggtitle("Copying Error (note duration)") +
  theme_classic() +
  theme(legend.position = "none")

plot_note_duration_error

plot_grid(plot_pitch_interval_error, plot_note_duration_error, nrow = 1)

ggsave("evolution_error.png", width = 16, height = 8, units = "cm")


################################################################################
# Analysis - scale
################################################################################


# 1. Round the MIDI notes to the nearest integer
data_v1_round <- data_v1 %>%
  select(id:register, sung_pitch1:sung_pitch5) %>%
  mutate(across(starts_with("sung_pitch"), round))

# 2. Transpose all melodies to start with middle C (MIDI 60)
transpose_to_c4 <- function(row) {
  transposition_interval <- 60 - row[1]  # Calculate the interval to transpose the first note to C4
  row + transposition_interval           # Apply the interval to the entire row
}

data_v1_transposed <- data_v1_round %>%
  rowwise() %>%  # Operate on each row individually
  mutate(
    transposed = list(transpose_to_c4(c_across(starts_with("sung_pitch"))))
  ) %>%
  ungroup() %>%
  mutate(
    sung_pitch1 = map_dbl(transposed, 1),
    sung_pitch2 = map_dbl(transposed, 2),
    sung_pitch3 = map_dbl(transposed, 3),
    sung_pitch4 = map_dbl(transposed, 4),
    sung_pitch5 = map_dbl(transposed, 5)
  ) %>%
  select(-transposed) 

# 3. Convert MIDI notes to letter sequences
midi_to_pitch_class <- function(midi_note) {
  pitch_classes <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  pitch_classes[(midi_note %% 12) + 1]
}

data_v1_sequenced <- data_v1_transposed %>%
  mutate(across(starts_with("sung_pitch"), midi_to_pitch_class))


# histogram of scales
hist_scales_data <- data_v1_sequenced %>%
  mutate(melody_sequence = paste0(sung_pitch1, sung_pitch2, sung_pitch3, sung_pitch4, sung_pitch5)) %>% 
  count(melody_sequence) %>%  # Count the number of occurrences of each sequence
  arrange(desc(n)) %>%  # Arrange sequences by their frequency
  slice_head(n = 10)  # Select the top 10 sequences

ggplot(hist_scales_data, aes(x = reorder(melody_sequence, -n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Melody Sequence", y = "Number of Melodies", title = "Histogram of Melody Sequences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

ggsave("histogram_scales.png", width = 16, height = 8, units = "cm")

# transition probabilities between nodes
# 1. Create a transition matrix
transitions <- melody_df %>%
  select(network_id, sung_pitch1, sung_pitch2, sung_pitch3, sung_pitch4, sung_pitch5) %>%
  pivot_longer(cols = sung_pitch1:sung_pitch5, names_to = "position", values_to = "note") %>%
  group_by(network_id) %>%  # Assuming 'id' is the unique identifier for each melody
  mutate(next_note = lead(note)) %>%  # Create a column for the next note in the sequence
  filter(!is.na(next_note)) %>%  # Remove the last note since it has no next note
  count(note, next_note) %>%  # Count transitions from one note to the next
  ungroup()

# 2. Calculate transition probabilities
transition_probabilities <- transitions %>%
  group_by(note) %>%
  mutate(probability = n / sum(n)) %>%  # Calculate the probability of each transition
  ungroup() %>% 
  select(-network_id)

write.csv(transition_probabilities, "transition_probabilities.csv", row.names = FALSE)
