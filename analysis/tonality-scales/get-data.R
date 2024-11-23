################################################################################
# Iterated singing (naturalistic)
# Script: Prepare data from psynet 11 export
################################################################################
# imports
library(tidyverse)
library(egg)
library(ggpubr)
# library(cowplot)
# library(viridis)

# methods
source("utils/utils.R")  # prepare data
# source("utils/plots.R")  # methods for plotting

# global parameters
set.seed(2024)
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 12
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)


################################################################################
# Batch 2: iterated singing (naturalistic) - 200 nodes (across-chains)
################################################################################
data_nodes <- read_csv("data/data-clean/sing-scales-v2/sing-scales-v2_node_data.csv")
data_trials <- read_csv("data/data-clean/sing-scales-v2/sing-scales-v2_trial_data.csv")

length(table(data_nodes$network_id))
table(data_nodes$degree)
length(table(data_trials$network_id))
length(table(data_trials$degree))

data_nets = prepare_trial_data(data_nodes[,-1], data_trials[,-1]) 

length(unique(data_nets$participant_id)) # 148
length(unique(data_nets$network_id)) # 200
table(data_nets$degree) 
table(data_nets$trial_maker_id)

data_nets <- data_nets %>% 
  mutate(trial_type = ifelse(degree != 0, "node_trial", "source_trial")) %>%
  select(-definition, -seed, -pitch_stats, -time_stats, -reason)

# Pitch and duration operations

# 1. Round the MIDI notes to the nearest integer
data_v1_round <- data_nets %>%
  mutate(across(starts_with("sung_pitch"), round, .names = "round_{.col}"))

# 2. Transpose all melodies to start with middle C (MIDI 60)
transpose_to_c4 <- function(row) {
  transposition_interval <- 60 - row[1]  # Calculate the interval to transpose the first note to C4
  row + transposition_interval           # Apply the interval to the entire row
}

data_v1_transposed <- data_v1_round %>%
  rowwise() %>%  
  mutate(
    transposed = list(transpose_to_c4(c_across(starts_with("round_sung_pitch"))))
  ) %>%
  ungroup() %>%
  mutate(
    round_sung_pitch1 = map_dbl(transposed, 1),
    round_sung_pitch2 = map_dbl(transposed, 2),
    round_sung_pitch3 = map_dbl(transposed, 3),
    round_sung_pitch4 = map_dbl(transposed, 4),
    round_sung_pitch5 = map_dbl(transposed, 5)
  ) %>%
  select(-transposed) 

# Calculate note duration in metrical level
data_v1_final <- data_v1_transposed %>%
  rowwise() %>%
  mutate(total_duration = sum(c_across(starts_with("sung_note_duration")))) %>% 
  # Calculate metrical time for each note
  mutate(
    sung_note_duration_metrical1 = (sung_note_duration1 / total_duration) * 5,
    sung_note_duration_metrical2 = (sung_note_duration2 / total_duration) * 5,
    sung_note_duration_metrical3 = (sung_note_duration3 / total_duration) * 5,
    sung_note_duration_metrical4 = (sung_note_duration4 / total_duration) * 5,
    sung_note_duration_metrical5 = (sung_note_duration5 / total_duration) * 5
  ) %>% 
  ungroup()


# save
data_v1_final %>% 
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = ',')) %>% 
  write.csv("data/data-clean/sing-scales-v2/data-sing-scales-v2_full.csv", row.names = FALSE)


################################################################################
# PILOT (Batch 1): iterated singing (naturalistic) - 100 nodes (across-chains)
################################################################################
data_nodes <- read_csv("data/data-clean/sing-scales-v1/sing-scales-v1_node_data.csv")
data_trials <- read_csv("data/data-clean/sing-scales-v1/sing-scales-v1_trial_data.csv")

length(table(data_nodes$network_id))
table(data_nodes$degree)
length(table(data_trials$network_id))
length(table(data_trials$degree))

data_nets = prepare_trial_data(data_nodes[,-1], data_trials[,-1]) 

length(unique(data_nets$participant_id)) # 64
length(unique(data_nets$network_id)) # 100
table(data_nets$degree) 
table(data_nets$trial_maker_id)

data_nets <- data_nets %>% 
  mutate(trial_type = ifelse(degree != 0, "node_trial", "source_trial")) %>%
  select(-definition, -seed, -pitch_stats, -time_stats, -reason)

# Pitch and duration operations

# 1. Round the MIDI notes to the nearest integer
data_v1_round <- data_nets %>%
  mutate(across(starts_with("sung_pitch"), round, .names = "round_{.col}"))

# 2. Transpose all melodies to start with middle C (MIDI 60)
transpose_to_c4 <- function(row) {
  transposition_interval <- 60 - row[1]  # Calculate the interval to transpose the first note to C4
  row + transposition_interval           # Apply the interval to the entire row
}

data_v1_transposed <- data_v1_round %>%
  rowwise() %>%  
  mutate(
    transposed = list(transpose_to_c4(c_across(starts_with("round_sung_pitch"))))
  ) %>%
  ungroup() %>%
  mutate(
    round_sung_pitch1 = map_dbl(transposed, 1),
    round_sung_pitch2 = map_dbl(transposed, 2),
    round_sung_pitch3 = map_dbl(transposed, 3),
    round_sung_pitch4 = map_dbl(transposed, 4),
    round_sung_pitch5 = map_dbl(transposed, 5)
  ) %>%
  select(-transposed) 

# Calculate note duration in metrical level
data_v1_final <- data_v1_transposed %>%
  rowwise() %>%
  mutate(total_duration = sum(c_across(starts_with("sung_note_duration")))) %>% 
  # Calculate metrical time for each note
  mutate(
    sung_note_duration_metrical1 = (sung_note_duration1 / total_duration) * 5,
    sung_note_duration_metrical2 = (sung_note_duration2 / total_duration) * 5,
    sung_note_duration_metrical3 = (sung_note_duration3 / total_duration) * 5,
    sung_note_duration_metrical4 = (sung_note_duration4 / total_duration) * 5,
    sung_note_duration_metrical5 = (sung_note_duration5 / total_duration) * 5
  ) %>% 
  ungroup()


# save
data_v1_final %>% 
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = ',')) %>% 
  write.csv("data/data-clean/sing-scales-v1/data-sing-scales-v1_full.csv", row.names = FALSE)

