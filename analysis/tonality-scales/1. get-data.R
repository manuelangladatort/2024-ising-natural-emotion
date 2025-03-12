################################################################################
# Iterated singing (naturalistic)
# Script: Prepare data from psynet 11 export
################################################################################
# imports
library(tidyverse)
library(egg)
library(ggpubr)

# methods
source("utils/utils.R")  # prepare data


# global parameters
set.seed(2024)
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 12
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)


################################################################################
# Varying number of notes: iterated singing (naturalistic) - 200 nodes (across-chains), starting melodies from 4 to 13 notes (20 melodies in each condition)
################################################################################
data_nodes <- read_csv("data/final-data/sing-varnotes-v1/sing-varnotes-v1_node_data.csv")
data_trials <- read_csv("data/final-data/sing-varnotes-v1/sing-varnotes-v1_trial_data.csv")

length(table(data_nodes$network_id))
table(data_nodes$degree)
length(table(data_trials$network_id))
length(table(data_trials$degree))

data_nets = prepare_trial_data(data_nodes[,-1], data_trials[,-1]) 

length(unique(data_nets$participant_id)) # 124
length(unique(data_nets$network_id)) # 200
table(data_nets$degree) 
table(data_nets$trial_maker_id)

data_nets <- data_nets %>% 
  mutate(trial_type = ifelse(degree != 0, "node_trial", "source_trial")) %>%
  select(-definition, -seed, -pitch_stats, -time_stats, -reason)

# Pitch and duration operations

# # 1. Round the MIDI notes to the nearest integer
# data_v1_round <- data_nets %>%
#   mutate(across(starts_with("sung_pitch"), round, .names = "round_{.col}"))
# 
# # 2. Transpose all melodies to start with middle C (MIDI 60)
# transpose_to_c4 <- function(row) {
#   transposition_interval <- 60 - row[1]  # Calculate the interval to transpose the first note to C4
#   row + transposition_interval           # Apply the interval to the entire row
# }

# data_v1_transposed <- data_v1_round %>%
#   rowwise() %>%  
#   mutate(
#     transposed = list(transpose_to_c4(c_across(starts_with("round_sung_pitch"))))
#   ) %>%
#   ungroup() %>%
#   mutate(
#     round_sung_pitch1 = map_dbl(transposed, 1),
#     round_sung_pitch2 = map_dbl(transposed, 2),
#     round_sung_pitch3 = map_dbl(transposed, 3),
#     round_sung_pitch4 = map_dbl(transposed, 4),
#     round_sung_pitch5 = map_dbl(transposed, 5)
#   ) %>%
#   select(-transposed) 


# save
data_nets %>% 
# data_v1_transposed %>% 
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = ',')) %>% 
  write.csv("data/final-data/sing-varnotes-v1/data-sing-varnotes-v1_full.csv", row.names = FALSE)


################################################################################
# Batch 2: iterated singing (naturalistic) - 200 nodes (across-chains)
################################################################################
data_nodes <- read_csv("data/final-data/sing-scales-v2/sing-scales-v2_node_data.csv")
data_trials <- read_csv("data/final-data/sing-scales-v2/sing-scales-v2_trial_data.csv")

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


# # 1. Round the MIDI notes to the nearest integer
# # data_v1_round <- data_nets %>%
# #   mutate(across(starts_with("sung_pitch"), round, .names = "round_{.col}"))
# 
# # # 2. Transpose all melodies to start with middle C (MIDI 60)
# # transpose_to_c4 <- function(row) {
# #   transposition_interval <- 60 - row[1]  # Calculate the interval to transpose the first note to C4
# #   row + transposition_interval           # Apply the interval to the entire row
# # }
# 
# # data_v1_transposed <- data_v1_round %>%
# #   rowwise() %>%  
# #   mutate(
# #     transposed = list(transpose_to_c4(c_across(starts_with("round_sung_pitch"))))
# #   ) %>%
# #   ungroup() %>%
# #   mutate(
# #     round_sung_pitch1 = map_dbl(transposed, 1),
# #     round_sung_pitch2 = map_dbl(transposed, 2),
# #     round_sung_pitch3 = map_dbl(transposed, 3),
# #     round_sung_pitch4 = map_dbl(transposed, 4),
# #     round_sung_pitch5 = map_dbl(transposed, 5)
# #   ) %>%
# #   select(-transposed) 


# save
data_nets %>% 
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = ',')) %>% 
  write.csv("data/final-data/sing-scales-v2/data-sing-scales-v2_full.csv", row.names = FALSE)


################################################################################
# PILOT (Batch 1): iterated singing (naturalistic) - 100 nodes (across-chains) --> ignore data
################################################################################
data_nodes <- read_csv("data/final-data/sing-scales-v1/sing-scales-v1_node_data.csv")
data_trials <- read_csv("data/final-data/sing-scales-v1/sing-scales-v1_trial_data.csv")

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


# save
data_nets %>% 
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = ',')) %>% 
  write.csv("data/final-data/sing-scales-v1/data-sing-scales-v1_full.csv", row.names = FALSE)

