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
# Batch 1: iterated singing (naturalistic) - 100 nodes (across-chains)
################################################################################
data_nodes <- read_csv("data/data-clean/natising1_node_data.csv")
data_trials <- read_csv("data/data-clean/natising1_trial_data.csv")

length(table(data_nodes$network_id))
table(data_nodes$degree)
length(table(data_trials$network_id))
length(table(data_trials$degree))

data_nets = prepare_trial_data(data_nodes[,-1], data_trials[,-1]) 

length(unique(data_nets$participant_id)) # 65
length(unique(data_nets$network_id)) # 100
table(data_nets$degree) 
table(data_nets$trial_maker_id)

# save
data_nets %>% 
  mutate(trial_type = ifelse(degree != 0, "node_trial", "source_trial")) %>%
  select(-definition, -seed, -pitch_stats, -time_stats, -reason) %>%
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = ',')) %>% 
  write.csv("data/data-clean/natising1_full.csv", row.names = FALSE)

