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

################################################################################
# functions
sort_json <- function(x){
  jsonlite::stream_in(textConnection(gsub("\\n", "", x)))
}

unpack_json_column = function(data, column_to_unpack){
  column_unpacked = sort_json(column_to_unpack)
  data_unpacked = as_tibble(cbind(data, column_unpacked), .name_repair = "universal")
  return(data_unpacked)
}

# data
data_pids <- read_csv("data/natising1/Participant.csv")

data_pids = data_pids %>% 
  filter(failed == FALSE) %>%
  select(id, singing_performance, gender, register, age, gmsi) 

# gmsi
data_pids$gmsi[is.na(data_pids$gmsi)] <- "{}"
data_pids_gmsi = unpack_json_column(data_pids, data_pids$gmsi) 

data_pids_gmsi$GMSI_MT = data_pids_gmsi$mean_scores_per_scale$`Musical Training`
data_pids_gmsi$GMSI_SA = data_pids_gmsi$mean_scores_per_scale$`Singing Abilities`

data_full_ready = data_pids_gmsi %>% 
  select(-gmsi, -mean_scores_per_scale, -response_scores) %>% 
  drop_na(age)

# save
data_full_ready %>% 
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = ',')) %>% 
  write.csv("data/participants-info.csv", row.names = FALSE)
