################################################################################
# Singing Networks
# Script: Supporting methods 
################################################################################
list_4int_sung.intervals = c("sung_interval1","sung_interval2","sung_interval3","sung_interval4")
list_4int_pitches = c("sung_pitch1","sung_pitch2","sung_pitch3", "sung_pitch4", "sung_pitch5")

list_2int_sung.intervals = c("sung_interval1","sung_interval2")
list_2int_pitches = c("sung_pitch1","sung_pitch2","sung_pitch3")

sort_json <- function(x){
  jsonlite::stream_in(textConnection(gsub("\\n", "", x)))
}


unpack_json_column = function(data, column_to_unpack){
  column_unpacked = sort_json(column_to_unpack)
  data_unpacked = as_tibble(cbind(data, column_unpacked), .name_repair = "universal")
  return(data_unpacked)
}


load_raw_data <- function(data_dir, app_name) {
  message("Loading raw data...")
  path.to.csvs = paste0(data_dir,"data-", app_name, "/csv/")
  tibble(
    path = list.files(path.to.csvs, full.names=TRUE),
    file = basename(path),
    id = gsub("\\.csv", "", file),
    data = map(path, read_csv, col_types = cols()) #, guess_max = 21474836)
  ) %>% {
    set_names(.$data, .$id)
  }
}


transpose_pitches_if.male = function(x){
  o = x + 12
  return(o)
}

calculate_RMSE = function(actual, predicted){
  rmse =sqrt(mean((actual - predicted)^2))
  return(rmse)
}

new_column_names = function(column, name){
  num_cols = length(column[[1]])
  labels=c()
  for (i in 1:num_cols) {
    label=paste0(name, i)
    labels=c(labels, label)
  }
  return(labels)
}


prepare_trial_data = function(data_nodes, data_trials){
  
  data_nodes = data_nodes %>%  select(-target_pitches:-target_note_durations, -trial_type)
  
  data_nodes$definition[is.na(data_nodes$definition)] <- "{}"
  data_nodes_unpacked = unpack_json_column(data_nodes, data_nodes$definition)
    
  
  # select target melodies source
  data_nodes_source <- data_nodes_unpacked %>% 
    filter(degree == 0)  %>% 
    mutate(
      target_pitches = as.list(target_pitches),
      target_intervals = as.list(target_intervals),
      target_note_durations = as.list(target_note_durations),
      target_ISIs = as.list(target_ISIs),
      target_onsets = as.list(onsets)
    ) %>% 
    select(-raw_audio)
  
  
  data_trials$analysis[is.na(data_trials$analysis)] <- "{}"
  
  data_trials_unpacked = unpack_json_column(data_trials, data_trials$analysis) %>% 
    select(-raw, -save_plot, -analysis)
  
  # Extract pitch and time stats
  data_trials_unpacked$root_mean_squared_pitch = data_trials_unpacked$pitch_stats$root_mean_squared_pitch 
  data_trials_unpacked$root_mean_squared_interval = data_trials_unpacked$pitch_stats$root_mean_squared_interval 
  data_trials_unpacked$note_duration_root_mean_squared = data_trials_unpacked$time_stats$note_duration_root_mean_squared

  # Merge data and add target melodies in seed
  merge_data = data_trials_unpacked %>% 
    mutate(degree = degree + 1) %>% 
    # add target melodies in seed
    bind_rows(data_nodes_source) %>% 
    mutate(
      sung_intervals = ifelse(degree == 0, target_intervals, sung_intervals),
      sung_pitches = ifelse(degree == 0, target_pitches, sung_pitches),
      sung_note_durations = ifelse(degree == 0, target_note_durations, sung_note_durations),
      sung_ISIs = ifelse(degree == 0, target_ISIs, sung_ISIs)
      )
  
  # unfolding
  column_names_int.sung = new_column_names(merge_data$sung_intervals, "sung_interval")
  column_names_int.target = new_column_names(merge_data$sung_intervals, "target_interval")
  column_names_pitch.sung = new_column_names(merge_data$sung_pitches, "sung_pitch")
  column_names_pitch.target = new_column_names(merge_data$sung_pitches, "target_pitch")
  column_names_IOI_sung = new_column_names(merge_data$sung_ISIs, "sung_IOI")
  column_names_IOI_target = new_column_names(merge_data$sung_ISIs, "target_IOI")

  final_data = merge_data %>% 
    separate(sung_intervals, column_names_int.sung, sep=",") %>%
    separate(target_intervals, column_names_int.target, sep=",") %>%
    separate(sung_pitches, column_names_pitch.sung, sep=",") %>%
    separate(target_pitches, column_names_pitch.target, sep=",") %>%
    separate(sung_ISIs, column_names_IOI_sung, sep=",") %>%
    separate(target_ISIs, column_names_IOI_target, sep=",") %>%
    mutate_at(column_names_int.sung, parse_number) %>%
    mutate_at(column_names_int.target, parse_number) %>%
    mutate_at(column_names_pitch.sung, parse_number) %>%
    mutate_at(column_names_pitch.target, parse_number) %>% 
    mutate_at(column_names_IOI_sung, parse_number)  %>% 
    mutate_at(column_names_IOI_target, parse_number) 

  return(final_data)
} 


prepare_data_validation = function(data, app_name){
  data_validation = data %>% 
    mutate(app = app_name) %>% 
    select(app, participant_id, id, network_id, degree, vertex_id, 
           trial_maker_id,
           sung_interval1:sung_interval4,
           root_mean_squared_interval
    ) %>% 
    # 5 tones
    unite("intervals", sung_interval1:sung_interval4, sep = ",", remove = FALSE)
  return(data_validation)
}


