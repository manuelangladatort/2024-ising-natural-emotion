################################################################################
# Current Biology (2022)
# Authors: Manuel Anglada-Tort, Peter Harrison, and Nori Jacoby
# Script: Supporting methods for melodic features extraction via bootstrap
################################################################################

# ----------------------------------------------
# Helper: Standard error
# ----------------------------------------------
se <- function(x) sqrt(var(x) / length(x))

# ----------------------------------------------
# Helper: Convert string of comma-separated numbers to numeric vector
# ----------------------------------------------
string_to_numeric_vector <- function(str) {
  as.numeric(unlist(strsplit(str, ",")))
}

# ----------------------------------------------
# Helper:Compute Root Mean Squared (RMSD) Difference between two vectors
# ----------------------------------------------
rmsd <- function(vec1, vec2) {
  len <- min(length(vec1), length(vec2))  # handle unequal lengths
  sqrt(mean((vec1[1:len] - vec2[1:len])^2))
}


sum_boot.data_to.plot = function(data, vars_to_sum){
  data_sum = data %>%
    group_by(degree) %>%
    dplyr::summarise_at(vars(vars_to_sum), list(mean = mean, sd = sd), na.rm = TRUE)
  return(data_sum)
}


get_bootstrapped_features_melody <- function(
    data_melodies, 
    vars_sung_pitches,
    vars_sung_intervals,
    vars_sung_IOIratios,
    nboot 
){
  store <- c()
  degrees = table(data_melodies$degree)
  data.ready = data_melodies %>%
    unite("list_sung_pitches", vars_sung_pitches, sep=",", remove=FALSE) %>% 
    unite("list_sung_ints", vars_sung_intervals, sep=",", remove=FALSE) %>% 
    unite("list_sung_ratios", vars_sung_IOIratios, sep=",", remove=FALSE) %>% 
    unite("list_target_ratios", vars_target_IOIratios, sep=",", remove=FALSE) 
  
  
  for (i in 1:nboot){
    print(paste("features boot", i, "out of", nboot))
    # sample with replacement
    data_sample <- data.ready %>% group_by(network_id) %>% group_split()
    data_sample <- sample(data_sample, length(data_sample), replace=TRUE)
    data_sample <- bind_rows(data_sample) 
    
    data_long_intervals = data_sample %>%
      mutate(row_id = row_number()) %>% 
      pivot_longer(vars_sung_intervals, names_to = "index", values_to = "interval") 
    
    data_long_IOIs = data_sample %>%
      mutate(row_id = row_number()) %>% 
      pivot_longer(vars_sung_IOIratios, names_to = "index", values_to = "interval") 
    
    data_long_pitches = data_sample %>%
      # select(-sung_IOI1:-sung_ratio3) %>% 
      mutate(row_id = row_number()) %>% 
      pivot_longer(vars_sung_pitches, names_to = "index", values_to = "interval") %>% 
      # convert ti pitch class
      mutate(interval = round(interval) %% 12)
    
    # entropy
    boot.entropies_pc = c()
    boot.entropies_interval = c()
    boot.entropies_IOI = c()
    for (d in 1:length(degrees)){
      degree.d = d-1
      print(paste0("degree ", degree.d, ", in boot ", i))
      
      data_long_pc_degree = data_long_pitches %>%  filter(degree == degree.d)
      data_long_intervals_degree = data_long_intervals %>%  filter(degree == degree.d)
      data_long_IOIs_degree = data_long_IOIs %>%  filter(degree == degree.d) 
      boot.entropies_pc[[d]] = get_entropy_0.25(data_long_pc_degree)
      boot.entropies_interval[[d]] = get_entropy_0.25(data_long_intervals_degree)
      boot.entropies_IOI[[d]] = get_entropy_0.25(data_long_IOIs_degree)
    }
    entropy_pc = do.call(rbind, boot.entropies_pc)
    entropy_interval = do.call(rbind, boot.entropies_interval)
    entropy_IOI_ratios = do.call(rbind, boot.entropies_IOI)
    
    # error
    data_error1 = data_sample %>%
      group_by(degree) %>%
      dplyr::summarise(
        mean_interval_error = mean(root_mean_squared_interval, na.rm = T),
        mean_ISI_error = mean(ISI_rms_error, na.rm = T),
      )
    
    # Compute RMSE of IOI ratios
    data_sample$RMSD <- purrr::map2_dbl(
      data_sample$list_sung_ratios,
      data_sample$list_target_ratios,
      ~ rmsd(string_to_numeric_vector(.x), string_to_numeric_vector(.y))
    )
    
    # RMSD average by degree
    rmse_values <- data_sample %>%
      group_by(degree) %>%
      summarise(rms_error_ratios = mean(RMSD, na.rm = TRUE)) %>% 
      mutate(rms_error_ratios = ifelse(degree == 0, NA, rms_error_ratios))
    
    
    store[[i]] = tibble(
      boot = i,
      degree = data_error1$degree,
      mean_interval_error = data_error1$mean_interval_error,
      rms_error_ratios = rmse_values$rms_error_ratios,
      mean_ISI_error = data_error1$mean_ISI_error,
      entropy_pc=entropy_pc,
      entropy_interval=entropy_interval,
      entropy_IOI_ratios=entropy_IOI_ratios
    )
    
  }
  o = do.call(rbind, store)
  
  return(o)
}


get_bootstrapped_features_freenote_melody <- function(
    data_melodies, 
    nboot 
){
  store <- c()
  degrees = table(data_melodies$degree)

  for (i in 1:nboot){
    print(paste("features boot", i, "out of", nboot))

    
    data_ready <- data_melodies %>% 
      select(participant_id:degree, network_id, 
             sung_intervals, sung_ratios, target_ratios,
             interval_rms_error = root_mean_squared_interval) 
  
     # sample with replacement
    data_sample <- data_ready %>% group_by(network_id) %>% group_split()
    data_sample <- sample(data_sample, length(data_sample), replace=TRUE)
    data_sample <- bind_rows(data_sample) 
    
    data_long_intervals = data_sample %>%
      select(participant_id:degree, sung_intervals) %>%
      mutate(sung_intervals = str_split(sung_intervals, ",")) %>%
      mutate(interval = purrr::map(sung_intervals, ~ string_to_numeric_vector(.x))) %>%
      unnest_longer(interval) 
    
    data_long_IOI_ratios = data_sample %>%
      select(participant_id:network_id, interval = sung_ratios) %>% 
      unnest_longer(interval) 
    
    # entropy
    boot.entropies_interval = c()
    boot.entropies_IOI = c()
    for (d in 1:length(degrees)){
      degree.d = d-1
      print(paste0("degree ", degree.d, ", in boot ", i))
      
      # entropy intervals
      data_long_intervals_degree = data_long_intervals %>%  filter(degree == degree.d)
      boot.entropies_interval[[d]] = get_entropy_0.25(data_long_intervals_degree)
      
      # entropy IOI ratios
      resolution = 0.05 # specify resolution
      data_long_IOIs_degree = data_long_IOI_ratios %>%  filter(degree == degree.d) 
      boot.entropies_IOI[[d]] = get_entropy_resolution(data_long_IOIs_degree, resolution)
    }
    entropy_interval = do.call(rbind, boot.entropies_interval)
    entropy_IOI = do.call(rbind, boot.entropies_IOI)
    
    # error
    data_error1 = data_sample %>%
      group_by(degree) %>%
      dplyr::summarise(
        mean_interval_error = mean(interval_rms_error, na.rm = T)
      )
    
    # Compute RMSE of IOI ratios
    data_sample$RMSD <- purrr::map2_dbl(
      data_sample$sung_ratios,
      data_sample$target_ratios,
      ~ rmsd(string_to_numeric_vector(as.character(.x)), 
             string_to_numeric_vector(as.character(.y)))
    )
    
    # RMSD average by degree
    rmse_values <- data_sample %>%
      group_by(degree) %>%
      summarise(rms_error_ratios = mean(RMSD, na.rm = TRUE)) %>% 
      mutate(rms_error_ratios = ifelse(degree == 0, NA, rms_error_ratios))
    
    store[[i]] = tibble(
      boot = i,
      degree = data_error1$degree,
      mean_interval_error = data_error1$mean_interval_error,
      rms_error_ratios = rmse_values$rms_error_ratios,
      entropy_interval=entropy_interval,
      entropy_IOI=entropy_IOI
    )
    
  }
  o = do.call(rbind, store)
  
  return(o)
}


get_proportion_large_ints_boot <- function(
    data_singing, 
    vars_sung_intervals,
    nboot
){
  
  degrees = table(data_singing$degree)
  
  data.ready = data_singing %>%
    unite("list_sung_ints", vars_sung_intervals, sep=",", remove=FALSE) %>% 
    select(participant_id:degree, network_id, list_sung_ints, 
           vars_sung_intervals, root_mean_squared_interval)
  
  store <- c()
  
  # loop over iterations
  for (i in 1:nboot){
    
    print(paste("features boot", i, "out of", nboot))
    
    # sample with replacement
    data_sample <- data.ready %>% group_by(network_id) %>% group_split()
    data_sample <- sample(data_sample, length(data_sample), replace=TRUE)
    data_sample <- bind_rows(data_sample) 
    
    store[[i]]  = data_sample %>% 
      pivot_longer(vars_sung_intervals)  %>% 
      mutate(abs_int = abs(value)) %>% 
      mutate(is_larger.than.7 = ifelse(abs_int > 7, 1 , 0)) %>% 
      group_by(degree) %>% 
      dplyr::summarise(
        n_large.7 = sum(is_larger.than.7), 
        n = n(),
        proportion = (n_large.7 / n) * 100
      )
    
  }
  o = do.call(rbind, store)
  return(o)
}


get_peaks_iterations <- function(data){
  
  peaks = get_num_peaks(data$x, data$y)
  
  x_points = c()
  y_points = c()
  for (j in 1:length(peaks[[1]])){
    x = peaks[[1]][[j]]$interval
    y = peaks[[1]][[j]]$value
    
    x_points = c(x_points, x)
    y_points = c(y_points, y)
    
    peaks_df = tibble(x = x_points, y = y_points) 
    
    n_peaks = length(peaks_df$x)
    
    peaks_table = tibble(
      i=j, 
      n_peaks=n_peaks, 
      peaks=list(peaks_df$x), 
      values=list(peaks_df$y)
    )
    
  }
  
  return(peaks_table)
}


get_entropy_iterations = function(data){
  
  intervals = data %>% 
    select(degree, interval)
  
  
  H.int.0 = c()
  H.int.0.25 = c()
  H.int.0.5 = c()
  H.int.0.1 = c()
  H.continous = c()
  
  # entropy
  dat_h_int_0 = round(intervals$interval)
  dat_h_int_0.25 = round(intervals$interval/0.25)*0.25
  dat_h_int_0.5 = round(intervals$interval/0.5)*0.5
  dat_h_int_0.1 = round(intervals$interval, 1)
  
  c.h_int_0 = categorical_entropy(dat_h_int_0)
  c.h_int_0.25 = categorical_entropy(dat_h_int_0.25)
  c.h_int_0.5 = categorical_entropy(dat_h_int_0.5)
  c.h_int_0.1 = categorical_entropy(dat_h_int_0.1)
  
  H.int.0 = c(H.int.0, c.h_int_0)
  H.int.0.25 = c(H.int.0.25, c.h_int_0.25)
  H.int.0.5 = c(H.int.0.5, c.h_int_0.5)
  H.int.0.1 = c(H.int.0.1, c.h_int_0.1)
  
  # continous method
  cont.entropies = entropy(intervals$interval, k = 10)
  cont.entropies_clean = cont.entropies[!is.na(cont.entropies) & !is.infinite(cont.entropies)]
  mean_cont.entropies = mean(cont.entropies_clean, na.rm = T)
  
  H.continous = c(H.continous, mean_cont.entropies)
  
  # store entropies
  entropies = tibble(
    H.int.0 = H.int.0,
    H.int.0.5 = H.int.0.5,
    H.int.0.25 = H.int.0.25,
    H.int.0.1 = H.int.0.1,
    H.continous = H.continous
  )
  
  return(entropies)
}


get_entropy_0.25 = function(data){
  
  intervals = data %>% 
    select(degree, interval)
  
  
  H.int.0.25 = c()

  # entropy
  dat_h_int_0.25 = round(intervals$interval/0.25)*0.25

  c.h_int_0.25 = categorical_entropy(dat_h_int_0.25)

  H.int.0.25 = c(H.int.0.25, c.h_int_0.25)
  
  return(H.int.0.25)
}


get_entropy_resolution = function(data, resolution){
  
  intervals = data %>% 
    select(degree, interval)
  
  
  H.int_store = c()
  
  # entropy
  dat_h_int <- round(intervals$interval / resolution) * resolution
  
  H.int = categorical_entropy(dat_h_int)
  
  H.int_store = c(H.int_store, H.int)
  
  return(H.int_store)
}


categorical_entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}


get_features_bootstraped_slider <- function(
    data_singing,
    data_melcon,
    vars_sung_intervals,
    nboot
    # bw,
    # interval_range
){
  
  degrees = table(data_singing$degree)
  
  data.ready = data_singing %>%
    select(participant_id:degree, network_id, 
           vars_sung_intervals, interval_error)
  
  store <- c()
  
  # loop over iterations
  for (i in 1:nboot){
    
    print(paste("features boot", i, "out of", nboot))
    
    # sample with replacement
    data_sample <- data.ready %>% group_by(network_id) %>% group_split()
    data_sample <- sample(data_sample, length(data_sample), replace=TRUE)
    data_sample <- bind_rows(data_sample) 
    
    # loop over generations
    # boot.peaks_by_degree = c()
    boot.entropies_by_degree = c()
    
    for (d in 1:length(degrees)){
      
      degree.d = d-1
      print(paste0("degree ", degree.d, ", in boot ", i))
      
      data_sample_degree = data_sample %>%  filter(degree == degree.d)

      # smooth
      # data_sample.degree_kde <- density(data_sample_degree[[vars_sung_intervals]],
      #                                        bw = bw,
      #                                        kernel = "gaussian",
      #                                        from = min(interval_range) - 3*bw,
      #                                        to = max(interval_range) + 3*bw
      # )
      # 
      # # peaks
      # boot.peaks_by_degree[[d]] = get_peaks_iterations(data_sample.degree_kde)
      
      # entropies
      boot.entropies_by_degree[[d]] = get_entropy_0.25(data_sample_degree)
      
    }
    
    # peaks_table = do.call(rbind, boot.peaks_by_degree)
    entropies = do.call(rbind, boot.entropies_by_degree)
    
    
  # melcon
    # melodic consonance
    data_sample_round = data_sample  %>%
      mutate(interval = round(interval,1)) %>% 
      select(degree, interval)
    
    data_sum_melcon = as_tibble(
      merge(data_sample_round, data_melcon, by = "interval")) %>%
      group_by(degree) %>%dplyr::summarise(m = mean(mean_rating))
    
    # others
    scores = data_sample %>%  
      group_by(degree) %>%
      dplyr::summarise(
        mean_abs_int_size = mean(abs(interval), na.rm =T),
        mean_error_interval = mean(interval_error, na.rm =T)
      )
    
    store[[i]] = tibble(
      boot = i,
      degree = scores$degree,
      # RMSE
      mean_error_interval = scores$mean_error_interval,
      # interval size
      mean_abs_int_size = scores$mean_abs_int_size,
      # peaks
      # peaks=peaks_table$n_peaks,
      # melcon
      melcon = data_sum_melcon$m,
      # information theory
      interval_entropy_0.25=entropies[,1]
    )
    
  }
  o = do.call(rbind, store)
  return(o)
}


any_input_list_to_numeric = function(input_list){
  if (is.character(input_list)){
    numeric_list = convert_string_numbers_to_numeric_vector(input_list)
  } else if (is.list(input_list)) {
    numeric_list = unlist(input_list)
  } else {
    numeric_list = input_list
  }
  return(numeric_list)
}

calculate_mean_abs_int_size = function(intervals_raw){
  ints = any_input_list_to_numeric(intervals_raw)
  actual_intervals = abs(ints)
  mean_abs_intervals = mean(actual_intervals)
  return(mean_abs_intervals)
}

se <- function(x) sqrt(var(x) / length(x))

convert_string_numbers_to_numeric_vector = function(intervals_string){
  library(comprehenr)
  intervals_string_sep = as.vector(strsplit(intervals_string, ",")[[1]]) # convert to strings separated
  list_intervals_num = to_vec(for(i in intervals_string_sep) as.numeric(i)) # convert to numeric
  return(list_intervals_num)
}

