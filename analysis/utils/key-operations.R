# methods for key estimation analysis

# key profile
# Define the Krumhansl-Schmuckler key profiles for major and minor keys
major_profile_KS <- c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_profile_KS <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

# revised version from Temperly 2001
major_profile_Temp2001 <- c(5.0, 2.0, 3.5, 2.0, 4.5, 4.0, 2.0, 4.5, 2.0, 3.5, 1.5, 4.0)
minor_profile_Temp2001 <- c(5.0, 2.0, 3.5, 4.5, 2.0, 4.0, 2.0, 4.5, 3.5, 2.0, 1.5, 4.0)

# use Albrecht and Shanahan (2013) # arrange horizontally
major_profile_ALbrecht2013 <- c(0.238, 0.006, 0.111, 0.006, 0.137, 0.094, 0.016, 0.214, 0.009, 0.080, 0.008, 0.081)
minor_profile_ALbrecht2013 <- c(0.220, 0.006, 0.104, 0.123, 0.019, 0.103, 0.012, 0.214, 0.062, 0.022, 0.061, 0.052)


chromatic_scale <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")

note_to_midi <- c("C" = 60, "C#" = 61, "D" = 62, "D#" = 63, "E" = 64, "F" = 65, 
                  "F#" = 66, "G" = 67, "G#" = 68, "A" = 69, "A#" = 70, "B" = 71)


# methods

# covert from midi to pitch class
midi_to_pitch_class <- function(midi_note) {
  pitch_classes <- chromatic_scale
  pitch_classes[(midi_note %% 12) + 1]
}


# covert from pitch class to midi
note_to_midi_converter <- function(note) {
  note_upper <- toupper(note) # Convert to uppercase
  midi_value <- note_to_midi[note_upper] # Map to MIDI value
  return(midi_value)
}


# Transpose melodies to start with middle C (MIDI 60)
transpose_to_c4 <- function(row) {
  transposition_interval <- 60 - row[1]  # Calculate the interval to transpose the first note to C4
  row + transposition_interval           # Apply the interval to the entire row
}


################################################################################
# from FANTASTIC
################################################################################
# We write out the Krumhansl-Kessler probe tone profiles.
# There are two such profiles:
# one for the major mode and one for the minor mode.
kk_profiles <- list(
  # Major = major_profile_ALbrecht2013,
  # Minor = minor_profile_ALbrecht2013
  Major = major_profile_KS,
  Minor = minor_profile_KS
)

get_kk_profile <- function(mode, tonic) {
  ref_profile <- kk_profiles[[mode]]
  absolute_pitch_classes <- 0:11
  relative_pitch_classes <- (absolute_pitch_classes - tonic) %% 12
  ref_profile[1 + relative_pitch_classes]
}

kk_all_key_profiles <- map_dfr(0:11, function(tonic) {
  c("Major", "Minor") %>% 
    set_names(., .) %>% 
    map_dfr(function(mode) {
      tibble(
        tonic = tonic,
        mode = mode,
        profile = list(get_kk_profile(mode, tonic))
      )
    })
})

kk_all_key_profiles


kk_estimate_key <- function(
    kk_all_key_profiles,
    pitches,  
    durations 
) {
  pc_distribution <- 
    tibble(pitch = pitches, duration = durations) %>%
    mutate(
      pitch_class = factor(pitch %% 12, levels = 0:11)
    ) %>%
    group_by(pitch_class, .drop = FALSE) %>%
    summarise(
      duration = sum(duration)
    ) %>%
    mutate(
      pitch_class = as.numeric(as.character(pitch_class)),
      rel_duration = duration / sum(duration)
    )
  
  correlations <- 
    kk_all_key_profiles %>% 
    mutate(
      correlation = map_dbl(
        # This function iterates over 
        profile,  # <-- the template profiles
        cor,      # <-- computing the Pearson correlation
        pc_distribution$rel_duration # <-- with the observed durations
      ))
  
  chosen <- correlations %>% slice(which.max(correlations$correlation))

  A0 <- sort(correlations$correlation, decreasing=TRUE)[1]
  A1 <- sort(correlations$correlation, decreasing=TRUE)[2]
  tonal.clarity <- A0 / A1
  tonal.spike <- A0 / sum(correlations$correlation[correlations$correlation > 0])
  
  chosen %>%
    select(tonic, mode, correlation) %>%
    mutate(tonal.clarity=tonal.clarity, tonal.spike=tonal.spike) %>%
    as.list()
}


# test
pitches = c(63, 62, 65, 67, 69, 67)
durations = c(1, 0.5, 0.5, 1, 0.5, 0.5)


kk_estimate_key(
  kk_all_key_profiles,
  pitches =   pitches, 
  durations = durations
)


# apply_key_finding(
#   data_melodies, 
#   kk_all_key_profiles
# )


# apply key finding algorithm
apply_key_finding <- function(data, kk_all_key_profiles) {
  
  results <- list()
  
  # iterate over melodies
  for (i in 1:nrow(data)) {
    
    print(paste("melody", i, "out of", nrow(data), "melodies"))
    
    # Extract pitches
    pitches <- as.numeric(c(data$sung_pitch1[i], data$sung_pitch2[i], data$sung_pitch3[i], data$sung_pitch4[i], data$sung_pitch5[i]))
    pitches <- round(pitches) 
    
    # Extract note durations
    durations <- strsplit(data$sung_note_durations[1], ",")[[1]]
    durations <- as.numeric(durations)
    
    # key estimation
    res_key <- kk_estimate_key(
      kk_all_key_profiles,
      pitches =   pitches, 
      durations = durations
    )
    
    estimated_key = chromatic_scale[1+res_key$tonic]
    
    
    # store the results in the list
    results[[i]] <- tibble(
      melody = i,
      mode = res_key$mode,
      estimated_key = estimated_key,
      estimated_tonic = res_key$tonic,
      tonalness = res_key$correlation,
      tonal.clarity = res_key$tonal.clarity,
      tonal.spike = res_key$tonal.spike
    )
  }
  
  # Return the list of results
  return(results)
}


# apply key finding algorithm (free notes experiment)
apply_key_finding_free_notes <- function(data, kk_all_key_profiles) {
  
  results <- list()
  
  # iterate over melodies
  for (i in 1:nrow(data)) {
    
    print(paste("melody", i, "out of", nrow(data), "melodies"))
    
    # Extract pitches
    pitches <- data$sung_pitches[i]
    pitches <- pitches %>% str_split(",")
    pitches <- pitches %>% unlist()
    pitches <- pitches %>% as.numeric() 
    pitches <- pitches %>% round() 
    
    # Extract note durations
    durations <- data$sung_note_durations[i]
    durations <- durations %>% str_split(",")
    durations <- durations %>% unlist()
    durations <- durations %>% as.numeric() 
    durations <- durations %>% round()
    
    # key estimation
    res_key <- kk_estimate_key(
      kk_all_key_profiles,
      pitches =   pitches, 
      durations = durations
    )
    
    estimated_key = chromatic_scale[1+res_key$tonic]
    
    
    # store the results in the list
    results[[i]] <- tibble(
      melody = i,
      melody_id = data$id[i],
      mode = res_key$mode,
      estimated_key = estimated_key,
      estimated_tonic = res_key$tonic,
      tonalness = res_key$correlation,
      tonal.clarity = res_key$tonal.clarity,
      tonal.spike = res_key$tonal.spike
    )
  }
  
  # Return the list of results
  return(results)
}


################################################################################
# from FANTASTIC
################################################################################
make.tonal.weights <- function(maj.vector, min.vector) { 
  w.matrix <- matrix(data=NA,nrow=12,ncol=24)
  for(i in 1:12) {
    if(i==1){j <- 0}
    else{j <- 1}
    h <- i-1
    k <- h-1
    #print(c(maj.vector[((12-k)*j):(12*j)], maj.vector[1:(12-h)]))
    w.matrix[,i] <- c(maj.vector[((12-k)*j):(12*j)], maj.vector[1:(12-h)])
    w.matrix[,i+12] <- c(min.vector[((12-k)*j):(12*j)], min.vector[1:(12-h)])
  }
  w.matrix <- as.data.frame(w.matrix)			
  colnames(w.matrix) <- c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B", "c","c#","d","d#","e","f","f#","g","g#","a","a#","b")
  w.matrix
}

compute.tonality.vector <- function(pitch, dur16, tonal.weights) {
  # Create a data frame containing pitch classes and weighted pitch values
  data <- data.frame(p.class = as.factor(pitch %% 12), weighted.p = pitch * dur16)
  
  # Sum the weighted pitch values grouped by pitch class (using tapply)
  tab <- tapply(data$weighted.p, data$p.class, sum)
  
  # Create a full tab of 12 elements representing each pitch class (C, C#, D, etc.) and assigned summed values to their corresponding positions
  full.tab <- rep(0, 12)
  full.tab[as.numeric(names(tab)) + 1] <- tab
  
  # Calculate correlation between the tonal weights and the pitch class vector
  corrmat <- apply(tonal.weights, 2, function(x) cor(x, full.tab))
}

compute.tonal.features <- function(tonality.vector) {
  A0 <- max(tonality.vector)
  A1 <- sort(tonality.vector, decreasing=TRUE)[2]
  ratio.A0A1 <- A0/A1
  tonal.spike <- A0/sum(tonality.vector[tonality.vector>0])
  if(which(tonality.vector==max(tonality.vector))>12) {
    mode <- "minor"}
  else{mode <- "major"}
  results <- data.frame(tonalness=A0,tonal.clarity=ratio.A0A1,tonal.spike,mode)
  rownames(results) <- NULL
  results
  
}

compute.tonal.features_updated <- function(tonality.vector) {
  A0 <- max(tonality.vector)
  A1 <- sort(tonality.vector, decreasing=TRUE)[2]
  ratio.A0A1 <- A0 / A1
  tonal.spike <- A0 / sum(tonality.vector[tonality.vector > 0])
  
  # Handle the case of multiple max indices by taking the first one
  max_index <- which(tonality.vector == max(tonality.vector))[1]
  
  if (max_index > 12) {
    mode <- "minor"
  } else {
    mode <- "major"
  }
  
  # Create the results data frame
  results <- data.frame(tonalness = A0, tonal.clarity = ratio.A0A1, tonal.spike, mode)
  rownames(results) <- NULL
  
  return(results)
}


# apply key finding algorithm
apply_key_finding_fantastic <- function(data, max_profile, min_profile) {
  
  # Initialize a list to store the results
  results <- list()
  
  # Iterate over each row in the dataset
  for (i in 1:nrow(data)) {
    
    print(paste("row", i, "out of", nrow(data), "rows"))
    
    # # Extract the pitches and durations for the current row
    # pitches <- as.numeric(c(data$round_sung_pitch1[i], data$round_sung_pitch2[i], data$round_sung_pitch3[i], data$round_sung_pitch4[i], data$round_sung_pitch5[i]))
    # durations <- as.numeric(c(data$sung_note_duration_metrical1[i], data$sung_note_duration_metrical2[i], data$sung_note_duration_metrical3[i], data$sung_note_duration_metrical4[i], data$sung_note_duration_metrical5[i]))
    
    # Extract pitches
    pitches <- as.numeric(c(data$sung_pitch1[i], data$sung_pitch2[i], data$sung_pitch3[i], data$sung_pitch4[i], data$sung_pitch5[i]))
    pitches <- round(pitches) 
    
    # Extract note durations
    durations <- strsplit(data$sung_note_durations[1], ",")[[1]]
    durations <- as.numeric(durations)
    
    # Apply the compute.tonality.vector function
    tonality.vector <- compute.tonality.vector(pitches, durations, make.tonal.weights(max_profile, min_profile))
    
    # add estimated key
    max_value <- max(tonality.vector)
    max_note <- names(tonality.vector)[which(tonality.vector == max_value)]
    
    print(paste("row", i, "estiamted key:", max_note))
    
    # Compute the tonal features
    tonal_features <- compute.tonal.features_updated(tonality.vector)
    tonal_features$estimated_key = max_note[[1]]
    
    # Store the results in the list
    results[[i]] <- tibble(
      tonalness = tonal_features$tonalness,
      tonal.clarity = tonal_features$tonal.clarity,
      tonal.spike = tonal_features$tonal.spike,
      mode = tonal_features$mode,
      estimated_key = tonal_features$estimated_key
    )
  }
  
  # Return the list of results
  return(results)
}

