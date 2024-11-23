# utils

# key profile
# Define the Krumhansl-Schmuckler key profiles for major and minor keys
major_profile_KS <- c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_profile_KS <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

# revised version from Temperly 2001
major_profile_Temp2001 <- c(5.0, 2.0, 3.5, 2.0, 4.5, 4.0, 2.0, 4.5, 2.0, 3.5, 1.5, 4.0)
minor_profile_Temp2001 <- c(5.0, 2.0, 3.5, 4.5, 2.0, 4.0, 2.0, 4.5, 3.5, 2.0, 1.5, 4.0)

# use Albrecht and Shanahan (2013)
major_profile_ALbrecht2013 <- c(0.238, 
                               0.006, 
                               0.111, 
                               0.006, 
                               0.137, 
                               0.094, 
                               0.016, 
                               0.214, 
                               0.009, 
                               0.080, 
                               0.008, 
                               0.081) 

minor_profile_ALbrecht2013 <- c(0.220, 
                                 0.006, 
                                 0.104, 
                                 0.123, 
                                 0.019, 
                                 0.103, 
                                 0.012, 
                                 0.214, 
                                 0.062, 
                                 0.022, 
                                 0.061, 
                                 0.052)

chromatic_scale <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B") # chromatic scale

# covert from midi to pitch class
midi_to_pitch_class <- function(midi_note) {
  pitch_classes <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  pitch_classes[(midi_note %% 12) + 1]
}


# covert from pitch class to midi
note_to_midi <- c("C" = 60, "C#" = 61, "D" = 62, "D#" = 63, "E" = 64, "F" = 65, 
                  "F#" = 66, "G" = 67, "G#" = 68, "A" = 69, "A#" = 70, "B" = 71)

note_to_midi_converter <- function(note) {
  note_upper <- toupper(note) # Convert to uppercase
  midi_value <- note_to_midi[note_upper] # Map to MIDI value
  return(midi_value)
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
apply_key_finding <- function(data, max_profile, min_profile) {
  
  # Initialize a list to store the results
  results <- list()
  
  # Iterate over each row in the dataset
  for (i in 1:nrow(data)) {
    
    # Extract the pitches and durations for the current row
    pitches <- as.numeric(c(data$round_sung_pitch1[i], data$round_sung_pitch2[i], data$round_sung_pitch3[i], data$round_sung_pitch4[i], data$round_sung_pitch5[i]))
    durations <- as.numeric(c(data$sung_note_duration_metrical1[i], data$sung_note_duration_metrical2[i], data$sung_note_duration_metrical3[i], data$sung_note_duration_metrical4[i], data$sung_note_duration_metrical5[i]))
    
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

