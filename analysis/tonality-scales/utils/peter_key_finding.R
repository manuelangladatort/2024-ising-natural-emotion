# Load dependencies (packages which
# provide us with some useful functions).
library(dplyr)
library(purrr)

# We write out the Krumhansl-Kessler probe tone profiles.
# There are two such profiles:
# one for the major mode and one for the minor mode.
kk_profiles <- list(
  Major = major_profile_ALbrecht2013,
  Minor = minor_profile_ALbrecht2013
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
  profile_chose = chosen$profile[[1]]

  A0 <- max(profile_chose)
  A1 <- sort(profile_chose, decreasing=TRUE)[2]
  tonal.clarity <- A0 / A1
  tonal.spike <- A0 / sum(profile_chose[profile_chose > 0])
  
  chosen %>%
    select(tonic, mode, correlation) %>%
    mutate(tonal.clarity=tonal.clarity, tonal.spike=tonal.spike) %>%
    as.list()
}


apply_key_finding_peter <- function(data, kk_all_key_profiles) {
  
  # Initialize a list to store the results
  results <- list()
  
  # Iterate over each row in the dataset
  for (i in 1:nrow(data)) {
    
    # Extract the pitches and durations for the current row
    pitches <- as.numeric(c(data$round_sung_pitch1[i], data$round_sung_pitch2[i], data$round_sung_pitch3[i], data$round_sung_pitch4[i], data$round_sung_pitch5[i]))
    durations <- as.numeric(c(data$sung_note_duration1[i], data$sung_note_duration2[i], data$sung_note_duration3[i], data$sung_note_duration4[i], data$sung_note_duration5[i]))
    
    res <- kk_estimate_key(
      kk_all_key_profiles,
      pitches =   pitches, 
      durations = durations
    )
    
    print(paste("row", i, "estiamted key:", res$tonic))
    
    # Store the results in the list
    results[[i]] <- tibble(
      tonic = res$tonic,
      mode = res$mode,
      tonalness = res$correlation,
      tonal.clarity = res$tonal.clarity,
      tonal.spike = res$tonal.spike
    )
  }
  
  # Return the list of results
  return(results)
}


# test
pitches = c(60, 62, 65, 67, 69, 67)
durations = c(1, 0.5, 0.5, 1, 0.5, 0.5)

# peter
kk_estimate_key(
  kk_all_key_profiles,
  pitches =   pitches, 
  durations = durations
)


# apply to dataset
tonal_features_list <- apply_key_finding_peter(data_melodies, kk_all_key_profiles)
tonal_features = do.call(rbind, tonal_features_list)


data_melodies$tonalness <- tonal_features$tonalness
data_melodies$tonal.clarity <- tonal_features$tonal.clarity
data_melodies$tonal.spike <- tonal_features$tonal.spike
data_melodies$mode <- tonal_features$mode
data_melodies$estimated_key <- tonal_features$tonic

