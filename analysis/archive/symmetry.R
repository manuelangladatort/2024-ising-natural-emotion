# Create random dataset of melodies
melodies <- replicate(10, sample(chromatic_scale, size = 5, replace = TRUE)) # Generate 10 melodies
melodies_df <- as.data.frame(t(melodies), stringsAsFactors = FALSE) # Convert to a data frame
colnames(melodies_df) <- c("Note1", "Note2", "Note3", "Note4", "Note5")
melodies_df

manual_melody1 <- c("C", "E", "C", "E", "C")
manual_melody2 <- c("C", "E", "E", "C", "E")

melodies_df <- rbind(melodies_df, manual_melody1)
melodies_df <- rbind(melodies_df, manual_melody2)


# Step 1: Create a transition matrix
transition_matrix <- matrix(0, nrow = 12, ncol = 12) # Initialize an empty transition matrix for the 12 notes of the chromatic scale
rownames(transition_matrix) <- chromatic_scale
colnames(transition_matrix) <- chromatic_scale

# Populate the transition matrix by counting transitions between consecutive notes in each melody
for (i in 1:nrow(melodies_df)) {
  melody <- as.vector(melodies_df[i, ])
  for (j in 1:(length(melody) - 1)) {
    from_note <- as.character(melody[j])
    to_note <- as.character(melody[j + 1])
    transition_matrix[from_note, to_note] <- transition_matrix[from_note, to_note] + 1
  }
}


# Step 2: Apply the symmetry function
calculate_symmetry <- function(t_matrix) {
  # Get the size of the square matrix
  m <- nrow(t_matrix)
  
  # Initialize sum for the symmetry calculation
  symmetry_sum <- 0
  
  # Loop over all pairs (i, j) in the transition matrix
  for (i in 1:m) {
    for (j in 1:m) {
      if (t_matrix[i, j] > 0 && t_matrix[j, i] > 0) {
        # Add the absolute difference of log-transformed transitions
        symmetry_sum <- symmetry_sum + abs(log(t_matrix[i, j]) - log(t_matrix[j, i]))
      }
    }
  }
  
  # Calculate S(t, m) using the provided formula
  symmetry_value <- symmetry_sum / (2 * m^2)
  
  return(symmetry_value)
}

# Calculate the symmetry of the transition matrix
symmetry_value <- calculate_symmetry(transition_matrix)

symmetry_value # 0.007629252


################################################################################
# apply to data
################################################################################
data_melodies_trans <-  data_melodies %>%
  select(id:degree,sung_pitch1:sung_pitch5)  %>% 
  mutate(across(starts_with("sung_pitch"), round)) %>%
  mutate(across(starts_with("sung_pitch"), midi_to_pitch_class))

#

data_melodies_trans_degree = data_melodies_trans %>% filter(degree == 10)

# Step 1: Create a transition matrix
transition_matrix <- matrix(0, nrow = 12, ncol = 12) # Initialize an empty transition matrix for the 12 notes of the chromatic scale
rownames(transition_matrix) <- chromatic_scale
colnames(transition_matrix) <- chromatic_scale

# Populate the transition matrix by counting transitions between consecutive notes in each melody
for (i in 1:nrow(data_melodies_trans_degree)) {
  melody <- as.character(data_melodies_trans_degree[i, c("sung_pitch1", "sung_pitch2", "sung_pitch3", "sung_pitch4", "sung_pitch5")])
  for (j in 1:(length(melody) - 1)) {
    from_note <- as.character(melody[j])
    to_note <- as.character(melody[j + 1])
    transition_matrix[from_note, to_note] <- transition_matrix[from_note, to_note] + 1
  }
}

symmetry_value <- calculate_symmetry(transition_matrix)
symmetry_value

