################################################################################
# Iterated singing (naturalistic)
# Script: Analysis
################################################################################
# import libraries
library(tidyverse)
library(egg)
library(ggpubr)
library(cowplot)
library(viridis)

source("utils/plots.R")

# global parameters
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 13
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

interval_range_pitch = c(45,75)
vertical.lines_pitch = seq(from=min(interval_range_pitch), to=max(interval_range_pitch), by = 1)

# import data
# data_melodies <- read_csv("data/data-clean/sing-scales-v1/data-sing-scales-v2_full.csv")
data_melodies <- read_csv("data/data-clean/sing-scales-v2/data-sing-scales-v2_full.csv")

table(data_melodies$degree)
length(table(data_melodies$network_id))


################################################################################
# Analysis - error
################################################################################
# calculate the mean interval error over generations
error_data = data_melodies %>% 
  group_by(degree) %>% 
  summarise(
    n = n(), 
    mean_pitch_interval_error = mean(root_mean_squared_interval, na.rm = T),
    sd_pitch_interval_error = sd(root_mean_squared_interval, na.rm = T),
    se_pitch_interval_error = sd_pitch_interval_error/sqrt(n),
    mean_ISI_error = mean(ISI_rms_error, na.rm = T),
    sd_ISI_error = sd(ISI_rms_error, na.rm = T),
    se_ISI_error = sd_ISI_error/sqrt(n)
  )

# plot pitch interval error
plot_pitch_interval_error = error_data %>%
  ggplot(aes(x= degree, y = mean_pitch_interval_error)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_pitch_interval_error - se_pitch_interval_error, 
                  ymax=mean_pitch_interval_error + se_pitch_interval_error),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10, 11)) +
  ylab("Copying Error (Pitch Interval)") +
  xlab("Iteration") +
  ggtitle("Pitch transmissibiity") +
  theme_classic() +
  theme(legend.position = "none")

plot_pitch_interval_error


# plot note duration error
plot_ISI_error = error_data %>%
  ggplot(aes(x= degree, y = mean_ISI_error)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_ISI_error - se_ISI_error, 
                  ymax=mean_ISI_error + se_ISI_error),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10, 11)) +
  ylab("Copying Error (Inter-Onset-Interval)") +
  xlab("Iteration") +
  ggtitle("Rhythm transmissibiity") +
  theme_classic() +
  theme(legend.position = "none")

plot_ISI_error

plot_grid(plot_pitch_interval_error, plot_ISI_error, nrow = 1)


ggsave("evolution_error.png", width = 16, height = 8, units = "cm")


# Normalize the data using z-score
error_data <- error_data %>%
  filter(degree != 0) %>% 
  mutate(
    z_pitch_interval_error = (mean_pitch_interval_error - mean(mean_pitch_interval_error)) / sd(mean_pitch_interval_error),
    z_pitch_interval_se = se_pitch_interval_error / sd(mean_pitch_interval_error),
    z_ISI_error = (mean_ISI_error - mean(mean_ISI_error)) / sd(mean_ISI_error),
    z_ISI_se = se_ISI_error / sd(mean_ISI_error)
  )

# Reshape the data to long format for easier plotting
error_data_long <- error_data %>%
  select(degree, z_pitch_interval_error, z_pitch_interval_se, z_ISI_error, z_ISI_se) %>%
  pivot_longer(
    cols = c(z_pitch_interval_error, z_ISI_error),
    names_to = "error_type",
    values_to = "z_error"
  ) %>%
  mutate(
    z_se = ifelse(error_type == "z_pitch_interval_error", z_pitch_interval_se, z_ISI_se)
  )

# Create the combined plot
plot_ISI_error = error_data %>%
  ggplot(aes(x= degree, y = mean_ISI_error)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_ISI_error - se_ISI_error, 
                  ymax=mean_ISI_error + se_ISI_error),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10, 11)) +
  ylab("Copying Error (Inter-Onset-Interval)") +
  xlab("Iteration") +
  ggtitle("Rhythm transmissibiity") +
  theme_classic() +
  theme(legend.position = "none")


combined_plot <- error_data_long %>%
  ggplot(aes(x = degree, y = z_error, group = error_type, fill = error_type)) +
  geom_line() +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = z_error - z_se, ymax = z_error + z_se), alpha = 0.3) +
  labs(
    x = "Iteration",
    y = "Normalized Copying Error (z-score)",
    color = "Error Type",
    fill = "Error Type",
  ) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10, 11)) +
  scale_color_manual(
    values = c("z_pitch_interval_error" = "blue", "z_ISI_error" = "red"),
    labels = c("Pitch Interval", "Inter-Onset-Interval")
  ) +
  scale_fill_manual(
    values = c("z_pitch_interval_error" = "blue", "z_ISI_error" = "red"),
    labels = c("Pitch Interval", "Inter-Onset-Interval")
  ) +
  theme_classic()

combined_plot

ggsave("evolution_error.png", width = 12, height = 8, units = "cm")


################################################################################
# Analysis - histogram of scales
################################################################################
# convert MIDI notes to letter sequences
midi_to_pitch_class <- function(midi_note) {
  pitch_classes <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  pitch_classes[(midi_note %% 12) + 1]
}

data_v1_sequenced <- data_v1_transposed %>%
  mutate(across(starts_with("round_sung_pitch"), midi_to_pitch_class))

hist_scales_data <- data_v1_sequenced %>%
  mutate(melody_sequence = paste0(round_sung_pitch1, round_sung_pitch2, round_sung_pitch3, round_sung_pitch4, round_sung_pitch5)) %>% 
  count(melody_sequence) %>%  # Count the number of occurrences of each sequence
  arrange(desc(n)) %>%  # Arrange sequences by their frequency
  slice_head(n = 10)  # Select the top 10 sequences

ggplot(hist_scales_data, aes(x = reorder(melody_sequence, -n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Melody Sequence", y = "Number of Melodies", title = "Histogram of Melody Sequences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

ggsave("histogram_scales.png", width = 16, height = 8, units = "cm")


# # transition probabilities between nodes
# # 1. Create a transition matrix
# transitions <- melody_df %>%
#   select(network_id, sung_pitch1, sung_pitch2, sung_pitch3, sung_pitch4, sung_pitch5) %>%
#   pivot_longer(cols = sung_pitch1:sung_pitch5, names_to = "position", values_to = "note") %>%
#   group_by(network_id) %>%  # Assuming 'id' is the unique identifier for each melody
#   mutate(next_note = lead(note)) %>%  # Create a column for the next note in the sequence
#   filter(!is.na(next_note)) %>%  # Remove the last note since it has no next note
#   count(note, next_note) %>%  # Count transitions from one note to the next
#   ungroup()
# 
# # 2. Calculate transition probabilities
# transition_probabilities <- transitions %>%
#   group_by(note) %>%
#   mutate(probability = n / sum(n)) %>%  # Calculate the probability of each transition
#   ungroup() %>% 
#   select(-network_id)
# 
# write.csv(transition_probabilities, "transition_probabilities.csv", row.names = FALSE)

################################################################################
# Interval between first and last note
################################################################################
data_dynamics = data_melodies %>% 
  select(id:participant_id, sung_pitch1:sung_pitch5, sung_note_duration_metrical1:sung_note_duration_metrical5, total_duration) %>% 
  mutate(first_last_invertval = abs (sung_pitch1- sung_pitch5))

# Prepare the data: select and reshape
data_dynamics_long <- data_melodies %>%
  select(id:participant_id, sung_note_duration_metrical1:sung_note_duration_metrical5) %>%
  pivot_longer(cols = starts_with("sung_note_duration"),
               names_to = "note",
               values_to = "duration")

# Create the boxplot
ggplot(data_dynamics_long, aes(x = note, y = duration)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Note Durations",
       x = "Note",
       y = "Duration") +
  scale_x_discrete(labels = c("N1", "N2", "N3", "N4", "N5")) +
  facet_wrap(~degree)

ggsave("results/boxplots_note_durations_evolution.png", width = 20, height = 15, units = "cm", bg = " white")

 
# note duration
data_dynamics_sum <-  data_dynamics %>% 
  group_by(degree) %>% 
  summarise(
    n = n(), 
    mean_first_last_invertval = mean(first_last_invertval, na.rm = T),
    sd_first_last_invertval = sd(first_last_invertval, na.rm = T),
    se_first_last_invertval = sd_first_last_invertval/sqrt(n),
    mean_note_duration = mean(sung_note_duration_metrical5, na.rm = T),
    sd_note_duration = sd(sung_note_duration_metrical5, na.rm = T),
    se_note_duration = sd_note_duration/sqrt(n),
    mean_total_duration = mean(total_duration, na.rm = T),
    sd_total_duration = sd(total_duration, na.rm = T),
    se_note_duration = sd_total_duration/sqrt(n)
  )


plot_total_duration = data_dynamics_sum %>%
  ggplot(aes(x= degree, y = mean_total_duration)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_total_duration - se_note_duration, 
                  ymax=mean_total_duration + se_note_duration),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  ylab("Total duration (seconds)") +
  xlab("Iteration") +
  ggtitle("Melodies become faster over time") +
  theme_classic() +
  theme(legend.position = "none",  title = element_text(size = 8))

plot_total_duration


plot_note_duration = data_dynamics_sum %>%
  ggplot(aes(x= degree, y = mean_note_duration)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_note_duration - se_note_duration, 
                  ymax=mean_note_duration + se_note_duration),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  ylab("Mean note duration (normalized)") +
  xlab("Iteration") +
  ggtitle("Last note becomes longer") +
  theme_classic() +
  theme(legend.position = "none",  title = element_text(size = 8))

plot_note_duration


plot_first_last_invertval = data_dynamics_sum %>%
  ggplot(aes(x= degree, y = mean_first_last_invertval)) + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_first_last_invertval - se_first_last_invertval, 
                  ymax=mean_first_last_invertval + se_first_last_invertval),  
              alpha = 0.4)  +
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  ylab("Abs diff first and last note (semitones)") +
  xlab("Iteration") +
  ggtitle("Melodies tend to finish with first note") +
  theme_classic() +
  theme(legend.position = "none", title = element_text(size = 8))

plot_first_last_invertval


plot_grid(plot_note_duration, plot_first_last_invertval, nrow = 1)

ggsave("results/evolution_lastnote2.png", width = 15, height = 8, units = "cm")


################################################################################
# Marginals
################################################################################
NBOOT = 1000
BW = 0.25

# marginals intervals
data_melodies_long_intervals =  data_melodies %>% 
  select(id:participant_id, sung_interval1:sung_interval4) %>%
  pivot_longer(cols = starts_with("sung_interval"),
               names_to = "interval_pos",
               values_to = "interval")

marginals_melodies_intervals = make_marginals_kde(data_melodies_long_intervals, c("interval"), NBOOT, BW, "marginals") 

marginals_melodies_intervals

ggsave("results/marginals_intervals.png", width = 14, height = 7, units = "cm")


marginals_melodies_intervals_separate = make_marginals_kde_separate_4ints(
  data_melodies, list_4int_sung.intervals, NBOOT, BW, "separate marginals"
)

marginals_melodies_intervals_separate

ggsave("results/marginals_intervals_separate.png", width = 12, height = 15, units = "cm")



# TODO: marginals pitches: needs more work
data_melodies_long_pitches =  data_melodies %>% 
  select(id:participant_id, round_sung_pitch1:round_sung_pitch5) %>%
  pivot_longer(cols = starts_with("round_sung_pitch"),
               names_to = "pitch_pos",
               values_to = "pitch") %>% 
  filter(degree %in% 7:10)

kde_data_pitches_d.7to10 = kde_bootstrap_1d(data_melodies_long_pitches, 
                                     NBOOT, 
                                     "pitch", 
                                     BW, 
                                     "chains", 
                                     interval_range_pitch
                                     )

kde_data_pitches_d.7to10 %>% ggplot(aes(x)) +
  scale_x_continuous(breaks=seq(min(vertical.lines_pitch), max(vertical.lines_pitch), 1),
                     limits = interval_range_pitch)  +
  geom_vline(xintercept = vertical.lines_pitch, colour = "lightgrey", linetype="dashed") +
  geom_ribbon(aes(ymin = avg - sdev, ymax = avg + sdev), alpha = .4, fill = "grey") +
  geom_line(aes(y = avg), color="black", size =  0.7) +
  # add peaks
  xlab("vname") + ylab("density") +
  theme_classic() + 
  theme(axis.text.x = element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.x = element_blank(), 
        axis.title.y =element_blank(), 
        axis.ticks.x=element_blank())


################################################################################
# 2. Contours 
################################################################################
library(cluster)      # For silhouette analysis
library(factoextra)   # For cluster visualization
library(umap)         # For UMAP dimensionality reduction


set.seed(2024)

# Prepare data for clustering
data_clusters <- prepare_data_for_clustering(data_melodies)

# Select relevant features for clustering
features <- data_clusters[, list_4int_pitches]

# Step 1: UMAP for Dimensionality Reduction
umap_config <- umap.defaults
umap_config$n_neighbors <- 15  # Adjust for local/global balance
umap_config$min_dist <- 0.1   # Adjust for compactness of clusters
umap_config$n_components <- 2 # Reduce to 2 dimensions for clustering

umap_res <- umap(features, config = umap_config)
umap_data <- as.data.frame(umap_res$layout)
colnames(umap_data) <- c("UMAP1", "UMAP2")  # Rename columns for clarity


# Step 2: Optimal Number of Clusters
fviz_nbclust(umap_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k") +
  theme_minimal()
 
fviz_nbclust(umap_data, kmeans, method = "silhouette") +
  labs(title = "Silhouette Analysis for Optimal k") +
  theme_minimal()


# Step 3: Clustering
# Set the number of clusters 
k <- 4  

cluster <- kmeans(umap_data, centers = k, nstart = 50)
umap_data$cluster <- as.factor(cluster$cluster)

# Step 4: Validate Clustering with Silhouette Score
silhouette_score <- silhouette(cluster$cluster, dist(umap_data))
mean_silhouette_score <- mean(silhouette_score[, 3])
mean_silhouette_score # 0.59


# Step 4: Visualize Clusters
ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 1, alpha = 0.7) +
  labs(
    title = "UMAP (k = 8)",
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "Cluster"
  ) +
  theme_minimal()


ggsave(paste0("UMAP_clusters_k4.png"),
       height = 12, width = 15,
       units = "cm", bg = "white",
       dpi=300)


# Step 6: Visualize Clustering Results
cluster_summary <- data_clusters %>%
  bind_cols(umap_data) %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    across(starts_with("UMAP"), mean),
    .groups = "drop"
  )
print(cluster_summary)


data_nets_clusters_sum = data_clusters%>%
  bind_cols(umap_data) %>% 
  # filter(degree %in% 8:10) %>%
  rename(N1 = sung_pitch1, N2 = sung_pitch2, N3 = sung_pitch3, N4 = sung_pitch4, N5 = sung_pitch5) %>%
  pivot_longer(cols = N1:N5, names_to = "index", values_to = "pitch") %>%
  group_by(network_id, cluster, index) %>% 
  dplyr::summarise(n=n(), av.pitch = mean(pitch, na.rm = T)) %>% 
  group_by(cluster, index) %>%
  dplyr::summarise(
    n=n(),
    av = mean(av.pitch, na.rm = T),
    sd = sd(av.pitch, na.rm = T),
    se = sd/ sqrt(n)
  ) 


plot_clusters = data_nets_clusters_sum %>% 
  ggplot(aes(x=as.factor(index), y=av, group=cluster, fill=cluster)) + 
  geom_ribbon(aes(ymin=av-se, ymax=av+se),  alpha = 0.75) + 
  geom_line() +
  facet_wrap(~cluster, nrow = 2) +
  # ggtitle(z) +
  theme(axis.text.x = element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right",
        legend.text = element_text(size=12),
        # legend.title = element_blank(),
        plot.title = element_text(size=6))

plot_clusters


ggsave(paste0("UMAP_clusters_sd.png"),
       plot_clusters,
       height = 8, width = 18,
       units = "cm",
       dpi=300)

