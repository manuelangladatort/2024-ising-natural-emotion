################################################################################
# Iterated singing (naturalistic)
# Script: Analysis
################################################################################
# import libraries
library(tidyverse)
library(egg)
library(ggpubr)
library(cowplot)

# global parameters
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 12
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

# import data
natising1_test <- read_csv("data/natising1_test.csv")


################################################################################
# Analysis
################################################################################

################################################################################
# calculate the mean interval error over generations
error_data = natising1_test %>% 
  group_by(degree) %>% 
  summarise(
    n = n(), 
    mean_pitch_interval_error = mean(root_mean_squared_interval, na.rm = T),
    sd_pitch_interval_error = sd(root_mean_squared_interval, na.rm = T),
    se_pitch_interval_error = sd_pitch_interval_error/sqrt(n),
    mean_note_duration_error = mean(note_duration_root_mean_squared, na.rm = T),
    sd_note_duration_error = sd(note_duration_root_mean_squared, na.rm = T),
    se_note_duration_error = sd_note_duration_error/sqrt(n)
  )

# plot pitch interval error
plot_pitch_interval_error = error_data %>%
  ggplot(aes(x= degree, y = mean_pitch_interval_error)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_pitch_interval_error - se_pitch_interval_error, 
                  ymax=mean_pitch_interval_error + se_pitch_interval_error),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  # add labels
  ylab("Copying Error (Pitch Interval)") +
  xlab("Iteration") +
  ggtitle("Evolution Copying Error (pitch interval)") +
  theme_classic() +
  theme(legend.position = "none")

plot_pitch_interval_error


# plot note duration error
plot_note_duration_error = error_data %>%
  ggplot(aes(x= degree, y = mean_note_duration_error)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_note_duration_error - se_note_duration_error, 
                  ymax=mean_note_duration_error + se_note_duration_error),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  # add labels
  ylab("Copying Error (Note Duration)") +
  xlab("Iteration") +
  ggtitle("Evolution Copying Error (time)") +
  theme_classic() +
  theme(legend.position = "none")

plot_note_duration_error

plot_grid(plot_pitch_interval_error, plot_note_duration_error, nrow = 1)

ggsave("evolution_error.png", width = 16, height = 8, units = "cm")



################################################################################
# calculate the mean ratings of valence and arousal
rating_data = natising1_test %>% 
  group_by(degree) %>% 
  summarise(
    n = n(), 
    mean_valence = mean(valence, na.rm = T),
    sd_valence = sd(valence, na.rm = T),
    se_valence = mean_valence/sqrt(n),
    mean_arousal = mean(arousal, na.rm = T),
    sd_arousal = sd(arousal, na.rm = T),
    se_arousal = sd_arousal/sqrt(n),
  ) %>% 
  # we do not have data for ratings in the last generation of singing production
  # I am thus removing that generation from the data
  mutate(degree = degree -1) %>% 
  filter(degree != -1)

# plot mean ratings valence
plot_valence = rating_data %>%
  ggplot(aes(x= degree, y = mean_valence)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_valence - se_valence, 
                  ymax=mean_valence + se_valence),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0, 1,2,3,4,5,6,7,8,9)) +
  # add labels
  ylab("Mean rating valence") +
  xlab("Iteration") +
  ggtitle("Evolution Emotion (valence)") +
  theme_classic() +
  theme(legend.position = "none")

plot_valence


# plot mean ratings arousal
plot_arousal = rating_data %>%
  ggplot(aes(x= degree, y = mean_arousal)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_arousal - se_arousal, 
                  ymax=mean_arousal + se_arousal),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0, 1,2,3,4,5,6,7,8,9)) +
  # add labels
  ylab("Mean rating arousal") +
  xlab("Iteration") +
  ggtitle("Evolution Emotion (arousal)") +
  theme_classic() +
  theme(legend.position = "none")

plot_arousal

plot_grid(plot_valence, plot_arousal)

ggsave("evolution_emotion.png", width = 16, height = 8, units = "cm")


################################################################################
# Calculate the mean ratings of valence and arousal
rating_data = natising1_test %>% 
  group_by(degree) %>% 
  summarise(
    n = n(), 
    mean_valence = mean(valence, na.rm = T),
    sd_valence = sd(valence, na.rm = T),
    se_valence = mean_valence/sqrt(n),
    mean_arousal = mean(arousal, na.rm = T),
    sd_arousal = sd(arousal, na.rm = T),
    se_arousal = sd_arousal/sqrt(n),
  ) %>% 
  # Remove data for ratings in the last generation of singing production
  mutate(degree = degree -1) %>% 
  filter(degree != -1)

# Plot mean ratings in a two-dimensional space
plot_valence_arousal = rating_data %>%
  ggplot(aes(x= mean_valence, y = mean_arousal)) + 
  # plot line connecting points
  geom_line(aes(group = 1)) +
  # plot points
  geom_point(size = 2, shape=21, aes(fill=degree)) +
  # add text labels for degrees
  geom_text(aes(label = degree), vjust = -1, hjust = 1) +
  # add standard error bars
  geom_errorbar(aes(ymin=mean_arousal - se_arousal, 
                    ymax=mean_arousal + se_arousal), 
                width=0.1, alpha = 0.4) +
  geom_errorbarh(aes(xmin=mean_valence - se_valence, 
                     xmax=mean_valence + se_valence), 
                 height=0.1, alpha = 0.4) +
  # set x and y axis limits
  scale_x_continuous(limits = c(3, 7), breaks = 3:7) +
  scale_y_continuous(limits = c(3, 7), breaks = 3:7) +
  geom_vline(xintercept = 5, linetype = "dashed") +
  geom_hline(yintercept = 5, linetype = "dashed") +
  # add labels
  xlab("Mean rating valence") +
  ylab("Mean rating arousal") +
  ggtitle("Evolution Emotion (valence vs arousal)") +
  theme_classic() +
  theme(legend.position = "none")

plot_valence_arousal



################################################################################
# Classify melodies in happy or sad
mean_ratings_end = natising1_test %>% 
  filter(degree %in% 7:10) %>%
  group_by(network_id) %>% 
  summarise(
    n = n(), 
    mean_valence = mean(valence, na.rm = T),
    sd_valence = sd(valence, na.rm = T),
    mean_arousal = mean(arousal, na.rm = T),
    sd_arousal = sd(arousal, na.rm = T),
    se_valence = sd_valence/sqrt(n),
    se_arousal = sd_arousal/sqrt(n)
  )  %>% 
  mutate(emotion =   case_when(
    mean_valence > 5 & mean_arousal > 5 ~ "happy",
    mean_valence < 5 & mean_arousal < 5 ~ "sad",
    TRUE ~ "neutral"
  ))

mean_ratings_end %>%
  ggplot(aes(x= mean_valence, y = mean_arousal, fill = emotion)) + 
  # plot points
  geom_point(size = 2, shape=21, aes(fill = emotion)) +
  scale_x_continuous(limits = c(2, 8), breaks = 2:8) +
  scale_y_continuous(limits = c(2, 8), breaks = 2:8) +
  geom_vline(xintercept = 5, linetype = "dashed") +
  geom_hline(yintercept = 5, linetype = "dashed") +
  # add labels
  xlab("Mean rating valence") +
  ylab("Mean rating arousal") +
  ggtitle("Distribution of melodies on the emotion space (degree 7:10)") +
  theme_classic() +
  theme(legend.position = "right")

ggsave("emotion_melodies_last3generations.png", width = 15, height = 15, units = "cm")


# over time
mean_ratings_end = natising1_test %>% 
  mutate(degree_group = case_when(
    degree %in% 0:3 ~ "1:3",
    degree %in% 4:6 ~ "4:6",
    degree %in% 7:10 ~ "7:10",
  )) %>%
  group_by(network_id, degree_group) %>% 
  summarise(
    n = n(), 
    mean_valence = mean(valence, na.rm = T),
    mean_arousal = mean(arousal, na.rm = T),
  )  %>% 
  mutate(emotion =   case_when(
    mean_valence > 5 & mean_arousal > 5 ~ "happy",
    mean_valence < 5 & mean_arousal < 5 ~ "sad",
    TRUE ~ "neutral"
  ))

mean_ratings_end %>%
  ggplot(aes(x= mean_valence, y = mean_arousal, fill = emotion)) + 
  # plot points
  geom_point(size = 2, shape=21, aes(fill = emotion)) +
  scale_x_continuous(limits = c(2, 8), breaks = 2:8) +
  scale_y_continuous(limits = c(2, 8), breaks = 2:8) +
  geom_vline(xintercept = 5, linetype = "dashed") +
  geom_hline(yintercept = 5, linetype = "dashed") +
  # add labels
  xlab("Mean rating valence") +
  ylab("Mean rating arousal") +
  ggtitle("Distribution of melodies on the emotion space over time") +
  theme_classic() +
  theme(legend.position = "right") +
  facet_wrap(~degree_group)

ggsave("emotion_melodies_evolve.png", width = 22, height = 12, units = "cm")


################################################################################
# repeat analysis above with emotions
emotions_melodies = mean_ratings_end %>%  select(network_id, emotion)

natising1_test = natising1_test %>%  left_join(emotions_melodies, by = "network_id")


rating_data = natising1_test %>% 
  group_by(degree, emotion) %>% 
  summarise(
    n = n(), 
    mean_valence = mean(valence, na.rm = T),
    sd_valence = sd(valence, na.rm = T),
    se_valence = mean_valence/sqrt(n),
    mean_arousal = mean(arousal, na.rm = T),
    sd_arousal = sd(arousal, na.rm = T),
    se_arousal = sd_arousal/sqrt(n),
  ) %>% 
  # we do not have data for ratings in the last generation of singing production
  # I am thus removing that generation from the data
  mutate(degree = degree -1) %>% 
  filter(degree != -1)

# plot mean ratings valence
plot_valence = rating_data %>%
  filter(emotion != "neutral") %>%
  ggplot(aes(x= degree, y = mean_valence, fill = emotion)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_valence - se_valence, 
                  ymax=mean_valence + se_valence),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0, 1,2,3,4,5,6,7,8,9)) +
  # add labels
  ylab("Mean rating valence") +
  xlab("Iteration") +
  ggtitle("Evolution Emotion (valence)") +
  theme_classic() +
  theme(legend.position = "right")

plot_valence


# plot mean ratings arousal
plot_arousal = rating_data %>%
  filter(emotion != "neutral") %>%
  ggplot(aes(x= degree, y = mean_arousal, fill = emotion)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_arousal - se_arousal, 
                  ymax=mean_arousal + se_arousal),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(0, 1,2,3,4,5,6,7,8,9)) +
  # add labels
  ylab("Mean rating arousal") +
  xlab("Iteration") +
  ggtitle("Evolution Emotion (arousal)") +
  theme_classic() +
  theme(legend.position = "right")

plot_arousal

plot_grid(plot_valence, plot_arousal)

ggsave("evolution_emotion_categoties.png", width = 16, height = 8, units = "cm")


# calculate the mean interval error over generations
error_data = natising1_test %>% 
  group_by(degree, emotion) %>% 
  summarise(
    n = n(), 
    mean_pitch_interval_error = mean(root_mean_squared_interval, na.rm = T),
    sd_pitch_interval_error = sd(root_mean_squared_interval, na.rm = T),
    se_pitch_interval_error = sd_pitch_interval_error/sqrt(n),
    mean_note_duration_error = mean(note_duration_root_mean_squared, na.rm = T),
    sd_note_duration_error = sd(note_duration_root_mean_squared, na.rm = T),
    se_note_duration_error = sd_note_duration_error/sqrt(n)
  )

# plot pitch interval error
plot_pitch_interval_error = error_data %>%
  filter(emotion != "neutral") %>%
  ggplot(aes(x= degree, y = mean_pitch_interval_error, fill = emotion)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_pitch_interval_error - se_pitch_interval_error, 
                  ymax=mean_pitch_interval_error + se_pitch_interval_error),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  # add labels
  ylab("Copying Error (Pitch Interval)") +
  xlab("Iteration") +
  ggtitle("Evolution Copying Error (pitch interval)") +
  theme_classic() +
  theme(legend.position = "right")

plot_pitch_interval_error


# plot note duration error
plot_note_duration_error = error_data %>%
  filter(emotion != "neutral") %>%
  ggplot(aes(x= degree, y = mean_note_duration_error, fill = emotion)) + 
  # plot line
  geom_line()+
  # plot standard deviation (SD) around the line
  geom_ribbon(aes(ymin=mean_note_duration_error - se_note_duration_error, 
                  ymax=mean_note_duration_error + se_note_duration_error),  
              alpha = 0.4)  +
  # plot points
  geom_point(size = 2, shape=21) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  # add labels
  ylab("Copying Error (Note Duration)") +
  xlab("Iteration") +
  ggtitle("Evolution Copying Error (time)") +
  theme_classic() +
  theme(legend.position = "right")

plot_note_duration_error

plot_grid(plot_pitch_interval_error, plot_note_duration_error, nrow = 1)

ggsave("evolution_error_categories.png", width = 16, height = 8, units = "cm")
