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
natising1_test <- read_csv("data/natising1_emotions.csv")


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
# repeat analysis above with emotions
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
  # filter(emotion != "neutral") %>% # comment in to remove the "neutral" condition
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
  # filter(emotion != "neutral") %>% # comment in to remove the "neutral" condition
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
  # filter(emotion != "neutral") %>% # comment in to remove the "neutral" condition
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
  # filter(emotion != "neutral") %>% # comment in to remove the "neutral" condition
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
