################################################################################
# Iterated singing (tonality & scales)
# Script: figure 3
################################################################################
# import 
library(tidyverse)
library(egg)
library(ggpubr)
library(cowplot)

source("utils/plots.R")


# global 
loadNamespace("egg")
theme_set(theme_pubr())

MAX_INTERVAL_SIZE = 13
interval_range = c(-MAX_INTERVAL_SIZE,MAX_INTERVAL_SIZE)
vertical.lines = seq(from=min(interval_range), to=max(interval_range), by = 1)

# data
data_melodies <- read_csv("data/data-clean/sing-scales-v2/data-sing-scales-v2_full_tonality.csv")


################################################################################
# Clustering 
################################################################################
library(cluster)      # For silhouette analysis
library(factoextra)   # For cluster visualization
library(umap)         # For UMAP dimensionality reduction


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


# Step 2: Optimal Number of Clusters --> it shows k = 4
# fviz_nbclust(umap_data, kmeans, method = "wss") +
#   labs(title = "Elbow Method for Optimal k") +
#   theme_minimal()
# 
# fviz_nbclust(umap_data, kmeans, method = "silhouette") +
#   labs(title = "Silhouette Analysis for Optimal k") +
#   theme_minimal()


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
data_clusters %>%
  bind_cols(umap_data) %>% 
  ggplot(aes(x = UMAP1, y = UMAP2, color = cluster)) +
    geom_point(size = 1, alpha = 0.7) +
    labs(
      title = "UMAP (k = 8)",
      x = "UMAP Dimension 1",
      y = "UMAP Dimension 2",
      color = "Cluster"
    ) +
    theme_classic()


ggsave(paste0("results/figure3/UMAP_clusters_k4.png"),
       height = 12, width = 15,
       units = "cm", bg = "white",
       dpi=300)


# Visualize Clustering Results
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


ggsave(paste0("results/figure3/UMAP_clusters_sd.png"),
       plot_clusters,
       height = 8, width = 16,
       units = "cm",
       dpi=300)

# plote evolution
data_clusters%>%
  bind_cols(umap_data) %>% 
  group_by(degree, cluster) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot(aes(x=degree, y=n, group=cluster)) +
  geom_line(aes(color=cluster))+
  geom_point(aes(color=cluster)) +
  ylab("Percentage") +
  facet_wrap(~cluster, ncol = 2) 


# do contours change based on mode?

data_clusters %>%
  bind_cols(umap_data) %>% 
  # filter(degree %in% 9:10) %>%
  ggplot(aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 1, alpha = 0.7) +
  labs(
    title = "UMAP (k = 8)",
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "Cluster"
  ) +
  theme_minimal() +
  facet_wrap(~mode, nrow = 1)


data_filtered = data_clusters %>%
  bind_cols(umap_data) %>% 
  filter(degree %in% 9:11) 

data_filtered %>%
  group_by(mode, cluster) %>%
  summarise(
    count = n(),
    percent = (count / nrow(data_filtered)) * 100,
    se = sqrt((percent / 100) * (1 - (percent / 100)) / count) * 100  # Binomial SE
  ) %>%
  ggplot(aes(x = cluster, y = percent, fill = mode)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(
    aes(ymin = percent - se, ymax = percent + se),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  theme_minimal()


ggsave(paste0("results/figure3/UMAP_clusters_mode.png"),
       height = 8, width = 16,
       units = "cm", bg = "white",
       dpi=300)
