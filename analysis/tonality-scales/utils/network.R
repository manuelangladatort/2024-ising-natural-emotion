library(jsonlite)
library(entropy)

MAX_ITER <- 10

run_edge_similarity = function(data){
  all_apps <- unique(data$unique_tag)
  
  all_agree<- list()
  all_sim<- list()
  all_stats<-list()
  
  for (app in all_apps) {
    print(app)
    
    current <- data[data$unique_tag == app, ]
    
    list_of_edges <- c()
    
    base_layer <- current[current$degree == 0, ]
    
    for (i in 1:nrow(base_layer)) {
      vid <- base_layer[i, "vertex_id"]
      dep_ids <- jsonlite::fromJSON(as.character(base_layer[i, "dependent_vertex_ids"]))
      for (dep_vid in dep_ids) {
        if (vid < dep_vid) {
          list_of_edges <- c(list_of_edges, toJSON(c(as.integer(vid), as.integer(dep_vid))))
        } else {
          list_of_edges <- c(list_of_edges, toJSON(c(as.integer(dep_vid), as.integer(vid))))
        }
      }
    }
    list_of_edges <- unique(list_of_edges)
    print(length(list_of_edges))
    
    agreement_list <- c()
    distance_list <- c()
    store <- c()
    
    for (it in 0:MAX_ITER) {
      agree_list <- c()
      sim_list <- c()
      
      for (edge in list_of_edges) {
        v <- as.integer(fromJSON(edge))
        v1 <- v[1]
        v2 <- v[2]
        df <- current[current$degree == it, ]
        c1 <- df[df$vertex_id == v1, "kmeans_cluster"]
        c2 <- df[df$vertex_id == v2, "kmeans_cluster"]
        
        m1 <- df[df$vertex_id == v1, c("sung_interval1", "sung_interval2",  "sung_interval3",  "sung_interval4")]
        m2 <- df[df$vertex_id == v2, c("sung_interval1", "sung_interval2",  "sung_interval3",  "sung_interval4")]
        m1 = c(m1$sung_interval1, m1$sung_interval2, m1$sung_interval3, m1$sung_interval4)
        m2 = c(m2$sung_interval1, m2$sung_interval2, m2$sung_interval3, m2$sung_interval4)
        distance <- sqrt(sum((m1 - m2)^2))
        sim_list <- c(sim_list, distance)
        
        if (identical(c1, c2)) {
          agree_list <- c(agree_list, 1)
        } else {
          agree_list <- c(agree_list, 0)
        }
      }
      agreement_list <- c(agreement_list, mean(agree_list))
      distance_list <- c(distance_list, mean(sim_list))
      
      store[[(it + 1)]] <- tibble(
        unique_tag = app,
        degree = it,
        mean_agreement = mean(agree_list),
        # sd_agreement = sd(agree_list),
        mean_similarity = mean(sim_list),
        # sd_similarity = sd(sim_list),
        n = length(agree_list),
        se_agreement = sd(agree_list) / sqrt(n),
        se_similarity = sd(sim_list) / sqrt(n)
      )
      
    }
    
    all_agree[[app]] <- agreement_list
    all_sim[[app]] <- distance_list
    all_stats[[app]] = do.call(rbind, store)
    
  }
  
  all_stats = do.call(rbind, all_stats)
  
  return(all_stats)
}


run_set_similarity = function(data){
  all_apps <- unique(data$unique_tag)
  
  all_agree<- list()
  all_sim<- list()
  all_stats<-list()
  
  for (app in all_apps) {
    print(app)
    
    current <- data[data$unique_tag == app, ]
    
    list_of_edges <- c()
    
    base_layer <- current[current$degree == 0, ]
    
    for (i in 1:nrow(base_layer)) {
      vid <- base_layer[i, "vertex_id"]
      dep_ids <- jsonlite::fromJSON(as.character(base_layer[i, "dependent_vertex_ids"]))
      list_of_edges <- c(list_of_edges, toJSON(c(as.integer(vid), as.integer(dep_ids))))
      }
    list_of_edges <- unique(list_of_edges)
    print(length(list_of_edges))
    
    agreement_list <- c()
    distance_list <- c()
    store <- c()
    
    for (it in 0:MAX_ITER) {
      agree_list <- c()
      sim_list <- c()
      
      for (edge in list_of_edges) {
        v <- as.integer(fromJSON(edge))
        
        v1 <- v[1]
        v2 <- v[2]
        v3 <- v[3]
        v4 <- v[4]
        v5 <- v[5]
        
        df <- current[current$degree == it, ]
        
        c1 <- df[df$vertex_id == v1, "kmeans_cluster"]
        c2 <- df[df$vertex_id == v2, "kmeans_cluster"]
        c3 <- df[df$vertex_id == v3, "kmeans_cluster"]
        c4 <- df[df$vertex_id == v4, "kmeans_cluster"]
        c5 <- df[df$vertex_id == v5, "kmeans_cluster"]
        
        m1 <- df[df$vertex_id == v1, c("sung_interval1", "sung_interval2",  "sung_interval3",  "sung_interval4")]
        m2 <- df[df$vertex_id == v2, c("sung_interval1", "sung_interval2",  "sung_interval3",  "sung_interval4")]
        m3 <- df[df$vertex_id == v3, c("sung_interval1", "sung_interval2",  "sung_interval3",  "sung_interval4")]
        m4 <- df[df$vertex_id == v4, c("sung_interval1", "sung_interval2",  "sung_interval3",  "sung_interval4")]
        m5 <- df[df$vertex_id == v5, c("sung_interval1", "sung_interval2",  "sung_interval3",  "sung_interval4")]
        
        m1 = c(m1$sung_interval1, m1$sung_interval2, m1$sung_interval3, m1$sung_interval4)
        m2 = c(m2$sung_interval1, m2$sung_interval2, m2$sung_interval3, m2$sung_interval4)
        m3 = c(m3$sung_interval1, m3$sung_interval2, m3$sung_interval3, m3$sung_interval4)
        m4 = c(m4$sung_interval1, m4$sung_interval2, m4$sung_interval3, m4$sung_interval4)
        m5 = c(m5$sung_interval1, m5$sung_interval2, m5$sung_interval3, m5$sung_interval4)
        
        average_euclidean_distance <- calculate_average_euclidean_distance(m1, m2, m3, m4, m5)
        
        sim_list <- c(sim_list, average_euclidean_distance)

        vec <- c(c1$kmeans_cluster, c2$kmeans_cluster, c3$kmeans_cluster, c4$kmeans_cluster, c5$kmeans_cluster)
        value_counts <- table(vec)
        total_elements <- length(vec)
        duplicate_elements <- sum(value_counts[value_counts > 1])
        proportion_duplicates <- duplicate_elements / total_elements
        agree_list <- c(agree_list, proportion_duplicates)
      }
      agreement_list <- c(agreement_list, mean(agree_list))
      distance_list <- c(distance_list, mean(sim_list))
      
      store[[(it + 1)]] <- tibble(
        unique_tag = app,
        degree = it,
        mean_agreement = mean(agree_list),
        mean_similarity = mean(sim_list),
        n = length(agree_list),
        se_agreement = sd(agree_list) / sqrt(n),
        se_similarity = sd(sim_list) / sqrt(n)
      )
      
    }
    
    # all_agree[[app]] <- agreement_list
    # all_sim[[app]] <- distance_list
    all_stats[[app]] = do.call(rbind, store)
    
  }
  
  all_stats = do.call(rbind, all_stats)
  
  return(all_stats)
}


calculate_average_euclidean_distance <- function(m1, m2, m3, m4, m5) {
  # Store the vectors in a list
  vectors <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5)
  
  # Initialize a variable to sum the distances
  total_distance <- 0
  num_pairs <- 0
  
  # Calculate the pairwise Euclidean distances
  for (i in 1:(length(vectors) - 1)) {
    for (j in (i + 1):length(vectors)) {
      total_distance <- total_distance + sqrt(sum((vectors[[i]] - vectors[[j]])^2))
      num_pairs <- num_pairs + 1
    }
  }
  
  # Calculate the average distance
  average_distance <- total_distance / num_pairs
  
  return(average_distance)
}


# cluster entropy
get_cluster_entropy <- function(
    data, 
    nboot,
    max_iter = MAX_ITER,
    use_contour_clusters = TRUE
){
  
  # choose cluster type
  if (use_contour_clusters) {
    unique_clusters <- unique(data$contour_cluster)
    cluster_id_list <- sapply(1:nrow(data), function(i) {
      which(data$contour_cluster[i] == unique_clusters) - 1
    })
    num_clusters <- length(unique_clusters)
    data_diverstiy <- data %>% mutate(cluster_id = cluster_id_list)
  } else {
    unique_clusters <- unique(data$kmeans_cluster)
    num_clusters <- length(unique_clusters)
    data_diverstiy <- data %>% mutate(cluster_id = kmeans_cluster)
  }
  
  all_apps <- unique(data_diverstiy$unique_tag)
  
  # Function to calculate entropy
  entropy <- function(prob) {
    prob <- prob[prob > 0] # Remove zero probabilities to avoid log(0)
    -sum(prob * log(prob))
  }
  
  # loop over boots
  store <- list()
  for (i in 1:nboot){
    
    cluster_entropy <- list()
    
    print(paste("features boot", i, "out of", nboot))
    
    # sample with replacement
    data_sample <- data_diverstiy %>% group_by(network_id) %>% group_split()
    data_sample <- sample(data_sample, length(data_sample), replace=TRUE)
    data_sample <- bind_rows(data_sample) 
    
    # loop over apps
    for (a in all_apps) {
      mean_entropy_per_iter <- c()
      print(a)
      current_subset <- data_sample %>% filter(unique_tag == a)
      
      for (it in 0:max_iter) {
        current <- filter(current_subset, degree == it)
        probability <- sapply(1:num_clusters, function(i) {
          sum(current$cluster_id == i) / length(table(data_sample$vertex_id))
        })
        mean_entropy_per_iter <- c(mean_entropy_per_iter, entropy(probability))
      }
      
      cluster_entropy[[a]] <- tibble(
        degree = rep(0:max_iter),
        mean_entropy_per_iter = mean_entropy_per_iter,
        unique_tag = a
      )
      
    }
    
    cluster_entropy = do.call(rbind, cluster_entropy)
    
    store[[i]] = cluster_entropy %>%  mutate(boot = 1)
  }
  
  o = do.call(rbind, store)
  
  return(o)
}

