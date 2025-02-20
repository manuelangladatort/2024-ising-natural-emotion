# This function is a bootstrap function that takes a data frame, 
# a number of bootstraps, a variable name, a sigma value, a grouping variable, 
# and a range of values to plot. 
# 
# It returns a data frame with the x values, 
# the standard deviation of the bootstrapped values, and the average of the bootstrapped values.

kde_bootstrap_1d <- function(data, nboot, vname, sigma, over, interval_range){
  
  variants <- data.frame()
  
  for (i in 1:nboot){
    
    if (over == "participants"){
      data_sample <- data %>% group_by(participant_id) %>% group_split()
    } else if (over == "chains"){
      data_sample <- data %>% group_by(network_id) %>% group_split()
    } else {
      data_sample <- NA
    }
    
    data_sample <- sample(data_sample, length(data_sample), replace=TRUE)
    data_sample <- bind_rows(data_sample) 
    
    kde <- density(data_sample[[vname]],
                   bw = sigma,
                   kernel = "gaussian",
                   from = min(interval_range) - 3*sigma,
                   to = max(interval_range) + 3*sigma)
    
    df <- data.frame(y=kde$y)
    names(df)[1]<-paste0("y", toString(i))
    
    if (i==1) { variants <- df} else {variants <- cbind(variants,df)}
    
    # df<-data.frame(x=kde$x,y=kde$y)
    # p <- df %>% ggplot(aes(x=x,y=y)) + geom_line()
  }
  
  toplot <- data.frame(
    x = kde$x,
    sdev = transform(variants, sdev=apply(variants,1, sd, na.rm = TRUE))['sdev'],
    avg = transform(variants, avg=apply(variants,1, mean, na.rm = TRUE))['avg']
  )
  
  toplot
}