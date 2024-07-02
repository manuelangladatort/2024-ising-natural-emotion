################################################################################
# Current Biology (2022)
# Authors: Manuel Anglada-Tort, Peter Harrison, and Nori Jacoby
# Script: Methods for plotting
################################################################################


################################################################################
# 2D KDE
################################################################################
add_peaks_marginals_loop <- function(plot, dots, dots_ci, color){
  p <-   plot
  
  for(i in 1:nrow(dots_ci)){ 
    p <- p +
      annotate("rect", xmin = dots_ci$min_ci[[i]], xmax = dots_ci$max_ci[[i]], ymin = -Inf, ymax = Inf, fill = color, alpha = .1, color = NA) +
      geom_point(x = dots$x[[i]], y = dots$y[[i]], color="red", size = 2)
  }
  
  return(p)
}


# same as above but coloring dots too
add_peaks_marginals_loop_colored.dots <- function(plot, dots, dots_ci, color){
  p <-   plot
  
  for(i in 1:nrow(dots_ci)){ 
    p <- p +
      annotate("rect", xmin = dots_ci$min_ci[[i]], xmax = dots_ci$max_ci[[i]], ymin = -Inf, ymax = Inf, fill = color, alpha = .1, color = NA) +
      geom_point(x = dots$x[[i]], y = dots$y[[i]], color=color, size = 2)
  }
  
  return(p)
}


generate_2Dkde_main <- function(data, dots1_ci, dots2_ci, interval_range, bw){
  data_int_class = data %>%
    mutate(int_class = ifelse(degree %in% 0, "seed",
                              ifelse(degree %in% 1:3, "first3",
                                     ifelse(degree %in% 4:7, "second3","last3")))) 
  
  data_int_class$int_class = factor(data_int_class$int_class, 
                                    levels = c("seed", "first3", "second3", "last3"),
                                    labels=c("Generation 0", "Generation 1:3", "Generation 4:6", "Generation 7:10"))
  
  # individual marginals
  
  # plot main 1
  marginal_int1 = make_marginal_1d(data_int_class, sung_interval1, bw) 
  marginal_int2 = make_marginal_1d(data_int_class, sung_interval2, bw) 
  
  # find y based on peaks (last generation)
  data_int_class_last3 = data_int_class %>%  filter(int_class == "Generation 7:10")
  
  dots1 = get_sing_dots_marginals(data_int_class_last3, dots1_ci, sung_interval1, bw)
  dots2 = get_sing_dots_marginals(data_int_class_last3, dots2_ci, sung_interval2, bw)
  
  # add peaks and CI in marginals
  marginal_int1 = add_peaks_marginals_loop(marginal_int1, dots1, dots1_ci, "red")
  marginal_int2 = add_peaks_marginals_loop(marginal_int2, dots2, dots2_ci, "red")
  
  uniform_density <- 1 / (diff(c(interval_range[[1]],interval_range[[2]])) *diff(c(interval_range[[1]],interval_range[[2]])))
  
  p <- data_int_class %>%
    filter(degree %in% 7:10) %>%
    ggplot(aes(x = sung_interval1, y = sung_interval2)) +
    stat_density_2d(aes(fill = ..density.. / (1 / uniform_density)), 
                    geom = "raster", 
                    contour = FALSE, 
                    adjust = 1/2,
                    # alpha=0.9,
                    n = 500) +
    scale_fill_viridis_c("Density", scales::pretty_breaks(n = 3)) + 
    scale_x_continuous(breaks=seq(interval_range[[1]],interval_range[[2]],1),limits=c(interval_range[[1]],interval_range[[2]])) +
    scale_y_continuous(breaks=seq((interval_range[[1]]),interval_range[[2]],1),limits=c(interval_range[[1]],interval_range[[2]])) +
    labs(fill = NULL,x="Interval 1",y="Interval 2") +
    geom_point(size = 0.1, colour = "white", alpha = 0.6) + 
    geom_point(data = data_int_class, 
               mapping = aes(x = sung_interval1, y = sung_interval2),
               size = 0.01, colour = "white", alpha = 0.2) + 
    theme(aspect.ratio=1,
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none") 
  
  empty_plot = ggplot() +
    theme_void() +
    xlab(NULL) #optional, but safer in case another theme is applied later
  
  all <- egg::ggarrange(
    marginal_int1 + 
      theme(axis.text = element_blank(), 
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_text(size = 8)), 
    empty_plot, 
    p,
    marginal_int2 + 
      scale_y_continuous("Density") +
      coord_flip() + 
      theme(axis.text = element_blank(), 
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_text(size = 8)), 
    ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  
  
  # out = list(all, p)
  return(all)
}


generate_2Dkde_basic = function(data, interval_range){
  
  uniform_density <- 1 / (diff(c(interval_range[[1]],interval_range[[2]])) *diff(c(interval_range[[1]],interval_range[[2]])))
  
  p <- data %>%
    ggplot(aes(x = sung_interval1, y = sung_interval2)) +
    stat_density_2d(aes(fill = ..density.. / (1 / uniform_density)), 
                    geom = "raster", 
                    contour = FALSE, 
                    adjust = 1/2,
                    # alpha=0.9,
                    n = 500) +
    scale_fill_viridis_c("Density", scales::pretty_breaks(n = 3)) + 
    scale_x_continuous(breaks=seq(interval_range[[1]],interval_range[[2]],1),limits=c(interval_range[[1]],interval_range[[2]])) +
    scale_y_continuous(breaks=seq((interval_range[[1]]),interval_range[[2]],1),limits=c(interval_range[[1]],interval_range[[2]])) +
    labs(fill = NULL,x="Interval 1",y="Interval 2") +
    geom_point(size = 0.1, colour = "white", alpha = 0.6) + 
    theme(aspect.ratio=1,
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none") 
  return(p)
}


generate_2Dkde_basic2 <- function(data, interval_range, bw){
  data_int_class = data %>%
    mutate(int_class = ifelse(degree %in% 0, "seed",
                              ifelse(degree %in% 1:3, "first3",
                                     ifelse(degree %in% 4:7, "second3","last3")))) 
  
  data_int_class$int_class = factor(data_int_class$int_class, 
                                    levels = c("seed", "first3", "second3", "last3"),
                                    labels=c("Generation 0", "Generation 1:3", "Generation 4:6", "Generation 7:10"))
  
  # individual marginals
  
  # plot main 1
  marginal_int1 = make_marginal_1d(data_int_class, sung_interval1, bw) 
  marginal_int2 = make_marginal_1d(data_int_class, sung_interval2, bw) 
  
  uniform_density <- 1 / (diff(c(interval_range[[1]],interval_range[[2]])) *diff(c(interval_range[[1]],interval_range[[2]])))
  
  p <- data_int_class %>%
    filter(degree %in% 7:10) %>%
    ggplot(aes(x = sung_interval1, y = sung_interval2)) +
    stat_density_2d(aes(fill = ..density.. / (1 / uniform_density)), 
                    geom = "raster", 
                    contour = FALSE, 
                    adjust = 1/2,
                    # alpha=0.9,
                    n = 500) +
    scale_fill_viridis_c("Density", scales::pretty_breaks(n = 3)) + 
    scale_x_continuous(breaks=seq(interval_range[[1]],interval_range[[2]],1),limits=c(interval_range[[1]],interval_range[[2]])) +
    scale_y_continuous(breaks=seq((interval_range[[1]]),interval_range[[2]],1),limits=c(interval_range[[1]],interval_range[[2]])) +
    labs(fill = NULL,x="Interval 1",y="Interval 2") +
    geom_point(size = 0.1, colour = "white", alpha = 0.6) + 
    geom_point(data = data_int_class, 
               mapping = aes(x = sung_interval1, y = sung_interval2),
               size = 0.01, colour = "white", alpha = 0.2) + 
    theme(aspect.ratio=1,
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none") 
  
  empty_plot = ggplot() +
    theme_void() +
    xlab(NULL) #optional, but safer in case another theme is applied later
  
  all <- egg::ggarrange(
    marginal_int1 + 
      theme(axis.text = element_blank(), 
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_text(size = 8)), 
    empty_plot, 
    p,
    marginal_int2 + 
      scale_y_continuous("Density") +
      coord_flip() + 
      theme(axis.text = element_blank(), 
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_text(size = 8)), 
    ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  
  
  # out = list(all, p)
  return(all)
}


################################################################################
# 1D KDE
################################################################################
make_marginals_kde = function(data, sung_intervals, n_samples, BW,  boot_over = "chains"){
  
  # last 3 iterations
  data_8.10 = data %>%
    dplyr::filter(degree %in% 8:10)  %>%
    dplyr::select(degree, network_id, sung_intervals) %>% 
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  # first 3 iterations
  data_1.3 = data %>%
    dplyr::filter(degree %in% 1:3)  %>%
    dplyr::select(degree, network_id, sung_intervals) %>% 
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  # seed
  data_seed = data %>%
    dplyr::filter(degree == 0)  %>%
    dplyr::select(degree, network_id, sung_intervals) %>%
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  data_to_plot_8.10 = kde_bootstrap_1d(data_8.10, n_samples, 
                                       "sung_intervals", 
                                       BW, 
                                       boot_over, 
                                       interval_range
  )
  data_to_plot_1.3 = kde_bootstrap_1d(data_1.3, n_samples,
                                      "sung_intervals",
                                      BW,
                                      boot_over,
                                      interval_range
  )
  data_to_plot_0 = kde_bootstrap_1d(data_seed, n_samples,
                                    "sung_intervals",
                                    (BW*3),
                                    boot_over,
                                    interval_range
  )
  
  colfunc <- colorRampPalette(c("red", "white"))
  cols = colfunc(10)
  
  
  p = data_to_plot_8.10 %>% ggplot(aes(x)) +
    geom_line(data=data_to_plot_1.3, aes(y = avg), color=cols[7], size = 0.5) +
    geom_line(data=data_to_plot_0, aes(y = avg), color="darkred", size =0.5,linetype="dashed") +
    scale_x_continuous(breaks=seq(min(vertical.lines), max(vertical.lines), 1),
                       limits = c(-13,13))  +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    geom_ribbon(aes(ymin = avg - sdev, ymax = avg + sdev), alpha = .4, fill = "grey") +
    geom_line(aes(y = avg), color="black", size =  0.7) +
    # add peaks
    xlab("vname") + ylab("density") +
    theme_classic() + 
    # labs(title = tt,
    #      subtitle = "Last 3 iterations (black 95% CI); first 3 iterations (light red); seed (darkred)") +
    theme(axis.text.x = element_text(size=10), 
          axis.text.y=element_text(size=10), 
          axis.title.x = element_blank(), 
          axis.title.y =element_blank(), 
          axis.ticks.x=element_blank())
  
  return(p)
}


make_marginals_kde_separate_4ints = function(data, sung_intervals, NBOOT, BW){
  
  marginals_int1 = make_marginals_kde(data, sung_intervals[1], NBOOT, BW)
  
  marginals_int2 = make_marginals_kde(data, sung_intervals[2], NBOOT, BW)
  
  marginals_int3 = make_marginals_kde(data, sung_intervals[3], NBOOT, BW)
  
  marginals_int4 = make_marginals_kde(data, sung_intervals[4], NBOOT, BW)
  
  p = plot_grid(marginals_int1, marginals_int2, marginals_int3, marginals_int4,
                ncol = 1)
  
  return(p)
}


make_marginal_1d = function(data, var, bw){
  evar = enquo(var)
  p = data %>% 
    ggplot(aes(x = !! evar, color=int_class)) +
    geom_density(data = subset(data, int_class == "Generation 0"),
                 size=0.25, 
                 bw = (bw * 3),
                 n = 1000) +
    geom_density(data = subset(data, int_class == "Generation 1:3"),
                 size=0.5,
                 bw = bw,
                 n = 1000)  +
    geom_density(data = subset(data, int_class == "Generation 7:10"),
                 size=0.7,
                 bw = bw,
                 n = 1000) +
    scale_color_manual(values=c("#800000", "#818181", "#333333"))  +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed", size = 0.25) +
    xlim(interval_range[[1]],interval_range[[2]]) + theme(axis.title.x=element_blank())  +
    theme_classic() + 
    theme(axis.text.x = element_blank(), 
          axis.text.y=element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y =element_blank(), 
          axis.ticks.x=element_blank(),
          legend.position = "none",
          legend.text = element_blank(), 
          legend.title = element_blank())
  return(p)
}


################################################################################
# Marginals
################################################################################
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


make_marginals_1datasets = function(
    data1, data2,
    peaks1,
    sung_intervals,
    NBOOT, bw, interval_range,
    colors
){
  
  # last 3 iterations
  # data1_seed = data1 %>% 
  #   filter(degree == 0) %>% 
  #   pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  data1_last3 = data1 %>% 
    filter(degree %in% 8:10) %>% 
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  data2_last3 = data2 %>% 
    filter(degree %in% 8:10) %>% 
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  # 
  # data1_seed_smooted = kde_bootstrap_1d(data1_seed, NBOOT, "sung_intervals",
  #                                             (bw*3), "chains", interval_range)
  
  
  data1_last3_smooted = kde_bootstrap_1d(data1_last3, NBOOT, "sung_intervals",
                                         bw, "chains", interval_range)
  
  
  data2_last3_smooted = kde_bootstrap_1d(data2_last3, NBOOT, "sung_intervals",
                                         bw, "chains", interval_range)
  
  plot = data1_last3_smooted %>% ggplot(aes(x)) +
    # geom_line(data=data1_seed_smooted, aes(y = avg), color="darkred", 
    #          size =0.25, linetype="dashed", alpha = .8) +
    geom_line(data=data2_last3_smooted, aes(y = avg), color=colors[[2]],
              linetype="dashed",
              size =0.5) +
    geom_line(aes(y = avg), 
              # linetype="dashed",
              color="black",
              size =  0.5) +
    geom_ribbon(aes(ymin = avg - sdev, ymax = avg + sdev), 
                alpha = .3, 
                fill = colors[[1]]) +
    scale_x_continuous(breaks=seq(min(vertical.lines), max(vertical.lines), 1),
                       limits = c(min(vertical.lines), max(vertical.lines)))  +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    xlab("vname") + ylab("density") +
    theme_classic() + 
    # labs(title = tt,
    #      subtitle = subtt) +
    theme(axis.text.x = element_text(size=10), 
          axis.text.y=element_text(size=10), 
          axis.title.x = element_blank(), 
          axis.title.y =element_blank(), 
          axis.ticks.x=element_blank())
  
  dots1 = get_sing_dots_marginals(data1_last3_smooted, peaks1, x, bw,
                                  is_kde_already = TRUE)
  
  plot_with_peaks1 = add_peaks_marginals_loop(plot, dots1, peaks1, "red")
  
  return(plot_with_peaks1)
}


make_marginals_2datasets_slider = function(
    data1, data2, 
    peaks1,
    sung_intervals,
    NBOOT, BW, interval_range,
    colors
){
  
  # last 3 iterations
  data1_8.10 = data1 %>%
    dplyr::filter(degree %in% 8:10)  %>%
    dplyr::select(degree, network_id, sung_intervals) %>% 
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  data2_8.10 = data2 %>%
    dplyr::filter(degree %in% 8:10)  %>%
    dplyr::select(degree, network_id, sung_intervals) %>% 
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  data1_to_plot_8.10 = kde_bootstrap_1d(data1_8.10, NBOOT, 
                                        "sung_intervals", 
                                        BW, 
                                        "chains", 
                                        interval_range
  )
  data2_to_plot_8.10 = kde_bootstrap_1d(data2_8.10, NBOOT, 
                                        "sung_intervals", 
                                        BW, 
                                        "chains", 
                                        interval_range
  )
  
  col1 = colors[[1]]
  col2 = colors[[2]]
  
  plot = data1_to_plot_8.10 %>% ggplot(aes(x)) +
    scale_x_continuous(breaks=seq(min(vertical.lines), max(vertical.lines), 1),
                       limits = c(min(vertical.lines), max(vertical.lines)))  +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    # geom_ribbon(aes(ymin = avg - sdev, ymax = avg + sdev), alpha = .2, fill = col1) +
    geom_line(aes(y = avg), color="black", size =  0.5, linetype="dashed") +
    geom_ribbon(data=data2_to_plot_8.10, aes(ymin = avg - sdev, ymax = avg + sdev), alpha = .3, fill = col2) +
    geom_line(data=data2_to_plot_8.10, aes(y = avg), color=col2, size = 0.5) +
    # add peaks
    xlab("vname") + ylab("density") +
    theme_classic() + 
    # labs(title = tt,
    #      subtitle = subtt) +
    theme(axis.text.x = element_text(size=10), 
          axis.text.y=element_text(size=10), 
          axis.title.x = element_blank(), 
          axis.title.y =element_blank(), 
          axis.ticks.x=element_blank())
  
  dots1 = get_sing_dots_marginals(data2_to_plot_8.10, peaks1, x, bw,
                                  is_kde_already = TRUE)
  
  plot_with_peaks1 = add_peaks_marginals_loop(plot, dots1, peaks1, "red")
  
  return(plot_with_peaks1)
}


make_marginals_2datasets = function(
    data1, data2, 
    peaks1, peaks2,
    sung_intervals1,
    sung_intervals2,
    NBOOT, BW, interval_range,
    colors
){
  
  # last 3 iterations
  data1_8.10 = data1 %>%
    dplyr::filter(degree %in% 8:10)  %>%
    dplyr::select(degree, network_id, sung_intervals1) %>% 
    pivot_longer(sung_intervals1, names_to = "interval", values_to = "sung_intervals")
  
  data2_8.10 = data2 %>%
    dplyr::filter(degree %in% 8:10)  %>%
    dplyr::select(degree, network_id, sung_intervals2) %>% 
    pivot_longer(sung_intervals2, names_to = "interval", values_to = "sung_intervals")
  
  data1_to_plot_8.10 = kde_bootstrap_1d(data1_8.10, NBOOT, 
                                        "sung_intervals", 
                                        BW, 
                                        "chains", 
                                        interval_range
  )
  data2_to_plot_8.10 = kde_bootstrap_1d(data2_8.10, NBOOT, 
                                        "sung_intervals", 
                                        BW, 
                                        "chains", 
                                        interval_range
  )
  
  col1 = colors[[1]]
  col2 = colors[[2]]
  
  # peaks
  dots1 = get_sing_dots_marginals(data1_to_plot_8.10, peaks1, x, bw,
                                  is_kde_already = TRUE)
  dots2 = get_sing_dots_marginals(data2_to_plot_8.10, peaks2, x, bw,
                                  is_kde_already = TRUE)
  
  
  plot = data1_to_plot_8.10 %>% ggplot(aes(x)) +
    scale_x_continuous(breaks=seq(min(vertical.lines), max(vertical.lines), 1),
                       limits = c(min(vertical.lines), max(vertical.lines)))  +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    geom_ribbon(aes(ymin = avg - sdev, ymax = avg + sdev), alpha = .2, fill = col1) +
    geom_line(aes(y = avg), color="black", size =  0.5, linetype="dashed") +
    geom_ribbon(data=data2_to_plot_8.10, aes(ymin = avg - sdev, ymax = avg + sdev), alpha = .3, fill = col2) +
    geom_line(data=data2_to_plot_8.10, aes(y = avg), color=col2, size = 0.5) +
    # add peaks
    xlab("vname") + ylab("density") +
    theme_classic() + 
    # labs(title = tt,
    #      subtitle = subtt) +
    theme(axis.text.x = element_text(size=8), 
          axis.text.y=element_text(size=8), 
          axis.title.x = element_blank(), 
          axis.title.y =element_blank(), 
          axis.ticks.x=element_blank())
  
  
  plot_with_peaks1 = add_peaks_marginals_loop(plot, dots1, peaks1, col1)
  plot_with_peaks2 = add_peaks_marginals_loop(plot_with_peaks1, dots2, peaks2, col2)
  
  return(plot_with_peaks2)
}


make_marginals_3datasets = function(
    data1, data2, data3,
    peaks2, peaks3,
    # tt, subtt,
    sung_intervals,
    NBOOT, BW, interval_range,
    colors
){
  
  # last 3 iterations
  data1_8.10 = data1 %>%
    dplyr::filter(degree %in% 8:10)  %>%
    dplyr::select(degree, network_id, sung_intervals) %>% 
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  data2_8.10 = data2 %>%
    dplyr::filter(degree %in% 8:10)  %>%
    dplyr::select(degree, network_id, sung_intervals) %>% 
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  data3_8.10 = data3 %>%
    dplyr::filter(degree %in% 8:10)  %>%
    dplyr::select(degree, network_id, sung_intervals) %>% 
    pivot_longer(sung_intervals, names_to = "interval", values_to = "sung_intervals")
  
  data1_to_plot_8.10 = kde_bootstrap_1d(data1_8.10, NBOOT, 
                                        "sung_intervals", 
                                        BW, 
                                        "chains", 
                                        interval_range
  )
  data2_to_plot_8.10 = kde_bootstrap_1d(data2_8.10, NBOOT, 
                                        "sung_intervals", 
                                        BW, 
                                        "chains", 
                                        interval_range
  )
  data3_to_plot_8.10 = kde_bootstrap_1d(data3_8.10, NBOOT, 
                                        "sung_intervals", 
                                        BW, 
                                        "chains", 
                                        interval_range
  )
  
  col1 = colors[[1]]
  col2 = colors[[2]]
  col3 = colors[[3]]
  
  # peaks
  dots2 = get_sing_dots_marginals(data2_to_plot_8.10, peaks2, x, bw,
                                  is_kde_already = TRUE)
  dots3 = get_sing_dots_marginals(data3_to_plot_8.10, peaks3, x, bw,
                                  is_kde_already = TRUE)
  
  
  plot = data1_to_plot_8.10 %>% ggplot(aes(x)) +
    scale_x_continuous(breaks=seq(min(vertical.lines), max(vertical.lines), 1),
                       limits = c(min(vertical.lines), max(vertical.lines)))  +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    # geom_ribbon(aes(ymin = avg - sdev, ymax = avg + sdev), alpha = .3, fill = col1) +
    geom_line(aes(y = avg), color="black", size =  0.5, alpha = 0.8, linetype = "dashed") +
    geom_ribbon(data=data2_to_plot_8.10, aes(ymin = avg - sdev, ymax = avg + sdev), 
                alpha = .3, fill = col2) +
    geom_line(data=data2_to_plot_8.10, aes(y = avg), 
              color = col2, size = 0.5, alpha = 0.8) +
    geom_ribbon(data=data3_to_plot_8.10, aes(ymin = avg - sdev, ymax = avg + sdev), 
                alpha = .3, fill = col3) +
    geom_line(data=data3_to_plot_8.10, aes(y = avg),
              color = col3, , size = 0.5, alpha = 0.8) +
    xlab("vname") + ylab("density") +
    theme_classic() + 
    # labs(title = tt, subtitle = subtt) +
    theme(axis.text.x = element_text(size=10), 
          axis.text.y=element_text(size=10), 
          axis.title.x = element_blank(), 
          axis.title.y =element_blank(), 
          axis.ticks.x=element_blank())
  
  plot_with_peaks1 = add_peaks_marginals_loop_colored.dots(plot, dots2, peaks2, col2)
  plot_with_peaks2 = add_peaks_marginals_loop_colored.dots(plot_with_peaks1, dots3, peaks3, col3)
  
  
  return(plot_with_peaks2)
}


################################################################################
# Melodic pleasantness
################################################################################
smooth_melcon_ratings = function(data, n_bootstrap=1000){
  dyads_by_participant <- data %>% 
    group_by(participant_id) %>% group_split()
  
  dyads_smoothed_bootstrapped <- map_dfr(1:n_bootstrap, function(i) {
    n_participants <- length(dyads_by_participant)
    ind <- sample(n_participants, n_participants, replace = TRUE)
    data_bootstrapped <- bind_rows(dyads_by_participant[ind])
    tibble(
      iteration = i,
      interval =seq(min(interval_range),max(interval_range),by=0.1),
      rating = smooth_1d_gaussian(data_bootstrapped$target_interval,data_bootstrapped$answer,interval,BW)
    )
  })
  
  dyads_smoothed_final <- 
    dyads_smoothed_bootstrapped %>% 
    group_by(interval) %>% 
    dplyr::summarise(
      mean_rating = mean(rating, na.rm = TRUE),
      se_rating = sd(rating, na.rm = TRUE)
    )
  return(dyads_smoothed_final)
}


plot_melcon_ratings = function(data, color_to_fill0, interval_range){
  p = data %>% 
    ggplot(aes(interval, mean_rating, ymin = mean_rating - se_rating, ymax = mean_rating + se_rating)) +
    geom_ribbon(fill = color_to_fill0, alpha = 0.4) +
    geom_line(color="black") +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    scale_x_continuous(breaks=seq(min(interval_range), max(interval_range), 1)) +
    theme_classic() +
    theme(axis.text.x = element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          # axis.ticks.x=element_blank(),
          # legend.position = c(0.8, 0.875),
          legend.text = element_blank(),
          legend.title = element_blank())
  return(p)
}


plot_melcon_ratings_onlyline = function(data, interval_range){
  p = data %>% 
    ggplot(aes(interval, mean_rating, ymin = mean_rating - se_rating, ymax = mean_rating + se_rating)) +
    geom_line(color="black") +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    scale_x_continuous(breaks=seq(min(interval_range), max(interval_range), 1)) 
  return(p)
}


################################################################################
# Features 1 experiment
################################################################################
plot_features = function(data){
  
  error = summarise_feature(data, RMSE_interval)
  int.size = summarise_feature(data, abs_int_size)
  melcon = summarise_feature(data, melcon)
  peaks = summarise_feature(data, n_peaks)
  
  # entropy_round =  summarise_feature(data, entropy_round)
  # entropy_0.5 =  summarise_feature(data, entropy_0.5)
  entropy_0.25 =  summarise_feature(data, entropy_0.25)
  # entropy_0.1  =  summarise_feature(data,  entropy_0.1 )
  # entropy_continous  =  summarise_feature(data,  entropy_continous )
  
  plot_error = make_lineplot_ribon_features(
    error, degree, mean, sd, "RMSE interval") 
  plot_int.size = make_lineplot_ribon_features(
    int.size, degree, mean, sd, "Abs mean interval") 
  plot_melcon = make_lineplot_ribon_features(
    melcon, degree, mean, sd, "Abs mean interval") 
  
  plot_peaks = make_lineplot_ribon_features(
    peaks, degree, mean, sd, "N peaks") 
  
  # plot_entropy_round = make_lineplot_ribon_features(
  #   entropy_round, degree, mean, sd, "Entropy (round)") 
  # plot_entropy_0.5 = make_lineplot_ribon_features(
  #   entropy_0.5, degree, mean, sd, "Entropy (0.5)") 
  plot_entropy_0.25 = make_lineplot_ribon_features(
    entropy_0.25, degree, mean, sd, "Entropy (0.25)") 
  # plot_entropy_0.1 = make_lineplot_ribon_features(
  #   entropy_0.1, degree, mean, sd, "Entropy (0.1)") 
  # plot_entropy_continous = make_lineplot_ribon_features(
  #   entropy_continous, degree, mean, sd, "Entropy (FNN)") 
  
  list_out = list(
    plot_error=plot_error, 
    plot_int.size=plot_int.size, 
    plot_melcon=plot_melcon,
    plot_peaks=plot_peaks,
    # plot_entropy_round=plot_entropy_round,
    # plot_entropy_0.5=plot_entropy_0.5,
    # plot_entropy_0.1=plot_entropy_0.1,
    # plot_entropy_continous=plot_entropy_continous,
    plot_entropy_0.25=plot_entropy_0.25
  )
  
  return(list_out)
}


make_lineplot_ribon_features = function(data, var1, var2, se_var2, y_label){
  evar1 = enquo(var1)
  evar2 = enquo(var2)
  ese_var2 = enquo(se_var2)
  
  dMean <- data %>%
    filter(!! evar1 == 0) %>%
    dplyr::summarise(m = mean(!! evar2))
  
  p =   data %>%
    ggplot(aes(x= !! evar1, y = !! evar2, group = 1)) + 
    geom_ribbon(aes(ymin=!! evar2 - !! ese_var2, 
                    ymax=!! evar2 + !! ese_var2),  alpha = 0.25)  +
    geom_line(color="black") +
    ylab(y_label) +
    geom_point(size = 2, shape=21) +
    geom_point(fill = "white", size = 2, shape=21) +
    geom_hline(data = dMean, aes(yintercept = m), linetype="dashed", color="black") +
    scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
    # scale_color_manual(values=c("#FDE725FF", "#440254FF")) +
    theme_classic()
  
  p0 = p +   theme(axis.text.x = element_text(size=10),
                   axis.text.y=element_text(size=10),
                   legend.position = "none",
                   legend.title = element_blank())
  return(p0)
}


summarise_feature = function(data, feature){
  efeature = enquo(feature)
  
  data_sum = data %>%
    group_by(degree) %>%
    dplyr::summarise(
      n=n(),
      mean=mean(!! efeature),
      sd=sd(!! efeature)
    )
  return(data_sum)
}


summarise_feature_app = function(data, feature){
  efeature = enquo(feature)
  
  data_sum = data %>%
    group_by(degree, app) %>%
    dplyr::summarise(
      n=n(),
      mean=mean(!! efeature),
      sd=sd(!! efeature)
    )
  return(data_sum)
}


################################################################################
# Features 2 experiments
################################################################################
sum_boot.data_by.group = function(data, vars_to_sum){
  
  data_sum = data %>%
    group_by(degree, app) %>%
    dplyr::summarise_at(vars(vars_to_sum), list(mean = mean, sd = sd), na.rm = TRUE)
  
  return(data_sum)
}


sum.norm_boot.data_by.group = function(data){
  
  features_compare_sum = sum_boot.data_by.group(data, vars_to_sum)
  
  
  data_sum_norm = features_compare_sum %>% 
    group_by(app) %>% 
    mutate(RMSE_interval_mean = RMSE_interval_mean-RMSE_interval_mean[degree==1][1L]) %>% 
    mutate(abs_int_size_mean = abs_int_size_mean-abs_int_size_mean[degree==0][1L]) %>% 
    mutate(melcon_mean = melcon_mean-melcon_mean[degree==0][1L]) %>% 
    mutate(entropy_0.25_mean = entropy_0.25_mean-entropy_0.25_mean[degree==0][1L]) 
  
  
  return(data_sum_norm)
}


plot_features_2datasets = function(data, normalize, colors, is_viridis, legend_off){
  
  if (normalize == TRUE) {
    features_compare_sum = sum.norm_boot.data_by.group(data)
    
  } else {
    features_compare_sum = sum_boot.data_by.group(data, vars_to_sum)
  }
  
  plot_error = make_lineplot_ribon_bygroup(
    features_compare_sum, RMSE_interval_mean, RMSE_interval_sd, app,  "RMSE interval",
    colors, is_viridis, legend_off) 
  plot_int.size = make_lineplot_ribon_bygroup(
    features_compare_sum, abs_int_size_mean, abs_int_size_sd, app,  "Abs mean interval",
    colors, is_viridis, legend_off) 
  plot_entropy_0.25 = make_lineplot_ribon_bygroup(
    features_compare_sum, entropy_0.25_mean, entropy_0.25_sd, app,  "Entropy (0.25)",
    colors, is_viridis, legend_off) 
  plot_melcon = make_lineplot_ribon_bygroup(
    features_compare_sum, melcon_mean, melcon_sd, app,  "Melodic Consonance",
    colors, is_viridis, legend_off) 
  
  list_out = list(
    plot_error=plot_error, 
    plot_int.size=plot_int.size, 
    plot_entropy_0.25=plot_entropy_0.25,
    plot_melcon=plot_melcon
  )
  
  return(list_out)
}


plot_2tones_marginals = function(data, data_melcon){
  
  colfunc <- colorRampPalette(c("red", "white"))
  cols = colfunc(10)
  
  data_slider_seed = data %>% 
    filter(degree == 0) 
  
  data_slider_first3 = data %>% 
    filter(degree %in% 1:3) 
  
  data_slider_last3 = data %>% 
    filter(degree %in% 8:10) 
  
  data_slider_seed_smooted = kde_bootstrap_1d(data_slider_seed, 1000, "interval",
                                              (BW*3), "chains", interval_range)
  
  data_slider_first3_smooted = kde_bootstrap_1d(data_slider_first3, 1000, "interval",
                                                BW, "chains", interval_range)
  
  data_slider_last3_smooted = kde_bootstrap_1d(data_slider_last3, 1000, "interval",
                                               BW, "chains", interval_range)
  
  
  marginals_slider <- data_slider_last3_smooted %>% ggplot(aes(x)) +
    geom_line(data=data_slider_seed_smooted, aes(y = avg), color="darkred", 
              size =0.5, linetype="dashed") +
    geom_line(data=data_slider_first3_smooted, aes(y = avg), color=cols[[7]], 
              size =0.5) +
    geom_line(aes(y = avg), color="black", size =  0.75) +
    geom_ribbon(aes(ymin = avg - sdev, ymax = avg + sdev), alpha = .3, fill = "darkgrey") +
    scale_x_continuous(breaks=seq(min(vertical.lines), max(vertical.lines), 1),
                       limits = c(min(interval_range), max(interval_range)))  +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    xlab("end with unison") + ylab("density") +
    theme_classic() + 
    theme(axis.text.x = element_text(size=10), 
          axis.text.y=element_text(size=10), 
          axis.title.x = element_blank(), 
          axis.title.y =element_blank(), 
          axis.ticks.x=element_blank(),
          legend.position = "none",
          legend.text = element_blank(), 
          legend.title = element_blank())
  return(marginals_slider)
}



plot_features_2datasets_slider = function(data, vars_to_sum, normalize, colors, is_viridis, legend_off){
  
  if (normalize == TRUE) {
    features_compare_sum_pre = sum_boot.data_by.group(data, vars_to_sum)
    
    
    features_compare_sum = features_compare_sum_pre %>% 
      group_by(app) %>% 
      mutate(mean_error_interval_mean = mean_error_interval_mean-mean_error_interval_mean[degree==1]) %>% 
      mutate(mean_abs_int_size_mean = mean_abs_int_size_mean-mean_abs_int_size_mean[degree==0]) %>% 
      mutate(n_peaks_mean = n_peaks_mean-n_peaks_mean[degree==0]) %>%
      mutate(interval_entropy_0.25_mean = interval_entropy_0.25_mean-interval_entropy_0.25_mean[degree==0]) 
    
  } else {
    features_compare_sum = sum_boot.data_by.group(data, vars_to_sum)
  }
  
  plot_error = make_lineplot_ribon_bygroup(
    features_compare_sum, mean_error_interval_mean, mean_error_interval_sd, app,  "Interval error",
    colors, is_viridis, legend_off) 
  plot_int.size = make_lineplot_ribon_bygroup(
    features_compare_sum, mean_abs_int_size_mean, mean_abs_int_size_sd, app,  "Abs mean interval",
    colors, is_viridis, legend_off) 
  plot_peaks = make_lineplot_ribon_bygroup(
    features_compare_sum, n_peaks_mean, n_peaks_sd, app,  "N peaks",
    colors, is_viridis, legend_off)
  plot_entropy_0.25 = make_lineplot_ribon_bygroup(
    features_compare_sum, interval_entropy_0.25_mean, interval_entropy_0.25_sd, app,  "Entropy (0.25)",
    colors, is_viridis, legend_off) 
  
  list_out = list(
    plot_error=plot_error, 
    plot_int.size=plot_int.size, 
    plot_peaks=plot_peaks,
    plot_entropy_0.25=plot_entropy_0.25
  )
  
  return(list_out)
}


make_lineplot_ribon_bygroup = function(
    data, var1, se_var1, group_var,  x_lab,
    colors, is_viridis=TRUE, legend_off=TRUE){
  
  evar1 = enquo(var1)
  ese_var1 = enquo(se_var1)
  egroup_var = enquo(group_var)
  
  dMean <- data %>%
    filter(degree == 0) %>%
    group_by(!! egroup_var) %>%
    dplyr::summarise(m = mean(!! evar1, na.rm=T))
  
  p =   data %>%
    ggplot(aes(x= degree, y = !! evar1, group = !! egroup_var, fill = !! egroup_var)) + 
    geom_line()+
    geom_ribbon(aes(ymin=!! evar1 - !! ese_var1, 
                    ymax=!! evar1 + !! ese_var1),  alpha = 0.4)  +
    geom_point(aes(fill = !! egroup_var), size = 2, shape=21) +
    # scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    ylab(x_lab) +
    xlab("Iteration") +
    theme_classic()
  
  if (is_viridis){
    p0 = p + scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
      geom_hline(data = dMean, aes(yintercept = m), linetype="dashed") 
  } else {
    p0 = p +  scale_fill_manual(values=colors) +
      geom_hline(data = dMean, aes(yintercept = m), linetype="dashed", color = "black") 
  }
  
  if (legend_off){
    p1 = p0 +   theme(axis.text.x = element_text(size=10),
                      axis.text.y=element_text(size=10),
                      legend.position = "none",
                      legend.title = element_blank())
  } else {
    p1 = p0 +   theme(axis.text.x = element_text(size=10),
                      axis.text.y=element_text(size=10),
                      legend.position = "right",
                      legend.title = element_blank())
  }
  
  
  
  return(p1)
}


make_lineplot_ribon_bygroup_simple = function(
    data, var1, se_var1, group_var,  x_lab,
    colors, is_viridis=TRUE, legend_off=TRUE){
  
  evar1 = enquo(var1)
  ese_var1 = enquo(se_var1)
  egroup_var = enquo(group_var)
  
  p =   data %>%
    ggplot(aes(x= degree, y = !! evar1, group = !! egroup_var, fill = !! egroup_var)) + 
    geom_line()+
    geom_ribbon(aes(ymin=!! evar1 - !! ese_var1, 
                    ymax=!! evar1 + !! ese_var1),  alpha = 0.4)  +
    geom_point(aes(fill = !! egroup_var), size = 2, shape=21) +
    # scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    ylab(x_lab) +
    xlab("Iteration") +
    theme_classic()
  
  if (is_viridis){
    p0 = p + scale_fill_viridis(discrete = TRUE, option = "D", direction = -1)
  } else {
    p0 = p +  scale_fill_manual(values=colors) 
  }
  
  if (legend_off){
    p1 = p0 +   theme(axis.text.x = element_text(size=8),
                      axis.text.y=element_text(size=8),
                      legend.position = "none",
                      legend.title = element_blank())
  } else {
    p1 = p0 +   theme(axis.text.x = element_text(size=8),
                      axis.text.y=element_text(size=8),
                      legend.position = "right",
                      legend.title = element_blank())
  }
  
  
  
  return(p1)
}


################################################################################
# Contours
################################################################################
se_custom <- function(x) sqrt(var(x) / length(x))

plot_av.contour = function(data){
  # data$degree = as.factor(data$degree)
  
  plot = ggplot(data, aes(x=pitch, y=mean, 
                          group = degree,
                          fill = degree)) + 
    geom_line(aes(color=degree)) +
    geom_ribbon(aes(ymin=mean-se, 
                    ymax=mean+se),  alpha = 0.2) +
    scale_fill_viridis(option = "A") +
    scale_color_viridis(option = "A") +
    theme(axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.text = element_text(size=12),
          legend.title = element_blank())
  return(plot)
}


run_contour_analysis = function(data, sung_pitch_vars, n_degree){
  x = prepare_data_av.contours(data, sung_pitch_vars)
  p1 = plot_av.contour(x)
  
  if (n_degree == 10) {
    p2a = plot_av.contour_chains(data, sung_pitch_vars, 1, 3) +
      ggtitle("Generation 1 to 3")
    p2b = plot_av.contour_chains(data, sung_pitch_vars, 4, 6) +
      ggtitle("Generation 4 to 6")
    p2c = plot_av.contour_chains(data, sung_pitch_vars, 7, 10) +
      ggtitle("Generation 7 to 10")
    
    p2.evolve = plot_grid(p2a,p2b,p2c, ncol = 3)
  } else {
    p2a = plot_av.contour_chains(data, sung_pitch_vars, 0, 0) +
      ggtitle("Generation 0")
    p2b = plot_av.contour_chains(data, sung_pitch_vars, 1, 2) +
      ggtitle("Generation 1 to 2")
    p2c = plot_av.contour_chains(data, sung_pitch_vars, 4, 5) +
      ggtitle("Generation 4 to 5")
    
    p2.evolve = plot_grid(p2a, p2b, p2c, ncol = 3)
  }
  
  return(list(
    data=x,
    plot_av.contour1 = p1,
    plot_av.contour2 = p2c,
    plot_av.contour3 = p2.evolve
  ))
}


plot_av.contour_chains = function(data, sung_interval_vars, l.degree, h.degree){
  data_contours = data %>%
    filter(degree %in% l.degree:h.degree) %>% 
    select(degree, network_id, sung_interval_vars) %>% 
    pivot_longer(sung_interval_vars, names_to = "index", values_to = "pitch") %>%
    group_by(index, network_id) %>%
    dplyr::summarise(
      n=n(),
      av = mean(pitch, na.rm = T),
      sd = sd(pitch, na.rm = T),
      se = sd/ sqrt(n)
    ) 
  data_contours_mean  = data %>%
    filter(degree %in% l.degree:h.degree) %>% 
    select(degree, network_id, sung_interval_vars) %>% 
    pivot_longer(sung_interval_vars, names_to = "index", values_to = "pitch") %>%
    group_by(index) %>%
    dplyr::summarise(
      n=n(),
      av = mean(pitch, na.rm = T),
      sd = sd(pitch, na.rm = T),
      se = sd/ sqrt(n)
    ) %>%
    mutate(network_id = 999)
  
  p = data_contours %>% 
    ggplot(aes(x=as.factor(index), y=av, group = network_id,
               fill = network_id)) + 
    geom_line(aes(color=network_id), size = 0.15) +
    # geom_line(data = data_contours_mean, size=1, color = "red") +
    geom_ribbon(aes(ymin=av-se,
                    ymax=av+se),  alpha = 0.05) +
    geom_ribbon(data = data_contours_mean, 
                aes(ymin=av-se, ymax=av+se),  
                alpha = 0.5, color = "red", fill = "red") +
    scale_fill_viridis(option = "D") +
    # scale_color_viridis(option = "D") +
    theme(axis.text.x = element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          legend.text = element_text(size=12)
    )
  return(p)
}


prepare_data_av.contours = function(data, sung_pitch_vars){
  data_pitches_mean = data %>%
    select(degree, sung_pitch_vars) %>% 
    group_by(degree) %>% 
    summarise_at(vars(sung_pitch_vars), mean, na.rm=T) %>% 
    pivot_longer(sung_pitch_vars, "pitch", values_to = "mean")
  
  data_pitches_se = data %>%
    select(degree, sung_pitch_vars) %>% 
    group_by(degree) %>% 
    summarise_at(vars(sung_pitch_vars), se_custom) %>% 
    pivot_longer(sung_pitch_vars, "pitch", values_to = "se") %>% 
    select(-degree, -pitch)
  
  data_pitches = bind_cols(data_pitches_mean, data_pitches_se) %>% 
    separate(pitch, c("noise", "pitch"), sep="_") %>%  select(-noise) 
  
  return(data_pitches)
}


plot_av_contours_general = function(data, num_notes, col){
  data_contours = data_contours %>%
    filter(num_sung_pitches == num_notes) %>%
    arrange(degree) %>%
    dplyr::select(id, degree, network_id, sung_pitches, net_num_notes)  %>%
    unnest(sung_pitches) %>%
    group_by(id) %>%
    dplyr::mutate(index = row_number()) %>%
    filter(index < (num_notes + 1)) %>%
    group_by(network_id, index) %>% 
    dplyr::summarise(sung_pitches = mean(sung_pitches)) %>% 
    group_by(index) %>%
    dplyr::summarise(
      n=n(),
      av = mean(sung_pitches, na.rm = T),
      sd = sd(sung_pitches, na.rm = T),
      se = sd/ sqrt(n)
    ) 
  
  plot = plot_contours_avereaged(data_contours, col)
  return(plot)
}


plot_contours_avereaged = function(data, col){
  plot = ggplot(data, aes(x=as.factor(index), y=av, group=1)) + 
    geom_line() +
    geom_ribbon(aes(ymin=av-se, 
                    ymax=av+se),  alpha = 0.25, fill=col) +
    geom_point(fill = col, size = 2, shape=21) +
    ylim(57,63)
  
  plot0 = 
    plot + theme(axis.text.x = element_text(size=10),
                 axis.text.y=element_text(size=10),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 legend.position = "right",
                 legend.text = element_text(size=12),
                 legend.title = element_blank())
  return(plot0)
}


plot_clustered_contours = function(data, k, vars_pitches){
  
  Ns = table(data$cluster)
  total = sum(Ns)
  Ns_percent = round((Ns / total) * 100, 2)
  
  x<-0
  z<-NULL
  while (x < k)  {  
    x <- x+1  
    z <- c(paste(z, paste0('C', x, " = ", Ns_percent[[x]], "%", collapse = "," )))
  }
  
  data_countours = data %>%
    pivot_longer(cols = vars_pitches, names_to = "index", values_to = "pitch") %>%
    group_by(network_id, cluster, index) %>% 
    dplyr::summarise(
      n=n(),
      av.pitch = mean(pitch, na.rm = T),
    ) %>% 
    group_by(cluster, index) %>%
    dplyr::summarise(
      n=n(),
      av = mean(av.pitch, na.rm = T),
      sd = sd(av.pitch, na.rm = T),
      se = sd/ sqrt(n)
    ) 
  
  data_countours$cluster = as.factor(data_countours$cluster)
  
  plot = ggplot(data_countours, aes(x=as.factor(index), y=av, 
                                    group=cluster, 
                                    fill=cluster)) + 
    geom_ribbon(aes(ymin=av-se, 
                    ymax=av+se),  alpha = 0.5) + 
    geom_line() +
    # scale_fill_manual(values=cols) +
    facet_wrap(~cluster, ncol = 2, scales = "free") +
    ggtitle(z) +
    theme(axis.text.x = element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.text = element_text(size=12),
          # legend.title = element_blank(),
          plot.title = element_text(size=6))
  
  return(plot)
}


make_contour_histogram_over_time = function(data_all_nets){
  data_nets_contour_code = data_all_nets %>%
    select(id, trial_maker_id, network_id, degree, list_4int_sung.intervals) %>%
    pivot_longer(cols = list_4int_sung.intervals, names_to = "interval_pos", values_to = "interval") %>%
    # # simple
    mutate(contour_code = ifelse(interval > 0.5, "A",
                                 ifelse(interval < -0.5, "D",
                                        ifelse(interval < 0.5 & interval > -0.5, "S", "NA")))) %>%
    # more
    # mutate(contour_code = ifelse(interval > 0.5 & abs(interval) > 3, "AA",
    #                              ifelse(interval > 0.5 & abs(interval) <= 3, "A",
    #                                     ifelse(interval < -0.5 & abs(interval) > 3, "DD",
    #                                            ifelse(interval < -0.5 & abs(interval) <= 3, "D",
    #                                                   ifelse(interval < 0.5 & interval > -0.5, "S", "NA")))))) %>%
    pivot_wider(id_cols = c("trial_maker_id","network_id","id", "degree"), names_from = interval_pos, values_from = contour_code)  %>%
    mutate(contour_coded = paste0(sung_interval1,"-",sung_interval2,"-",sung_interval3,"-",sung_interval4)) 
  
  plot_countours_histogram_over_time = data_nets_contour_code %>%
    # filter(degree>=20) %>%
    mutate(generations = case_when(
      degree %in% 0:3 ~ "0:3",
      degree %in% 4:7 ~ "4:7",
      degree %in% 8:10 ~ "8:10",
    )) %>% 
    group_by(trial_maker_id, contour_coded, generations) %>%
    dplyr::summarise(n = n()) %>%
    ggplot(aes(x = reorder(contour_coded, -n), y=n, fill=contour_coded)) +
    geom_bar(stat="identity")+theme_classic() +
    facet_wrap(~ trial_maker_id + generations, scales = "free_y") +  
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 5,angle = 90, vjust = 0.5, hjust=1)
    )
  
  data_nets_contour_megaplot = data_nets_contour_code %>%
    group_by(trial_maker_id, contour_coded, degree) %>%
    dplyr::summarise(n = n()) %>%
    filter(n > 0)
  
  
  plot_heatmap_contours_over_time = ggplot(
    data_nets_contour_megaplot, 
    aes(x = as.factor(degree), y =  reorder(contour_coded, n))) + 
    geom_raster(aes(fill=n)) + 
    scale_fill_gradient(low="grey90", high="red") +
    labs(x="iteration", y="contours", 
         title="Evolution all possible countours") +
    facet_wrap(~ trial_maker_id) +  
    theme_bw() + 
    theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
          axis.text.y=element_text(size=8),
          plot.title=element_text(size=11)
    )
  
  
  return(list(plot_countours_histogram_over_time, plot_heatmap_contours_over_time))
}


prepare_data_for_clustering = function(data){
  data_clusters = data %>% 
    mutate(
      mean_pitch = ((sung_pitch1 + sung_pitch2 + sung_pitch3 + sung_pitch4 + sung_pitch5) / 5),
      across(starts_with("sung_pitch"), ~. - mean_pitch) 
    ) %>%
    select(everything(), sung_pitch1, sung_pitch2, sung_pitch3, sung_pitch4, sung_pitch5)
  return(data_clusters)
}

find_clusters = function(data, k){
  # kmeans clustering
  cluster_kmeans <- kmeans(dt[, 11:15], 6, nstart = 50)
  
  # Hierarchical Clustering
  require("stats")
  cluster_hclust_dist <- hclust(dist(dt[, 11:15]))
  cluster_hclust<- cutree(cluster_hclust_dist, 6)
  
  return(list(cluster_kmeans$cluster, cluster_hclust))
}


# run cluster analysis
run_cluster_analysis = function(data, k){
  data_clusters_pitches_filter = prepare_data_for_clustering(data)
  
  pca_res_pitches = prcomp(data_clusters_pitches_filter[, list_4int_pitches])
  print(summary(pca_res_pitches))
  
  cluster <- kmeans(data_clusters_pitches_filter[, list_4int_pitches], k, nstart = 50)
  
  data_clusters_pitches_filter$cluster <- as.factor(cluster$cluster)
  
  plot1 = autoplot(pca_res_pitches,  data = data_clusters_pitches_filter, colour = 'cluster', size = 1) +
    theme(axis.text.x = element_text(size=6),
          axis.text.y=element_text(size=8),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.text = element_text(size=10),
          legend.title = element_blank()) +
    facet_wrap(~app + degree, nrow = 3)  +
    ggtitle("PCA over generations")
  
  data_clusters_prototypes = data_clusters_pitches_filter %>% 
    rename(N1 = sung_pitch1,
           N2 = sung_pitch2,
           N3 = sung_pitch3,
           N4 = sung_pitch4,
           N5 = sung_pitch5) 
  
  data_clusters_prototypes_seed = data_clusters_prototypes %>% 
    filter(degree == 0)
  
  data_clusters_prototypes_first3 = data_clusters_prototypes %>% 
    filter(degree %in% 1:3)
  
  data_clusters_prototypes_last3 = data_clusters_prototypes %>% 
    filter(degree %in% 8:10)
  
  plot2a = plot_clustered_contours(data_clusters_prototypes_seed, 
                                   k, c("N1", "N2", "N3", "N4", "N5")) 
  plot2b = plot_clustered_contours(data_clusters_prototypes_first3, 
                                   k, c("N1", "N2", "N3", "N4", "N5")) 
  plot2c = plot_clustered_contours(data_clusters_prototypes_last3, 
                                   k, c("N1", "N2", "N3", "N4", "N5")) 
  
  plot3 = data_clusters_pitches_filter %>% 
    group_by(degree, cluster, app) %>% 
    dplyr::summarise(n = n()) %>% 
    ggplot(aes(x=degree, y=n, group=app)) +
    geom_line(aes(color=app))+
    geom_point(aes(color=app)) +
    ylab("Percentage") +
    facet_wrap(~cluster, ncol = 2) +
    theme(legend.position = "right") 
  
  return(list(DT = data_clusters_pitches_filter, 
              plot1_pca = plot1,
              plot2_clusters_seed = plot2a, 
              plot2_clusters_first3 = plot2b, 
              plot2_clusters_last3 = plot2c, 
              plot3_clusters_evolution = plot3))
}


plot_cluster_proportion = function(data){
  plot = data %>%  group_by(degree, cluster, trial_maker_id) %>% 
    dplyr::summarise(n = n()) %>% 
    ggplot(aes(x=degree, y=n, group=trial_maker_id)) +
    geom_line(aes(color=trial_maker_id))+
    geom_point(aes(color=trial_maker_id)) +
    ylab("Percentage") +
    facet_wrap(~cluster, ncol = 2) 
  return(plot)
}


run_cluster_analysis_compare_batches = function(data, k){
  data_clusters_pitches_filter = prepare_data_for_clustering(data)
  
  pca_res_pitches = prcomp(data_clusters_pitches_filter[, list_4int_pitches])
  print(summary(pca_res_pitches))
  
  cluster <- kmeans(data_clusters_pitches_filter[, list_4int_pitches], k, nstart = 50)
  
  data_clusters_pitches_filter$cluster <- as.factor(cluster$cluster)
  
  plot1 = autoplot(pca_res_pitches,  data = data_clusters_pitches_filter, colour = 'cluster', size = 1) +
    theme(axis.text.x = element_text(size=6),
          axis.text.y=element_text(size=8),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.text = element_text(size=10),
          legend.title = element_blank()) +
    facet_wrap(~app + degree, nrow = 3)  +
    ggtitle("PCA over generations")
  
  data_clusters_prototypes = data_clusters_pitches_filter %>% 
    rename(N1 = sung_pitch1,
           N2 = sung_pitch2,
           N3 = sung_pitch3,
           N4 = sung_pitch4,
           N5 = sung_pitch5) 
  
  data_clusters_prototypes_last3 = data_clusters_prototypes %>% 
    filter(degree %in% 8:10)
  
  plot2_clusters_last3 = plot_clustered_contours(data_clusters_prototypes_last3, 
                                                 k, c("N1", "N2", "N3", "N4", "N5")) 
  
  plot3a = data_clusters_pitches_filter %>% 
    filter(batch == "b1") %>% 
    plot_cluster_proportion() + 
    theme(legend.position = "bottom") +
    ggtitle("Batch 1")
  
  plot3b = data_clusters_pitches_filter %>% 
    filter(batch == "b2") %>% 
    plot_cluster_proportion() + 
    theme(legend.position = "bottom") +
    ggtitle("Batch 2")
  
  plot3c = data_clusters_pitches_filter %>% 
    filter(batch == "b3") %>% 
    plot_cluster_proportion() + 
    theme(legend.position = "bottom") +
    ggtitle("Batch 3")
  
  plot3_clusters_evolution = plot_grid(plot3a, plot3b, plot3c, ncol = 3)
  
  return(list(DT = data_clusters_pitches_filter, 
              plot1_pca = plot1,
              plot2_clusters_last3 = plot2_clusters_last3, 
              plot3_clusters_evolution = plot3_clusters_evolution))
}


run_contour_analysis_compare_batches = function(data){
  data_nets_contour_code = data %>%
    select(id, app, trial_maker_id, network_id, degree, list_4int_sung.intervals) %>%
    pivot_longer(cols = list_4int_sung.intervals, names_to = "interval_pos", values_to = "interval") %>%
    mutate(cluster = ifelse(interval > 0, "A","D")) %>%
    pivot_wider(id_cols = c("trial_maker_id","network_id","id", "degree", "app"), names_from = interval_pos, values_from = cluster)  %>%
    mutate(cluster = paste0(sung_interval1,"-",sung_interval2,"-",sung_interval3,"-",sung_interval4)) %>% 
    select(-sung_interval1:-sung_interval4)
  
  plot = data_nets_contour_code %>% 
    group_by(app, degree, trial_maker_id, cluster) %>% 
    summarise(n = n()) %>% 
    ggplot(aes(x=as.factor(degree), y=n, 
               group=cluster, 
               color=cluster, 
               fill=cluster)) + 
    geom_line() +
    facet_wrap(~cluster, ncol = 2, scales = "free") +
    # ggtitle(z) +
    facet_wrap(~app + trial_maker_id, nrow = 3) +
    theme(axis.text.x = element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.text = element_text(size=12),
          # legend.title = element_blank(),
          plot.title = element_text(size=6))
  
  return(list(plot, data_nets_contour_code))
}


################################################################################
# Free N notes
################################################################################
plot_free_n.notes_overall.trend = function(data, col){
  btw_free_all_sum = data %>%
    group_by(degree) %>%
    dplyr::summarise(
      n=n(),
      mean_num_pitches = mean(num_sung_pitches, na.rm = T),
      median_num_pitches = median(num_sung_pitches, na.rm = T),
      sd_num_pitches = sd(num_sung_pitches, na.rm = T),
      se = sd_num_pitches/sqrt(n)
    ) 
  
  plot_mean_notes = btw_free_all_sum %>% 
    mutate(degree=as.factor(degree)) %>% 
    ggplot(aes(degree, mean_num_pitches, group=1)) +
    geom_line() +
    geom_ribbon(aes(ymin=mean_num_pitches-se, 
                    ymax=mean_num_pitches+se),   alpha = 0.25, fill = col) +
    geom_point(fill = col, size = 2, shape=21) +
    scale_y_continuous(breaks=seq(6,9,1),limits=c(6,8.5)) +
    xlab("Iteration") +
    ylab("Mean number of notes") +
    geom_hline(aes(yintercept = 8), linetype="dashed")  +
    theme(axis.text.x = element_text(size=10),
          axis.text.y=element_text(size=10),
          legend.position = "none",
          legend.title = element_blank())
  
  return(plot_mean_notes)
}


plot_free_n.notes_last.histogram = function(data){
  btw_free_all_sum = data %>% 
    group_by(degree, num_sung_pitches) %>% 
    dplyr::summarise(
      n =n()
    ) %>% 
    filter(degree != 0 )
  
  seed = tibble(
    degree= rep(0, 9),
    num_sung_pitches = seq(4,12,1),
    n = rep(24, 9)  # all melodies started with 24 chains
  )
  
  btw_free_all_sum_all = rbind(btw_free_all_sum, seed)
  
  btw_free_all_sum_sum = btw_free_all_sum_all %>% 
    mutate(
      gg = case_when(
        degree == 0 ~ "initial seed melodies",
        degree < 4 ~ "1-3 generations",
        degree < 7 ~ "4-7 generations",
        degree < 11 ~ "8-10 generations"
      )
    ) %>% 
    group_by(num_sung_pitches, gg) %>% 
    dplyr::summarise(mean_n = mean(n), sd_n = sd(n), se_n = sd_n/sqrt(n()))
  
  p = btw_free_all_sum_sum %>% 
    filter(gg == "8-10 generations") %>% 
    ggplot(aes(x=as.factor(num_sung_pitches), y=mean_n,
               fill=as.factor(num_sung_pitches)))+
    geom_bar(stat="identity", color="black") +
    geom_errorbar(aes(ymin=mean_n-se_n, ymax=mean_n+se_n), width=.2, 
                  position=position_dodge(.9)) +
    theme(legend.position = "none") +
    scale_fill_viridis_d(option = "D", direction = 1) +
    xlab("Number of notes") +
    ylab("Frequency")  +
    geom_hline(aes(yintercept = 24), linetype="dashed",color="black") +
    theme(axis.text.x = element_text(size=8),
          axis.text.y=element_text(size=8),
          legend.position = "none",
          legend.title = element_blank())
  
  return(p)
}


################################################################################
# functions to important and manipulate peaks data from MATLAB
################################################################################
# get peaks from MATLAB's bootstrap peakfinding method 
get_sig_peaks = function(data, sig_level){
  cols = colnames(data)
  
  peaks = data %>% 
    pivot_longer(cols) %>% 
    group_by(name) %>% 
    summarize(
      count = n(),
      non.na_count = sum(!is.na(value)),
      mean = mean(value, na.rm =T),
      sd = sd(value, na.rm =T) * 1.96,
      min_ci = mean - sd,
      max_ci = mean + sd,
      significane = (non.na_count/ count) * 100
    ) %>% 
    filter(significane >= sig_level)
  
  
  peaks_sum = tibble(
    values = peaks$mean,
    min_ci = peaks$min_ci,
    max_ci = peaks$max_ci,
    significance = peaks$significane
  )
  
  return(peaks_sum)
}


# find the y locations of the peaks given x
get_sing_dots_marginals <- function(dat, peaks, var, bw, is_kde_already = FALSE){ 
  evar = enquo(var)
  
  if (is_kde_already){
    marginals =  dat %>% 
      ggplot(aes(x = !! evar, y = avg)) +
      geom_line()
  } else {
    marginals =  dat %>% 
      ggplot(aes(x = !! evar)) +
      geom_density(size=0.25, n = 1000, bw = bw) 
  }
  
  smooths_raw <- ggplot_build(marginals)$data[[1]]
  smooths <- tibble(x = smooths_raw$x, y = smooths_raw$y)
  
  xs = c()
  ys = c()
  
  for(i in 1:nrow(peaks)){ 
    y = smooths$y[which.min(abs(smooths$x - peaks$values[[i]]))] 
    ys = c(y, ys)
    xs = c(peaks$values[[i]], xs)
  }
  
  points = tibble(
    x = xs,
    y = ys
  )
  
  return(points)
}


get_sing_dots_marginals_ratings <- function(plot_ratings, peaks){ 
  smooths_raw <- ggplot_build(plot_ratings)$data[[1]]
  smooths <- tibble(x = smooths_raw$x, y = smooths_raw$y)
  
  xs = c()
  ys = c()
  
  for(i in 1:nrow(peaks)){ 
    y = smooths$y[which.min(abs(smooths$x - peaks$values[[i]]))] 
    ys = c(y, ys)
    xs = c(peaks$values[[i]], xs)
  }
  
  points = tibble(
    x = xs,
    y = ys
  )
  
  return(points)
}


read_boot_peaks_data = function(path){
  boot_peaks_data = as_tibble(read.table(path, header = TRUE, sep = ",", dec = ".")) %>% 
    pivot_longer(iter1:iter11, names_to = "degree", values_to = "n_peaks") %>% 
    mutate(
      degree.peaks = parse_number(degree),
      degree.peaks = degree.peaks -1,
      degree.peaks = as.factor(degree.peaks)
    ) %>% 
    select(-degree)
  return(boot_peaks_data)
}



################################################################################
# siulations
################################################################################
prepare_data_sim = function(data){
  sum_mean =
    data %>% 
    summarise_all(mean) %>% 
    pivot_longer(`-15`:`15`, names_to = "x", values_to = "mean") %>% 
    mutate(x = as.numeric(x))
  
  sum_sd =
    data %>% 
    summarise_all(sd) %>% 
    pivot_longer(`-15`:`15`, names_to = "x", values_to = "sd") %>% 
    mutate(x = as.numeric(x))
  
  sum_100_M_intrval = sum_mean %>% 
    bind_cols(sum_sd[,2])
  return(sum_100_M_intrval) 
}


prepare_data_sim_generations = function(data){
  degree_seq = seq(0,10,1)
  
  store = c()
  for(i in degree_seq){ 
    print(i)
    
    df = data %>% 
      filter(degree == i ) %>% 
      summarise_all(mean) %>% 
      pivot_longer(`-15`:`15`, names_to = "x", values_to = "mean") %>% 
      mutate(x = as.numeric(x))
    
    store[[(i+1)]] = df
  }
  res = do.call(rbind, store)
  return(res)
}


plot_simulation_models = function(data1, data2, colors){
  plot = data1 %>% 
    ggplot(aes(x)) +
    geom_line(data=data2, aes(y = mean), color="black", 
              size =0.5, linetype="dashed") +
    geom_line(aes(y = mean), 
              # linetype="dashed",
              color=colors[[2]],
              size =  0.5) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
                alpha = .3,
                fill = colors[[2]]) +
    scale_x_continuous(breaks=seq(min(vertical.lines), max(vertical.lines), 1),
                       limits = c(min(vertical.lines), max(vertical.lines)))  +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    xlab("vname") + ylab("density") +
    theme_classic() + 
    # labs(title = tt,
    #      subtitle = subtt) +
    theme(axis.text.x = element_text(size=10), 
          axis.text.y=element_text(size=10), 
          axis.title.x = element_blank(), 
          axis.title.y =element_blank(), 
          axis.ticks.x=element_blank())
  return(plot)
}


plot_simulation_models_generations = function(data1, data2, colors){
  
  data1_selected = data1 %>%  
    filter(degree %in% c(0, 2, 4, 6, 8, 10))
  
  data2_selected = data2 %>%  
    filter(degree %in% c(0, 2, 4, 6, 8, 10))
  
  plot = data1_selected %>% 
    ggplot(aes(x)) +
    geom_line(data=data2_selected, aes(y = mean), color="black", 
              size =0.5, linetype="dashed") +
    geom_line(aes(y = mean), 
              # linetype="dashed",
              color=colors_slider[[2]],
              size =  0.5) +
    scale_x_continuous(breaks=seq(min(vertical.lines), max(vertical.lines), 1),
                       limits = c(min(vertical.lines), max(vertical.lines)))  +
    geom_vline(xintercept = vertical.lines, colour = "lightgrey", linetype="dashed") +
    xlab("vname") + ylab("density") +
    theme_classic() + 
    facet_wrap(~degree, ncol = 1) +
    theme(axis.text.x = element_text(size=8), 
          axis.text.y=element_text(size=8), 
          axis.title.x = element_blank(), 
          axis.title.y =element_blank(), 
          axis.ticks.x=element_blank())
  
  return(plot)
}


