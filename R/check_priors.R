check_priors <- function(problem){
  # Plot prior densities for the three model parameters, and the prior 
  # predictive densities for the recruitment rate at a random site and
  # the time needed to fully recruit
  p_so <- plot_so_prior(problem)
  p_mean_rr <- plot_mean_rr_prior(problem)
  p_sd_rr <- plot_sd_rr_prior(problem)
  p_pp_rr <- plot_rr_prior_predictive(problem)
  p_pp_t <- plot_t_prior_predictive(problem)
  
  return(list(p_so,
              p_mean_rr,
              p_sd_rr,
              p_p_rr,
              p_pp_t))
}

plot_so_prior <- function(problem){
  
  shape <- problem$so_hp_a
  rate <- problem$so_hp_b
  lower <- max(0, shape/rate - 3*(shape/rate^2))
  upper <- shape/rate + 3*(shape/rate^2)
  
  x <- seq(lower, upper, length.out = 10^3)
  df <- data.frame(lambda = x,
                   pr = stats::dgamma(x, shape, rate=rate))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(lambda, pr)) + ggplot2::geom_line() +
    ggplot2::xlab("Site opening rate") + ggplot2::ylab("Prior prob.")
    ggplot2::theme_minimal()
    
  return(p)
}
