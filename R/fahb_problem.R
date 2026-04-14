new_fahb_problem <- function(N, m, t, p_t, 
                             so_hp_a, so_hp_b,
                             mean_rr_hp_a, mean_rr_hp_b,
                             sd_rr_hp_a, sd_rr_hp_b,
                             n_sims,
                             sims){
  
  structure(list(N=N, m=m, t=t, p_t=p_t,
                 so_hp_a=so_hp_a, so_hp_b=so_hp_b,
                 mean_rr_hp_a=mean_rr_hp_a, mean_rr_hp_b = mean_rr_hp_b,
                 sd_rr_hp_a=sd_rr_hp_a, sd_rr_hp_b=sd_rr_hp_b,
                 n_sims=n_sims,
                 sims=sims),
            class = "fahb")
}

validate_fahb_problem <- function(y){

  return(TRUE)  
}

fahb_problem <- function(N = 320, m = 20 , t = 0.167,
                         so_hps = c(30, 2.85), 
                         mean_rr_hps = c(2, 0.329), 
                         sd_rr_hps = c(30, 100),
                         n_sims = 10^3){
  # Defaults from our GUSTO example
  
  # Check inputs make sense
  if(N < 1 | m < 1){
    stop("N and m must be >= 1")
  }
  if(t < 0 | t > 1){
    stop("t must be in [0,1]")
  }
  if(any(c(so_hps, mean_rr_hps, sd_rr_hps) < 0)){
    stop("All yperparameters must be > 0")
  }
  
  # Calculate exp_T - the expected time for trial to recruit
  exp_T <- exp_rec_time(m, N, so_hps, mean_rr_hps)
  
  # Calculate p_t - the calendar time of the internal pilot analysis
  p_t <- exp_T*t
  
  new_fahb_problem(N, m, t, p_t,
                   so_hp_a = so_hps[1], so_hp_b = so_hps[2],
                   mean_rr_hp_a = mean_rr_hps[1], mean_rr_hp_b = mean_rr_hps[2],
                   sd_rr_hp_a = sd_rr_hps[1], sd_rr_hp_b = sd_rr_hps[2],
                   n_sims,
                   sims = NULL)
  
  }
  
exp_rec_time <- function(m, N, so_hps, mean_rr_hps){
  
  lambda_a <- so_hps[1]; lambda_b <- so_hps[2]
  beta_m <- mean_rr_hps[1]; beta_s <- mean_rr_hps[2]
  
  # Expected rate of recruitment after all sites open
  final_rate <- exp(beta_m + beta_s^2/2)*m
  # Expected time until all sites open
  all_open <- m/(lambda_a/lambda_b)
  # Expected number recruited when all sites open
  n_0 <- final_rate*all_open/2
  if(n_0 >= N){
    exp_time <- sqrt(2*N*all_open/final_rate)
  } else {
    exp_time <- (N - (n_0 - final_rate*all_open))/final_rate
  }
  return(exp_time)
}