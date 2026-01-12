library(extraDistr)

generate_data <- function(m, int_t, target_n, 
                               beta_m, beta_s, v_df, v_sc, setup_r_a, setup_r_b) {
  
  # m - total number of sites to be set up
  # int_t - time of interim analysis (years)
  # target_n - target recruitment, which we will cap at
  # beta_m, beta_s - hyperparameters for the (normal) prior on beta_0
  # v_df, v_sc = hyperparameters for the (half scaled t) prior on var_l
  # setup_r_a, _b - hyperparameters for (gamma) prior on yearly site set up rate
  
  # Max length of recruitment period in years (pretty arbitrary)
  end_rec <- 10 
  
  # True per year recruitment rates for each site
  ## Sample beta_0 from prior
  beta_0 <- rnorm(1, mean = beta_m, sd = beta_s)
  ## Sample var_l from prior
  var_l <- abs(rlst(1, df = v_df, mu = 0, sigma = v_sc))^2
  ## Sample the site rates from a log-normal 
  lambdas <- exp(rnorm(m, beta_0, sqrt(var_l)))
  
  # True setup rate, from the prior
  setup_r <- rgamma(1, setup_r_a, setup_r_b)
  
  # Simulate setup times
  setup_ts <- cumsum(rexp(m, setup_r))
  
  # Note how many sites are set up at interim
  #pilot_sites <- which(setup_ts <= int_t)
  #m_p <- length(pilot_sites)
  m_p <- sum(setup_ts <= int_t)
  
  # Get the different recruitment rates over time
  ## Each row is a recruitment period, changing when a site comes online
  ## Columns are start times, end times, and true overall recruitment rates
  rec_rates <- matrix(c(0, setup_ts,
                        setup_ts, end_rec,
                        0, cumsum(lambdas)), ncol = 3)
  
  # Simulate numbers recruited in each period and add these as a column
  ## First get the vector of expected numbers recruited in each period
  exp_rec <- (rec_rates[,3]*(rec_rates[,2] - rec_rates[,1]))[2:(m+1)]
  ## Now sample from a Poisson for each period
  rec_rates <- cbind(rec_rates, c(0, rpois(m, exp_rec)))
  
  # Get time at which total target n is hit
  ## First get the period when it happens
  fin_period <- which(cumsum(rec_rates[,4]) > target_n)[1]
  ## Determine how many people need to arrive in that final period
  n_needed <- target_n - sum(rec_rates[1:(fin_period-1), 4])
  ## Simulate when the target is hit, which follows a gamma distribution
  rec_time <- rec_rates[fin_period, 1] + rgamma(1, shape = n_needed, rate = rec_rates[fin_period, 3])
  
  # Get total number recruited at interim
  if(m_p > 0){
    ## First get period (row in matrix) where interim occurs
    int_period <- which(rec_rates[,2] > int_t)[1]
    ## Start with the number recruited from all previous periods
    n_p <- sum(rec_rates[1:(int_period-1), 4])
    ## Simulate the extra number recruited in the final period, using the fact
    ## that arrivals will be uniformly distributed over the period
    n_p <- n_p +  sum(runif(rec_rates[int_period, 4], rec_rates[int_period, 1], rec_rates[int_period, 2]) < int_t)
    ## Distribute these participants to sites in proportion to their expected
    ## numbers, given their true rates and times open
    n_ps <- rmultinom(1, n_p, prob = (int_t - setup_ts[1:m_p])*lambdas[1:m_p])
    
    # Create a data frame storing the sites open at interim, the numbers 
    # they each recruited, and how long they were recruiting for
  
    df <- data.frame(y = n_ps[1:m_p],
                     c = 1:m_p,
                     t = int_t - setup_ts[1:m_p])
  } else {
    # If no sites are recruited by interim, handle this manually
    df <- data.frame(y = 0, c = 0, t = 0)
  }
  
  # Output the interim data and the time to reach target n
  return(list(data = df, rec_time = rec_time))
  
  #generate_data(m=20, int_t=0.5, target_n=300, beta_m=1.75, beta_s=0.3, v_df=10, v_sc=0.4, setup_r_a=10, setup_r_b=1)
}
