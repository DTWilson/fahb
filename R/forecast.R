forecast <- function(problem, rewrite = FALSE){
  
  if(!is.null(problem$sims) & !rewrite){
    stop("Forecasts have been simulated already - to discard, use rewrite = TRUE")
  }
  
  r <- t(replicate(problem$n_sims, 
                    simulate_data(beta_m=problem$mean_rr_hp_a, beta_s=problem$mean_rr_hp_b,
                                  v_sh=problem$sd_rr_hp_a, v_r=problem$sd_rr_hp_b,
                                  setup_r_a=problem$so_hp_a, setup_r_b=problem$so_hp_b,
                                  target_n=problem$N, m=problem$m, int_t=problem$p_t)))
  r <- as.data.frame(r)
  names(r) <- c("rec_T", "n_p", "m_p", "r_p")
   
  problem$sims <- r
  return(problem)
}
