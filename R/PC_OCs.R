library(mco)

PC_OCs <- function(scenario){
  # scenario is an integer ID
  # This function finds efficient (Pareto optimal) decision rules of the 
  # standard PC form. Rules are evaluated w.r.t. their FPR and FNR, as defined 
  # by the threshold feasibility of that scenario.
  # Returns a list of a reduced summary of the Pareto front, and a vector
  # describing the extenet of redundancy in the full set of efficient 
  # solutions. 
  
  df <- readRDS(paste0("./data/res_", scenario, "_full.rds"))
  
  max_m_p <- max(df$m_p)
  max_n_p <- max(df$n_p)
  max_r_p <- max(df$r_p)
  
  opt <- nsga2(OCs, 3, 2,
               lower.bounds = rep(-1, 3),
               upper.bounds = c(max_m_p, max_n_p, max_r_p),
               popsize = 500, generations = 500,
               thr = 3.5, df = df)
  
  # Summarise rules by finding the optimal FNR for a series of nominal FPRs
  nom_fprs <- seq(0, 1, 0.01)
  opt_vals <- opt$value[order(-opt$value[,1]),]
  opt_vals <- rbind(c(1,0), opt_vals, c(0,1))
  opt_fnrs <- sapply(nom_fprs, opt_fnr, opt_vals = opt_vals)
  
  # Summarise the redundancy in efficient rules
  rules <- opt$par
  rules <- cbind(rules, apply(rules, 1, function(x) any(x < 0)))
  
  red_prop <- apply(rules[,1:3], 2, function(x) mean(x < 0))
  any_red_prop <- mean(rules[,4])
  
  return(list(matrix(c(nom_fprs, opt_fnrs), ncol = 2),
              c(red_prop, any_red_prop)))
}

OCs <- function(rule, thr, df){
  # Calculate FPR and FNR for a given rule
  
  feas <- df$rec_t < thr
  go <- df$m_p >= rule[1] & df$n_p >= rule[2] & df$r_p >= rule[3]
  
  fpr <- sum(go[!feas])/sum(!feas)
  fnr <- sum(!go[feas])/sum(feas)
  
  return(c(fpr, fnr))
}

opt_fnr <- function(nom_fpr, opt_vals){
  # Find the best FNR for a nominal FPR
  return(opt_vals[opt_vals[,1] <= nom_fpr, drop = FALSE][1,2])
}
