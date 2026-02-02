library(mco)

PC_OCs_curve <- function(scenario){
  # scenario is an integer ID
  # This function finds efficient (Pareto optimal) decision rules of the 
  # standard PC form. Rules are evaluated w.r.t. their FPR and FNR, as defined 
  # by the threshold feasibility of that scenario.
  # Returns a list of a reduced summary of the Pareto front, and a vector
  # describing the extenet of redundancy in the full set of efficient 
  # solutions. 
  
  df <- readRDS(paste0("./data/res_", scenario, "_full.rds"))
  scenarios <- read.csv("R/scenarios.csv")
  sce <- scenarios[scenarios$id == scenario,][1,]
  
  max_m_p <- max(df$m_p)
  max_n_p <- max(df$n_p)
  max_r_p <- max(df$r_p)
  
  opt <- nsga2(PC_OCs, 3, 2,
               lower.bounds = rep(-1, 3),
               upper.bounds = c(max_m_p, max_n_p, max_r_p),
               popsize = 100, generations = 100,
               thr = sce$thr, df = df)
  
  # Summarise rules by finding the optimal FNR for a series of nominal FPRs
  ## First, for every efficient rule record if the components were redundant
  opt_vals <- opt$value
  opt_vals <- cbind(opt_vals, opt$par, opt$par <= 0)

  ## Reduce down to a summary
  opt_vals <- opt_vals[order(-opt_vals[,1]),]

  nom_fprs <- seq(0.01, 0.99, 0.01)
  opt_fnrs <- t(sapply(nom_fprs, PC_opt_fnr, opt_vals = opt_vals))
  
  opt_full <- cbind(nom_fprs, opt_fnrs)
  
  ## Add extreme points
  #opt_full <- rbind(c(0,1,1,1,1),
  #                  opt_full, 
  #                  c(1,0,1,1,1))
  
  return(opt_full)
}

PC_OCs <- function(rule, thr, df){
  # Calculate FPR and FNR for a given rule
  
  feas <- df$rec_t < thr
  go <- df$m_p >= rule[1] & df$n_p >= rule[2] & df$r_p >= rule[3]
  
  fpr <- sum(go[!feas])/sum(!feas)
  fnr <- sum(!go[feas])/sum(feas)
  
  return(c(fpr, fnr))
}

PC_opt_fnr <- function(nom_fpr, opt_vals){
  # Find the best FNR for a nominal FPR
  return(opt_vals[opt_vals[,1] <= nom_fpr, ,drop = FALSE][1,2:8])
}
