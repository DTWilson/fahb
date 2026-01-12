
Bayes_OCs <- function(scenario){
  # scenario is an integer ID
  # This function looks at a range of Bayes rules (i.e. threshold values) and
  # finds the Pareto front given by that range.
  # Returns a list of a reduced summary of the Pareto front.
  
  df <- readRDS(paste0("./data/res_", scenario, "_full.rds"))
  
  Bayes_rule <- seq(0, max(df$pred_t), length.out = 500)
  
  opt_vals <- t(sapply(Bayes_rule, OCs, thr = 3.5, df = df))
  
  # Summarise rules by finding the optimal FNR for a series of nominal FPRs
  nom_fprs <- seq(0, 1, 0.01)
  opt_vals <- opt$value[order(-opt$value[,1]),]
  opt_vals <- rbind(c(1,0), opt_vals, c(0,1))
  opt_fnrs <- sapply(nom_fprs, opt_fnr, opt_vals = opt_vals)
  
  return(matrix(c(nom_fprs, opt_fnrs), ncol = 2))
}

OCs <- function(rule, thr, df){
  # Calculate FPR and FNR for a given Bayes rule
  
  feas <- df$rec_t < thr
  go <- df$pred_t < rule
  
  fpr <- sum(go[!feas])/sum(!feas)
  fnr <- sum(!go[feas])/sum(feas)
  
  return(c(fpr, fnr))
}
