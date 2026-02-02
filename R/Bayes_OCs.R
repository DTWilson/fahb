
Bayes_OCs_curve <- function(scenario){
  # scenario is an integer ID
  # This function looks at a range of Bayes rules (i.e. threshold values) and
  # finds the Pareto front given by that range.
  # Returns a list of a reduced summary of the Pareto front.
  
  df <- readRDS(paste0("./data/res_", scenario, "_full.rds"))
  scenarios <- read.csv("R/scenarios.csv")
  sce <- scenarios[scenarios$id == scenario,][1,]
  
  Bayes_rule <- seq(0, max(df$pred_t, na.rm = T), length.out = 500)
  
  na_count <- sum(is.na(df$pred_t))
  if(na_count > 0) cat(na_count, "NAs in scenario", scenario)
  df <- df[!is.na(df$pred_t),]
  
  opt_vals <- t(sapply(Bayes_rule, Bayes_OCs, thr = sce$thr, df = df))
  opt_vals <- cbind(opt_vals, Bayes_rule)
  
  # Summarise rules by finding the optimal FNR for a series of nominal FPRs
  nom_fprs <- seq(0.01, 0.99, 0.01)
  opt_vals <- opt_vals[order(-opt_vals[,1]),]
  #opt_vals <- rbind(c(1,0), opt_vals, c(0,1))
  opt_fnrs <- t(sapply(nom_fprs, Bayes_opt_fnr, opt_vals = opt_vals))
  
  return(matrix(c(nom_fprs, opt_fnrs), ncol = 3))
}

Bayes_OCs <- function(rule, thr, df){
  # Calculate FPR and FNR for a given Bayes rule
  
  feas <- df$rec_t < thr
  go <- !((df$pred_t >= rule) | (df$pred_t < 0))
  
  fpr <- sum(go[!feas])/(sum(!feas))
  fnr <- sum(!go[feas])/(sum(feas))
  
  return(c(fpr, fnr))
}

Bayes_opt_fnr <- function(nom_fpr, opt_vals){
  # Find the best FNR for a nominal FPR
  return(opt_vals[opt_vals[,1] <= nom_fpr, ,drop = FALSE][1,2:3])
}
