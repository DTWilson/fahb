new_fahb_design <- function(problem,
                            PC_OCs, Bayes_OCs){
  structure(list(Prog_Crit_OCs = PC_OCs,
                 Bayes_OCs = Bayes_OCs),
            class = "fahb_design")
}

fahb_design <- function(problem){
  if(is.null(problem$sims)){
    stop("Problem does not contain trial simulations - see fahb::forecast()")
  }
  
  cat("Searching for efficient progression criteria...\n")
  PC_OCs <- prog_crit_ocs(problem)

  cat("Approximating Bayesian operating characteristics...\n")
  Bayes_OCs <- bayesian_ocs(problem)

  new_fahb_design(problem,
                  PC_OCs, Bayes_OCs)
}


