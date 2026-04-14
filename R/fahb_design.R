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


#' Print a fahb design object
#' 
#' The default print method for a `fahb_design` object.
#' 
#' @param x object of class `fahb_design` as produced by `fahb_design()`.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return no return value, called for side effects.
#' 
#' @export
print.fahb_design <- function(x, ...){
  cat("Standard progression criteria\n")
  cat("\n")
  x$Prog_Crit_OCs
  cat("\n")
  
  cat("Bayesian approximation\n")
  cat("\n")
  x$Bayes_OCs
  cat("\n")
}
