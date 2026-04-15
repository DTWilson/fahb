new_fahb_design <- function(problem,
                            PC_OCs, Bayes_OCs){
  structure(list(Prog_Crit_OCs = PC_OCs,
                 Bayes_OCs = Bayes_OCs),
            class = "fahb_design")
}

#' Build a `fahb_design` object
#' 
#' Given a `fahb_problem` object, find efficient progression decision rules. 
#' These can include rules of the standard "progression criteria form", or rules
#' based on a Bayesian analysis of the pilot trial data, or both. 
#'
#' @param problem 
#'
#' @returns an object of class `fahb_design`.
#' @export
#'
#' @examples
#' problem <- forecast(fahb_problem())
#' fahb_design(problem)
#' 
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
  print(x$Prog_Crit_OCs)
  cat("\n")
  
  cat("Bayesian approximation\n")
  cat("\n")
  print(x$Bayes_OCs)
  cat("\n")
  cat("FPR - False Positive Rate\n")
  cat("FNR - False Negative Rate\n\n")
  cat("n_p, m_p, r_p - Probabilistic thresholds for standard\n")
  cat("                progression criteria on the number recruited,\n")
  cat("                number of sites opened, and the recruitment rate\n")
  cat("                (participants per site per year) respectively\n\n")
  cat("T_p - Bayesian decision rule threshold for the posterior predictive\n")
  cat("      expected time until full recruitment\n")
}


#' Plot operating characteristics of fahb designs
#' 
#' Takes an object of class `fahb_design` and plots the estimated operating 
#' characteristics of decision rules - based on standard progression criteria,
#' an approximate Bayesian analysis, or both.
#' 
#' @param x object of class `fahb_design` as produced by `fahb_design().`
#' @param ... further arguments passed to or from other methods.
#' 
#' @return no return value, called for side effects.
#' 
#' @export
plot.fahb_design <- function(x, ...){
  PC_OCs <- x$Prog_Crit_OCs[,1:2]
  PC_OCs$type <- "PC"
  Bayes_OCs <- x$Bayes_OCs[,1:2]
  Bayes_OCs$type <- "Bayes"
  
  all_OCs <- rbind(PC_OCs, Bayes_OCs)
  
  p <- ggplot2::ggplot(all_OCs, ggplot2::aes(FPR, FNR, colour = type)) + ggplot2::geom_step() +
    ggplot2::scale_color_discrete(name = "Method") +
    ggplot2::xlab("False Positive Rate") +
    ggplot2::ylab("False Negative Rate") +
    ggplot2::theme_minimal()
  
  return(p)
}
