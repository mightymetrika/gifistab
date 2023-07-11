#' Summary of Stability Analysis for Linear Models
#'
#' This function generates a summary of the stability analysis performed by 'gstab.lm'. It summarizes the original model and each stability check, returning a list of these summaries.
#'
#' @param object An object of class 'gstab_lm'.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A list with a summary of the original model and summaries of each stability check.
#' @seealso
#' \code{\link{gstab}} for performing the stability analysis,
#' @export
#' @examples
#' # Assuming that `model` is an object of class 'gstab_lm'
#' n <- 20
#' set.seed(376)
#' data <- data.frame(y = 3*stats::rnorm(n) +5,
#'                    x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
#'                    x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
#'
#' formula <- y ~ x1 + x2
#' model <- stats::lm(formula, data = data)
#'
#' stab_res <- gstab(model = model, nboot = 100)
#' summary(stab_res)
summary.gstab_lm <- function(object, ...) {

  # Original model
  original_summary <- broom::tidy(object$original)

  # Replication stability
  replication_stability_summary <- summary_replication_stability(object$stability$replication_stability)

  # Statistical stability
  statistical_stability_summary <- summary_statistical_stability(object$stability$statistical_stability)

  # Stability under data selection
  data_selection_stability_summary <- summary_data_selection_stability(object$stability$stability_under_data_selection)

  # Stability under model selection
  model_selection_stability_summary <- summary_model_selection_stability(object$stability$stability_under_model_selection)

  # Numercal stability
  numerical_stability_summary <- summary_numerical_stability(object$stability$numerical_stability)

  # Analytical and algebraic
  analytic_and_algebraic_stability_summary <- summary_analytic_and_algebraic_stability(object$stability$analytic_and_algebraic_stability)

  # Selection of technique
  technique_stability_summary <- summary_technique_stability(object$stability$stability_under_selection_of_technique)

  # Combining all summaries
  summary_out <- list(
    original_summary = original_summary,
    replication_stability_summary = replication_stability_summary,
    statistical_stability_summary = statistical_stability_summary,
    data_selection_stability_summary = data_selection_stability_summary,
    model_selection_stability_summary = model_selection_stability_summary,
    numerical_stability_summary = numerical_stability_summary,
    analytic_and_algebraic_stability_summary = analytic_and_algebraic_stability_summary,
    technique_stability_summary = technique_stability_summary
  )

  return(summary_out)


}
