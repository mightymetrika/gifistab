#' Summary of Stability Analysis for Linear Models
#'
#' This function generates a summary of the stability analysis performed by 'gstab.lm'. It summarizes the original model and each stability check, returning a list of these summaries.
#'
#' @param object An object of class 'gstab_lm'.
#' @param conf.int Logical variable. If TRUE, include a confidence interval in
#' the summary.
#' @param conf.level Confidence level for confidence interval when conf.int is
#' TRUE.
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
#' summary(stab_res, conf.int = TRUE, conf.level = 0.95)
summary.gstab_lm <- function(object, conf.int, conf.level, ...) {

  # Original model
  original_summary <- broom::tidy(object$original, conf.int, conf.level)

  # Replication stability
  replication_stability_summary <- summary_replication_stability(object$stability$replication_stability, conf.int, conf.level)

  # Statistical stability
  statistical_stability_summary <- summary_statistical_stability(object$stability$statistical_stability, conf.int, conf.level)

  # Stability under data selection
  data_selection_stability_summary <- summary_data_selection_stability(object$stability$stability_under_data_selection, conf.int, conf.level)

  # Stability under model selection
  model_selection_stability_summary <- summary_model_selection_stability(object$stability$stability_under_model_selection)

  # Numercal stability
  numerical_stability_summary <- summary_numerical_stability(object$stability$numerical_stability, conf.int, conf.level)

  # Analytical and algebraic
  analytic_and_algebraic_stability_summary <- summary_analytic_and_algebraic_stability(object$stability$analytic_and_algebraic_stability)

  # Selection of technique
  technique_stability_summary <- summary_technique_stability(object$stability$stability_under_selection_of_technique, conf.int, conf.level)

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

#' Summary for Stability Analysis for Generalized Linear Models
#'
#' This function produces a summary of the stability analysis for a generalized linear model.
#'
#' @param object An object of class "gstab_glm", usually, a result of a call to gstab.glm.
#' @param conf.int Logical variable. If TRUE, include a confidence interval in
#' the summary.
#' @param conf.level Confidence level for confidence interval when conf.int is
#' TRUE.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A list with the summary of the original model and the summaries of each stability check.
#' @export
#' @examples
#'   # Generating example data
#' n <- 20
#' set.seed(376)
#' data <- data.frame(y = stats::rbinom(n, 1, 0.5),
#'                    x1 = 3*stats::rnorm(n) +5 + stats::rnorm(n, 2, 0.3),
#'                    x2 = 2*stats::rnorm(n) + 1.5*stats::rnorm(n) + stats::rnorm(n, 1, 0.05))
#' formula <- y ~ x1 + x2
#'
#' # Fit the model
#' model <- stats::glm(formula, family = binomial, data = data)
#'
#' #Test with nboot
#' stab_res <- suppressWarnings(gstab(model = model, nboot = 100))
#' stab_sum <- summary(stab_res, conf.int = FALSE)
summary.gstab_glm <- function(object, conf.int, conf.level, ...) {

  # Apply each type of summary
  original_summary <- broom::tidy(object$original, conf.int, conf.level)
  replication_stability_summary <- summary_replication_stability(object$stability$replication_stability, conf.int, conf.level)
  statistical_stability_summary <- summary_statistical_stability(object$stability$statistical_stability, conf.int, conf.level)
  data_selection_stability_summary <- summary_data_selection_stability(object$stability$stability_under_data_selection, conf.int, conf.level)
  model_selection_stability_summary <- summary_model_selection_stability(object$stability$stability_under_model_selection)
  numerical_stability_summary <- summary_numerical_stability(object$stability$numerical_stability, conf.int, conf.level)
  analytic_and_algebraic_stability_summary <- summary_analytic_and_algebraic_stability(object$stability$analytic_and_algebraic_stability)
  technique_stability_summary <- summary_technique_stability(object$stability$stability_under_selection_of_technique, conf.int, conf.level)

  # Creating output
  out <- list(original_summary = original_summary,
              replication_stability_summary = replication_stability_summary,
              statistical_stability_summary = statistical_stability_summary,
              data_selection_stability_summary = data_selection_stability_summary,
              model_selection_stability_summary = model_selection_stability_summary,
              numerical_stability_summary = numerical_stability_summary,
              analytic_and_algebraic_stability_summary = analytic_and_algebraic_stability_summary,
              technique_stability_summary = technique_stability_summary)

  # Return output
  return(out)
}
