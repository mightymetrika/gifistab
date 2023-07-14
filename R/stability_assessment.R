#' Stability Assessment
#'
#' Performs a stability assessment of a model based on the framework presented by
#' Michailides and de Leeuw (1998). The stability assessment considers various
#' forms of stability, including replication stability, statistical stability,
#' stability under data selection, stability under model selection, numerical
#' stability, analytic and algebraic stability, and stability under selection of
#' technique.
#'
#' @param data A data frame containing the data to be used for model fitting.
#' @param formula A formula describing the model to be fitted.
#' @param engine A function for fitting the model. This could be a function like
#' \code{stats::lm} or \code{stats::glm}.
#' @param new_data An optional data frame to be used for replication stability
#' assessment. If provided, the model will be fitted on this new data set and the
#' results compared with the original model.
#' @param nboot An optional integer specifying the number of bootstrap resamples
#' to use for replication stability assessment.
#' @param variable_to_remove An optional character string specifying a variable
#' to be removed for stability under model selection assessment.
#' @param variable_of_interest An optional character string specifying a variable
#' of interest for backward selection in stability under model selection assessment.
#' @param conf.int Logical variable. If TRUE, include a confidence interval in
#' the summary. Defaults to TRUE.
#' @param conf.level Confidence level for confidence interval when conf.int is
#' TRUE. Defaults to 0.95.
#' @param ... Additional arguments to be passed to the `engine` function.
#'
#' @return A list containing the results of the stability assessment. The list
#' includes the original `gstab` object, the summary of the `gstab` object, and
#' the explanations of the `gstab` object.
#'
#' @references
#' Michailides, G., & de Leeuw, J. (1998). The Gifi system for nonlinear multivariate analysis. eScholarship, University of California, Los Angeles. \url{https://escholarship.org/uc/item/0789f7d3}
#'
#' @export
#'
#' @examples
#' # To be provided...
#'
#' @seealso
#'   \code{\link[base]{do.call}}
#'
stability_assessment <- function(data, formula, engine, new_data = NULL,
                                 nboot = NULL, variable_to_remove = NULL,
                                 variable_of_interest = NULL,
                                 conf.int = TRUE, conf.level = 0.95, ...) {
  # Fit the model using the specified engine and arguments
  fit_model <- do.call(engine, c(list(formula, data = data), list(...)))

  # Perform stability assessment
  gstab_result <- gstab(fit_model, new_data = new_data, nboot = nboot, variable_to_remove = variable_to_remove, variable_of_interest = variable_of_interest, ...)

  # Generate summary of stability assessment
  gstab_summary_result <- summary(gstab_result, conf.int, conf.level)

  # Generate explanations of stability assessment
  gstab_explainer_result <- stab_explainer(gstab_result)

  # Return a list containing the results
  result <- list(gstab = gstab_result,
                 gstab_summary = gstab_summary_result,
                 gstab_explainer = gstab_explainer_result)

  return(result)
}
