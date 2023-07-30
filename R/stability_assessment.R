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
#' @param seed An optional integer that sets the seed for the random number
#' generator in order to obtain reproducible results. If NULL (default), the
#' random number generation will not be reproducible.
#' @param nf Noise factor as a percentage. For non-binomial families, this is used to
#' determine the standard deviation of the noise added to the transformed response
#' variable. For binomial families with a binary response, this is used to determine
#' the fraction of the response variable to flip (i.e., change 0 to 1 or 1 to 0).
#' @param ... Additional arguments to be passed to the `engine` function.
#'
#' @return A list containing the results of the stability assessment. The list
#' includes the original `gstab` object, the summary of the `gstab` object, the
#' plots of the `gstab_sum` object, and the explanations of the `gstab` object.
#'
#' @references
#' Michailides, G., & de Leeuw, J. (1998). The Gifi system for nonlinear multivariate analysis. eScholarship, University of California, Los Angeles. \url{https://escholarship.org/uc/item/0789f7d3}
#'
#' @export
#'
#' @examples
#' formula <- y ~ x1 + x2
#' stability_assessment(data = n20_seed376_lm,
#'                      formula = formula,
#'                      engine = stats::lm)
stability_assessment <- function(data, formula, engine, new_data = NULL,
                                 nboot = NULL, variable_to_remove = NULL,
                                 variable_of_interest = NULL,
                                 conf.int = TRUE, conf.level = 0.95, seed = NULL,
                                 nf = 0.05, ...) {

  # Check that data is a data frame
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  # Check that formula is a formula
  if (!inherits(formula, "formula")) {
    stop("formula must be a formula object.")
  }

  # Check that engine is a function
  if (!is.function(engine)) {
    stop("engine must be a function.")
  }

  # Check that new_data is a data frame, if provided
  if (!is.null(new_data) && !is.data.frame(new_data)) {
    stop("new_data must be a data frame.")
  }

  # Check that nboot is a single integer, if provided
  if (!is.null(nboot)) {
    if (!is.numeric(nboot) || length(nboot) != 1 || nboot != floor(nboot)) {
      stop("nboot must be a single integer.")
    }
  }

  # Check that variable_to_remove is a character string, if provided
  if (!is.null(variable_to_remove) && !is.character(variable_to_remove)) {
    stop("variable_to_remove must be a character string.")
  }

  # Check that variable_of_interest is a character string, if provided
  if (!is.null(variable_of_interest) && !is.character(variable_of_interest)) {
    stop("variable_of_interest must be a character string.")
  }

  # Check that conf.int is a logical value
  if (!is.logical(conf.int) || length(conf.int) != 1) {
    stop("conf.int must be a logical value.")
  }

  # Check that conf.level is a numeric value between 0 and 1
  if (!is.numeric(conf.level) || length(conf.level) != 1 || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a numeric value between 0 and 1.")
  }

  # Check that seed is a single integer, if provided
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || seed != floor(seed)) {
      stop("seed must be a single integer.")
    }
  }

  # Set random number seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Fit the model using the specified engine and arguments
  fit_model <- do.call(engine, c(list(formula, data = data), list(...)))

  # Perform stability assessment
  gstab_result <- gstab(fit_model, new_data = new_data, nboot = nboot, variable_to_remove = variable_to_remove, variable_of_interest = variable_of_interest, nf = nf, ...)

  # Generate summary of stability assessment
  gstab_summary_result <- summary(gstab_result, conf.int, conf.level)

  # Generate plot of stability assessment
  gstab_plot_result<- plot(gstab_summary_result, conf.int)

  # Generate explanations of stability assessment
  gstab_explainer_result <- stab_explainer(gstab_result)

  # Return a list containing the results
  result <- list(gstab = gstab_result,
                 gstab_summary = gstab_summary_result,
                 gstab_plot = gstab_plot_result,
                 gstab_explainer = gstab_explainer_result)

  return(result)
}
