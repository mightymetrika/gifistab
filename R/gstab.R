#' Stability Analysis for Models
#'
#' Generic function for performing a stability analysis on a model. It calls the
#' appropriate method based on the class of its argument.
#'
#' @param model A fitted model object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Depends on the method called. If 'lm', a list with the original model
#' and the results of each stability check.
#'
#' @keywords internal
gstab <- function(model, ...) {
  UseMethod("gstab")
}

#' Stability Analysis for Linear Models
#'
#' This function performs a stability analysis on a linear model (of class 'lm'),
#' checking for different types of stabilities as defined by Michailides and de
#' Leeuw (1998). The stability tests include replication stability, statistical
#' stability, stability under data selection, stability under model selection,
#' numerical stability, analytic and algebraic stability, and stability under
#' selection of technique. The output is a list with the original model and the
#' results of each stability check.
#'
#' @param model An object of class "lm": a fitted linear model.
#' @param new_data An optional data frame in which to look for variables with which
#' to predict. If omitted, the fitted values are used.
#' @param nboot Number of bootstrap resamples to perform in the replication
#' stability test. If NULL (default), no resampling is done.
#' @param variable_to_remove Variable to remove for the stability under model
#' selection test. If NULL (default), no variable is removed.
#' @param variable_of_interest Variable to consider for backward selection in the
#' stability under model selection test. If NULL (default), no variable is considered.
#' @param nf Noise factor as a percentage. For non-binomial families, this is used to
#' determine the standard deviation of the noise added to the transformed response
#' variable. For binomial families with a binary response, this is used to determine
#' the fraction of the response variable to flip (i.e., change 0 to 1 or 1 to 0).
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A list with the original model and the results of each stability check.
#' @references
#' Michailides, G., & de Leeuw, J. (1998). The Gifi system for nonlinear multivariate
#' analysis. eScholarship, University of California, Los Angeles.
#' https://escholarship.org/uc/item/0789f7d3
#'
#' @keywords internal
gstab.lm <- function(model, new_data = NULL, nboot = NULL,
                     variable_to_remove = NULL,
                     variable_of_interest = NULL, nf = 0.05, ...) {

  # Extract data and formula from the model
  data <- model$model
  formula <- stats::formula(model)

  # Apply each type of stability
  replication_stability_results <- replication_stability(model, data, formula, new_data, nboot, ...)
  statistical_stability_results <- statistical_stability(model, data, formula, nf = nf, ...)
  stability_under_data_selection_results <- stability_under_data_selection(model, data, formula, ...)
  stability_under_model_selection_results <- stability_under_model_selection(model, data, formula, variable_to_remove, variable_of_interest, ...)
  numerical_stability_results <- numerical_stability(model, data, formula, ...)
  analytic_and_algebraic_stability_results <- analytic_and_algebraic_stability(model, ...)
  stability_under_selection_of_technique_results <- stability_under_selection_of_technique(model, data, formula, ...)

  # Creating output
  out <- list(original = model,
              stability = list(replication_stability = replication_stability_results,
                               statistical_stability = statistical_stability_results,
                               stability_under_data_selection = stability_under_data_selection_results,
                               stability_under_model_selection = stability_under_model_selection_results,
                               numerical_stability = numerical_stability_results,
                               analytic_and_algebraic_stability = analytic_and_algebraic_stability_results,
                               stability_under_selection_of_technique = stability_under_selection_of_technique_results))

  # Class & return output
  class(out) <- c("gstab_lm", "gstab")
  return(out)
}

#' Stability Analysis for Generalized Linear Models
#'
#' This function performs a stability analysis on a generalized linear model
#' (of class 'glm'), checking for different types of stabilities as defined by
#' Michailides and de Leeuw (1998). The stability tests include replication
#' stability, statistical stability, stability under data selection, stability
#' under model selection, numerical stability, analytic and algebraic stability,
#' and stability under selection of technique. The output is a list with the
#' original model and the results of each stability check.
#'
#' @param model An object of class "glm": a fitted generalized linear model.
#' @param new_data An optional data frame in which to look for variables with
#' which to predict. If omitted, the fitted values are used.
#' @param nboot Number of bootstrap resamples to perform in the replication
#' stability test. If NULL (default), no resampling is done.
#' @param variable_to_remove Variable to remove for the stability under model
#' selection test. If NULL (default), no variable is removed.
#' @param variable_of_interest Variable to consider for backward selection in the
#' stability under model selection test. If NULL (default), no variable is considered.
#' @param nf Noise factor as a percentage. For non-binomial families, this is used to
#' determine the standard deviation of the noise added to the transformed response
#' variable. For binomial families with a binary response, this is used to determine
#' the fraction of the response variable to flip (i.e., change 0 to 1 or 1 to 0).
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A list with the original model and the results of each stability check.
#' @references
#' Michailides, G., & de Leeuw, J. (1998). The Gifi system for nonlinear multivariate analysis. eScholarship, University of California, Los Angeles. https://escholarship.org/uc/item/0789f7d3
#'
#' @keywords internal
gstab.glm <- function(model, new_data = NULL, nboot = NULL,
                      variable_to_remove = NULL,
                      variable_of_interest = NULL, nf = 0.05, ...) {

  # Extract data and formula from the model
  data <- model$model
  formula <- stats::formula(model)

  # Apply each type of stability
  replication_stability_results <- replication_stability(model, data, formula, new_data, nboot, ...)
  statistical_stability_results <- statistical_stability(model, data, formula, nf = nf, ...)
  stability_under_data_selection_results <- stability_under_data_selection(model, data, formula, ...)
  stability_under_model_selection_results <- stability_under_model_selection(model, data, formula, variable_to_remove, variable_of_interest, ...)
  numerical_stability_results <- numerical_stability(model, data, formula, ...)
  analytic_and_algebraic_stability_results <- analytic_and_algebraic_stability(model, ...)
  stability_under_selection_of_technique_results <- stability_under_selection_of_technique(model, data, formula, ...)

  # Creating output
  out <- list(original = model,
              stability = list(replication_stability = replication_stability_results,
                               statistical_stability = statistical_stability_results,
                               stability_under_data_selection = stability_under_data_selection_results,
                               stability_under_model_selection = stability_under_model_selection_results,
                               numerical_stability = numerical_stability_results,
                               analytic_and_algebraic_stability = analytic_and_algebraic_stability_results,
                               stability_under_selection_of_technique = stability_under_selection_of_technique_results))

  # Class & return output
  class(out) <- c("gstab_glm", "gstab")
  return(out)
}
