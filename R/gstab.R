# General stability function (S3 generic)
gstab <- function(model, ...) {
  UseMethod("gstab")
}

# S3 method for lm models
gstab.lm <- function(data, formula, new_data = NULL, nboot = NULL, variable_to_remove = NULL, variable_of_interest = NULL, ...) {

  # Run main model
  model <- stats::lm(formula, data = data, ...)

  # Apply each type of stability
  replication_stability_results <- replication_stability(model, data, formula, new_data, nboot, ...)
  statistical_stability_results <- statistical_stability(model, data, formula, ...)
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
