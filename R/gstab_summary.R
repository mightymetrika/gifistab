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



# summary_replication_stability <- function(object) {
#   new_model <- broom::tidy(object$new_model)
#   boot_models <- handle_boot_model_summaries(object$boot_models)
#   list(new_model = new_model, boot_models = boot_models)
# }
#
# summary_statistical_stability <- function(object){
#   tidy_list_elements(object$stability$statistical_stability)
# }
#
# summary_data_selection_stability <- function(object){
#   tidy_list_elements(object$stability$stability_under_data_selection)
# }
#
# summary_model_selection_stability <- function(object){
#   tidy_list_elements(object$stability$stability_under_model_selection)
# }
#
# summary_numerical_stability <- function(object){
#   broom::tidy(object$stability$numerical_stability)
# }
#
# summary_analytic_and_algebraic_stability <- function(object){
#   object$stability$analytic_and_algebraic_stability
# }
#
# summary_technique_stability <- function(object){
#   tidy_list_elements(object$stability$stability_under_selection_of_technique)
# }
