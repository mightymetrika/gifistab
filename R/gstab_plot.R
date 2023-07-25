#' Plotting Function for `gstab_sum` Objects
#'
#' This function creates a list of plots for each of the stability assessments
#' performed in the `gstab` function. The plots are intended to provide a
#' visual overview of the stability of the model estimates under various
#' conditions.
#'
#' @param x A `gstab_sum` object, which is the output of the `summary.gstab`
#' function.
#' @param conf.int Logical. If TRUE, confidence intervals are added to the plots.
#' Default is TRUE.
#' @param ... Additional arguments passed to the underlying plotting functions.
#'
#' @return A list of ggplot2 objects, each representing a different stability
#' assessment.
#'
#' @keywords internal
plot.gstab_sum <- function(x, conf.int = TRUE, ...){
  # Get plots
  replication_stability_plot <- plot_replication_stability(x, conf.int)
  statistical_stability_plot <- plot_statistical_stability(x, conf.int)
  data_selection_stability_plot <- plot_data_selection_stability(x, conf.int)
  model_selection_stability_plot <- plot_model_selection_stability(x)
  numerical_stability_plot <- plot_numerical_stability(x, conf.int)
  analytic_and_algebraic_stability_plot <- plot_analytic_and_algebraic_stability(x)
  technique_stability_plot <- plot_technique_stability(x, conf.int)

  # Output plots
  out <- list(replication_stability_plot = replication_stability_plot,
              statistical_stability_plot = statistical_stability_plot,
              data_selection_stability_plot = data_selection_stability_plot,
              model_selection_stability_plot = model_selection_stability_plot,
              numerical_stability_plot = numerical_stability_plot,
              analytic_and_algebraic_stability_plot = analytic_and_algebraic_stability_plot,
              technique_stability_plot = technique_stability_plot)
}
