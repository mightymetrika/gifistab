#' Create Replication Stability Assessment Plot
#'
#' This internal function creates ggplot objects for the replication stability
#' assessment. It generates two separate plots, one comparing original model
#' estimates with new model estimates, and another comparing original model
#' estimates with bootstrap mean estimates.
#'
#' @param obj A list object returned from the `replication_stability` function,
#' containing original model summary, and summaries for new model and bootstrap
#' models.
#' @param conf.int Logical variable. If `TRUE`, add error bars to the points in
#' the plot, representing confidence intervals.
#'
#' @return A list containing ggplot2 objects. The list includes `new` which is a
#' ggplot object of the comparison between original and new model estimates
#' (if new data was provided), and `boot` which is a ggplot object of the
#' comparison between original and bootstrap mean estimates (if bootstrap
#' resampling was performed).
#'
#' @keywords internal
plot_replication_stability <- function(obj, conf.int) {
  original_estimates <- obj$original_summary
  new_estimates <- obj$replication_stability_summary$new_model
  boot_mean_estimates <- obj$replication_stability_summary$boot_models$boot_mean

  n_terms_original <- nrow(original_estimates)
  n_terms_new <- nrow(new_estimates)
  n_terms_boot <- nrow(boot_mean_estimates)

  p_new <- NULL
  if (n_terms_new > 0){
    plot_df_new <- data.frame(
      term = c(original_estimates$term, new_estimates$term),
      estimate = c(original_estimates$estimate, new_estimates$estimate),
      conf.low = if(conf.int) c(original_estimates$conf.low, new_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_new)),
      conf.high = if(conf.int) c(original_estimates$conf.high, new_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_new)),
      type = c(rep("Original", n_terms_original), rep("New", n_terms_new))
    )

    p_new <- create_plot(plot_df_new, conf.int)
  }

  p_boot <- NULL
  if (!is.null(n_terms_boot)){
    plot_df_boot <- data.frame(
      term = c(original_estimates$term, boot_mean_estimates$term),
      estimate = c(original_estimates$estimate, boot_mean_estimates$estimate),
      conf.low = if(conf.int) c(original_estimates$conf.low, boot_mean_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_boot)),
      conf.high = if(conf.int) c(original_estimates$conf.high, boot_mean_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_boot)),
      type = c(rep("Original", n_terms_original), rep("Bootstrap Mean", n_terms_boot))
    )

    p_boot <- create_plot(plot_df_boot, conf.int)
  }

  out <- list(new = p_new, boot = p_boot)
  return(out)
}

#' Create Statistical Stability Assessment Plot
#'
#' This internal function creates a ggplot object for the statistical stability
#' assessment. It generates a plot comparing original model estimates with
#' estimates from models fitted on data with added noise and permuted noise.
#'
#' @param obj A list object returned from the `statistical_stability` function,
#' containing original model summary, and summaries for models fitted on noisy
#' data and permuted noisy data.
#' @param conf.int Logical variable. If `TRUE`, add error bars to the points in
#' the plot, representing confidence intervals.
#'
#' @return A ggplot2 object of the comparison between original, noisy, and
#' permuted noisy model estimates.
#'
#' @keywords internal
plot_statistical_stability <- function(obj, conf.int) {

  # Initialize empty plot_df
  plot_df <- data.frame()

  # Check for NULL and add to plot_df accordingly
  if (!is.null(obj$original_summary)) {
    original_estimates <- obj$original_summary
    n_terms_original <- nrow(original_estimates)
    plot_df <- rbind(plot_df, data.frame(
      term = original_estimates$term,
      estimate = original_estimates$estimate,
      conf.low = if(conf.int) original_estimates$conf.low else rep(NA, n_terms_original),
      conf.high = if(conf.int) original_estimates$conf.high else rep(NA, n_terms_original),
      type = rep("Original", n_terms_original)
    ))
  }

  if (!is.null(obj$statistical_stability_summary$noisy_model)) {
    noisy_estimates <- obj$statistical_stability_summary$noisy_model
    n_terms_noisy <- nrow(noisy_estimates)
    plot_df <- rbind(plot_df, data.frame(
      term = noisy_estimates$term,
      estimate = noisy_estimates$estimate,
      conf.low = if(conf.int) noisy_estimates$conf.low else rep(NA, n_terms_noisy),
      conf.high = if(conf.int) noisy_estimates$conf.high else rep(NA, n_terms_noisy),
      type = rep("Noisy", n_terms_noisy)
    ))
  }

  if (!is.null(obj$statistical_stability_summary$permuted_noisy_model)) {
    permuted_noisy_estimates <- obj$statistical_stability_summary$permuted_noisy_model
    n_terms_permuted_noisy <- nrow(permuted_noisy_estimates)
    plot_df <- rbind(plot_df, data.frame(
      term = permuted_noisy_estimates$term,
      estimate = permuted_noisy_estimates$estimate,
      conf.low = if(conf.int) permuted_noisy_estimates$conf.low else rep(NA, n_terms_permuted_noisy),
      conf.high = if(conf.int) permuted_noisy_estimates$conf.high else rep(NA, n_terms_permuted_noisy),
      type = rep("Permuted Noisy", n_terms_permuted_noisy)
    ))
  }

  # Create the plot
  p <- create_plot(plot_df, conf.int)
  return(p)
}

#' Create Stability under Data Selection Assessment Plot
#'
#' This function creates a list of ggplot objects for the stability under data
#' selection assessment. It generates plots comparing original model estimates
#' with estimates from models fitted on resampled data, data with outliers
#' removed, and stratified bootstrap data.
#'
#' @param obj A list object returned from the `stability_under_data_selection`
#' function, containing original model summary, and summaries for models fitted
#' on bootstrap data, no outlier data, and three stratified bootstrap datasets.
#' @param conf.int Logical variable. If `TRUE`, add error bars to the points in
#' the plot, representing confidence intervals.
#'
#' @return A list of ggplot2 objects of the comparison between original and
#' various data selected model estimates.
#'
#' @keywords internal
plot_data_selection_stability <- function(obj, conf.int) {
  original_estimates <- obj$original_summary
  bootstrap_estimates <- obj$data_selection_stability_summary$bootstrap_model
  no_outlier_estimates <- obj$data_selection_stability_summary$no_outlier_model
  strata_boot_estimates1 <- obj$data_selection_stability_summary$strata_boot_model1
  strata_boot_estimates2 <- obj$data_selection_stability_summary$strata_boot_model2
  strata_boot_estimates3 <- obj$data_selection_stability_summary$strata_boot_model3


  n_terms_original <- nrow(original_estimates)
  n_terms_bootstrap <- nrow(bootstrap_estimates)
  n_terms_no_outlier <- nrow(no_outlier_estimates)
  n_terms_strata_boot1 <- nrow(strata_boot_estimates1)
  n_terms_strata_boot2 <- nrow(strata_boot_estimates2)
  n_terms_strata_boot3 <- nrow(strata_boot_estimates3)

  # Create a list to store the plots
  plot_list <- list("bootstrap" = NULL,
                    "no_outlier" = NULL,
                    "strata_bootstrap" = NULL)

  # Original vs Bootstrap
  if (!is.null(bootstrap_estimates)) {
  plot_df_bootstrap <- data.frame(
    term = c(original_estimates$term, bootstrap_estimates$term),
    estimate = c(original_estimates$estimate, bootstrap_estimates$estimate),
    conf.low = if(conf.int) c(original_estimates$conf.low, bootstrap_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_bootstrap)),
    conf.high = if(conf.int) c(original_estimates$conf.high, bootstrap_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_bootstrap)),
    type = c(rep("Original", n_terms_original), rep("Bootstrap", n_terms_bootstrap))
  )

  plot_list[["bootstrap"]] <- create_plot(plot_df_bootstrap, conf.int)
  }

  # Original vs No Outlier
  if (!is.null(no_outlier_estimates)){
    plot_df_no_outlier <- data.frame(
      term = c(original_estimates$term, no_outlier_estimates$term),
      estimate = c(original_estimates$estimate, no_outlier_estimates$estimate),
      conf.low = if(conf.int) c(original_estimates$conf.low, no_outlier_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_no_outlier)),
      conf.high = if(conf.int) c(original_estimates$conf.high, no_outlier_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_no_outlier)),
      type = c(rep("Original", n_terms_original), rep("No Outlier", n_terms_no_outlier))
    )

    plot_list[["no_outlier"]] <- create_plot(plot_df_no_outlier, conf.int)
  }

  # Original vs Stratified Bootstrap
  if (!is.null(strata_boot_estimates1) && !is.null(strata_boot_estimates2) && !is.null(strata_boot_estimates3)) {
  plot_df_strata_boot <- data.frame(
    term = c(original_estimates$term, strata_boot_estimates1$term, strata_boot_estimates2$term, strata_boot_estimates3$term),
    estimate = c(original_estimates$estimate, strata_boot_estimates1$estimate, strata_boot_estimates2$estimate, strata_boot_estimates3$estimate),
    conf.low = if(conf.int) c(original_estimates$conf.low,
                              strata_boot_estimates1$conf.low,
                              strata_boot_estimates2$conf.low,
                              strata_boot_estimates3$conf.low) else c(rep(NA, n_terms_original),
                                                                      rep(NA, n_terms_strata_boot1),
                                                                      rep(NA, n_terms_strata_boot2),
                                                                      rep(NA, n_terms_strata_boot3)),
    conf.high = if(conf.int) c(original_estimates$conf.high,
                              strata_boot_estimates1$conf.high,
                              strata_boot_estimates2$conf.high,
                              strata_boot_estimates3$conf.high) else c(rep(NA, n_terms_original),
                                                                      rep(NA, n_terms_strata_boot1),
                                                                      rep(NA, n_terms_strata_boot2),
                                                                      rep(NA, n_terms_strata_boot3)),
    type = c(rep("Original", n_terms_original),
             rep("Strata 1 Bootstrap", n_terms_strata_boot1),
             rep("Strata 2 Bootstrap", n_terms_strata_boot2),
             rep("Strata 3 Bootstrap", n_terms_strata_boot3))
  )

  plot_list[["strata_bootstrap"]] <- create_plot(plot_df_strata_boot, conf.int)
  }

  return(plot_list)
}

#' Create Stability under Model Selection Assessment Plot
#'
#' This function creates a ggplot object for the stability under model selection
#' assessment. It generates a bar plot comparing p-values for the original model
#' with those from models with the intercept toggled, a user-selected variable
#' removed, and the least useful variable removed.
#'
#' @param obj A list object returned from the `stability_under_model_selection`
#' function, containing summaries for models with the intercept toggled, a
#' user-selected variable removed, and the least useful variable removed.
#'
#' @return A ggplot2 object of the comparison of p-values among various model
#' selection strategies. If there are no data to plot, the function will return `NULL` and issue a warning.
#'
#' @keywords internal
plot_model_selection_stability <- function(obj) {
  # Extract the summaries
  toggle_intercept_summary <- if (nrow(obj$model_selection_stability_summary$toggle_intercept) > 0)
    cbind(obj$model_selection_stability_summary$toggle_intercept, type = "Toggle Intercept") else NULL

  remove_variable_summary <- if (nrow(obj$model_selection_stability_summary$remove_variable) > 0)
    cbind(obj$model_selection_stability_summary$remove_variable, type = "Remove Variable") else NULL

  remove_least_useful_summary <- if (nrow(obj$model_selection_stability_summary$remove_least_useful) > 0)
    cbind(obj$model_selection_stability_summary$remove_least_useful, type = "Remove Least Useful") else NULL

  # Combine the summaries into a single data frame
  plot_df <- do.call("rbind", list(toggle_intercept_summary, remove_variable_summary, remove_least_useful_summary))

  # If plot_df is NULL or has no rows, return NULL
  if (is.null(plot_df) || nrow(plot_df) == 0) {
    warning("No data to plot.")
    return(NULL)
  }

  # Filter out rows with NA p-values
  plot_df <- dplyr::filter(plot_df, !is.na(p.value))

  # Create a bar plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = type, y = p.value, fill = type)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(x = "Model Selection Strategy", y = "p-value") +
    ggplot2::theme_minimal()

  return(p)
}

#' Create Numerical Stability Assessment Plot
#'
#' This function creates a ggplot object for the numerical stability assessment.
#' It visualizes the comparison of the original model estimates and the estimates
#' from the model fitted on data perturbed by a small amount of random noise.
#'
#' @param obj A list object returned from the `numerical_stability` function,
#' containing summaries for the original model and the model fitted on perturbed
#' data.
#' @param conf.int A number indicating the confidence interval level
#' (e.g., 0.95 for a 95% confidence interval).
#'
#' @return A ggplot2 object of the comparison of estimates from the original
#' model and the perturbed model.
#'
#' @keywords internal
plot_numerical_stability <- function(obj, conf.int) {

  # Extract the summary of the numerical stability model
  original_summary <- obj$original_summary
  numerical_stability_summary <- obj$numerical_stability_summary

  # Combine the original estimates and the estimates from the numerical stability model into a single data frame
  plot_df <- rbind(
    cbind(original_summary, type = "Original"),
    cbind(numerical_stability_summary, type = "Numerical Stability")
  )

  # Create the plot
  p <- create_plot(plot_df, conf.int)

  return(p)
}

# plot_analytic_and_algebraic_stability <- function(obj) {
#
#   # Create a data frame for plotting
#   plot_df <- data.frame(kappa = obj$analytic_and_algebraic_stability_summary, model = "Model")
#
#   # Create the plot
#   p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = model, y = kappa)) +
#     ggplot2::geom_col(fill = "steelblue") +
#     ggplot2::geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
#     ggplot2::annotate("text", x = 1, y = 31, label = "Kappa = 30", hjust = 0) +
#     ggplot2::labs(y = "Kappa", x = "", title = "") +
#     ggplot2::theme_minimal()
#
#   return(p)
# }

#' Create Analytic and Algebraic Stability Assessment Plot
#'
#' This function creates a ggplot object for the analytic and algebraic stability
#' assessment. It visualizes the comparison of condition numbers (kappa) based
#' on the L1 and Linf norms.
#'
#' @param obj A list object returned from the `analytic_and_algebraic_stability`
#' function, containing the condition numbers for L1 and Linf norms.
#'
#' @return A ggplot2 object displaying the condition numbers based on the L1 and
#' Linf norms, indicating the sensitivity of the model to changes in the
#' independent variables.
#'
#' @keywords internal
plot_analytic_and_algebraic_stability <- function(obj) {

  # Create a data frame for plotting
  plot_df <- data.frame(kappa = c(obj$analytic_and_algebraic_stability_summary$L1,
                                  obj$analytic_and_algebraic_stability_summary$Linf),
                        norm = c("L1", "Linf"))

  # Create the plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = norm, y = kappa, fill = norm)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    ggplot2::annotate("text", x = 1, y = 31, label = "Kappa = 30", hjust = 0) +
    ggplot2::labs(y = "Kappa", x = "Norm", title = "") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = c("L1" = "steelblue", "Linf" = "skyblue"))

  return(p)
}

#' Create Stability Under Selection of Technique Assessment Plot
#'
#' This function generates a ggplot object that visualizes the comparison between
#' the original model and a robust regression model, assessing the stability of
#' the model under different fitting techniques.
#'
#' @param obj A list object returned from the 'stability_under_selection_of_technique'
#' function, containing the model summaries for the original and robust regression
#' models.
#' @param conf.int Logical. If TRUE, confidence intervals are added to the plot.
#'
#' @return A ggplot2 object showing the model estimates from the original and
#' robust regression models, demonstrating the stability of the model under
#' different fitting techniques. If the robust model did not converge, a NULL
#' object is returned.
#'
#' @keywords internal
plot_technique_stability <- function(obj, conf.int) {
  if (is.null(obj$technique_stability_summary)) {
    message("Technique stability plot cannot be created because model did not converge.")
    return(NULL)
  }

  # Extract the original and robust regression summaries
  original_estimates <- obj$original_summary
  robust_estimates <- obj$technique_stability_summary

  n_terms_original <- nrow(original_estimates)
  n_terms_robust <- nrow(robust_estimates)

  # Combine into a single data frame
  plot_df_technique <- data.frame(
    term = c(original_estimates$term, robust_estimates$term),
    estimate = c(original_estimates$estimate, robust_estimates$estimate),
    conf.low = if(conf.int) c(original_estimates$conf.low, robust_estimates$conf.low) else c(rep(NA, n_terms_original), rep(NA, n_terms_robust)),
    conf.high = if(conf.int) c(original_estimates$conf.high, robust_estimates$conf.high) else c(rep(NA, n_terms_original), rep(NA, n_terms_robust)),
    type = c(rep("Original", n_terms_original), rep("Robust", n_terms_robust))
  )

  # Create the plot
  plot <- create_plot(plot_df_technique, conf.int)

  return(plot)
}

#' Create Stability Assessment Plot
#'
#' This internal function creates a ggplot object for the stability assessment.
#' It plots estimates for each term in the model and differentiates types with
#' color. Additionally, if `conf.int` is `TRUE`, it adds error bars to the points,
#' representing confidence intervals.
#'
#' @param plot_df A data frame containing the data to be used for plotting.
#' This data frame should contain columns 'term', 'estimate', 'type', and if
#' `conf.int` is `TRUE`, 'conf.low' and 'conf.high'.
#' @param conf.int Logical variable. If `TRUE`, add error bars to the points in
#' the plot, representing confidence intervals.
#'
#' @return A ggplot2 object of the stability assessment plot.
#'
#' @keywords internal
create_plot <- function(plot_df, conf.int) {
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = term, y = estimate, color = type)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Term", y = "Estimate", color = "Type") +
    ggplot2::theme_minimal()

  if (conf.int){
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2)
  }

  return(p)
}
