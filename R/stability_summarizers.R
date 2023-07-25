#' Summarize Results of Replication Stability Assessment
#'
#' This function summarizes the results of a replication stability assessment.
#' It creates a tidy data frame for the model fitted on new data (if provided),
#' and summarizes the bootstrap models (if available).
#'
#' @param obj A list containing `new_model` fitted on new_data (if provided),
#' and `boot_models` which is a list of models fitted on bootstrap samples
#' (if nboot is provided).
#' @param conf.int Logical, if `TRUE` confidence intervals for the estimates are
#' returned.
#' @param conf.level The confidence level to use for the confidence intervals.
#'
#' @return A list with two elements:
#' - 'new_model': A tidy data frame summarizing the model fitted on new_data
#'    (if provided).
#' - 'boot_models': A list of summarized bootstrap models, each as a tidy data
#'    frame, (if bootstrap models were provided in 'obj').
#'
#' @keywords internal
summary_replication_stability <- function(obj, conf.int, conf.level) {
  new_model <- broom::tidy(obj$new_model, conf.int, conf.level)
  boot_models <- handle_boot_model_summaries(obj$boot_models, conf.int, conf.level)
  list(new_model = new_model, boot_models = boot_models)
}

#' Summarize Results of Statistical Stability Assessment
#'
#' This function summarizes the results of a statistical stability assessment.
#' It creates tidy data frames for the model fitted on data with added random
#' noise and data with permuted noise.
#'
#' @param obj A list containing `noisy_model` fitted on the data with added
#' random noise and `permuted_noisy_model` fitted on the data with permuted noise.
#' @param conf.int Logical, if `TRUE` confidence intervals for the estimates are
#' returned.
#' @param conf.level The confidence level to use for the confidence intervals.
#'
#' @return A list with two elements:
#' - 'noisy_model': A tidy data frame summarizing the model fitted on data with
#'    added random noise.
#' - 'permuted_noisy_model': A tidy data frame summarizing the model fitted on
#'    data with permuted noise.
#'
#' @keywords internal
summary_statistical_stability <- function(obj, conf.int, conf.level){
  tidy_list_elements(obj, conf.int = conf.int, conf.level = conf.level)
}

#' Summarize Results of Stability Under Data Selection Assessment
#'
#' This function summarizes the results of a stability under data selection
#' assessment. It creates tidy data frames for the models fitted on resampled data,
#' data with outliers removed, and stratified bootstrap data.
#'
#' @param obj A list containing `bootstrap_model` fitted on resampled data,
#' `no_outlier_model` fitted on data with outliers removed, and
#' `strata_boot_models` list of models fitted on stratified bootstrap data.
#' @param conf.int Logical, if `TRUE` confidence intervals for the estimates are
#' returned.
#' @param conf.level The confidence level to use for the confidence intervals.
#'
#' @return A list with five elements:
#' - 'bootstrap_model': A tidy data frame summarizing the model fitted on
#'    resampled data.
#' - 'no_outlier_model': A tidy data frame summarizing the model fitted on data
#'    with outliers removed.
#' - 'strata_boot_model1': A tidy data frame summarizing the first model fitted
#'    on stratified bootstrap data.
#' - 'strata_boot_model2': A tidy data frame summarizing the second model fitted
#'    on stratified bootstrap data.
#' - 'strata_boot_model3': A tidy data frame summarizing the third model fitted
#'    on stratified bootstrap data.
#'
#' @keywords internal
summary_data_selection_stability <- function(obj, conf.int, conf.level){
  obj <- list(bootstrap_model = obj[["bootstrap_model"]],
              no_outlier_model = obj[["no_outlier_model"]],
              strata_boot_model1 = obj[["strata_boot_models"]][[1]],
              strata_boot_model2 = obj[["strata_boot_models"]][[2]],
              strata_boot_model3 = obj[["strata_boot_models"]][[3]])
  tidy_list_elements(obj, conf.int = conf.int, conf.level = conf.level)
}

#' Summary Statistics for Stability Under Model Selection
#'
#' This function extracts and tidies the summary statistics for each model
#' produced by the `stability_under_model_selection` function. The summary
#' statistics include likelihood ratio test results for each model comparison:
#' toggling the intercept, removing a specific variable, and removing the least
#' useful variable.
#'
#' @param obj A list object returned by `stability_under_model_selection`.
#'
#' @return A list of tidy tibbles for each component of the input list. Each tibble
#' contains summary statistics for the respective likelihood ratio test. This
#' includes degrees of freedom, test statistic value, and p-value, among other
#' things. If any error occurs during the tidying process, a warning is suppressed,
#' and the raw (untidied) result is returned.
#'
#' @keywords internal
summary_model_selection_stability <- function(obj){
  suppressWarnings(tidy_list_elements(obj))
}

#' Summary Statistics for Numerical Stability
#'
#' This function provides summary statistics for the model fitted on perturbed
#' data obtained by the `numerical_stability` function. The statistics summarize
#' the effects of small perturbations in the numerical variables on the model fit,
#' thereby assessing the model's numerical stability.
#'
#' @param obj A fitted model object returned by the `numerical_stability` function.
#' @param conf.int Logical indicating whether to compute confidence intervals for
#' the parameter estimates.
#' @param conf.level The confidence level to use for the confidence interval if
#' `conf.int = TRUE`.
#'
#' @return A tibble summarizing the coefficients, standard errors, statistical
#' test results, and (optionally) confidence intervals for the coefficients of
#' the model. Each row corresponds to an individual term in the regression model's formula.
#'
#' @keywords internal
summary_numerical_stability <- function(obj, conf.int, conf.level){
  broom::tidy(obj, conf.int, conf.level)
}

#' Summary of Analytic and Algebraic Stability
#'
#' This function provides the summary for the analytic and algebraic stability
#' of a fitted model, calculated by the `analytic_and_algebraic_stability` function.
#'
#' @param obj A list object returned by the `analytic_and_algebraic_stability`
#' function, containing the L1 norm and Linf norm condition numbers of the model.
#'
#' @return The function directly returns the input argument `obj`, which is a list
#' containing the L1 norm and Linf norm condition numbers of the model.
#'
#' @keywords internal
summary_analytic_and_algebraic_stability <- function(obj){
  return(obj)
}

#' Summary of Stability Under Selection of Technique
#'
#' This function provides a summary of the stability under the selection of
#' technique of a fitted model. It generates the summary from a model object
#' obtained from the `stability_under_selection_of_technique` function.
#'
#' @param obj A fitted robust regression model object (`robustbase::lmrob` or
#' `robustbase::glmrob`), or NULL if the robust regression model could not be
#' fitted.
#' @param conf.int Logical indicating whether or not to compute confidence interval
#' around coefficients.
#' @param conf.level Numeric scalar indicating the confidence level to be used
#' for confidence interval calculation.
#'
#' @return A tidy data frame summarizing the robust regression model, or NULL
#' if an error occurs during tidying.
#'
#' @keywords internal
summary_technique_stability <- function(obj, conf.int, conf.level){
  tryCatch({
    broom::tidy(obj, conf.int, conf.level)
  }, error = function(e) {
    message("Error in tidying model: ", e$message)
    return(NULL)
  })
}

# Helpers

#' Apply broom::tidy on List Elements
#'
#' This function applies the `broom::tidy` function to each element of a list,
#' extracting relevant statistics for each model object in the list. If the
#' `broom::tidy` operation fails on a model object, the function catches the
#' error, prints a warning, and returns `NULL` for that particular model object.
#'
#' @param list_elements A list of model objects for which to extract statistics.
#' @param conf.int Logical, if `TRUE` confidence intervals for the estimates are
#' returned.
#' @param conf.level The confidence level to use for the confidence intervals.
#'
#' @return A list of tibbles with model statistics, same length as `list_elements`.
#' If `broom::tidy` operation fails on a model object, the corresponding list
#' element will be `NULL`.
#'
#' @keywords internal
tidy_list_elements <- function(list_elements, conf.int, conf.level) {
  lapply(list_elements, function(x) {
    tryCatch(
      broom::tidy(x, conf.int = conf.int, conf.level = conf.level),
      error = function(e) {
        warning("Error in broom::tidy: ", e$message)
        NULL
      }
    )
  })
}

#' Handle Bootstrap Model Summaries
#'
#' This function computes and returns summary statistics for a list of
#' bootstrapped model objects. If the input list is `NULL`, the function returns
#' a list with two `NULL` elements.
#'
#' @param boot_models A list of bootstrapped model objects.
#' @param conf.int Logical, if `TRUE` confidence intervals for the estimates are
#' returned.
#' @param conf.level The confidence level to use for the confidence intervals.
#'
#' @return A list with two elements:
#' - 'boot_mean': A data frame with the mean values for each model term, including
#'   the estimate, standard error, proportion of significant p-values, and if
#'   `conf.int` is `TRUE`, the confidence interval for the estimate.
#' - 'boot_sd': A data frame with the standard deviations of the estimate and
#'   standard error for each model term.
#'
#' @keywords internal
handle_boot_model_summaries <- function(boot_models, conf.int, conf.level) {
  if (is.null(boot_models)) {
    return(list(boot_mean = NULL, boot_sd = NULL))
  }

  boot_model_summaries <- lapply(boot_models, broom::tidy)
  boot_stats <- lapply(boot_model_summaries, function(model_summary){
    data.frame(
      term = model_summary$term,
      estimate = model_summary$estimate,
      std_error = model_summary$std.error,
      p_value = model_summary$p.value
    )
  })

  boot_stats_df <- dplyr::bind_rows(boot_stats)

  boot_mean_df <- boot_stats_df |>
    dplyr::group_by(term) |>
    dplyr::summarise(
      conf.low = if (conf.int) stats::quantile(estimate, probs = (1 - conf.level) / 2, na.rm = TRUE) else NULL,
      conf.high = if (conf.int) stats::quantile(estimate, probs = 1 - (1 - conf.level) / 2, na.rm = TRUE) else NULL,
      estimate = mean(estimate, na.rm = TRUE),
      std.error = mean(std_error, na.rm = TRUE),
      prop_sig = mean(p_value < 0.05, na.rm = TRUE)
    )

  if (conf.int){
    boot_mean_df <- boot_mean_df |>
      dplyr::select(term, estimate, std.error, prop_sig, conf.low, conf.high)
  }

  boot_sd_df <- boot_stats_df |>
    dplyr::group_by(term) |>
    dplyr::summarise(
      estimate = stats::sd(estimate, na.rm = TRUE),
      std.error = stats::sd(std_error, na.rm = TRUE)
    )

  return(list(boot_mean = boot_mean_df, boot_sd = boot_sd_df))
}
