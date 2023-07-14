summary_replication_stability <- function(obj, conf.int, conf.level) {
  new_model <- broom::tidy(obj$new_model, conf.int, conf.level)
  boot_models <- handle_boot_model_summaries(obj$boot_models, conf.int, conf.level)
  list(new_model = new_model, boot_models = boot_models)
}

summary_statistical_stability <- function(obj, conf.int, conf.level){
  tidy_list_elements(obj, conf.int = conf.int, conf.level = conf.level)
}

summary_data_selection_stability <- function(obj, conf.int, conf.level){
  obj <- list(bootstrap_model = obj[["bootstrap_model"]],
              no_outlier_model = obj[["no_outlier_model"]],
              strata_boot_model1 = obj[["strata_boot_models"]][[1]],
              strata_boot_model2 = obj[["strata_boot_models"]][[2]],
              strata_boot_model3 = obj[["strata_boot_models"]][[3]])
  tidy_list_elements(obj, conf.int = conf.int, conf.level = conf.level)
}

summary_model_selection_stability <- function(obj){
  suppressWarnings(tidy_list_elements(obj))
}

summary_numerical_stability <- function(obj, conf.int, conf.level){
  broom::tidy(obj, conf.int, conf.level)
}

summary_analytic_and_algebraic_stability <- function(obj){
  return(obj)
}

summary_technique_stability <- function(obj, conf.int, conf.level){
  broom::tidy(obj, conf.int, conf.level)
}

# Helpers

# Applying broom::tidy on list elements
tidy_list_elements <- function(list_elements, conf.int, conf.level) {
  lapply(list_elements, broom::tidy, conf.int = conf.int, conf.level = conf.level)
}

# Function to handle the boot model summaries
# handle_boot_model_summaries <- function(boot_models) {
#   if (is.null(boot_models)) {
#     return(list(boot_mean = NULL, boot_sd = NULL))
#   }
#
#   boot_model_summaries <- lapply(boot_models, broom::tidy)
#   boot_stats <- lapply(boot_model_summaries, function(model_summary){
#     data.frame(
#       term = model_summary$term,
#       estimate = model_summary$estimate,
#       std_error = model_summary$std.error,
#       p_value = model_summary$p.value
#     )
#   })
#
#   boot_stats_df <- dplyr::bind_rows(boot_stats)
#
#   boot_mean_df <- boot_stats_df |>
#     dplyr::group_by(term) |>
#     dplyr::summarise(
#       estimate = mean(estimate, na.rm = TRUE),
#       std.error = mean(std_error, na.rm = TRUE),
#       prop_sig = mean(p_value < 0.05, na.rm = TRUE)
#     )
#
#   boot_sd_df <- boot_stats_df |>
#     dplyr::group_by(term) |>
#     dplyr::summarise(
#       estimate = stats::sd(estimate, na.rm = TRUE),
#       std.error = stats::sd(std_error, na.rm = TRUE)
#     )
#
#   return(list(boot_mean = boot_mean_df, boot_sd = boot_sd_df))
# }

# Function to handle the boot model summaries
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
      dplyr::select(estimate, std.error, prop_sig, conf.low, conf.high)
  }

  boot_sd_df <- boot_stats_df |>
    dplyr::group_by(term) |>
    dplyr::summarise(
      estimate = stats::sd(estimate, na.rm = TRUE),
      std.error = stats::sd(std_error, na.rm = TRUE)
    )

  return(list(boot_mean = boot_mean_df, boot_sd = boot_sd_df))
}
