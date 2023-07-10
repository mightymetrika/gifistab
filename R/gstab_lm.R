gstab_lm <- function(data, formula, new_data = NULL, nboot = NULL, variable_to_remove = NULL, variable_of_interest = NULL, ...) {

  # Run main model
  original_model <- stats::lm(formula, data = data, ...)

  # a) Replication Stability
  replication_stability = {
    new_model <- NULL
    boot_models <- NULL
    if (!is.null(new_data)) {
      new_model <- stats::lm(formula, data = new_data, ...)
    }

    if (!is.null(nboot)) {
      boot_func <- function(...) {
        bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
        stats::lm(formula, data = bootstrap_data, ...)
      }
      boot_models <- lapply(1:nboot, function(x) boot_func(...))
    }
    list(new_model = new_model, boot_models = boot_models)
  }

  # b) Statistical Stability
  statistical_stability = {
    noise <- stats::rnorm(nrow(data))
    noisy_data <- data
    response_variable <- all.vars(formula)[1]
    noisy_data[[response_variable]] <- noisy_data[[response_variable]] + noise
    noisy_model <- stats::lm(formula, data = noisy_data, ...)

    permuted_noise <- sample(noise)
    permuted_noisy_data <- data
    permuted_noisy_data[[response_variable]] <- permuted_noisy_data[[response_variable]] + permuted_noise
    permuted_noisy_model <- stats::lm(formula, data = permuted_noisy_data, ...)

    list(noisy_model = noisy_model, permuted_noisy_model = permuted_noisy_model)
  }

  # c) Stability under Data Selection
  stability_under_data_selection = {
    bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
    bootstrap_model <- stats::lm(formula, data = bootstrap_data, ...)

    no_outlier_data <- data[-which(robustbase::covMcd(data)$mah > stats::qchisq(0.975, ncol(data))), ]
    no_outlier_model <- stats::lm(formula, data = no_outlier_data, ...)

    list(bootstrap_model = bootstrap_model, no_outlier_model = no_outlier_model)
  }

  # d) Stability under Model Selection
  stability_under_model_selection = {
    has_intercept <- attr(stats::terms(original_model), "intercept")
    new_model <- if (has_intercept) stats::update(original_model, . ~ . - 1) else stats::update(original_model, . ~ . + 0)
    toggle_intercept <- lmtest::lrtest(original_model, new_model)

    remove_variable <- NULL
    if (!is.null(variable_to_remove)) {
      if (!variable_to_remove %in% colnames(data)) stop("Specified variable not found in the data.")
      new_model <- stats::update(original_model, stats::as.formula(paste(". ~ . -", variable_to_remove)))
      remove_variable <- lmtest::lrtest(original_model, new_model)
    }

    remove_least_useful <- NULL
    if (!is.null(variable_of_interest)) {
      full_model <- stats::lm(stats::as.formula(paste(variable_of_interest, "~ .")), data = data)
      new_model <- MASS::stepAIC(full_model, direction = "backward")
      remove_least_useful <- lmtest::lrtest(full_model, new_model)
    }

    list(toggle_intercept = toggle_intercept, remove_variable = remove_variable, remove_least_useful = remove_least_useful)
  }

  # e) Numerical Stability
  numerical_stability = stats::lm(formula, data = data + stats::rnorm(prod(dim(data))) * .Machine$double.eps^0.5, ...)

  # f) Analytic and Algebraic Stability
  kappa <- kappa(original_model)
  if (kappa > 30) warning("Severe multicollinearity detected")

  # g) Stability under Selection of Technique
  robust_regression = MASS::rlm(formula, data = data, ...)

  # Creating output
  out <- list(original = original_model,
              stability = list(replication_stability = replication_stability,
                               statistical_stability = statistical_stability,
                               stability_under_data_selection = stability_under_data_selection,
                               stability_under_model_selection = stability_under_model_selection,
                               numerical_stability = numerical_stability,
                               analytic_and_algebraic_stability = kappa,
                               stability_under_selection_of_technique = list(robust_regression = robust_regression)))

  # Class & return output
  class(out) <- c("gstab_lm", "gstab")
  return(out)
}

# Main summary function
summary.gstab_lm <- function(object, ...) {

  # Original model
  original_summary <- broom::tidy(object$original)

  # Replication stability
  replication_stability_summary <- list(
    new_model = broom::tidy(object$stability$replication_stability$new_model),
    boot_models = handle_boot_model_summaries(object$stability$replication_stability$boot_models)
  )

  # Statistical stability
  statistical_stability_summary <- tidy_list_elements(object$stability$statistical_stability)

  # Stability under data selection
  stability_under_data_selection_summary <- tidy_list_elements(object$stability$stability_under_data_selection)

  # Stability under model selection
  stability_under_model_selection_summary <- tidy_list_elements(object$stability$stability_under_model_selection)

  # Numerical stability
  numerical_stability_summary <- broom::tidy(object$stability$numerical_stability)

  # Analytic and algebraic stability
  analytic_and_algebraic_stability_summary <- object$stability$analytic_and_algebraic_stability

  # Stability under selection of technique
  stability_under_selection_of_technique_summary <- tidy_list_elements(object$stability$stability_under_selection_of_technique)

  # Combining all summaries
  summary_out <- list(
    original_summary = original_summary,
    replication_stability_summary = replication_stability_summary,
    statistical_stability_summary = statistical_stability_summary,
    stability_under_data_selection_summary = stability_under_data_selection_summary,
    stability_under_model_selection_summary = stability_under_model_selection_summary,
    numerical_stability_summary = numerical_stability_summary,
    analytic_and_algebraic_stability_summary = analytic_and_algebraic_stability_summary,
    stability_under_selection_of_technique_summary = stability_under_selection_of_technique_summary
  )

  return(summary_out)
}

# Helper functions

# Applying broom::tidy on list elements
tidy_list_elements <- function(list_elements) {
  lapply(list_elements, broom::tidy)
}

# Function to handle the boot model summaries
handle_boot_model_summaries <- function(boot_models) {
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
      estimate = mean(estimate, na.rm = TRUE),
      std.error = mean(std_error, na.rm = TRUE),
      prop_sig = mean(p_value < 0.05, na.rm = TRUE)
    )

  boot_sd_df <- boot_stats_df |>
    dplyr::group_by(term) |>
    dplyr::summarise(
      estimate = stats::sd(estimate, na.rm = TRUE),
      std.error = stats::sd(std_error, na.rm = TRUE)
    )

  return(list(boot_mean = boot_mean_df, boot_sd = boot_sd_df))
}
