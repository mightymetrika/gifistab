gstab_lm <- function(data, formula, new_data = NULL, nboot = NULL, variable_to_remove = NULL, variable_of_interest = NULL, ...) {

  original_model <- stats::lm(formula, data = data, ...)

  stability <- list(

    # a) Replication Stability
    replication_stability = {
      if (!is.null(new_data)) {
        # Fit model on new data
        new_model <- stats::lm(formula, data = new_data, ...)
      } else {
        new_model <- NULL
      }

      if (!is.null(nboot)) {
        # Bootstrap resampling
        # boot_models <- replicate(nboot, function(...) {
        #   bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
        #   stats::lm(formula, data = bootstrap_data, ...)
        # }, simplify = FALSE)
        # Bootstrap resampling
        boot_func <- function(...) {
          bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
          stats::lm(formula, data = bootstrap_data, ...)
        }
        boot_models <- sapply(1:nboot, function(x) boot_func(...), simplify = FALSE)
      } else {
        boot_models <- NULL
      }


      list(
        new_model = new_model,
        boot_models = boot_models
      )
    },

    # b) Statistical Stability
    statistical_stability = {
      # Add random noise to the response variable
      noise <- stats::rnorm(nrow(data))
      noisy_data <- data
      response_variable <- all.vars(formula)[1]
      noisy_data[[response_variable]] <- noisy_data[[response_variable]] + noise
      noisy_model <- stats::lm(formula, data = noisy_data, ...)

      # Permute the noise and add it to the response variable
      permuted_noise <- sample(noise)
      permuted_noisy_data <- data
      permuted_noisy_data[[response_variable]] <- permuted_noisy_data[[response_variable]] + permuted_noise
      permuted_noisy_model <- stats::lm(formula, data = permuted_noisy_data, ...)

      list(noisy_model = noisy_model, permuted_noisy_model = permuted_noisy_model)
    },

    # c) Stability under Data Selection
    stability_under_data_selection = {
      # Bootstrap resampling
      bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
      bootstrap_model <- stats::lm(formula, data = bootstrap_data, ...)
      # Removing outliers
      no_outlier_data <- data[-which(robustbase::covMcd(data)$mah > stats::qchisq(0.975, ncol(data))), ]
      no_outlier_model <- stats::lm(formula, data = no_outlier_data, ...)
      list(bootstrap_model = bootstrap_model, no_outlier_model = no_outlier_model)
    },

    # d) Stability under Model Selection
    stability_under_model_selection = {
      list(
        toggle_intercept = {
          has_intercept <- attr(stats::terms(original_model), "intercept")
          new_model <- if (has_intercept) stats::update(original_model, . ~ . - 1) else stats::update(original_model, . ~ . + 0)
          lmtest::lrtest(original_model, new_model)
        },

        remove_variable = {
          if (!is.null(variable_to_remove)) {
            if (!variable_to_remove %in% colnames(data)) stop("Specified variable not found in the data.")
            new_model <- stats::update(original_model, stats::as.formula(paste(". ~ . -", variable_to_remove)))
            lmtest::lrtest(original_model, new_model)
          } else NULL
        },

        remove_least_useful = {
          if (!is.null(variable_of_interest)) {
            full_model <- stats::lm(stats::as.formula(paste(variable_of_interest, "~ .")), data = data)
            new_model <- MASS::stepAIC(full_model, direction = "backward")
            lmtest::lrtest(full_model, new_model)
          } else NULL
        })
    },

    # e) Numerical Stability
    numerical_stability = {
      # Perturbing data
      perturbed_data <- data + stats::rnorm(prod(dim(data))) * .Machine$double.eps^0.5
      stats::lm(formula, data = perturbed_data, ...)
    },

    # f) Analytic and Algebraic Stability
    analytic_and_algebraic_stability = {
      # In the context of linear models, this often refers to multicollinearity
      kappa <- kappa(original_model)
      if (kappa > 30) warning("Severe multicollinearity detected")
      kappa
    },

    # g) Stability under Selection of Technique
    stability_under_selection_of_technique = {
      list(
        robust_regression = {
          MASS::rlm(formula, data = data, ...) # Robust linear regression
        }
      )
    }
  )

  out <- list(original = original_model,
              stability = stability)

  class(out) <- c("gstab_lm", "gstab")

  return(out)
}

summary.gstab_lm <- function(object, ...) {

  # Original model
  original_summary <- broom::tidy(object$original)

  # Replication stability
  replication_stability_summary <- list(
    new_model = if (!is.null(object$stability$replication_stability$new_model)) broom::tidy(object$stability$replication_stability$new_model) else NULL,
    boot_models = if (!is.null(object$stability$replication_stability$boot_models)) {
      boot_model_summaries <- lapply(object$stability$replication_stability$boot_models, broom::tidy)
      boot_coefs <- lapply(boot_model_summaries, function(x) x$estimate)
      boot_se <- lapply(boot_model_summaries, function(x) x$std.error)
      boot_pvals <- lapply(boot_model_summaries, function(x) x$p.value)
      boot_sd_coefs <- sapply(boot_coefs, function(x) stats::sd(x, na.rm = TRUE))
      boot_sd_se <- sapply(boot_se, function(x) stats::sd(x, na.rm = TRUE))
      boot_mean_coefs <- sapply(boot_coefs, mean, na.rm = TRUE)
      boot_mean_se <- sapply(boot_se, mean, na.rm = TRUE)

      # Proportion of p-values below 0.05
      boot_prop_sig <- sapply(boot_pvals, function(x) mean(x < 0.05, na.rm = TRUE))

      # Convert to data frames
      boot_mean_df <- data.frame(term = names(boot_mean_coefs), estimate = boot_mean_coefs, std.error = boot_mean_se, prop_sig = boot_prop_sig)
      boot_sd_df <- data.frame(term = names(boot_sd_coefs), estimate = boot_sd_coefs, std.error = boot_sd_se)

      list(
        boot_mean = boot_mean_df,
        boot_sd = boot_sd_df
      )
    } else NULL
  )

  # Statistical stability
  statistical_stability_summary <- list(
    noisy_model = broom::tidy(object$stability$statistical_stability$noisy_model),
    permuted_noisy_model = broom::tidy(object$stability$statistical_stability$permuted_noisy_model)
  )

  # Stability under data selection
  stability_under_data_selection_summary <- list(
    bootstrap_model = broom::tidy(object$stability$stability_under_data_selection$bootstrap_model),
    no_outlier_model = broom::tidy(object$stability$stability_under_data_selection$no_outlier_model)
  )

  # Stability under model selection
  stability_under_model_selection_summary <- list(
    toggle_intercept = object$stability$stability_under_model_selection$toggle_intercept,
    remove_variable = if(!is.null(object$stability$stability_under_model_selection$remove_variable)){
      object$stability$stability_under_model_selection$remove_variable} else {NULL},
    remove_least_useful = if(!is.null(object$stability$stability_under_model_selection$remove_least_useful)){
      object$stability$stability_under_model_selection$remove_least_useful} else{NULL}
  )

  # Numerical stability
  numerical_stability_summary <- broom::tidy(object$stability$numerical_stability)

  # Analytic and algebraic stability
  analytic_and_algebraic_stability_summary <- object$stability$analytic_and_algebraic_stability

  # Stability under selection of technique
  stability_under_selection_of_technique_summary <- list(
    robust_regression = broom::tidy(object$stability$stability_under_selection_of_technique$robust_regression)
  )

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

# gstab_lm <- function(data, formula, new_data = NULL, nboot = NULL, variable_to_remove = NULL, variable_of_interest = NULL, ...) {
#   original_model <- stats::lm(formula, data = data, ...)
#
#   # a) Replication Stability
#   # replication_stability = {
#   #   new_model <- NULL
#   #   boot_models <- NULL
#   #   if (!is.null(new_data)) {
#   #     # Fit model on new data
#   #     new_model <- stats::lm(formula, data = new_data, ...)
#   #   }
#   #
#   #   if (!is.null(nboot)) {
#   #     # Bootstrap resampling
#   #     boot_models <- replicate(nboot, function(...) {
#   #       bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
#   #       stats::lm(formula, data = bootstrap_data, ...)
#   #     }, simplify = FALSE)
#   #   }
#   #   list(new_model = new_model, boot_models = boot_models)
#   # }
#
#   replication_stability = {
#     new_model <- NULL
#     boot_models <- NULL
#     if (!is.null(new_data)) {
#       # Fit model on new data
#       new_model <- stats::lm(formula, data = new_data, ...)
#     }
#
#     if (!is.null(nboot)) {
#       # Bootstrap resampling
#       boot_func <- function(...) {
#         bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
#         stats::lm(formula, data = bootstrap_data, ...)
#       }
#       boot_models <- lapply(1:nboot, function(x) boot_func(...))
#     }
#     list(new_model = new_model, boot_models = boot_models)
#   }
#   # b) Statistical Stability
#   statistical_stability = {
#     # Add random noise to the response variable
#     noise <- stats::rnorm(nrow(data))
#     noisy_data <- data
#     response_variable <- all.vars(formula)[1]
#     noisy_data[[response_variable]] <- noisy_data[[response_variable]] + noise
#     noisy_model <- stats::lm(formula, data = noisy_data, ...)
#
#     # Permute the noise and add it to the response variable
#     permuted_noise <- sample(noise)
#     permuted_noisy_data <- data
#     permuted_noisy_data[[response_variable]] <- permuted_noisy_data[[response_variable]] + permuted_noise
#     permuted_noisy_model <- stats::lm(formula, data = permuted_noisy_data, ...)
#
#     list(noisy_model = noisy_model, permuted_noisy_model = permuted_noisy_model)
#   }
#
#   # c) Stability under Data Selection
#   stability_under_data_selection = {
#     # Bootstrap resampling
#     bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
#     bootstrap_model <- stats::lm(formula, data = bootstrap_data, ...)
#
#     # Removing outliers
#     no_outlier_data <- data[-which(robustbase::covMcd(data)$mah > stats::qchisq(0.975, ncol(data))), ]
#     no_outlier_model <- stats::lm(formula, data = no_outlier_data, ...)
#
#     list(bootstrap_model = bootstrap_model, no_outlier_model = no_outlier_model)
#   }
#
#   # d) Stability under Model Selection
#   stability_under_model_selection = {
#     has_intercept <- attr(stats::terms(original_model), "intercept")
#     new_model <- if (has_intercept) stats::update(original_model, . ~ . - 1) else stats::update(original_model, . ~ . + 0)
#     toggle_intercept <- lmtest::lrtest(original_model, new_model)
#
#     remove_variable <- NULL
#     if (!is.null(variable_to_remove)) {
#       if (!variable_to_remove %in% colnames(data)) stop("Specified variable not found in the data.")
#       new_model <- stats::update(original_model, stats::as.formula(paste(". ~ . -", variable_to_remove)))
#       remove_variable <- lmtest::lrtest(original_model, new_model)
#     }
#
#     remove_least_useful <- NULL
#     if (!is.null(variable_of_interest)) {
#       full_model <- stats::lm(stats::as.formula(paste(variable_of_interest, "~ .")), data = data)
#       new_model <- MASS::stepAIC(full_model, direction = "backward")
#       remove_least_useful <- lmtest::lrtest(full_model, new_model)
#     }
#
#     list(toggle_intercept = toggle_intercept, remove_variable = remove_variable, remove_least_useful = remove_least_useful)
#   }
#
#   # e) Numerical Stability
#   numerical_stability = stats::lm(formula, data = data + stats::rnorm(prod(dim(data))) * .Machine$double.eps^0.5, ...)
#
#   # f) Analytic and Algebraic Stability
#   kappa <- kappa(original_model)
#   if (kappa > 30) warning("Severe multicollinearity detected")
#
#   # g) Stability under Selection of Technique
#   robust_regression = MASS::rlm(formula, data = data, ...)
#
#   # Creating output
#   out <- list(original = original_model,
#               stability = list(replication_stability = replication_stability,
#                                statistical_stability = statistical_stability,
#                                stability_under_data_selection = stability_under_data_selection,
#                                stability_under_model_selection = stability_under_model_selection,
#                                numerical_stability = numerical_stability,
#                                analytic_and_algebraic_stability = kappa,
#                                stability_under_selection_of_technique = list(robust_regression = robust_regression)))
#
#   class(out) <- c("gstab_lm", "gstab")
#   return(out)
# }

# summary.gstab_lm <- function(object, ...) {
#   # helper function to apply broom::tidy to elements of list
#   tidy_list_elements <- function(list_elements) {
#     if (is.null(list_elements)) return(NULL)
#     lapply(list_elements, broom::tidy)
#   }
#
#   # Original model
#   original_summary <- broom::tidy(object$original)
#
#   # Replication stability
#   if(!is.null(object$stability$replication_stability$boot_models)) {
#     boot_model_summaries <- lapply(object$stability$replication_stability$boot_models, broom::tidy)
#     boot_stats <- lapply(boot_model_summaries, `[[`, c("estimate", "std.error", "p.value"))
#     boot_stats_df <- do.call(data.frame, boot_stats)
#     boot_sd_coefs <- apply(boot_stats_df$estimate, 2, stats::sd, na.rm = TRUE)
#     boot_sd_se <- apply(boot_stats_df$std.error, 2, stats::sd, na.rm = TRUE)
#     boot_mean_coefs <- apply(boot_stats_df$estimate, 2, mean, na.rm = TRUE)
#     boot_mean_se <- apply(boot_stats_df$std.error, 2, mean, na.rm = TRUE)
#     boot_prop_sig <- apply(boot_stats_df$p.value, 2, function(x) mean(x < 0.05, na.rm = TRUE))
#     boot_mean_df <- data.frame(term = names(boot_mean_coefs), estimate = boot_mean_coefs, std.error = boot_mean_se, prop_sig = boot_prop_sig)
#     boot_sd_df <- data.frame(term = names(boot_sd_coefs), estimate = boot_sd_coefs, std.error = boot_sd_se)
#   }
#   replication_stability_summary <- list(
#     new_model = tidy_list_elements(object$stability$replication_stability$new_model),
#     boot_models = list(boot_mean = boot_mean_df, boot_sd = boot_sd_df)
#   )
#
#   # Statistical stability
#   statistical_stability_summary <- tidy_list_elements(object$stability$statistical_stability)
#
#   # Stability under data selection
#   stability_under_data_selection_summary <- tidy_list_elements(object$stability$stability_under_data_selection)
#
#   # Stability under model selection
#   stability_under_model_selection_summary <- tidy_list_elements(object$stability$stability_under_model_selection)
#
#   # Numerical stability
#   numerical_stability_summary <- broom::tidy(object$stability$numerical_stability)
#
#   # Analytic and algebraic stability
#   analytic_and_algebraic_stability_summary <- object$stability$analytic_and_algebraic_stability
#
#   # Stability under selection of technique
#   stability_under_selection_of_technique_summary <- tidy_list_elements(object$stability$stability_under_selection_of_technique)
#
#   # Combining all summaries
#   summary_out <- list(
#     original_summary = original_summary,
#     replication_stability_summary = replication_stability_summary,
#     statistical_stability_summary = statistical_stability_summary,
#     stability_under_data_selection_summary = stability_under_data_selection_summary,
#     stability_under_model_selection_summary = stability_under_model_selection_summary,
#     numerical_stability_summary = numerical_stability_summary,
#     analytic_and_algebraic_stability_summary = analytic_and_algebraic_stability_summary,
#     stability_under_selection_of_technique_summary = stability_under_selection_of_technique_summary
#   )
#
#   return(summary_out)
# }
