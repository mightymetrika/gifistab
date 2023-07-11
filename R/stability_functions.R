# Helper function to determine the fitting model
fit_model_func <- function(model) {
  if ("lm" %in% class(model)[[1]]) {
    return(stats::lm)
  } else {
    return(stats::glm)
  }
}

replication_stability <- function(model, data, formula, new_data = NULL, nboot = NULL, ...) {
  new_model <- NULL
  boot_models <- NULL
  fit_model <- fit_model_func(model)

  if (!is.null(new_data)) {
    new_model <- fit_model(formula, data = new_data, ...)
  }

  if (!is.null(nboot)) {
    boot_func <- function(...) {
      bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
      fit_model(formula, data = bootstrap_data, ...)
    }
    boot_models <- lapply(1:nboot, function(x) boot_func(...))
  }

  list(new_model = new_model, boot_models = boot_models)
}

statistical_stability <- function(model, data, formula, ...) {
  noise <- stats::rnorm(nrow(data))
  noisy_data <- data
  response_variable <- all.vars(formula)[1]
  noisy_data[[response_variable]] <- noisy_data[[response_variable]] + noise
  fit_model <- fit_model_func(model)
  noisy_model <- fit_model(formula, data = noisy_data, ...)

  permuted_noise <- sample(noise)
  permuted_noisy_data <- data
  permuted_noisy_data[[response_variable]] <- permuted_noisy_data[[response_variable]] + permuted_noise
  permuted_noisy_model <- fit_model(formula, data = permuted_noisy_data, ...)

  list(noisy_model = noisy_model, permuted_noisy_model = permuted_noisy_model)
}

stability_under_data_selection <- function(model, data, formula, ...) {
  bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
  fit_model <- fit_model_func(model)
  bootstrap_model <- fit_model(formula, data = bootstrap_data, ...)

  no_outlier_model <- NULL

  # Outlier detection based on robust multivariate Mahalanobis distances
  tryCatch({
    outlier_indices <- which(robustbase::covMcd(data)$mah > stats::qchisq(0.975, ncol(data)))
    no_outlier_data <- data[-outlier_indices, ]
    if (nrow(no_outlier_data) > 0) {
      no_outlier_model <- fit_model(formula, data = no_outlier_data, ...)
    }
  }, error = function(e) {
    # Fallback: remove data points that are more than 3 standard deviations away from the mean
    is_outlier <- apply(data, 2, function(x) abs(scale(x)) > 3)
    no_outlier_data <- data[!rowSums(is_outlier), ]
    if (nrow(no_outlier_data) > 0) {
      no_outlier_model <- fit_model(formula, data = no_outlier_data, ...)
    }
  })

  list(bootstrap_model = bootstrap_model, no_outlier_model = no_outlier_model)
}

stability_under_model_selection <- function(model, data, formula, variable_to_remove = NULL, variable_of_interest = NULL, ...) {
  has_intercept <- attr(stats::terms(model), "intercept")
  fit_model <- fit_model_func(model)
  new_model <- if (has_intercept) stats::update(model, . ~ . - 1) else stats::update(model, . ~ . + 0)
  toggle_intercept <- if ("lm" %in% class(model)[[1]]) lmtest::lrtest(model, new_model) else NULL

  remove_variable <- NULL
  if (!is.null(variable_to_remove)) {
    if (!variable_to_remove %in% colnames(data)) stop("Specified variable not found in the data.")
    new_model <- stats::update(model, stats::as.formula(paste(". ~ . -", variable_to_remove)))
    remove_variable <- if ("lm" %in% class(model)[[1]]) lmtest::lrtest(model, new_model) else NULL
  }

  remove_least_useful <- NULL
  if (!is.null(variable_of_interest)) {
    full_model <- fit_model(stats::as.formula(paste(variable_of_interest, "~ .")), data = data)
    new_model <- MASS::stepAIC(full_model, direction = "backward")
    remove_least_useful <- if ("lm" %in% class(model)[[1]]) lmtest::lrtest(full_model, new_model) else NULL
  }

  list(toggle_intercept = toggle_intercept, remove_variable = remove_variable, remove_least_useful = remove_least_useful)
}

numerical_stability <- function(model, data, formula, ...) {
  fit_model <- fit_model_func(model)
  fit_model(formula, data = data + stats::rnorm(prod(dim(data))) * .Machine$double.eps^0.5, ...)
}


analytic_and_algebraic_stability <- function(model, ...) {
  kappa <- kappa(model)
  if (kappa > 30) warning("Severe multicollinearity detected")
  return(kappa)
}

stability_under_selection_of_technique <- function(model, data, formula, ...) {
  if ("lm" %in% class(model)[[1]]) {
    robust_regression = MASS::rlm(formula, data = data, ...)
  } else {
    # Here, one could use some robust alternative to GLMs. I'll leave it as NULL for now.
    robust_regression = NULL
  }
  robust_regression
}
