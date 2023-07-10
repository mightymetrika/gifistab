replication_stability <- function(model, data, formula, new_data = NULL, nboot = NULL, ...) {
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

  out = list(new_model = new_model, boot_models = boot_models)

  class(out) <- "rep_stab"

  return(out)
}

statistical_stability <- function(model, data, formula, ...) {
  noise <- stats::rnorm(nrow(data))
  noisy_data <- data
  response_variable <- all.vars(formula)[1]
  noisy_data[[response_variable]] <- noisy_data[[response_variable]] + noise
  noisy_model <- stats::lm(formula, data = noisy_data, ...)

  permuted_noise <- sample(noise)
  permuted_noisy_data <- data
  permuted_noisy_data[[response_variable]] <- permuted_noisy_data[[response_variable]] + permuted_noise
  permuted_noisy_model <- stats::lm(formula, data = permuted_noisy_data, ...)

  out = list(noisy_model = noisy_model, permuted_noisy_model = permuted_noisy_model)

  class(out) <- "stat_stab"

  return(out)
}

stability_under_data_selection <- function(model, data, formula, ...) {
  bootstrap_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
  bootstrap_model <- stats::lm(formula, data = bootstrap_data, ...)

  no_outlier_data <- data[-which(robustbase::covMcd(data)$mah > stats::qchisq(0.975, ncol(data))), ]
  no_outlier_model <- stats::lm(formula, data = no_outlier_data, ...)

  out <- list(bootstrap_model = bootstrap_model, no_outlier_model = no_outlier_model)

  class(out) <- "dsel_stab"

  return(out)
}

stability_under_model_selection <- function(model, data, formula, variable_to_remove = NULL, variable_of_interest = NULL, ...) {
  has_intercept <- attr(stats::terms(model), "intercept")
  new_model <- if (has_intercept) stats::update(model, . ~ . - 1) else stats::update(model, . ~ . + 0)
  toggle_intercept <- lmtest::lrtest(model, new_model)

  remove_variable <- NULL
  if (!is.null(variable_to_remove)) {
    if (!variable_to_remove %in% colnames(data)) stop("Specified variable not found in the data.")
    new_model <- stats::update(model, stats::as.formula(paste(". ~ . -", variable_to_remove)))
    remove_variable <- lmtest::lrtest(model, new_model)
  }

  remove_least_useful <- NULL
  if (!is.null(variable_of_interest)) {
    full_model <- stats::lm(stats::as.formula(paste(variable_of_interest, "~ .")), data = data)
    new_model <- MASS::stepAIC(full_model, direction = "backward")
    remove_least_useful <- lmtest::lrtest(full_model, new_model)
  }

  out <- list(toggle_intercept = toggle_intercept, remove_variable = remove_variable, remove_least_useful = remove_least_useful)

  class(out) <- "msel_stab"

  return(out)
}

numerical_stability <- function(model, data, formula, ...) {
  out <- stats::lm(formula, data = data + stats::rnorm(prod(dim(data))) * .Machine$double.eps^0.5, ...)

  class(out) <- "num_stab"

  return(out)
}

analytic_and_algebraic_stability <- function(model, ...) {
  kappa <- kappa(model)
  if (kappa > 30) warning("Severe multicollinearity detected")

  out <- kappa

  class(out) <- "aa_stab"

  return(out)
}

stability_under_selection_of_technique <- function(model, data, formula, ...) {
  robust_regression = MASS::rlm(formula, data = data, ...)

  out <- robust_regression

  class(out) <- "tsel_stab"

  return(out)
}
