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

  strata <- sample(rep(1:3, nrow(data) / 3))
  strata_boot_models <- lapply(1:3, function(x) {
    data_subset <- data[strata == x, ]
    boot_subset <- data_subset[sample(nrow(data_subset), nrow(data), replace = TRUE), ]
    fit_model(formula, data = boot_subset, ...)
  })

  list(bootstrap_model = bootstrap_model,
       no_outlier_model = no_outlier_model,
       strata_boot_models = strata_boot_models)
}

stability_under_model_selection <- function(model, data, formula, variable_to_remove = NULL, variable_of_interest = NULL, ...) {
  has_intercept <- attr(stats::terms(model), "intercept")
  fit_model <- fit_model_func(model)
  new_model <- if (has_intercept) stats::update(model, . ~ . - 1) else stats::update(model, . ~ . + 0)
  toggle_intercept <- lmtest::lrtest(model, new_model)

  remove_variable <- NULL
  if (!is.null(variable_to_remove)) {

    # If variable_to_remove is an interaction, just remove the interaction
    if (grepl(":", variable_to_remove)) {
      new_formula <- stats::as.formula(paste(". ~ . -", variable_to_remove))
      new_model <- stats::update(model, new_formula)
      remove_variable <- lmtest::lrtest(model, new_model)
    } else {
      if (!variable_to_remove %in% colnames(data)) stop("Specified variable not found in the data.")

      # Extract all variables in the formula
      all_vars <- all.vars(formula)

      # Find all variables and interactions that include variable_to_remove
      vars_to_remove <- all_vars[grepl(paste0("\\b", variable_to_remove, "\\b"), all_vars)]

      # Create a new formula without those variables
      new_formula <- stats::as.formula(paste(". ~ . -", paste(vars_to_remove, collapse = " - ")))

      # Update the model
      new_model <- stats::update(model, new_formula)

      # Perform likelihood ratio test
      remove_variable <- lmtest::lrtest(model, new_model)
    }
  }

  remove_least_useful <- NULL
  if (!is.null(variable_of_interest)) {

    if (grepl(":", variable_of_interest)) {
      variable_of_interest <- unlist(strsplit(variable_of_interest, ":"))
    }

    # Ensure variable_of_interest is in the model
    if (!all(variable_of_interest %in% all.vars(formula))) {
      stop(paste("One or more variables of interest", paste(variable_of_interest, collapse = ", "), "are not in the model."))
    }

    # Expand any shorthand interactions in the formula
    formula <- stats::reformulate(labels(stats::terms(formula)), formula[[2]])

    # Find all variables and interactions that include any variable in variable_of_interest
    vars_to_keep <- all.vars(formula)[grepl(paste0("\\b(", paste(variable_of_interest, collapse = "|"), ")\\b"), all.vars(formula))]

    # Find all unique variables involved in these terms
    unique_vars <- unique(unlist(strsplit(vars_to_keep, ":|\\+")))

    # The "lower" model includes the variable of interest, its interactions,
    # and the variables involved in these interactions
    lower_formula <- stats::as.formula(paste(formula[[2]], "~", paste(unique_vars, collapse = " + ")))

    # The full model includes all variables
    full_model <- fit_model(formula, data = data)

    # Perform backward selection, but keep the lower model in the scope
    new_model <- MASS::stepAIC(full_model, direction = "backward", scope = list(lower = lower_formula, upper = formula))

    # Perform likelihood ratio test
    remove_least_useful <- lmtest::lrtest(full_model, new_model)
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
  robust_regression <- NULL

  tryCatch({
    if ("lm" %in% class(model)[[1]]) {
      robust_regression = robustbase::lmrob(formula, data = data, ...)
    } else if ("glm" %in% class(model)[[1]]) {
      robust_regression = robustbase::glmrob(formula, data = data, ...)
    }
  }, error = function(e) {
    message("Error in fitting robust regression: ", e$message)
  })

  robust_regression
}
