#' Determine the Engine Used for Model Fitting
#'
#' This function checks the class of the input model and returns the appropriate
#' engine function (`stats::lm` or `stats::glm`), used for model fitting. This function is
#' utilized as a helper in various stability assessments.
#'
#' @param model A fitted model object, either of class `lm` or `glm`.
#'
#' @return Function (`stats::lm` or `stats::glm`) corresponding to the input model's class.
#'
#' @keywords internal
fit_model_func <- function(model) {
  if ("lm" %in% class(model)[[1]]) {
    return(stats::lm)
  } else {
    return(stats::glm)
  }
}

#' Perform Replication Stability Assessment
#'
#' This function implements the replication stability assessment. It fits the
#' original model to a new data set (if provided) and performs bootstrap resampling
#' (if specified) to assess the stability of the model across different samples.
#'
#' @param model A fitted model object, either of class `lm` or `glm`.
#' @param data A data frame containing the data used for model fitting.
#' @param formula A formula describing the model to be fitted.
#' @param new_data An optional data frame to be used for replication stability
#' assessment. If provided, the model will be fitted on this new data set.
#' @param nboot An optional integer specifying the number of bootstrap resamples
#' to use for replication stability assessment.
#' @param ... Additional arguments to be passed to the `fit_model` function.
#'
#' @return A list containing `new_model` fitted on new_data (if provided),
#' and `boot_models` which is a list of models fitted on bootstrap samples
#' (if nboot is provided).
#'
#' @keywords internal
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

#' Perform Statistical Stability Assessment
#'
#' This function implements the statistical stability assessment. It fits the
#' original model to the data with added random noise and with permuted noise, thereby
#' assessing the model's sensitivity to random variations in the data.
#'
#' @param model A fitted model object, either of class `lm` or `glm`.
#' @param data A data frame containing the data used for model fitting.
#' @param formula A formula describing the model to be fitted.
#' @param nf Noise factor as a percentage. The amount of noise added to the
#' response variable will be nf multiplied by the response variable's standard
#' deviation after transformation or the percentage of response values to flip
#' (i.e, change 0 to 1 or 1 to 0) when the family is binomial or quasi binomial
#' and the response variable is binary.
#' @param ... Additional arguments to be passed to the `fit_model` function.
#'
#' @return A list containing `noisy_model` fitted on the data with added random noise
#' and `permuted_noisy_model` fitted on the data with added permuted noise.
#'
#' @keywords internal
statistical_stability <- function(model, data, formula, nf = 0.05, ...) {
  family <- family(model)$family
  response_variable <- all.vars(formula)[1]

  sf_flag <- ifelse(all(data[[response_variable]] %in% c(0, 1)), 1, 0)

  if (family == "gaussian") {
    response_transform <- function(x) x  # identity transformation
    inverse_transform <- function(x) x  # inverse of identity is identity
  } else if (family == "gamma" || family == "inverse.gaussian") {
    response_transform <- function(x) log(x)  # log transformation
    inverse_transform <- function(x) exp(x)  # inverse of log
  } else if (family == "poisson" || family == "quasipoisson") {
    response_transform <- function(x) log(x + 1)  # log transformation
    inverse_transform <- function(x) pmax(0, round(exp(x) - 1))  # inverse of log, rounded to nearest integer and capped at 0
  } else if (family == "binomial" || family == "quasibinomial") {
    if (sf_flag == 1) {
      flip_fraction <- nf  # adjust this as needed
      flip_indices <- sample(nrow(data), size = round(nrow(data) * flip_fraction))
      noise <- rep(0, nrow(data))
      noise[flip_indices] <- 1
      noisy_response <- abs(data[[response_variable]] - noise)
    } else {
      response_transform <- function(x) log(x / (1 - x))  # logit transformation
      inverse_transform <- function(x) 1 / (1 + exp(-x))  # inverse of logit
    }
  } else {
    stop(paste("The", family, "family is not supported."))
  }

  if (sf_flag == 0) {
    transformed_response <- response_transform(data[[response_variable]])
    noise_sd <- stats::sd(transformed_response) * nf  # standard deviation of the transformed response variable times noise factor
    noise <- stats::rnorm(nrow(data), sd = noise_sd)
    noisy_response <- inverse_transform(transformed_response + noise)
  }

  noisy_data <- data
  noisy_data[[response_variable]] <- noisy_response

  fit_model <- fit_model_func(model)
  noisy_model <- fit_model(formula, data = noisy_data, ...)

  permuted_noise <- sample(noise)

  if (sf_flag == 0) {
    permuted_noisy_response <- inverse_transform(transformed_response + permuted_noise)
  } else {
    permuted_noisy_response <- abs(data[[response_variable]] - permuted_noise)
  }

  permuted_noisy_data <- data
  permuted_noisy_data[[response_variable]] <- permuted_noisy_response

  permuted_noisy_model <- fit_model(formula, data = permuted_noisy_data, ...)

  list(noisy_model = noisy_model, permuted_noisy_model = permuted_noisy_model)
}




#' Perform Stability Under Data Selection Assessment
#'
#' This function implements the stability under data selection assessment. It
#' fits the specified model on three sets of data: resampled data, data with
#' outliers removed, and stratified bootstrap data, thereby assessing the model's
#' sensitivity to outliers and sampling variability.
#'
#' @param model A fitted model object, either of class `lm` or `glm`.
#' @param data A data frame containing the data used for model fitting.
#' @param formula A formula describing the model to be fitted.
#' @param ... Additional arguments to be passed to the `fit_model` function.
#'
#' @return A list containing `bootstrap_model` fitted on resampled data,
#' `no_outlier_model` fitted on data with outliers removed, and `strata_boot_models`
#' list of models fitted on stratified bootstrap data.
#'
#' @keywords internal
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

  # Identify numeric columns
  num_cols <- which(sapply(data, is.numeric))

  # Exclude factor variables
  factor_cols <- which(sapply(data, is.factor))
  num_cols <- setdiff(num_cols, factor_cols)

  # If the family of the model is binomial, quasibinomial, poisson, or quasipoisson, exclude the response variable
  family <- family(model)$family
  if (family %in% c("binomial", "quasibinomial", "poisson", "quasipoisson")) {
    response_variable <- all.vars(formula)[1]
    num_cols <- setdiff(num_cols, which(names(data) == response_variable))
  }

  # Create a copy of the data
  data_noisy <- data

  # Add noise only to numeric columns
  data_noisy[, num_cols] <- data[, num_cols] + stats::rnorm(length(num_cols)) * .Machine$double.eps^0.5

  fit_model(formula, data = data_noisy, ...)
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
