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
#' This function implements the statistical stability assessment by fitting the
#' original model to the data with added random noise and permuted noise. It takes into
#' account the family of the model (i.e., Gaussian, gamma, inverse Gaussian, Poisson,
#' quasi-Poisson, binomial, quasi-binomial), and assesses the model's sensitivity to
#' random variations in the data.
#'
#' The function first determines the family of the model and applies the appropriate
#' transformation to the response variable. For Gaussian, gamma, and inverse Gaussian
#' families, it uses an identity or log transformation; for Poisson and quasi-Poisson,
#' a log transformation; and for binomial and quasi-binomial, a logit transformation
#' or a binary flip depending on whether the response variable is a proportion or binary.
#'
#' If the response variable is binary (determined using a 'success/failure' flag), the
#' function flips a fraction of the response variable (changes from 0 to 1 or from 1 to 0).
#' For non-binary variables, it adds normally distributed noise to the transformed response.
#' After adding the noise, the inverse of the transformation is applied to the noisy response
#' to bring it back to the original scale.
#'
#' @param model A fitted model object, either of class `lm` or `glm`.
#' @param data A data frame containing the data used for model fitting.
#' @param formula A formula describing the model to be fitted.
#' @param nf Noise factor as a percentage. For non-binomial families, this is used to
#' determine the standard deviation of the noise added to the transformed response
#' variable. For binomial families with a binary response, this is used to determine
#' the fraction of the response variable to flip (i.e., change 0 to 1 or 1 to 0).
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

  # Single resample bootstap model
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
    # Fallback: remove data points that fall outside of Q1 - 1.5*IQR or Q3 + 1.5*IQR
    is_outlier <- apply(data, 2, function(x) {
      IQR_x <- stats::IQR(x, na.rm = TRUE)
      Q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
      Q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
      x < (Q1 - 1.5 * IQR_x) | x > (Q3 + 1.5 * IQR_x)
    })
    no_outlier_data <- data[!rowSums(is_outlier), ]
    if (nrow(no_outlier_data) > 0) {
      no_outlier_model <- fit_model(formula, data = no_outlier_data, ...)
    }
  })

  # Stratified bootstrap models
  num_rows <- nrow(data)
  num_strata <- 3
  strata_size <- num_rows %/% num_strata
  remaining <- num_rows %% num_strata
  strata <- sample(c(rep(1:num_strata, each = strata_size), sample(1:num_strata, size = remaining)))
  strata_boot_models <- lapply(1:3, function(x) {
    data_subset <- data[strata == x, ]
    boot_subset <- data_subset[sample(nrow(data_subset), nrow(data), replace = TRUE), ]
    fit_model(formula, data = boot_subset, ...)
  })

  list(bootstrap_model = bootstrap_model,
       no_outlier_model = no_outlier_model,
       strata_boot_models = strata_boot_models)
}

#' Stability Under Model Selection
#'
#' This function assesses the stability of a model under three types of
#' specification changes:
#' 1. Toggling the intercept: This involves either removing the intercept from a
#' model that includes it or adding the intercept to a model that excludes it.
#' 2. Removing a specific variable: The function allows you to specify a particular
#' variable to be removed from the model. If the specified variable is part of an
#' interaction term, only the interaction will be removed, leaving the main effects
#' in place.
#' 3. Removing variables through backward selection: This approach uses a backward
#' stepwise selection process to remove variables that do not contribute significantly
#' to the model according to the Akaike information criterion (AIC), while preserving
#' the variable of interest. This process iteratively removes variables until the
#' AIC cannot be further reduced by removing any other variable.
#'
#' @param model A fitted model object. This can be of class `lm` or `glm`.
#' @param data A data frame containing the data that was used to fit the model.
#' @param formula A formula that describes the model to be fitted.
#' @param variable_to_remove A character string specifying a variable to be
#' removed from the model. If the variable is part of an interaction term, the
#' interaction is removed but not the main effects.
#' @param variable_of_interest A character string or a vector of strings specifying
#' the variable(s) of interest for the stability analysis under backward selection.
#' If the variable of interest is part of an interaction term, all variables in
#' the interaction are considered to be of interest.
#' @param ... Other parameters to be passed to the `lmtest::lrtest` function.
#'
#' @return A list with three components:
#' - `toggle_intercept`: This is an object of class `lmtest::lrtest` that compares
#' the fit of the model with and without an intercept.
#' - `remove_variable`: This is either an object of class `lmtest::lrtest` that
#' compares the fit of the model with and without the variable specified in
#' `variable_to_remove` parameter, or NULL if the `variable_to_remove` parameter
#' is NULL.
#' - `remove_least_useful`: This is an object of class `lmtest::lrtest` that
#' compares the fit of the full model and a model obtained by backward selection
#' while preserving the variable(s) specified in `variable_of_interest`. This
#' process removes variables until no further improvement in AIC is possible.
#' This component is NULL if the `variable_of_interest` parameter is NULL.
#'
#' @keywords internal
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
      stop(paste("One or more variables of interest",
                 paste(variable_of_interest, collapse = ", "),
                 "are not in the model."))
    }

    # Expand any shorthand interactions in the formula
    formula <- stats::reformulate(labels(stats::terms(formula)), formula[[2]])

    # Find all variables and interactions that include any variable in variable_of_interest
    vars_to_keep <- all.vars(formula)[grepl(paste0("\\b(",
                                                   paste(variable_of_interest,
                                                         collapse = "|"), ")\\b"),
                                            all.vars(formula))]

    # Find all unique variables involved in these terms
    unique_vars <- unique(unlist(strsplit(vars_to_keep, ":|\\+")))

    # The "lower" model includes the variable of interest, its interactions,
    # and the variables involved in these interactions
    lower_formula <- stats::as.formula(paste(formula[[2]], "~",
                                             paste(unique_vars, collapse = " + ")))

    # The full model includes all variables
    full_model <- fit_model(formula, data = data)

    # Perform backward selection, but keep the lower model in the scope
    new_model <- MASS::stepAIC(full_model,
                               direction = "backward",
                               scope = list(lower = lower_formula, upper = formula),
                               trace = FALSE)

    # Perform likelihood ratio test
    remove_least_useful <- lmtest::lrtest(full_model, new_model)
  }

  list(toggle_intercept = toggle_intercept,
       remove_variable = remove_variable,
       remove_least_useful = remove_least_useful)
}

#' Numerical Stability Assessment
#'
#' This function assesses the numerical stability of a fitted model by introducing
#' a small perturbation to the numerical variables in the dataset and comparing
#' the resulting model to the original. The results of the function can be used
#' to assess the robustness of a model to rounding errors and limited precision
#' computations.
#'
#' @param model A fitted model object, either of class `lm` or `glm`.
#' @param data A data frame containing the data used for model fitting.
#' @param formula A formula describing the model to be fitted.
#' @param ... Other parameters to be passed to the fitting function determined by
#' `fit_model_func`.
#'
#' @return The function returns the fitted model object obtained from the perturbed
#' data.
#'
#' @keywords internal
numerical_stability <- function(model, data, formula, ...) {
  fit_model <- fit_model_func(model)

  # Identify numeric columns
  num_cols <- which(sapply(data, is.numeric))

  # Exclude factor variables
  factor_cols <- which(sapply(data, is.factor))
  num_cols <- setdiff(num_cols, factor_cols)

  # If the family of the model is binomial, quasibinomial, poisson, or quasipoisson,
  #exclude the response variable
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

#' Analytic and Algebraic Stability Assessment
#'
#' This function assesses the analytic and algebraic stability of a fitted model
#' by calculating two condition numbers (kappa): one based on the L1 norm and
#' another based on the Linf norm of the model matrix.
#'
#' @param model A fitted model object, either of class `lm` or `glm`.
#' @param warn Logical. If TRUE (default), warnings are printed when the condition
#' number indicates severe multicollinearity (i.e., > 30).
#' @param ... Other parameters to be passed to the `kappa` function.
#'
#' @return A list with the L1 norm and Linf norm condition numbers.
#'
#' @keywords internal
analytic_and_algebraic_stability <- function(model, warn = TRUE, ...) {
  L1 <- kappa(model, term = "O")
  Linf <- kappa(model, term = "I")

  if (warn) {
    if (L1 > 30) warning("Severe multicollinearity detected (L1 norm)")
    if (Linf > 30) warning("Severe multicollinearity detected (Linf norm)")
  }

  kappa <- list(L1 = L1, Linf = Linf)
  return(kappa)
}

#' Stability Under Selection of Technique Assessment
#'
#' This function assesses the stability of a fitted model under the application
#' of different techniques. It fits a robust regression model using the same
#' specification as the original model and compares the results.
#'
#' @param model A fitted model object, either of class `lm` or `glm`.
#' @param data A data frame containing the data used for model fitting.
#' @param formula A formula describing the model to be fitted.
#' @param ... Other parameters passed to `robustbase::lmrob` or `robustbase::glmrob`.
#'
#' @return A fitted robust regression model object (`robustbase::lmrob` or
#' `robustbase::glmrob`), or NULL if the robust regression model could not be
#' fitted.
#'
#' @keywords internal
stability_under_selection_of_technique <- function(model, data, formula, ...) {
  robust_regression <- NULL

  tryCatch({
    if ("lm" %in% class(model)[[1]]) {
      robust_regression = robustbase::lmrob(formula, data = data, ...)
      if (!robust_regression$converged) {
        stop("Robust regression did not converge.")
      }
    } else if ("glm" %in% class(model)[[1]]) {
      robust_regression = robustbase::glmrob(formula, data = data, ...)
      if (!robust_regression$converged) {
        stop("Robust regression did not converge.")
      }
    }
  }, error = function(e) {
    message("Error in fitting robust regression: ", e$message)
  })

  robust_regression
}

