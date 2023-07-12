#' Stability Explainer
#'
#' Provides explanations and interpretations for each type of stability analysis.
#'
#' @param object A `gstab` object.
#'
#' @return A list of explanations and interpretations for each type of stability
#' analysis.
#' @export
#'
#' @seealso
#'   \code{\link[base]{UseMethod}}
#'
#' @examples
#' # To be provided...
#'
stab_explainer <- function(object) {
  UseMethod("stab_explainer")
}

#' Stability Explainer for Linear Models
#'
#' Provides explanations and interpretations for each type of stability analysis
#' for linear models.
#'
#' @param object A `gstab_lm` object.
#'
#' @return A list of explanations and interpretations for each type of stability
#' analysis for linear models.
#' @export
#'
#' @seealso
#'   \code{\link[base]{UseMethod}}
#'
#' @examples
#' # To be provided...
#'
stab_explainer.gstab_lm <- function(object) {

  stability_definitions <- list(
    "Reference" = "Michailides, G., & de Leeuw, J. (1998). The Gifi system for nonlinear multivariate analysis. eScholarship, University of California, Los Angeles. https://escholarship.org/uc/item/0789f7d3",
    "Replication Stability" = list(
      definition = "If a new data set is sampled and apply the same technique to this new set, then the results should not change dramatically (Michailides and de Leeuw, 1998).",
      explanation = "The function applies the original lm model on a new data set (if provided), and also runs bootstrap resampling (if specified). This helps in assessing the stability of the model across different samples.",
      interpretation = "Compare the coefficients, standard errors and p-values of the original model with those of the new model and bootstrap models. Significant differences may suggest lack of replication stability."
    ),
    "Statistical Stability" = list(
      definition = "It refers to the stability of the analysis whenever no new data set is formally sampled (Michailides and de Leeuw, 1998).",
      explanation = "The function fits the model on the data with added random noise and with permuted noise. This measures the model's sensitivity to random variations in the data.",
      interpretation = "Look at the differences in coefficients, standard errors and p-values between the original model and the models fitted with noisy data. Large differences could indicate a lack of statistical stability."
    ),
    "Stability under Data Selection" = list(
      definition = "Variations in the data are considered, by omitting either objects from the data set or variables from subsequent analysis (Michailides and de Leeuw, 1998).",
      explanation = "The function fits models on resampled data and on data with outliers removed. This helps in assessing the impact of outliers and sampling variability on the model.",
      interpretation = "Compare the model results with the resampled and no-outlier models. Significant differences may suggest instability under data selection."
    ),
    "Stability under Model Selection" = list(
      definition = "Small changes in the model should result in small changes in the results obtained (Michailides and de Leeuw, 1998).",
      explanation = "The function tests models with changes in the intercept and removal of a variable. It also attempts backward selection if a variable of interest is specified. This helps in assessing the stability of the model under small changes in model specification.",
      interpretation = "Look at the likelihood ratio tests between the original model and the models with modified specifications. Significant differences may indicate instability under model selection."
    ),
    "Numerical Stability" = list(
      definition = "It refers to the influence of rounding errors and of computation with limited precision on the results given by the techniques (Michailides and de Leeuw, 1998).",
      explanation = "The function fits the model on data that's been perturbed by a small amount of random noise. This assesses the impact of rounding errors and limited precision computations on the model.",
      interpretation = "Compare the coefficients, standard errors and p-values between the original model and the model with perturbed data. Large differences could suggest numerical instability."
    ),
    "Analytic and Algebraic Stability" = list(
      definition = "If the data structures and possible representations have enough mathematical structure, then formal expressions of the input-output analysis can be drawn from considering perturbations of the input (Michailides and de Leeuw, 1998).",
      explanation = "The function calculates the condition number (kappa) of the original model, which is a measure of multicollinearity. High multicollinearity can make the model unstable under slight changes in the input.",
      interpretation = "If the kappa value is above 30, there's severe multicollinearity, which may lead to instability in the model."
    ),
    "Stability under Selection of Technique" = list(
      definition = "Application of a number of different techniques to the same data set, aiming at answering the same question, results in approximately the same information (Michailides and de Leeuw, 1998).",
      explanation = "The function fits a robust linear regression model using the same specification as the original model. This helps in assessing the stability of the model under different fitting techniques.",
      interpretation = "Compare the coefficients, standard errors and p-values of the original model with those of the robust model. Significant differences might indicate a lack of stability under selection of technique."
    )
  )

  return(stability_definitions)
}

#' Stability Explainer for Linear Models
#'
#' Provides explanations and interpretations for each type of stability analysis
#' for linear models.
#'
#' @param object A `gstab_lm` object.
#'
#' @return A list of explanations and interpretations for each type of stability
#' danalysis for linear models.
#' @export
#'
#' @seealso
#'   \code{\link[base]{UseMethod}}
#'
#' @examples
#' # To be provided...
#'
stab_explainer.gstab_glm <- function(object) {

  stability_definitions <- list(
    "Reference" = "Michailides, G., & de Leeuw, J. (1998). The Gifi system for nonlinear multivariate analysis. eScholarship, University of California, Los Angeles. https://escholarship.org/uc/item/0789f7d3",
    "Replication Stability" = list(
      definition = "If a new data set is sampled and apply the same technique to this new set, then the results should not change dramatically (Michailides and de Leeuw, 1998).",
      explanation = "The function applies the original glm model (including the same link function) on a new data set (if provided), and also runs bootstrap resampling (if specified). This helps in assessing the stability of the model across different samples.",
      interpretation = "Compare the coefficients, standard errors and p-values of the original model with those of the new model and bootstrap models. Significant differences may suggest lack of replication stability."
    ),
    "Statistical Stability" = list(
      definition = "It refers to the stability of the analysis whenever no new data set is formally sampled (Michailides and de Leeuw, 1998).",
      explanation = "The function fits the model on the data with added random noise and with permuted noise. This measures the model's sensitivity to random variations in the data.",
      interpretation = "Look at the differences in coefficients, standard errors and p-values between the original model and the models fitted with noisy data. Large differences could indicate a lack of statistical stability."
    ),
    "Stability under Data Selection" = list(
      definition = "Variations in the data are considered, by omitting either objects from the data set or variables from subsequent analysis (Michailides and de Leeuw, 1998).",
      explanation = "The function fits models on resampled data and on data with outliers removed. This helps in assessing the impact of outliers and sampling variability on the model.",
      interpretation = "Compare the model results with the resampled and no-outlier models. Significant differences may suggest instability under data selection."
    ),
    "Stability under Model Selection" = list(
      definition = "Small changes in the model should result in small changes in the results obtained (Michailides and de Leeuw, 1998).",
      explanation = "The function tests models with changes in the intercept and removal of a variable. It also attempts backward selection if a variable of interest is specified. This helps in assessing the stability of the model under small changes in model specification.",
      interpretation = "Look at the likelihood ratio tests between the original model and the models with modified specifications. Significant differences may indicate instability under model selection."
    ),
    "Numerical Stability" = list(
      definition = "It refers to the influence of rounding errors and of computation with limited precision on the results given by the techniques (Michailides and de Leeuw, 1998).",
      explanation = "The function fits the model on data that's been perturbed by a small amount of random noise. This assesses the impact of rounding errors and limited precision computations on the model.",
      interpretation = "Compare the coefficients, standard errors and p-values between the original model and the model with perturbed data. Large differences could suggest numerical instability."
    ),
    "Analytic and Algebraic Stability" = list(
      definition = "If the data structures and possible representations have enough mathematical structure, then formal expressions of the input-output analysis can be drawn from considering perturbations of the input (Michailides and de Leeuw, 1998).",
      explanation = "The function calculates the condition number (kappa) of the original model, which is a measure of multicollinearity. High multicollinearity can make the model unstable under slight changes in the input.",
      interpretation = "If the kappa value is above 30, there's severe multicollinearity, which may lead to instability in the model."
    ),
    "Stability under Selection of Technique" = list(
      definition = "Application of a number of different techniques to the same data set, aiming at answering the same question, results in approximately the same information (Michailides and de Leeuw, 1998).",
      explanation = "For linear models, the function fits a robust linear regression model using the same specification as the original model. For generalized linear models, a robust alternative would be used if available. This helps in assessing the stability of the model under different fitting techniques.",
      interpretation = "Compare the coefficients, standard errors and p-values of the original model with those of the robust model. Significant differences might indicate a lack of stability under selection of technique."
    )
  )

  return(stability_definitions)
}
