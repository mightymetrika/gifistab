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
      explanation = "The replication stability function in the 'gifistab' package implements this definition by fitting the original model on a new data set (if provided) and also running bootstrap resampling (if specified). This helps to assess the stability of the model across different samples.

      The new data set should be a sample drawn from the same population as the main original sample and the sample size should be similar as well.

      The bootstrap resampling procedure involves fitting the model on a series of bootstrap samples. Each bootstrap sample is created by sampling the original data with replacement. This ensures that the bootstrap samples are representative of the original data set.

      The coefficients, standard errors, and p-values of the original model are compared to the coefficients, standard errors, and p-values of the new model and the bootstrap models. Significant differences between the original model and the new model or the bootstrap models may suggest a lack of replication stability.",
      interpretation = "The replication stability results can be used to assess the stability of the model across different samples. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the new model and the bootstrap models, then the model is considered to be replication stable. However, if there are significant differences between the original model and the new model or the bootstrap models, then the model may not be replication stable.

      In general, a model with good replication stability is less likely to be affected by random variations in the data. This means that the model is more likely to produce consistent results when it is applied to different samples of data."
    ),
    "Statistical Stability" = list(
      definition = "It refers to the stability of the analysis whenever no new data set is formally sampled (Michailides and de Leeuw, 1998).",
      explanation = "The statistical stability function in the 'gifistab' package implements this definition by fitting the original model on the data with added random noise and with permuted noise. This measures the model's sensitivity to random variations in the data.

      The random noise is added to the response variable in the data. This ensures that the noise is independent of the other variables in the data. The permuted noise is created by randomly shuffling the order of the noise values. This ensures that the noise is not correlated with the other variables in the data.

      The coefficients, standard errors, and p-values of the original model are compared to the coefficients, standard errors, and p-values of the models fitted with noisy data. Significant differences between the original model and the models fitted with noisy data may suggest a lack of statistical stability.",
      interpretation = "The statistical stability results can be used to assess the sensitivity of the model to random variations in the data. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the models fitted with noisy data, then the model is considered to be statistically stable. However, if there are significant differences between the original model and the models fitted with noisy data, then the model may not be statistically stable.

      In general, a model with good statistical stability is less likely to be affected by random variations in the data. This means that the model is more likely to produce consistent results when it is applied to different samples of data with different levels of noise."
    ),
    "Stability under Data Selection" = list(
      definition = "Variations in the data are considered, by omitting either objects from the data set or variables from subsequent analysis. The former corresponds to rejection of outliers and resampling techniques. In this framework, resampling techniques can be thought of as a form of replication stability, but without formally sampling a new data set (Michailides and de Leeuw, 1998).",
      explanation = "The stability under data selection function in the 'gifistab' package implements this definition by fitting the original model on three sets of data:

      Resampled data, which is created by sampling the original data with replacement. This ensures that the resampled data has the same sample size as the original data and that the samples are drawn from the same population, but the observations are not necessarily the same.

      Data with outliers removed, which is created by using robust multivariate Mahalanobis distances; if outlier identification using  robust multivariate Mahalanobis distances fails, then observations will be identified as outliers if the observation is 3 standard deviations away from the mean on any of the variables in the data. Outliers are removed from the data and the model is refitted.

      Stratified bootstrap data, which is created by first creating three random subsamples of the data where each observation belongs to one and only one subsample.  Subsamples are then used to create bootstrap samples of the same sample size as the original data. This ensures that the bootstrap samples are representative of the distribution of the data in the original data set.",
      interpretation = "The stability under data selection results can be used to assess the sensitivity of the model to outliers and sampling variability. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the models fitted with resampled data, with outliers removed, and with stratified bootstrap data, then the model is considered to be stable under data selection. However, if there are significant differences between the original model and the models fitted with resampled data, with outliers removed, or with stratified bootstrap data, then the model may not be stable under data selection.

      In general, a model with good stability under data selection is less likely to be affected by outliers or sampling variability."
    ),
    "Stability under Model Selection" = list(
      definition = "Small changes in the model should result in small changes in the results obtained (Michailides and de Leeuw, 1998).",
      explanation = "The stability under model selection function in the 'gifistab' package implements this definition by assessing the stability of a model to three types of changes:

      Toggling the intercept: This involves removing the intercept from a model that includes the intercept or adding the intercept from a model that excludes the intercept. After toggling the intercept the model is refit. If the results of the two models are similar, then the model is considered to be stable to changes in the intercept.

      Removing a variable: This involves removing a user selected variable from the model and then re-fitting the model. If the results of the two models are similar, then the model is considered to be stable with or without controlling for the user selected variable.

      Removing the least useful variable: This involves selecting a variable of interest fitting a full model with all of the variables and then using a backward stepwise selection procedure to remove the least useful variable that is not the variable of interest. If the results of the two models are similar, then the model is considered to be stable to changes in the least useful variable.",
      interpretation = "The results of the stability under model selection function can be used to assess the robustness of a model to changes in the model specification. If the results of the function show that the model is stable to changes in the intercept, the user selected variable to remove, and the least useful variable, then the model is considered to be robust. However, if the results of the function show that the model is not stable to changes in the model specification, then the model may be sensitive to changes in the data and may not be a reliable predictor. While this stability assessment is designed for the situation where the user wishes to make inferences on a variable of interest after controlling for other variables in the model, it may also provide useful stability information for situations where the user wishes to interpret multiple coefficients."
    ),
    "Numerical Stability" = list(
      definition = "It refers to the influence of rounding errors and of computation with limited precision on the results given by the techniques (Michailides and de Leeuw, 1998).",
      explanation = "The numerical stability function in the 'gifistab' package implements this definition by fitting the model on data that has been perturbed by a small amount of random noise. This assesses the impact of rounding errors and limited precision computations on the model.

      The function works by first fitting the model on the original data. Then, it perturbs the data by adding a small amount of normally distributed random noise. The amount of noise is specified by the .Machine$double.eps^0.5 argument. This argument is the square root of the machine epsilon, which is the smallest positive number that can be added to 1 without changing the value of 1.

      Once the data has been perturbed, the function fits the model again. The results of the two fits are then compared. If the results are similar, then the model is considered to be numerically stable. However, if the results are different, then the model may be numerically unstable.",
      interpretation = "The results of the numerical stability function can be used to assess the robustness of a model to rounding errors and limited precision computations. If the results of the function show that the model is numerically stable, then the model is considered to be robust. However, if the results of the function show that the model is not numerically stable, then the model may be sensitive to rounding errors and may not be a reliable predictor."
    ),
    "Analytic and Algebraic Stability" = list(
      definition = "If the data structures and possible representations have enough mathematical structure, then formal expressions of the input-output analysis can be drawn from considering perturbations of the input (Michailides and de Leeuw, 1998).",
      explanation = "The analytic and algebraic stability function in the 'gifistab' package implements this definition by calculating the condition number (kappa) of the original model. The condition number is a measure of multicollinearity, which is the degree to which the independent variables are correlated. High multicollinearity can make the model unstable under slight changes in the input.

      The function works by first calculating the determinant of the model's covariance matrix. Then, it calculates the ratio of the largest to smallest eigenvalues of the covariance matrix. This ratio is the condition number.

      A high condition number indicates that the model is sensitive to changes in the independent variables. This is because small changes in the independent variables can lead to large changes in the model coefficients. The function also prints a warning if the condition number is above 30. This is because a condition number of 30 is considered to be the threshold for severe multicollinearity.",
      interpretation = "The results of the analytic and algebraic stability function can be used to assess the robustness of a model to multicollinearity. If the condition number is high, then the model may be unstable under slight changes in the independent variables. However, if the condition number is low, then the model is considered to be robust."
    ),
    "Stability under Selection of Technique" = list(
      definition = "Application of a number of different techniques to the same data set, aiming at answering the same question, results in approximately the same information (Michailides and de Leeuw, 1998).",
      explanation = "The stability under selection of technique function in the 'gifistab' package implements this definition by fitting a robust regression model using the same specification as the original model. This helps in assessing the stability of the model under different fitting techniques.

      The function works by first fitting the original model. Then, it fits a robust regression model using the same specification as the original model. The robust regression model is fitted using the 'lmrob' function from the 'robustbase' R package.

      The results of the two models are then compared. If the results are similar, then the model is considered to be stable under different fitting techniques. However, if the results are different, then the model may be unstable under different fitting techniques.",
      interpretation = "The results of the stability under selection of technique function can be used to assess the robustness of a model to different fitting techniques. If the results of the two models are similar, then the model is considered to be robust. However, if the results of the two models are different, then the model may be unstable under different fitting techniques."
    )
  )

  return(stability_definitions)
}

#' Stability Explainer for Generalized Linear Models
#'
#' Provides explanations and interpretations for each type of stability analysis
#' for generalized linear models.
#'
#' @param object A `gstab_glm` object.
#'
#' @return A list of explanations and interpretations for each type of stability
#' analysis for generalized linear models.
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
      explanation = "The replication stability function in the 'gifistab' package implements this definition by fitting the original model on a new data set (if provided) and also running bootstrap resampling (if specified). This helps to assess the stability of the model across different samples.

      The new data set should be a sample drawn from the same population as the main original sample and the sample size should be similar as well.

      The bootstrap resampling procedure involves fitting the model on a series of bootstrap samples. Each bootstrap sample is created by sampling the original data with replacement. This ensures that the bootstrap samples are representative of the original data set.

      The coefficients, standard errors, and p-values of the original model are compared to the coefficients, standard errors, and p-values of the new model and the bootstrap models. Significant differences between the original model and the new model or the bootstrap models may suggest a lack of replication stability.",
      interpretation = "The replication stability results can be used to assess the stability of the model across different samples. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the new model and the bootstrap models, then the model is considered to be replication stable. However, if there are significant differences between the original model and the new model or the bootstrap models, then the model may not be replication stable.

      In general, a model with good replication stability is less likely to be affected by random variations in the data. This means that the model is more likely to produce consistent results when it is applied to different samples of data."
    ),
    "Statistical Stability" = list(
      definition = "It refers to the stability of the analysis whenever no new data set is formally sampled (Michailides and de Leeuw, 1998).",
      explanation = "The statistical stability function in the 'gifistab' package implements this definition by fitting the original model on the data with added random noise and with permuted noise. This measures the model's sensitivity to random variations in the data.

      The random noise is added to the response variable in the data. This ensures that the noise is independent of the other variables in the data. The permuted noise is created by randomly shuffling the order of the noise values. This ensures that the noise is not correlated with the other variables in the data.

      The coefficients, standard errors, and p-values of the original model are compared to the coefficients, standard errors, and p-values of the models fitted with noisy data. Significant differences between the original model and the models fitted with noisy data may suggest a lack of statistical stability.",
      interpretation = "The statistical stability results can be used to assess the sensitivity of the model to random variations in the data. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the models fitted with noisy data, then the model is considered to be statistically stable. However, if there are significant differences between the original model and the models fitted with noisy data, then the model may not be statistically stable.

      In general, a model with good statistical stability is less likely to be affected by random variations in the data. This means that the model is more likely to produce consistent results when it is applied to different samples of data with different levels of noise."
    ),
    "Stability under Data Selection" = list(
      definition = "Variations in the data are considered, by omitting either objects from the data set or variables from subsequent analysis (Michailides and de Leeuw, 1998).",
      explanation = "The stability under data selection function in the 'gifistab' package implements this definition by fitting the original model on three sets of data:

      Resampled data, which is created by sampling the original data with replacement. This ensures that the resampled data has the same sample size as the original data and that the samples are drawn from the same population, but the observations are not necessarily the same.

      Data with outliers removed, which is created by using robust multivariate Mahalanobis distances; if outlier identification using  robust multivariate Mahalanobis distances fails, then observations will be identified as outliers if the observation is 3 standard deviations away from the mean on any of the variables in the data. Outliers are removed from the data and the model is refitted.

      Stratified bootstrap data, which is created by first creating three random subsamples of the data where each observation belongs to one and only one subsample.  Subsamples are then used to create bootstrap samples of the same sample size as the original data. This ensures that the bootstrap samples are representative of the distribution of the data in the original data set.",
      interpretation = "The stability under data selection results can be used to assess the sensitivity of the model to outliers and sampling variability. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the models fitted with resampled data, with outliers removed, and with stratified bootstrap data, then the model is considered to be stable under data selection. However, if there are significant differences between the original model and the models fitted with resampled data, with outliers removed, or with stratified bootstrap data, then the model may not be stable under data selection.

      In general, a model with good stability under data selection is less likely to be affected by outliers or sampling variability."
    ),
    "Stability under Model Selection" = list(
      definition = "Small changes in the model should result in small changes in the results obtained (Michailides and de Leeuw, 1998).",
      explanation = "Not currently implemented for stats::glm engine.",
      interpretation = "Not currently implemented for stats::glm engine."
    ),
    "Numerical Stability" = list(
      definition = "It refers to the influence of rounding errors and of computation with limited precision on the results given by the techniques (Michailides and de Leeuw, 1998).",
      explanation = "The numerical stability function in the 'gifistab' package implements this definition by fitting the model on data that has been perturbed by a small amount of random noise. This assesses the impact of rounding errors and limited precision computations on the model.

      The function works by first fitting the model on the original data. Then, it perturbs the data by adding a small amount of normally distributed random noise. The amount of noise is specified by the .Machine$double.eps^0.5 argument. This argument is the square root of the machine epsilon, which is the smallest positive number that can be added to 1 without changing the value of 1.

      Once the data has been perturbed, the function fits the model again. The results of the two fits are then compared. If the results are similar, then the model is considered to be numerically stable. However, if the results are different, then the model may be numerically unstable.",
      interpretation = "The results of the numerical stability function can be used to assess the robustness of a model to rounding errors and limited precision computations. If the results of the function show that the model is numerically stable, then the model is considered to be robust. However, if the results of the function show that the model is not numerically stable, then the model may be sensitive to rounding errors and may not be a reliable predictor."
    ),
    "Analytic and Algebraic Stability" = list(
      definition = "If the data structures and possible representations have enough mathematical structure, then formal expressions of the input-output analysis can be drawn from considering perturbations of the input (Michailides and de Leeuw, 1998).",
      explanation = "The analytic and algebraic stability function in the 'gifistab' package implements this definition by calculating the condition number (kappa) of the original model. The condition number is a measure of multicollinearity, which is the degree to which the independent variables are correlated. High multicollinearity can make the model unstable under slight changes in the input.

      The function works by first calculating the determinant of the model's covariance matrix. Then, it calculates the ratio of the largest to smallest eigenvalues of the covariance matrix. This ratio is the condition number.

      A high condition number indicates that the model is sensitive to changes in the independent variables. This is because small changes in the independent variables can lead to large changes in the model coefficients. The function also prints a warning if the condition number is above 30. This is because a condition number of 30 is considered to be the threshold for severe multicollinearity.",
      interpretation = "The results of the analytic and algebraic stability function can be used to assess the robustness of a model to multicollinearity. If the condition number is high, then the model may be unstable under slight changes in the independent variables. However, if the condition number is low, then the model is considered to be robust."
    ),
    "Stability under Selection of Technique" = list(
      definition = "Application of a number of different techniques to the same data set, aiming at answering the same question, results in approximately the same information (Michailides and de Leeuw, 1998).",
      explanation = "The stability under selection of technique function in the 'gifistab' package implements this definition by fitting a robust regression model using the same specification as the original model. This helps in assessing the stability of the model under different fitting techniques.

      The function works by first fitting the original model. Then, it fits a robust regression model using the same specification as the original model. The robust regression model is fitted using the 'glmrob' function from the 'robustbase' R package.

      The results of the two models are then compared. If the results are similar, then the model is considered to be stable under different fitting techniques. However, if the results are different, then the model may be unstable under different fitting techniques.",
      interpretation = "The results of the stability under selection of technique function can be used to assess the robustness of a model to different fitting techniques. If the results of the two models are similar, then the model is considered to be robust. However, if the results of the two models are different, then the model may be unstable under different fitting techniques."
    )
  )

  return(stability_definitions)
}
