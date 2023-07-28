#' Stability Explainer
#'
#' Provides explanations and interpretations for each type of stability analysis.
#'
#' @param object A `gstab` object.
#'
#' @return A list of explanations and interpretations for each type of stability
#' analysis.
#'
#' @keywords internal
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
#'
#' @keywords internal
stab_explainer.gstab_lm <- function(object) {

  stability_definitions <- list(
    "Reference" = "Michailides, G., & de Leeuw, J. (1998). The Gifi system for nonlinear multivariate analysis. eScholarship, University of California, Los Angeles. https://escholarship.org/uc/item/0789f7d3",
    "Replication Stability" = list(
      definition = "Replication stability suggests that applying the same technique to a new dataset sampled from the same population should not dramatically change the results (Michailides and de Leeuw, 1998).",
      explanation = "The 'gifistab' package's replication stability function achieves this by fitting the original model to a new dataset (if provided) and by conducting bootstrap resampling (if specified). These operations help evaluate the model's stability across different samples.

      The new dataset should be sampled from the same population as the original and should have a similar sample size.

      The bootstrap resampling procedure fits the model to a series of bootstrap samples, each created by sampling the original data with replacement.

      The coefficients, standard errors, and p-values of the original model are compared to those of the new model and the bootstrap models. Significant differences between the original model and the new model or the bootstrap models may suggest a lack of replication stability.

      The `boot_mean_df` output provides the average estimate, standard error, and optionally, the confidence interval for each term in the model, averaged over all bootstrap samples. This offers a measure of the central tendency of each term's estimate and standard error across bootstrap samples.

      Additionally, `boot_mean_df` includes the proportion of bootstrap samples where the term's p-value was below 0.05, indicating statistical significance at the 0.05 alpha level. This represents the frequency of statistically significant results for each term across the bootstrap samples.

      The `boot_sd_df` output provides the standard deviation of the estimate and standard error for each term in the model, computed across all bootstrap samples. This offers a measure of the dispersion or variability of each term's estimate and standard error across the bootstrap samples.",
      interpretation = "The replication stability results can be used to assess the stability of the model across different samples. If the coefficients and standard errors of the original model are similar to those of the new model and the bootstrap models, the model is considered to be replication stable. Likewise, if the p-values of the original model are similar to the p-value of the new model and the proportion of statistically significant results from the bootstrap models, the model is also considered replication stable. Significant differences, however, may suggest a lack of replication stability.

      Generally, a model with good replication stability is less likely to be influenced by random variations in the data. This implies that the model is more likely to produce consistent results when applied to different data samples from the same population.

      The `boot_mean_df` and `boot_sd_df` outputs provide additional insights about the model's stability across bootstrap samples. Small standard deviations in the `boot_sd_df` output suggest that the estimates and standard errors for each term are stable across bootstrap samples. Conversely, large standard deviations might indicate high variability across bootstrap samples, suggesting a potential lack of stability.

      The `boot_mean_df` output can be used to compare the central tendency of each term's estimate and standard error across bootstrap samples to the corresponding values from the original model. Significant differences may indicate a potential lack of stability.

      Moreover, the proportion of statistically significant results in the `boot_mean_df` output reflects the general trend of statistical significance across bootstrap samples. A high proportion suggests that the term is often statistically significant, while a low proportion suggests that the term is rarely significant. This information can be used to assess the consistency of a term's significance in the model."
    ),
    "Statistical Stability" = list(
      definition = "It refers to the stability of the analysis whenever no new data set is formally sampled (Michailides and de Leeuw, 1998).",
      explanation = "The statistical stability function in the 'gifistab' package implements this definition by fitting the original model on the data with random noise and with permuted noise added to the response variable while taking into consideration the family of the model (i.e., Gaussian, gamma, inverse Gaussian, Poisson, quasi-Poisson, binomial, quasi-binomial).

      The function determines the family of the model and applies the appropriate transformation to the response variable. For Gaussian, gamma, and inverse Gaussian families, the function uses an identity or log transformation; for Poisson and quasi-Poisson, a log transformation; and for binomial and quasi-binomial, a logit transformation or a binary flip depending on whether the response variable is a proportion or binary.

      If the response variable is binary (a 'success/failure' flag is used to determine this), the function flips a fraction of the response variable (changes from 0 to 1 or from 1 to 0). For non-binary variables, the function adds normally distributed noise to the transformed response. After adding noise, the inverse of the transformation is applied to the noisy response to bring it back to the original scale.

      The permuted noise is created by randomly shuffling the order of the noise values and then adding this noise to the response variable.

      The coefficients, standard errors, and p-values of the original model are compared to those of the models fitted with a noisy response. Significant differences between these models may suggest a lack of statistical stability.",
      interpretation = "The statistical stability results can be used to assess the sensitivity of the model to random variations in the data. If the coefficients, standard errors, and p-values of the original model are similar to those of the models fitted with a noisy response, then the model is considered to be statistically stable. However, if there are significant differences between these models, then the model may not be statistically stable.

      In general, a model with good statistical stability is less likely to be affected by random variations in the data. This means that the model is more likely to produce consistent results when applied to different samples of data with different noise levels."
    ),
    # "Stability under Data Selection" = list(
    #   definition = "Variations in the data are considered, by omitting either objects from the data set or variables from subsequent analysis. The former corresponds to rejection of outliers and resampling techniques. In this framework, resampling techniques can be thought of as a form of replication stability, but without formally sampling a new data set (Michailides and de Leeuw, 1998).",
    #   explanation = "The stability under data selection function in the 'gifistab' package implements this definition by fitting the original model on three sets of data:
    #
    #   Resampled data, which is created by sampling the original data with replacement. This ensures that the resampled data has the same sample size as the original data and that the samples are drawn from the same population, but the observations are not necessarily the same.
    #
    #   Data with outliers removed, which is created by using robust multivariate Mahalanobis distances; if outlier identification using  robust multivariate Mahalanobis distances fails, then observations will be identified as outliers if the observation is 3 standard deviations away from the mean on any of the variables in the data. Outliers are removed from the data and the model is refitted.
    #
    #   Stratified bootstrap data, which is created by first creating three random subsamples of the data where each observation belongs to one and only one subsample.  Subsamples are then used to create bootstrap samples of the same sample size as the original data. This ensures that the bootstrap samples are representative of the distribution of the data in the original data set.",
    #   interpretation = "The stability under data selection results can be used to assess the sensitivity of the model to outliers and sampling variability. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the models fitted with resampled data, with outliers removed, and with stratified bootstrap data, then the model is considered to be stable under data selection. However, if there are significant differences between the original model and the models fitted with resampled data, with outliers removed, or with stratified bootstrap data, then the model may not be stable under data selection.
    #
    #   In general, a model with good stability under data selection is less likely to be affected by outliers or sampling variability."
    # ),
    "Stability under Data Selection" = list(
      definition = "This involves considering variations in the data by either omitting objects from the dataset or excluding variables from the subsequent analysis. This can correspond to the rejection of outliers and the application of resampling techniques. In this context, resampling techniques can be thought of as a form of replication stability, but without formally sampling a new dataset (Michailides and de Leeuw, 1998).",
      explanation = "The 'gifistab' package's stability under data selection function implements this definition by fitting the original model on three different datasets:

      1. Bootstrap data, created by sampling the original data with replacement. This ensures that the bootstrap data has the same sample size as the original data.

      2. Data with outliers removed, created by using robust multivariate Mahalanobis distances. If outlier identification using robust multivariate Mahalanobis distances fails, then an observation will be identified as an outlier if it is 3 standard deviations away from the mean on any of the variables in the data. Outliers are then removed from the data and the model is refitted.

      3. Stratified bootstrap data, created by first dividing the data into three random subsamples where each observation belongs to one and only one subsample. These subsamples are then used to create bootstrap samples of the same sample size as the original data.",
      interpretation = "The stability under data selection results can be used to assess the sensitivity of the model to outliers and sampling variability. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the models fitted to the bootstrap data, the data with outliers removed, and the stratified bootstrap data, then the model is considered stable under data selection. However, if there are significant differences between the original model and the models fitted to the bootstrap data, the data with outliers removed, or the stratified bootstrap data, then the model may not be stable under data selection.

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
      explanation = "The analytic and algebraic stability function in the 'gifistab' package implements this definition by calculating two condition numbers (kappa): one based on the L1 norm and another based on the Linf norm of the model. The condition number is a measure of multicollinearity, which is the degree to which the independent variables are correlated. High multicollinearity can make the model unstable under slight changes in the input.

      The L1 norm, also known as the Manhattan distance or taxicab norm, is the sum of the absolute values of the elements in a vector. The L1 norm-based condition number provides a measure of multicollinearity based on the maximum possible effect that a small change in a single variable can have on the estimated coefficients.

      The Linf norm, also known as the infinity norm or maximum norm, is the maximum absolute value in a vector. The Linf norm-based condition number provides a measure of multicollinearity based on the maximum possible effect that simultaneous small changes in all variables can have on the estimated coefficients.

      The function calculates the condition numbers using the 'kappa' function from base R, which calculates the ratio of the largest to smallest singular values of the model's predictor matrix. These condition numbers are returned in a list, with the L1 norm condition number labeled as 'L1' and the Linf norm condition number labeled as 'Linf'.

      A high condition number indicates that the model is sensitive to changes in the independent variables. This is because small changes in the independent variables can lead to large changes in the model coefficients. The function also prints a warning if either condition number is above 30, as this is considered to be the threshold for severe multicollinearity.",
      interpretation = "The results of the analytic and algebraic stability function can be used to assess the robustness of a model to multicollinearity. If either condition number is high, then the model may be unstable under slight changes in the independent variables. However, if both condition numbers are low, then the model is considered to be robust. In general, both the L1 and Linf norms provide useful information about multicollinearity, but they emphasize different aspects of the data. The L1 norm is more sensitive to outliers because it considers the sum of all the values, while the Linf norm is determined by the maximum value."
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
#'
#' @keywords internal
stab_explainer.gstab_glm <- function(object) {

  stability_definitions <- list(
    "Reference" = "Michailides, G., & de Leeuw, J. (1998). The Gifi system for nonlinear multivariate analysis. eScholarship, University of California, Los Angeles. https://escholarship.org/uc/item/0789f7d3",
    "Replication Stability" = list(
      definition = "Replication stability suggests that applying the same technique to a new dataset sampled from the same population should not dramatically change the results (Michailides and de Leeuw, 1998).",
      explanation = "The 'gifistab' package's replication stability function achieves this by fitting the original model to a new dataset (if provided) and by conducting bootstrap resampling (if specified). These operations help evaluate the model's stability across different samples.

      The new dataset should be sampled from the same population as the original and should have a similar sample size.

      The bootstrap resampling procedure fits the model to a series of bootstrap samples, each created by sampling the original data with replacement.

      The coefficients, standard errors, and p-values of the original model are compared to those of the new model and the bootstrap models. Significant differences between the original model and the new model or the bootstrap models may suggest a lack of replication stability.

      The `boot_mean_df` output provides the average estimate, standard error, and optionally, the confidence interval for each term in the model, averaged over all bootstrap samples. This offers a measure of the central tendency of each term's estimate and standard error across bootstrap samples.

      Additionally, `boot_mean_df` includes the proportion of bootstrap samples where the term's p-value was below 0.05, indicating statistical significance at the 0.05 alpha level. This represents the frequency of statistically significant results for each term across the bootstrap samples.

      The `boot_sd_df` output provides the standard deviation of the estimate and standard error for each term in the model, computed across all bootstrap samples. This offers a measure of the dispersion or variability of each term's estimate and standard error across the bootstrap samples.",
      interpretation = "The replication stability results can be used to assess the stability of the model across different samples. If the coefficients and standard errors of the original model are similar to those of the new model and the bootstrap models, the model is considered to be replication stable. Likewise, if the p-values of the original model are similar to the p-value of the new model and the proportion of statistically significant results from the bootstrap models, the model is also considered replication stable. Significant differences, however, may suggest a lack of replication stability.

      Generally, a model with good replication stability is less likely to be influenced by random variations in the data. This implies that the model is more likely to produce consistent results when applied to different data samples from the same population.

      The `boot_mean_df` and `boot_sd_df` outputs provide additional insights about the model's stability across bootstrap samples. Small standard deviations in the `boot_sd_df` output suggest that the estimates and standard errors for each term are stable across bootstrap samples. Conversely, large standard deviations might indicate high variability across bootstrap samples, suggesting a potential lack of stability.

      The `boot_mean_df` output can be used to compare the central tendency of each term's estimate and standard error across bootstrap samples to the corresponding values from the original model. Significant differences may indicate a potential lack of stability.

      Moreover, the proportion of statistically significant results in the `boot_mean_df` output reflects the general trend of statistical significance across bootstrap samples. A high proportion suggests that the term is often statistically significant, while a low proportion suggests that the term is rarely significant. This information can be used to assess the consistency of a term's significance in the model."
    ),
    "Statistical Stability" = list(
      definition = "It refers to the stability of the analysis whenever no new data set is formally sampled (Michailides and de Leeuw, 1998).",
      explanation = "The statistical stability function in the 'gifistab' package implements this definition by fitting the original model on the data with random noise and with permuted noise added to the response variable while taking into consideration the family of the model (i.e., Gaussian, gamma, inverse Gaussian, Poisson, quasi-Poisson, binomial, quasi-binomial).

      The function determines the family of the model and applies the appropriate transformation to the response variable. For Gaussian, gamma, and inverse Gaussian families, the function uses an identity or log transformation; for Poisson and quasi-Poisson, a log transformation; and for binomial and quasi-binomial, a logit transformation or a binary flip depending on whether the response variable is a proportion or binary.

      If the response variable is binary (a 'success/failure' flag is used to determine this), the function flips a fraction of the response variable (changes from 0 to 1 or from 1 to 0). For non-binary variables, the function adds normally distributed noise to the transformed response. After adding noise, the inverse of the transformation is applied to the noisy response to bring it back to the original scale.

      The permuted noise is created by randomly shuffling the order of the noise values and then adding this noise to the response variable.

      The coefficients, standard errors, and p-values of the original model are compared to those of the models fitted with a noisy response. Significant differences between these models may suggest a lack of statistical stability.",
      interpretation = "The statistical stability results can be used to assess the sensitivity of the model to random variations in the data. If the coefficients, standard errors, and p-values of the original model are similar to those of the models fitted with a noisy response, then the model is considered to be statistically stable. However, if there are significant differences between these models, then the model may not be statistically stable.

      In general, a model with good statistical stability is less likely to be affected by random variations in the data. This means that the model is more likely to produce consistent results when applied to different samples of data with different noise levels."
    ),
    "Stability under Data Selection" = list(
      definition = "This involves considering variations in the data by either omitting objects from the dataset or excluding variables from the subsequent analysis. This can correspond to the rejection of outliers and the application of resampling techniques. In this context, resampling techniques can be thought of as a form of replication stability, but without formally sampling a new dataset (Michailides and de Leeuw, 1998).",
      explanation = "The 'gifistab' package's stability under data selection function implements this definition by fitting the original model on three different datasets:

      1. Bootstrap data, created by sampling the original data with replacement. This ensures that the bootstrap data has the same sample size as the original data.

      2. Data with outliers removed, created by using robust multivariate Mahalanobis distances. If outlier identification using robust multivariate Mahalanobis distances fails, then an observation will be identified as an outlier if it is 3 standard deviations away from the mean on any of the variables in the data. Outliers are then removed from the data and the model is refitted.

      3. Stratified bootstrap data, created by first dividing the data into three random subsamples where each observation belongs to one and only one subsample. These subsamples are then used to create bootstrap samples of the same sample size as the original data.",
      interpretation = "The stability under data selection results can be used to assess the sensitivity of the model to outliers and sampling variability. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the models fitted to the bootstrap data, the data with outliers removed, and the stratified bootstrap data, then the model is considered stable under data selection. However, if there are significant differences between the original model and the models fitted to the bootstrap data, the data with outliers removed, or the stratified bootstrap data, then the model may not be stable under data selection.

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
      explanation = "The analytic and algebraic stability function in the 'gifistab' package implements this definition by calculating two condition numbers (kappa): one based on the L1 norm and another based on the Linf norm of the model. The condition number is a measure of multicollinearity, which is the degree to which the independent variables are correlated. High multicollinearity can make the model unstable under slight changes in the input.

      The L1 norm, also known as the Manhattan distance or taxicab norm, is the sum of the absolute values of the elements in a vector. The L1 norm-based condition number provides a measure of multicollinearity based on the maximum possible effect that a small change in a single variable can have on the estimated coefficients.

      The Linf norm, also known as the infinity norm or maximum norm, is the maximum absolute value in a vector. The Linf norm-based condition number provides a measure of multicollinearity based on the maximum possible effect that simultaneous small changes in all variables can have on the estimated coefficients.

      The function calculates the condition numbers using the 'kappa' function from base R, which calculates the ratio of the largest to smallest singular values of the model's predictor matrix. These condition numbers are returned in a list, with the L1 norm condition number labeled as 'L1' and the Linf norm condition number labeled as 'Linf'.

      A high condition number indicates that the model is sensitive to changes in the independent variables. This is because small changes in the independent variables can lead to large changes in the model coefficients. The function also prints a warning if either condition number is above 30, as this is considered to be the threshold for severe multicollinearity.",
      interpretation = "The results of the analytic and algebraic stability function can be used to assess the robustness of a model to multicollinearity. If either condition number is high, then the model may be unstable under slight changes in the independent variables. However, if both condition numbers are low, then the model is considered to be robust. In general, both the L1 and Linf norms provide useful information about multicollinearity, but they emphasize different aspects of the data. The L1 norm is more sensitive to outliers because it considers the sum of all the values, while the Linf norm is determined by the maximum value."
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
