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

      Moreover, the proportion of statistically significant results in the `boot_mean_df` output reflects the general trend of statistical significance across bootstrap samples. A high proportion suggests that the term is often statistically significant, while a low proportion suggests that the term is rarely significant. This information can be used to assess the consistency of a term's significance in the model.",
      caution = "While replication stability provides valuable insights, several factors should be considered. Firstly, the quality and characteristics of the new dataset greatly influence the results. If the new dataset is not representative of the same population as the original dataset, or if it varies significantly in size, the comparison between the original model and the model fitted on the new dataset might not accurately reflect the model's stability.

      Secondly, the bootstrap resampling procedure introduces randomness, leading to variability in the results across different runs. Therefore, a substantial number of bootstrap resamples (commonly 1000 or more) should be used to provide a more robust estimate of the model's replication stability. Furthermore, rerunning the bootstrap procedure with different random seed settings can be useful to verify the consistency of the results. If results vary dramatically with different seeds, this could indicate instability in the model.

      Thirdly, this stability assessment assumes that the relationships between the variables remain constant over time. Changes in these relationships between the time when the original data and the new data are collected could reflect changes in the underlying relationships rather than a lack of model stability.

      Lastly, the interpretation of the `boot_mean_df` and `boot_sd_df` outputs heavily relies on the assumption that the bootstrap samples are representative of the original data. If this assumption is violated, for example due to high skewness or kurtosis in the data, these bootstrap metrics may not provide a reliable measure of the model's stability.

      Therefore, while assessing replication stability, consider these factors and interpret the results accordingly."
    ),
    "Statistical Stability" = list(
      definition = "It refers to the stability of the analysis whenever no new data set is formally sampled (Michailides and de Leeuw, 1998).",
      explanation = "The statistical stability function in the 'gifistab' package implements this definition by fitting the original model on the data with random noise and with permuted noise added to the response variable while taking into consideration the family of the model (i.e., Gaussian, gamma, inverse Gaussian, Poisson, quasi-Poisson, binomial, quasi-binomial).

      The function determines the family of the model and applies the appropriate transformation to the response variable. For Gaussian, gamma, and inverse Gaussian families, the function uses an identity or log transformation; for Poisson and quasi-Poisson, a log transformation; and for binomial and quasi-binomial, a logit transformation or a binary flip depending on whether the response variable is a proportion or binary.

      If the response variable is binary (a 'success/failure' flag is used to determine this), the function flips a fraction of the response variable (changes from 0 to 1 or from 1 to 0). For non-binary variables, the function adds normally distributed noise to the transformed response. After adding noise, the inverse of the transformation is applied to the noisy response to bring it back to the original scale.

      The permuted noise is created by randomly shuffling the order of the noise values and then adding this noise to the response variable.

      The coefficients, standard errors, and p-values of the original model are compared to those of the models fitted with a noisy response. Significant differences between these models may suggest a lack of statistical stability.",
      interpretation = "The statistical stability results can be used to assess the sensitivity of the model to random variations in the data. If the coefficients, standard errors, and p-values of the original model are similar to those of the models fitted with a noisy response, then the model is considered to be statistically stable. However, if there are significant differences between these models, then the model may not be statistically stable.

      In general, a model with good statistical stability is less likely to be affected by random variations in the data. This means that the model is more likely to produce consistent results when applied to different samples of data with different noise levels.",
      caution = "While the concept of statistical stability is important, several cautions should be considered when interpreting the results. Firstly, this stability assessment is sensitive to the noise factor `nf`. Too large a noise factor can lead to models that are significantly different from the original model, while too small a noise factor might not adequately test the model's stability. The choice of noise factor should be guided by the context and the nature of the data.

      Secondly, the method assumes the noise is normally distributed, which might not always be the case. If the residuals of the original model are not normally distributed, the noise added to the response variable will not correctly reflect the variability in the data, potentially leading to incorrect conclusions about the model's stability.

      Thirdly, for binomial families with a binary response, the function flips a fraction of the response variable. This is a rather crude way of introducing variability, and it might not reflect the true nature of variability in binary response data.

      Therefore, when interpreting the results from the statistical stability assessment, consider these factors and the assumptions made by this method."
    ),
    "Stability under Data Selection" = list(
      definition = "This involves considering variations in the data by either omitting objects from the dataset or excluding variables from the subsequent analysis. This can correspond to the rejection of outliers and the application of resampling techniques. In this context, resampling techniques can be thought of as a form of replication stability, but without formally sampling a new dataset (Michailides and de Leeuw, 1998).",
      explanation = "The 'gifistab' package's stability under data selection function implements this definition by fitting the original model on three different datasets:

      1. Bootstrap data, created by sampling the original data with replacement. This ensures that the bootstrap data has the same sample size as the original data.

      2. Data with outliers removed, created by using robust multivariate Mahalanobis distances. If outlier identification using robust multivariate Mahalanobis distances fails, then an observation will be identified as an outlier if it is 3 standard deviations away from the mean on any of the variables in the data. Outliers are then removed from the data and the model is refitted.

      3. Stratified bootstrap data, created by first dividing the data into three random subsamples where each observation belongs to one and only one subsample. These subsamples are then used to create bootstrap samples of the same sample size as the original data.",
      interpretation = "The stability under data selection results can be used to assess the sensitivity of the model to outliers and sampling variability. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the models fitted to the bootstrap data, the data with outliers removed, and the stratified bootstrap data, then the model is considered stable under data selection. However, if there are significant differences between the original model and the models fitted to the bootstrap data, the data with outliers removed, or the stratified bootstrap data, then the model may not be stable under data selection.

      In general, a model with good stability under data selection is less likely to be affected by outliers or sampling variability.",
      caution = "Although stability under data selection provides a valuable perspective on the robustness of a model, there are several considerations to keep in mind when interpreting the results.

      Firstly, this method uses resampling techniques to create bootstrap and stratified bootstrap data. However, resampling is a random process, and different resamples may lead to different results. As such, you might consider running the assessment multiple times with different seeds for the random number generator to understand the variability in the results.

      Secondly, the function uses robust multivariate Mahalanobis distances to identify outliers, with a fallback to a simpler method of considering any observation more than three standard deviations from the mean as an outlier. Both methods have limitations and may not identify all outliers, particularly in complex or high-dimensional data. In addition, the removal of outliers is not always the best strategy, especially if those outliers are important or informative for the analysis.

      Lastly, the stratified bootstrap procedure divides the data into three subsamples, and the number of strata is fixed. This might not always be optimal, especially for large datasets or datasets with a strong structure or grouping. In such cases, a different stratification strategy might be more appropriate.

      Therefore, when interpreting the results from the stability under data selection assessment, consider these factors and the assumptions made by this method."
    ),
    "Stability under Model Selection" = list(
      definition = "Small changes in the model should result in small changes in the results obtained (Michailides and de Leeuw, 1998).",
      explanation = "The stability under model selection function in the 'gifistab' package implements this definition by assessing the stability of a model to three types of changes:

      Toggling the intercept: This involves removing the intercept from a model that includes the intercept or adding the intercept to a model that excludes the intercept. After toggling the intercept, the model is refit. If the results of the two models are similar, then the model is considered to be stable to changes in the intercept.

      Removing a variable: This involves removing a user-selected variable from the model and then re-fitting the model. If the results of the two models are similar, then the model is considered to be stable with or without controlling for the user-selected variable.

      Removing variables through backward selection: This involves selecting a variable of interest, fitting a full model with all variables, and then using a backward stepwise selection procedure guided by the Akaike information criterion (AIC) to remove variables iteratively. This process continues until no further improvement in AIC is possible, while preserving the variable of interest. If the results of the full model and the selected model are similar, then the model is considered to be stable to changes in the model specification.",
      interpretation = "The results of the stability under model selection function can be used to assess the robustness of a model to changes in the model specification. If the results of the function show that the model is stable to changes in the intercept, the user-selected variable to remove, and changes in model specification through backward selection, then the model is considered to be robust. However, if the results of the function show that the model is not stable to these changes, then the model may be sensitive to changes in the data and may not be a reliable predictor. While this stability assessment is designed for the situation where the user wishes to make inferences on a variable of interest after controlling for other variables in the model, it may also provide useful stability information for situations where the user wishes to interpret multiple coefficients.",
      caution = "While the stability under model selection provides valuable insights into the robustness of a model, it is important to keep the following considerations in mind:

      Firstly, the function toggles the intercept of the model, which means it either adds or removes the intercept. However, adding or removing the intercept changes the interpretation of the model. Without an intercept, the model is forced to go through the origin, which may not be suitable for all datasets or research questions. Be sure to consider the implications of this change for your specific analysis.

      Secondly, the function removes a specified variable from the model to check the effect of its removal. The choice of the variable to be removed could significantly influence the stability result. Be cautious when interpreting this result as it is contingent on the choice of the variable.

      Thirdly, the function uses backward selection based on the Akaike Information Criterion (AIC) to remove variables from the model. This is a greedy algorithm that makes the best choice at each step as it tries to find a local minimum, but it may not always lead to the global minimum. This means that the final model may not be the absolute best model.

      Lastly, the backward selection process is based on the Akaike Information Criterion (AIC) and preserves the variable(s) of interest. It's worth mentioning that AIC is just one of many possible criteria for model selection, and different criteria may lead to different selected models.

      Therefore, while stability under model selection is a valuable tool for assessing the robustness of a model, it is important to consider these factors when interpreting the results."
    ),
    "Numerical Stability" = list(
      definition = "It refers to the influence of rounding errors and of computation with limited precision on the results given by the techniques (Michailides and de Leeuw, 1998).",
      explanation = "The numerical stability function in the 'gifistab' package implements this definition by fitting the model on data that has been perturbed by a small amount of random noise. This assesses the impact of rounding errors and limited precision computations on the model.

      The function works by first fitting the model on the original data. Then, it perturbs the data by adding a small amount of normally distributed random noise. The amount of noise is specified by the .Machine$double.eps^0.5 argument. This argument is the square root of the machine epsilon, which is the smallest positive number that can be added to 1 without changing the value of 1.

      Once the data has been perturbed, the function fits the model again. The results of the two fits are then compared. If the results are similar, then the model is considered to be numerically stable. However, if the results are different, then the model may be numerically unstable.",
      interpretation = "The results of the numerical stability function can be used to assess the robustness of a model to rounding errors and limited precision computations. If the results of the function show that the model is numerically stable, then the model is considered to be robust. However, if the results of the function show that the model is not numerically stable, then the model may be sensitive to rounding errors and may not be a reliable predictor.",
      caution = "While the numerical stability function provides useful insights into the robustness of a model to rounding errors and limited precision computations, it is important to keep the following considerations in mind:

      Firstly, the function introduces a small amount of normally distributed random noise to the data. The magnitude of this noise is determined by the square root of the machine epsilon, which is the smallest positive number that can be added to 1 without changing the value of 1. While this is a small value, it might still introduce significant perturbations in the data, especially if the scale of the numeric variables is small. Be sure to consider the implications of this for your specific analysis.

      Secondly, the function only perturbs the numeric variables in the data. If the data contains categorical variables (factors), these are not perturbed. Therefore, the function may not fully assess the numerical stability of models that heavily rely on categorical variables.

      Lastly, if the family of the model is binomial, quasibinomial, poisson, or quasipoisson, the response variable is not perturbed. Therefore, the numerical stability function might not provide a full assessment of the model's stability if the response variable is sensitive to small perturbations.

      As such, while numerical stability is a valuable tool for assessing the robustness of a model, it is important to interpret the results in the context of these factors."
    ),
    "Analytic and Algebraic Stability" = list(
      definition = "If the data structures and possible representations have enough mathematical structure, then formal expressions of the input-output analysis can be drawn from considering perturbations of the input (Michailides and de Leeuw, 1998).",
      explanation = "The analytic and algebraic stability function in the 'gifistab' package implements this definition by calculating two condition numbers (kappa): one based on the L1 norm and another based on the Linf norm of the model. The condition number is a measure of multicollinearity, which is the degree to which the independent variables are correlated. High multicollinearity can make the model unstable under slight changes in the input.

      The L1 norm, also known as the Manhattan distance or taxicab norm, is the sum of the absolute values of the elements in a vector. The L1 norm-based condition number provides a measure of multicollinearity based on the maximum possible effect that a small change in a single variable can have on the estimated coefficients.

      The Linf norm, also known as the infinity norm or maximum norm, is the maximum absolute value in a vector. The Linf norm-based condition number provides a measure of multicollinearity based on the maximum possible effect that simultaneous small changes in all variables can have on the estimated coefficients.

      The function calculates the condition numbers using the 'kappa' function from base R, which calculates the ratio of the largest to smallest singular values of the model's predictor matrix. These condition numbers are returned in a list, with the L1 norm condition number labeled as 'L1' and the Linf norm condition number labeled as 'Linf'.

      A high condition number indicates that the model is sensitive to changes in the independent variables. This is because small changes in the independent variables can lead to large changes in the model coefficients. The function also prints a warning if either condition number is above 30, as this is considered to be the threshold for severe multicollinearity.",
      interpretation = "The results of the analytic and algebraic stability function can be used to assess the robustness of a model to multicollinearity. If either condition number is high, then the model may be unstable under slight changes in the independent variables. However, if both condition numbers are low, then the model is considered to be robust. In general, both the L1 and Linf norms provide useful information about multicollinearity, but they emphasize different aspects of the data. The L1 norm is more sensitive to outliers because it considers the sum of all the values, while the Linf norm is determined by the maximum value.",
      caution = "While the analytic and algebraic stability function is a valuable tool for assessing the stability of a model, there are a few important considerations to keep in mind:

      Firstly, the condition numbers computed by the function are measures of multicollinearity, which is the degree to which the independent variables in a model are correlated. However, they do not provide direct measures of stability or instability. A high condition number indicates the presence of multicollinearity, but it does not automatically mean the model is unstable. Conversely, a low condition number does not guarantee stability. Additional diagnostic checks are typically needed.

      Secondly, the function computes condition numbers based on the L1 and Linf norms of the model matrix. These norms capture different aspects of the data and can give different results. It is important to interpret both condition numbers in the context of the specific analysis.

      Thirdly, the function issues a warning if either condition number exceeds 30, which is a commonly used threshold for severe multicollinearity. However, this threshold is somewhat arbitrary, and the seriousness of multicollinearity can depend on the specific context of the analysis.

      Therefore, while the results from the analytic and algebraic stability function can provide useful insights into the numerical properties of a model, they should be interpreted as part of a broader analysis of model stability and robustness."
    ),
    "Stability under Selection of Technique" = list(
      definition = "Application of a number of different techniques to the same data set, aiming at answering the same question, results in approximately the same information (Michailides and de Leeuw, 1998).",
      explanation = "The stability under selection of technique function in the 'gifistab' package implements this definition by fitting a robust regression model using the same specification as the original model. This helps in assessing the stability of the model under different fitting techniques.

      The function works by first fitting the original model. Then, it fits a robust regression model using the same specification as the original model. The robust regression model is fitted using the 'lmrob' function from the 'robustbase' R package.

      The results of the two models are then compared. If the results are similar, then the model is considered to be stable under different fitting techniques. However, if the results are different, then the model may be unstable under different fitting techniques.",
      interpretation = "The results of the stability under selection of technique function can be used to assess the robustness of a model to different fitting techniques. If the results of the two models are similar, then the model is considered to be robust. However, if the results of the two models are different, then the model may be unstable under different fitting techniques.",
      caution = "While the stability under selection of technique function is a valuable tool for assessing the stability of a model, there are a few important considerations to keep in mind:

      Firstly, the function assesses stability by comparing the results of a regular regression model and a robust regression model. While this can provide valuable insights, it's important to remember that these are just two of many possible modeling techniques. The function does not consider other potential techniques, such as non-linear models, ensemble models, or non-parametric methods.

      Secondly, robust regression methods are designed to be less sensitive to outliers and high leverage observations than regular regression. This means that differences between the regular and robust models could be due to these types of observations. Before concluding that a model is unstable under different techniques, it could be helpful to examine the data for outliers and high leverage points.

      Thirdly, the function uses the 'lmrob' and 'glmrob' functions from the 'robustbase' package for fitting the robust models. These functions use specific algorithms and settings for robust regression. Different results might be obtained with other robust regression methods or settings.

      Finally, the function returns NULL if the robust regression model could not be fitted. This could happen for a variety of reasons, including numerical issues or problems with the data. If the function returns NULL, it's important to investigate why the robust model could not be fitted.

      Therefore, while the results from the stability under selection of technique function can provide useful insights into the stability of a model, they should be interpreted as part of a broader analysis of model stability and robustness."
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

      Moreover, the proportion of statistically significant results in the `boot_mean_df` output reflects the general trend of statistical significance across bootstrap samples. A high proportion suggests that the term is often statistically significant, while a low proportion suggests that the term is rarely significant. This information can be used to assess the consistency of a term's significance in the model.",
      caution = "While replication stability provides valuable insights, several factors should be considered. Firstly, the quality and characteristics of the new dataset greatly influence the results. If the new dataset is not representative of the same population as the original dataset, or if it varies significantly in size, the comparison between the original model and the model fitted on the new dataset might not accurately reflect the model's stability.

      Secondly, the bootstrap resampling procedure introduces randomness, leading to variability in the results across different runs. Therefore, a substantial number of bootstrap resamples (commonly 1000 or more) should be used to provide a more robust estimate of the model's replication stability. Furthermore, rerunning the bootstrap procedure with different random seed settings can be useful to verify the consistency of the results. If results vary dramatically with different seeds, this could indicate instability in the model.

      Thirdly, this stability assessment assumes that the relationships between the variables remain constant over time. Changes in these relationships between the time when the original data and the new data are collected could reflect changes in the underlying relationships rather than a lack of model stability.

      Lastly, the interpretation of the `boot_mean_df` and `boot_sd_df` outputs heavily relies on the assumption that the bootstrap samples are representative of the original data. If this assumption is violated, for example due to high skewness or kurtosis in the data, these bootstrap metrics may not provide a reliable measure of the model's stability.

      Therefore, while assessing replication stability, consider these factors and interpret the results accordingly."
    ),
    "Statistical Stability" = list(
      definition = "It refers to the stability of the analysis whenever no new data set is formally sampled (Michailides and de Leeuw, 1998).",
      explanation = "The statistical stability function in the 'gifistab' package implements this definition by fitting the original model on the data with random noise and with permuted noise added to the response variable while taking into consideration the family of the model (i.e., Gaussian, gamma, inverse Gaussian, Poisson, quasi-Poisson, binomial, quasi-binomial).

      The function determines the family of the model and applies the appropriate transformation to the response variable. For Gaussian, gamma, and inverse Gaussian families, the function uses an identity or log transformation; for Poisson and quasi-Poisson, a log transformation; and for binomial and quasi-binomial, a logit transformation or a binary flip depending on whether the response variable is a proportion or binary.

      If the response variable is binary (a 'success/failure' flag is used to determine this), the function flips a fraction of the response variable (changes from 0 to 1 or from 1 to 0). For non-binary variables, the function adds normally distributed noise to the transformed response. After adding noise, the inverse of the transformation is applied to the noisy response to bring it back to the original scale.

      The permuted noise is created by randomly shuffling the order of the noise values and then adding this noise to the response variable.

      The coefficients, standard errors, and p-values of the original model are compared to those of the models fitted with a noisy response. Significant differences between these models may suggest a lack of statistical stability.",
      interpretation = "The statistical stability results can be used to assess the sensitivity of the model to random variations in the data. If the coefficients, standard errors, and p-values of the original model are similar to those of the models fitted with a noisy response, then the model is considered to be statistically stable. However, if there are significant differences between these models, then the model may not be statistically stable.

      In general, a model with good statistical stability is less likely to be affected by random variations in the data. This means that the model is more likely to produce consistent results when applied to different samples of data with different noise levels.",
      caution = "While the concept of statistical stability is important, several cautions should be considered when interpreting the results. Firstly, this stability assessment is sensitive to the noise factor `nf`. Too large a noise factor can lead to models that are significantly different from the original model, while too small a noise factor might not adequately test the model's stability. The choice of noise factor should be guided by the context and the nature of the data.

      Secondly, the method assumes the noise is normally distributed, which might not always be the case. If the residuals of the original model are not normally distributed, the noise added to the response variable will not correctly reflect the variability in the data, potentially leading to incorrect conclusions about the model's stability.

      Thirdly, for binomial families with a binary response, the function flips a fraction of the response variable. This is a rather crude way of introducing variability, and it might not reflect the true nature of variability in binary response data.

      Therefore, when interpreting the results from the statistical stability assessment, consider these factors and the assumptions made by this method."
    ),
    "Stability under Data Selection" = list(
      definition = "This involves considering variations in the data by either omitting objects from the dataset or excluding variables from the subsequent analysis. This can correspond to the rejection of outliers and the application of resampling techniques. In this context, resampling techniques can be thought of as a form of replication stability, but without formally sampling a new dataset (Michailides and de Leeuw, 1998).",
      explanation = "The 'gifistab' package's stability under data selection function implements this definition by fitting the original model on three different datasets:

      1. Bootstrap data, created by sampling the original data with replacement. This ensures that the bootstrap data has the same sample size as the original data.

      2. Data with outliers removed, created by using robust multivariate Mahalanobis distances. If outlier identification using robust multivariate Mahalanobis distances fails, then an observation will be identified as an outlier if it is 3 standard deviations away from the mean on any of the variables in the data. Outliers are then removed from the data and the model is refitted.

      3. Stratified bootstrap data, created by first dividing the data into three random subsamples where each observation belongs to one and only one subsample. These subsamples are then used to create bootstrap samples of the same sample size as the original data.",
      interpretation = "The stability under data selection results can be used to assess the sensitivity of the model to outliers and sampling variability. If the coefficients, standard errors, and p-values of the original model are similar to the coefficients, standard errors, and p-values of the models fitted to the bootstrap data, the data with outliers removed, and the stratified bootstrap data, then the model is considered stable under data selection. However, if there are significant differences between the original model and the models fitted to the bootstrap data, the data with outliers removed, or the stratified bootstrap data, then the model may not be stable under data selection.

      In general, a model with good stability under data selection is less likely to be affected by outliers or sampling variability.",
      caution = "Although stability under data selection provides a valuable perspective on the robustness of a model, there are several considerations to keep in mind when interpreting the results.

      Firstly, this method uses resampling techniques to create bootstrap and stratified bootstrap data. However, resampling is a random process, and different resamples may lead to different results. As such, you might consider running the assessment multiple times with different seeds for the random number generator to understand the variability in the results.

      Secondly, the function uses robust multivariate Mahalanobis distances to identify outliers, with a fallback to a simpler method of considering any observation more than three standard deviations from the mean as an outlier. Both methods have limitations and may not identify all outliers, particularly in complex or high-dimensional data. In addition, the removal of outliers is not always the best strategy, especially if those outliers are important or informative for the analysis.

      Lastly, the stratified bootstrap procedure divides the data into three subsamples, and the number of strata is fixed. This might not always be optimal, especially for large datasets or datasets with a strong structure or grouping. In such cases, a different stratification strategy might be more appropriate.

      Therefore, when interpreting the results from the stability under data selection assessment, consider these factors and the assumptions made by this method."
    ),
    "Stability under Model Selection" = list(
      definition = "Small changes in the model should result in small changes in the results obtained (Michailides and de Leeuw, 1998).",
      explanation = "Not currently implemented for stats::glm engine.",
      interpretation = "Not currently implemented for stats::glm engine.",
      caution = "Not currently implemented for stats::glm engine."
    ),
    "Numerical Stability" = list(
      definition = "It refers to the influence of rounding errors and of computation with limited precision on the results given by the techniques (Michailides and de Leeuw, 1998).",
      explanation = "The numerical stability function in the 'gifistab' package implements this definition by fitting the model on data that has been perturbed by a small amount of random noise. This assesses the impact of rounding errors and limited precision computations on the model.

      The function works by first fitting the model on the original data. Then, it perturbs the data by adding a small amount of normally distributed random noise. The amount of noise is specified by the .Machine$double.eps^0.5 argument. This argument is the square root of the machine epsilon, which is the smallest positive number that can be added to 1 without changing the value of 1.

      Once the data has been perturbed, the function fits the model again. The results of the two fits are then compared. If the results are similar, then the model is considered to be numerically stable. However, if the results are different, then the model may be numerically unstable.",
      interpretation = "The results of the numerical stability function can be used to assess the robustness of a model to rounding errors and limited precision computations. If the results of the function show that the model is numerically stable, then the model is considered to be robust. However, if the results of the function show that the model is not numerically stable, then the model may be sensitive to rounding errors and may not be a reliable predictor.",
      caution = "While the numerical stability function provides useful insights into the robustness of a model to rounding errors and limited precision computations, it is important to keep the following considerations in mind:

      Firstly, the function introduces a small amount of normally distributed random noise to the data. The magnitude of this noise is determined by the square root of the machine epsilon, which is the smallest positive number that can be added to 1 without changing the value of 1. While this is a small value, it might still introduce significant perturbations in the data, especially if the scale of the numeric variables is small. Be sure to consider the implications of this for your specific analysis.

      Secondly, the function only perturbs the numeric variables in the data. If the data contains categorical variables (factors), these are not perturbed. Therefore, the function may not fully assess the numerical stability of models that heavily rely on categorical variables.

      Lastly, if the family of the model is binomial, quasibinomial, poisson, or quasipoisson, the response variable is not perturbed. Therefore, the numerical stability function might not provide a full assessment of the model's stability if the response variable is sensitive to small perturbations.

      As such, while numerical stability is a valuable tool for assessing the robustness of a model, it is important to interpret the results in the context of these factors."
    ),
    "Analytic and Algebraic Stability" = list(
      definition = "If the data structures and possible representations have enough mathematical structure, then formal expressions of the input-output analysis can be drawn from considering perturbations of the input (Michailides and de Leeuw, 1998).",
      explanation = "The analytic and algebraic stability function in the 'gifistab' package implements this definition by calculating two condition numbers (kappa): one based on the L1 norm and another based on the Linf norm of the model. The condition number is a measure of multicollinearity, which is the degree to which the independent variables are correlated. High multicollinearity can make the model unstable under slight changes in the input.

      The L1 norm, also known as the Manhattan distance or taxicab norm, is the sum of the absolute values of the elements in a vector. The L1 norm-based condition number provides a measure of multicollinearity based on the maximum possible effect that a small change in a single variable can have on the estimated coefficients.

      The Linf norm, also known as the infinity norm or maximum norm, is the maximum absolute value in a vector. The Linf norm-based condition number provides a measure of multicollinearity based on the maximum possible effect that simultaneous small changes in all variables can have on the estimated coefficients.

      The function calculates the condition numbers using the 'kappa' function from base R, which calculates the ratio of the largest to smallest singular values of the model's predictor matrix. These condition numbers are returned in a list, with the L1 norm condition number labeled as 'L1' and the Linf norm condition number labeled as 'Linf'.

      A high condition number indicates that the model is sensitive to changes in the independent variables. This is because small changes in the independent variables can lead to large changes in the model coefficients. The function also prints a warning if either condition number is above 30, as this is considered to be the threshold for severe multicollinearity.",
      interpretation = "The results of the analytic and algebraic stability function can be used to assess the robustness of a model to multicollinearity. If either condition number is high, then the model may be unstable under slight changes in the independent variables. However, if both condition numbers are low, then the model is considered to be robust. In general, both the L1 and Linf norms provide useful information about multicollinearity, but they emphasize different aspects of the data. The L1 norm is more sensitive to outliers because it considers the sum of all the values, while the Linf norm is determined by the maximum value.",
      caution = "While the analytic and algebraic stability function is a valuable tool for assessing the stability of a model, there are a few important considerations to keep in mind:

      Firstly, the condition numbers computed by the function are measures of multicollinearity, which is the degree to which the independent variables in a model are correlated. However, they do not provide direct measures of stability or instability. A high condition number indicates the presence of multicollinearity, but it does not automatically mean the model is unstable. Conversely, a low condition number does not guarantee stability. Additional diagnostic checks are typically needed.

      Secondly, the function computes condition numbers based on the L1 and Linf norms of the model matrix. These norms capture different aspects of the data and can give different results. It is important to interpret both condition numbers in the context of the specific analysis.

      Thirdly, the function issues a warning if either condition number exceeds 30, which is a commonly used threshold for severe multicollinearity. However, this threshold is somewhat arbitrary, and the seriousness of multicollinearity can depend on the specific context of the analysis.

      Therefore, while the results from the analytic and algebraic stability function can provide useful insights into the numerical properties of a model, they should be interpreted as part of a broader analysis of model stability and robustness."
    ),
    "Stability under Selection of Technique" = list(
      definition = "Application of a number of different techniques to the same data set, aiming at answering the same question, results in approximately the same information (Michailides and de Leeuw, 1998).",
      explanation = "The stability under selection of technique function in the 'gifistab' package implements this definition by fitting a robust regression model using the same specification as the original model. This helps in assessing the stability of the model under different fitting techniques.

      The function works by first fitting the original model. Then, it fits a robust regression model using the same specification as the original model. The robust regression model is fitted using the 'lmrob' function from the 'robustbase' R package.

      The results of the two models are then compared. If the results are similar, then the model is considered to be stable under different fitting techniques. However, if the results are different, then the model may be unstable under different fitting techniques.",
      interpretation = "The results of the stability under selection of technique function can be used to assess the robustness of a model to different fitting techniques. If the results of the two models are similar, then the model is considered to be robust. However, if the results of the two models are different, then the model may be unstable under different fitting techniques.",
      caution = "While the stability under selection of technique function is a valuable tool for assessing the stability of a model, there are a few important considerations to keep in mind:

      Firstly, the function assesses stability by comparing the results of a regular regression model and a robust regression model. While this can provide valuable insights, it's important to remember that these are just two of many possible modeling techniques. The function does not consider other potential techniques, such as non-linear models, ensemble models, or non-parametric methods.

      Secondly, robust regression methods are designed to be less sensitive to outliers and high leverage observations than regular regression. This means that differences between the regular and robust models could be due to these types of observations. Before concluding that a model is unstable under different techniques, it could be helpful to examine the data for outliers and high leverage points.

      Thirdly, the function uses the 'lmrob' and 'glmrob' functions from the 'robustbase' package for fitting the robust models. These functions use specific algorithms and settings for robust regression. Different results might be obtained with other robust regression methods or settings.

      Finally, the function returns NULL if the robust regression model could not be fitted. This could happen for a variety of reasons, including numerical issues or problems with the data. If the function returns NULL, it's important to investigate why the robust model could not be fitted.

      Therefore, while the results from the stability under selection of technique function can provide useful insights into the stability of a model, they should be interpreted as part of a broader analysis of model stability and robustness."
    )
  )

  return(stability_definitions)
}
