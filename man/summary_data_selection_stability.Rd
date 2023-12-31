% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stability_summarizers.R
\name{summary_data_selection_stability}
\alias{summary_data_selection_stability}
\title{Summarize Results of Stability Under Data Selection Assessment}
\usage{
summary_data_selection_stability(obj, conf.int, conf.level)
}
\arguments{
\item{obj}{A list containing \code{bootstrap_model} fitted on resampled data,
\code{no_outlier_model} fitted on data with outliers removed, and
\code{strata_boot_models} list of models fitted on stratified bootstrap data.}

\item{conf.int}{Logical, if \code{TRUE} confidence intervals for the estimates are
returned.}

\item{conf.level}{The confidence level to use for the confidence intervals.}
}
\value{
A list with three elements:
\itemize{
\item 'bootstrap_model': A tidy data frame summarizing the model fitted on
resampled data.
\item 'no_outlier_model': A tidy data frame summarizing the model fitted on data
with outliers removed.
\item 'strata_boot_model': A list of three stratified bootstrap fitted models
\itemize{
\item 'strata_boot_model1': A tidy data frame summarizing the first model fitted
on stratified bootstrap data.
\item 'strata_boot_model2': A tidy data frame summarizing the second model fitted
on stratified bootstrap data.
\item 'strata_boot_model3': A tidy data frame summarizing the third model fitted
on stratified bootstrap data.
}
}
}
\description{
This function summarizes the results of a stability under data selection
assessment. It creates tidy data frames for the models fitted on resampled data,
data with outliers removed, and stratified bootstrap data.
}
\keyword{internal}
