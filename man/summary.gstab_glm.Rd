% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gstab_summary.R
\name{summary.gstab_glm}
\alias{summary.gstab_glm}
\title{Summary for Stability Analysis for Generalized Linear Models}
\usage{
\method{summary}{gstab_glm}(object, conf.int, conf.level, ...)
}
\arguments{
\item{object}{An object of class "gstab_glm", usually, a result of a call to gstab.glm.}

\item{conf.int}{Logical variable. If TRUE, include a confidence interval in
the summary.}

\item{conf.level}{Confidence level for confidence interval when conf.int is
TRUE.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A list with the summary of the original model and the summaries of each stability check.
}
\description{
This function produces a summary of the stability analysis for a generalized linear model.
}
\keyword{internal}
