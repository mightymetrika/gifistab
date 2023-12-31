% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stability_plotters.R
\name{create_plot}
\alias{create_plot}
\title{Create Stability Assessment Plot}
\usage{
create_plot(plot_df, conf.int)
}
\arguments{
\item{plot_df}{A data frame containing the data to be used for plotting.
This data frame should contain columns 'term', 'estimate', 'type', and if
\code{conf.int} is \code{TRUE}, 'conf.low' and 'conf.high'.}

\item{conf.int}{Logical variable. If \code{TRUE}, add error bars to the points in
the plot, representing confidence intervals.}
}
\value{
A ggplot2 object of the stability assessment plot.
}
\description{
This internal function creates a ggplot object for the stability assessment.
It plots estimates for each term in the model and differentiates types with
color. Additionally, if \code{conf.int} is \code{TRUE}, it adds error bars to the points,
representing confidence intervals.
}
\keyword{internal}
