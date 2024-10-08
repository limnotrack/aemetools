% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_uncertainty.R
\name{plot_uncertainty}
\alias{plot_uncertainty}
\title{Plot the uncertainty of the output of a sensitivity analysis}
\usage{
plot_uncertainty(sa, na.rm = TRUE, bins = 30)
}
\arguments{
\item{sa}{list; of sensitivity analysis results read in with \code{\link{read_sa}}}

\item{na.rm}{a logical evaluating to \code{TRUE} or \code{FALSE}
    indicating whether \code{NA} values should be stripped before the
    computation proceeds.}

\item{bins}{integer; number of bins for the histogram. Default is 30.}
}
\value{
\code{ggplot} object
}
\description{
Plot the uncertainty of the output of a sensitivity analysis
}
