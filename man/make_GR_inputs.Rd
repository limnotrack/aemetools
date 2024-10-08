% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_GR_inputs.R
\name{make_GR_inputs}
\alias{make_GR_inputs}
\title{Make inputs for the GR models}
\usage{
make_GR_inputs(
  id,
  reaches,
  lake,
  catchments,
  obs_flow = NULL,
  met,
  lat = NULL,
  FUN_MOD = airGR::RunModel_GR6J,
  plot = FALSE
)
}
\arguments{
\item{id}{numeric; Reach ID}

\item{reaches}{sf; object with reaches as linestrings.}

\item{lake}{sf; polygon of lake shore.}

\item{catchments}{sf; polygon of catchmentss.}

\item{obs_flow}{dataframe; containing Date and flow in m3/s.}

\item{met}{dataframe; containing Date, air temperature and precipitation}

\item{lat}{numeric; latitude. If NULL, uses the latitude from the centre
of the lake.}

\item{FUN_MOD}{function from the \code{airGR} package to be used. Defaults to
\code{airGR::RunModel_GR6J}}

\item{plot}{logical; plot the reaches, lake and catchment? Defaults to
FALSE.}
}
\value{
list of inputs for \code{run_GR()}.
}
\description{
Make inputs for the GR models
}
