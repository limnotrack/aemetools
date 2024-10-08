% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_web_sf.R
\name{read_web_sf}
\alias{read_web_sf}
\title{Read spatial feature (sf) data from online databases as sf object}
\usage{
read_web_sf(url, layer_id, key = NULL)
}
\arguments{
\item{url}{character, URL of the web service}

\item{layer_id}{numeric; layer ID value for the raster layer on the LINZ
data service. See \url{https://data.linz.govt.nz/}}

\item{key}{character; LINZ API key. This can be set as an environment variable
using the \code{add_linz_key()} function or passed as a character.
See \code{?add_linz_key} for more information with setting up the API key.}
}
\value{
sf object
}
\description{
Read spatial feature (sf) data from online databases as sf object
}
