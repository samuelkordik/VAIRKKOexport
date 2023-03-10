% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/behavioral.R
\name{export_behavioral_incidents}
\alias{export_behavioral_incidents}
\title{Exports Behavioral Incidents}
\usage{
export_behavioral_incidents(limit = NULL)
}
\arguments{
\item{limit}{optional for how many incidents to limit scraping details to.}
}
\value{
Tibble of incidents
}
\description{
Retrieves behavioral incidents, with nested tibbles for heirarchal internal data.
}
