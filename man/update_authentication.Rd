% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authentication.R
\name{update_authentication}
\alias{update_authentication}
\title{Stores Login Credentials}
\usage{
update_authentication(static = FALSE)
}
\arguments{
\item{static}{TRUE only displays the results without prompting for updates unless none is found.}
}
\value{

}
\description{
Stores login credentials to VAIRKKO securely on the system keyring.
See ?keyring for details on system credential stores.
}
