% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{convert_to_isodecimal}
\alias{convert_to_isodecimal}
\title{Convert any recognised ISO11784/5 format to Decimal format}
\usage{
convert_to_isodecimal(.data, format = NULL)
}
\arguments{
\item{.data}{A vector of character strings}

\item{format}{a string indicating which format the ID is in, intended to be used with output from get_iso11784_format
c('unknown', 'isodecimal', 'isodothex','iso64bitl', 'iso64bitr')}
}
\value{
A converted vector of character strings
}
\description{
Convert any recognised ISO11784/5 format to Decimal format
}
\examples{
convert_to_isodecimal(c('3E7.02DFDC1C35','8000F9C2DFDC1C36','EC383BFB439F0001'))
}
