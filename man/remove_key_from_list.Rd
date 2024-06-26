% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_keys_from_list.R
\name{remove_key_from_list}
\alias{remove_key_from_list}
\title{remove keys from a list by name}
\usage{
remove_key_from_list(list, keys)
}
\arguments{
\item{list}{A normal list}

\item{keys}{A vector of keys to be removed from \code{list}}
}
\value{
\code{list} without the members of  \code{keys}
}
\description{
remove keys from a list by name
}
\examples{
list <- list(A = 1, B = "water", C = "0", D = 99)
duflor:::remove_key_from_list(list = list,c("C","D"))
}
\keyword{internal}
