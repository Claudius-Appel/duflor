% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rectangularRange_HSV.R
\name{rectangularRange_HSV}
\alias{rectangularRange_HSV}
\title{find pixels with color-values lying between upper_bound and lower_bound}
\usage{
rectangularRange_HSV(pixel.array, upper_bound, lower_bound, check_V = FALSE)
}
\arguments{
\item{pixel.array}{image array as loaded via duflor::load_image()}

\item{upper_bound}{EITHER:
\itemize{
\item list of upper HSV-bounds, e.g. \code{list(green = c(H_green_lower,S_green_lower,V_green_lower),drought = c(H_drought_lower,S_drought_lower,V_drought_lower))}
\item single vector of length 3 declaring a set of HSV-values
}}

\item{lower_bound}{see \code{upper_bound}}

\item{check_V}{boolean toggle to also check the \code{VALUE}-component of an HSV-pixel}
}
\value{
upon success, returns a list-object with the following elements:
\itemize{
\item \code{pixel.idx} - pixel-locations of pixels detected between lower and upper bound.
\item \code{pixel.count} - number of pixels detected between lower and upper bound
\item \code{img.fraction} - fraction of image detected between lower and upper bound
\item \code{original.img} - fed-in pixel.array
}

Upon failure to find any matching pixels, an empty matrix of dimensions \verb{[0, 1:2]} is returned
}
\description{
Takes HSV-formatted pixel.array and bounds.
When determining which pixels lie within the bounds, only the \code{hue}- and \code{saturation}-
values are respected. The \code{value}-component is not considered.
}
\note{
The use of  \code{\link[=rectangularRange_HSV_cpp]{rectangularRange_HSV_cpp()}} is strongly suggested due to its drastically faster execution.
Adopted from \link[countcolors:rectangularRange]{countcolors::rectangularRange}, reworked to work in HSV color
space instead of RGB and simplified.
}
\examples{
\dontrun{
rectangularRange_HSV(
    pixel.array = pixel.array,
    lower_bound = an_lower_bound,
    upper_bound = an_upper_bound
)
}
}
