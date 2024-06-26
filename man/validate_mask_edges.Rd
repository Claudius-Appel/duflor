% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_mask_edges.R
\name{validate_mask_edges}
\alias{validate_mask_edges}
\title{Check for a given mask pixel.idx-object if its edges are clean}
\usage{
validate_mask_edges(mask.idx, mask_name, array_dim, image.path)
}
\arguments{
\item{mask.idx}{pixel-locations of pixels detected for this mask}

\item{mask_name}{the mask's name, to print useful warnings}

\item{array_dim}{dimensions of the loaded \code{pixel.array}}

\item{image.path}{path to image-file}
}
\value{
list of booleans for each edge. \code{TRUE} values signify that a given boundary was infringed upon.
}
\description{
Check for a given spectrum's mask if the pixels on its edges are part of its
masked spectrum. That would suggest that the bounding area was set too restrictively, cutting
off pixels which are potentially valid for this mask.
}
\examples{
\dontrun{
# load a subset of an image
pixel.array <- duflor::load_image(image.path,
                                  subset_only = T,
                                  crop_left = 20,
                                  crop_right = 20,
                                  crop_bottom = 20,
                                  crop_top = 20
                                  )

# get pixels which belong to a given mask, and return the results for it
ret <- extract_pixels_HSV(pixel.array, lower_bound, upper_bound, F)

# check for each mask if its edges are valid.
for (mask in names(ret)) {
    duflor:::validate_mask_edges(ret[[mask]]$pixel.idx,mask, dim(pixel.array)[1:2],image.path)
}
}
}
