#' find pixels with color-values lying between upper_bound and lower_bound
#'
#' Takes HSV-formatted pixel.array and bounds.
#' When determining which pixels lie within the bounds, only the `hue`- and `saturation`-
#' values are respected. The `value`-component is not considered.
#' @inheritParams .main_args
#'
#' @return list-object with the following elements:
#' - `pixel.idx` - pixel-locations of pixels detected between lower and upper bound.
#' - `pixel.count` - number of pixels detected between lower and upper bound
#' - `img.fraction` - fraction of image detected between lower and upper bound
#' - `original.img` - fed-in pixel.array
#' @export
#'
#' @examples
#' \dontrun{
#' rectangularRange_HSV(
#'     pixel.array = pixel.array,
#'     lower_bound = an_lower_bound,
#'     upper_bound = an_upper_bound
#' )
#' }
rectangularRange_HSV <- function(pixel.array, upper_bound, lower_bound) {
    # pixel.array[X,Y,1,[H]]
    # pixel.array[X,Y,1,[S]]
    # pixel.array[X,Y,1,[V]]
    # j <- (lower_bound[1] <= pixel.array[, , 1, 1] & pixel.array[, , 1, 1] <= upper_bound[1])
    # k <- (lower_bound[2] <= pixel.array[, , 1, 2] & pixel.array[, , 1, 2] <= upper_bound[2])
    idx <- which((lower_bound[1] <= pixel.array[, , 1, 1] & pixel.array[, , 1, 1] <= upper_bound[1])
                 & (lower_bound[2] <= pixel.array[, , 1, 2] & pixel.array[, , 1, 2] <= upper_bound[2])
                 # & (lower_bound[3] <= pixel.array[, , 1, 3] & pixel.array[, , 1, 3] <= upper_bound[3])
                 ,arr.ind = TRUE
    )
    if (length(idx) == 0) { # no pixels match the requirements.
        return(list(
            pixel.idx = 0,
            pixel.count = 0,
            img.fraction = NA,
            original.img = pixel.array
        ))
    }
    return.list <- list(
        pixel.idx = idx,
        pixel.count = nrow(idx),
        img.fraction = nrow(idx) / (nrow(pixel.array) * ncol(pixel.array))
    )
    return(return.list)
}

