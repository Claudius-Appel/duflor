#' Extract pixels from image which fall inbetween lower- and upper bounds.
#'
#' @inheritParams .main_args
#' @param bundle_pixelarray logical, indicating if the input parameter `pixel.array` is to be bundled into the return-value
#' This is useful to retain `pixel.array` into the output if this function is called in a loop.
#' @param check_value boolean toggle to also check the `VALUE`-component of an HSV-pixel
#'
#' @return EITHER:
#' list-object with the following elements (when supplying one one pair of bounds)
#' - `pixel.idx` - pixel-locations of pixels detected between lower and upper bound.
#' - `pixel.count` - number of pixels detected between lower and upper bound
#' - `img.fraction` - fraction of image detected between lower and upper bound
#' - `original.img` - fed-in pixel.array
#' or
#' list-object with the following elements (when supplying one one pair of bounds)
#' - `pixel.idx` - pixel-locations of pixels detected between lower and upper bound.
#' - `pixel.count` - number of pixels detected between lower and upper bound
#' - `img.fraction` - fraction of image detected between lower and upper bound
#' - `original.img` - fed-in pixel.array
#' - `indicator.img` - `pixel.array` with hsv-values of pixels at positions `pixel.idx` modified.
#' @export
#' @importFrom stringr str_flatten_comma
#' @importFrom grDevices rgb2hsv
#' @importFrom stringr str_c
#'
extract_pixels_HSV <- function(pixel.array, lower_bound, upper_bound, fast_eval = TRUE, bundle_pixelarray = FALSE,check_value = FALSE) {
    if (is.list(lower_bound) && is.list(upper_bound)) {
        if (length(lower_bound) != length(upper_bound)) {
            stop(
                simpleError(
                    str_c("Parameters 'lower_bound' and 'upper_bound' must contain the same number of color-declarations/spectrums.")
                )
            )
        }
        inconsistent_list_params <- get_unique_list_elements(lower_bound, upper_bound)
        if (as.logical(length(inconsistent_list_params) > 0)) {
            stop(
                simpleError(
                    str_c(
                        "Parameters 'lower_bound' and 'upper_bound' do not declare the same spectrums.",
                        " ",
                        "The following spectrums are unique to either one of them:\n",
                        str_flatten_comma(inconsistent_list_params)
                    )
                )
            )
        }
        return_Object <- list()
        if (as.logical(bundle_pixelarray)) {
            return_Object$pixel.array <- pixel.array
        }
        if (as.logical(fast_eval)) {
            for (type in names(lower_bound)) {
                ret <- list()
                ret$pixel.idx <- rectangularRange_HSV_cpp(
                    H = pixel.array[,,,1],
                    S = pixel.array[,,,2],
                    V = pixel.array[,,,3],
                    lower_bound = lower_bound[[type]],
                    upper_bound = upper_bound[[type]],
                    image_width = dim(pixel.array)[1],check_V = as.logical(check_value)
                ) + 1

                ## the '+1' is required to handle Cpp being 0-indexed, while R is 1-indexed.
                ret$pixel.idx <- as.matrix(ret$pixel.idx)
                mode(ret$pixel.idx) <- "integer"
                ret$pixel.count <- nrow(ret$pixel.idx)
                ret$img.fraction <- nrow(ret$pixel.idx)/ (nrow(pixel.array) * ncol(pixel.array))
                return_Object[[type]] <- ret
            }
        } else {
            for (type in names(lower_bound)) {
                ret <- list()
                ret$pixel.idx <- rectangularRange_HSV(
                    pixel.array = pixel.array,
                    lower_bound = lower_bound[[type]], # TODO: implement list-ranges to detect all relvant ranges??
                    upper_bound = upper_bound[[type]] # TODO: implement list-ranges to detect all relvant ranges??
                )
                ret$pixel.count <- nrow(ret$pixel.idx)
                ret$img.fraction <- nrow(ret$pixel.idx)/ (nrow(pixel.array) * ncol(pixel.array))
                return_Object[[type]] <- ret
            }
        }
        return(return_Object)
    } else {
        if (as.logical(fast_eval)) {
            ret <- rectangularRange_HSV_cpp(
                H = pixel.array[,,,1],
                S = pixel.array[,,,2],
                V = pixel.array[,,,3],
                lower_bound = lower_bound[[type]],
                upper_bound = upper_bound[[type]],
                image_width = dim(pixel.array)[1]
            ) + 1
            ## the '+1' is required to handle Cpp being 0-indexed, while R is 1-indexed.
            ret <- as.matrix(ret)
            mode(ret) <- "integer"
            return(ret)
        } else {
            return(rectangularRange_HSV(
                pixel.array = pixel.array,
                lower_bound = lower_bound,
                upper_bound = upper_bound
            ))
        }
    }
}
