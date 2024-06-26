#' Extract pixels from image which fall inbetween lower- and upper bounds.
#'
#' @inheritParams .main_args
#' @param bundle_pixelarray logical, indicating if the input parameter `pixel.array` is to be bundled into the return-value
#' This is useful to retain `pixel.array` into the output if this function is called in a loop.
#' @param check_value boolean toggle to also check the `VALUE`-component of an HSV-pixel
#' @param use_single_iteration_cpp Use [rectangularRange_HSV_iteronce_cpp()] for computing hits per range. This is slightly more efficient than using [rectangularRange_HSV_cpp()].
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
extract_pixels_HSV <- function(pixel.array, lower_bound, upper_bound, fast_eval = TRUE, bundle_pixelarray = FALSE, check_value = FALSE, use_single_iteration_cpp = FALSE) {
    # structure of the return object:
    #
    # return_Object[[spectrum_name]]
    #   - pixel.idx
    #   - pixel.count
    #   - img.fraction
    #   - (bundle_pixelarray==TRUE?pixel.array)
    #
    # IN CASE OF SINGLE SPECTRUM,
    #
    # return_Object[[1]]
    #   - pixel.idx
    #   - pixel.count
    #   - img.fraction
    #   - (bundle_pixelarray==TRUE?pixel.array)
    #
    #
    # Overview: For details, see function definitions for respective extraction-
    # function.
    #
    # rectangularRange_HSV
    # - slowest, must iterate over all desired spectra
    # - `pixel.count` and `img.fraction` must be calculated separately
    # rectangularRange_HSV_cpp
    # - middle path, must iterate over all desired spectra
    # - `pixel.count` and `img.fraction` must be calculated separately
    # rectangularRange_HSV_iteronce_cpp
    # - fastest, does the iterating itself and returns the bundled results for all spectra
    # - `pixel.count` and `img.fraction` are already calculated
    # - spectrum names must be assigned to the first-level entries of its return-values separately
    # - input must be a matrix, **not** a list
    #
    return_Object <- list()
    if (is.list(lower_bound) && is.list(upper_bound)) { # multiple spectra
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

        if (as.logical(fast_eval)) {
            if (as.logical(use_single_iteration_cpp)) {
                nlb <- do.call(rbind,lower_bound)
                dimnames(nlb) <- c()
                nub <- do.call(rbind,upper_bound)
                dimnames(nub) <- c()
                return_Object <- rectangularRange_HSV_iteronce_cpp(
                    H = pixel.array[,,,1],
                    S = pixel.array[,,,2],
                    V = pixel.array[,,,3],
                    lower_bound = nlb,
                    upper_bound = nub,
                    image_width = dim(pixel.array)[1],
                    check_V = as.logical(check_value)
                )
                names(return_Object) <- names(lower_bound) ## assign names
            } else {
                for (spectrum_name in names(lower_bound)) {
                    ret <- list()
                    ret$pixel.idx <- rectangularRange_HSV_cpp(
                        H = pixel.array[,,,1],
                        S = pixel.array[,,,2],
                        V = pixel.array[,,,3],
                        lower_bound = lower_bound[[spectrum_name]],
                        upper_bound = upper_bound[[spectrum_name]],
                        image_width = dim(pixel.array)[1],
                        check_V = as.logical(check_value)
                    )
                    ret$pixel.idx <- as.matrix(ret$pixel.idx)
                    mode(ret$pixel.idx) <- "integer"
                    ret$pixel.count <- nrow(ret$pixel.idx)
                    ret$img.fraction <- nrow(ret$pixel.idx)/ (nrow(pixel.array) * ncol(pixel.array))
                    return_Object[[spectrum_name]] <- ret
                }
            }
        } else {
            for (spectrum_name in names(lower_bound)) {
                ret <- list()
                ret$pixel.idx <- rectangularRange_HSV(
                    pixel.array = pixel.array,
                    lower_bound = lower_bound[[spectrum_name]],
                    upper_bound = upper_bound[[spectrum_name]],
                    check_V = as.logical(check_value)
                )
                ret$pixel.count <- nrow(ret$pixel.idx)
                ret$img.fraction <- nrow(ret$pixel.idx)/ (nrow(pixel.array) * ncol(pixel.array))
                return_Object[[spectrum_name]] <- ret
            }
        }
        if (as.logical(bundle_pixelarray)) {
            return_Object$pixel.array <- pixel.array
        }
    } else { # a single spectrum
        return_Object[[1]] <- list()
        if (as.logical(fast_eval)) {
            if (as.logical(use_single_iteration_cpp)) {
                nlb <- matrix(lower_bound,ncol = 3)
                nub <- matrix(upper_bound, ncol = 3)
                return_Object <- rectangularRange_HSV_iteronce_cpp(
                    H = pixel.array[,,,1],
                    S = pixel.array[,,,2],
                    V = pixel.array[,,,3],
                    lower_bound = nlb,
                    upper_bound = nub,
                    image_width = dim(pixel.array)[1],
                    check_V = as.logical(check_value)
                )
                names(return_Object) <- names(lower_bound) ## assign names
            } else {
                ret <- list()
                ret$pixel.idx <- rectangularRange_HSV_cpp(
                    H = pixel.array[,,,1],
                    S = pixel.array[,,,2],
                    V = pixel.array[,,,3],
                    lower_bound = lower_bound,
                    upper_bound = upper_bound,
                    image_width = dim(pixel.array)[1],
                    check_V = as.logical(check_value)
                )
                ret$pixel.idx <- as.matrix(ret$pixel.idx)
                mode(ret$pixel.idx) <- "integer"
                ret$pixel.count <- nrow(ret$pixel.idx)
                ret$img.fraction <- nrow(ret$pixel.idx)/ (nrow(pixel.array) * ncol(pixel.array))
                return_Object[[1]] <- ret
            }
        } else {
            return_Object[[1]]$pixel.idx <- rectangularRange_HSV(
                pixel.array = pixel.array,
                lower_bound = lower_bound,
                upper_bound = upper_bound
            )
            return_Object[[1]]$pixel.count <- nrow(return_Object[[1]]$pixel.idx)
            return_Object[[1]]$img.fraction <- nrow(return_Object[[1]]$pixel.idx) / (nrow(pixel.array) * ncol(pixel.array))
        }
        if (as.logical(bundle_pixelarray)) {
            return_Object$pixel.array <- pixel.array
        }
    }
    return(return_Object)
}
