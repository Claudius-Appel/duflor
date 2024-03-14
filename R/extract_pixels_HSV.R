#' Title
#'
#' @inheritParams .main_args
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
#'
extract_pixels_HSV <- function(pixel.array, lower_bound, upper_bound, target.color = "hotpink", get_indicator = FALSE, plot_indicator = FALSE, mask_extreme = FALSE, return_hsv = TRUE) {
    if (is.list(lower_bound) && is.list(upper_bound)) {
        if (length(lower_bound) != length(upper_bound)) {
            stop(
                simpleError(
                    str_c("Parameters 'lower_bound' and 'upper_bound' must contain the same number of color-declarations.")
                )
            )
        }
        inconsistent_list_params <- get_unique_list_elements(lower_bound, upper_bound)
        if (as.logical(length(inconsistent_list_params) > 0)) {
            stop(
                simpleError(
                    str_c(
                        "Parameters 'lower_bound' and 'upper_bound' do not declare the same parameters.",
                        " ",
                        "The following parameters are unique to either one of them:\n",
                        str_flatten_comma(inconsistent_list_params)
                    )
                )
            )
        }
        inconsistent_list_params <- get_unique_list_elements(lower_bound, target.color)
        if (as.logical(length(inconsistent_list_params) > 0)) {
            stop(
                simpleError(
                    str_c(
                        "Parameters 'lower_bound' and 'target.color' do not declare the same parameters.",
                        " ",
                        "The following parameters are unique to either one of them:\n",
                        str_flatten_comma(inconsistent_list_params)
                    )
                )
            )
        }
        return_Object <- list()
        for (type in names(lower_bound)) {
            ret <- rectangularRange_HSV(
                pixel.array = pixel.array,
                lower_bound = lower_bound[[type]], # TODO: implement list-ranges to detect all relvant ranges??
                upper_bound = upper_bound[[type]] # TODO: implement list-ranges to detect all relvant ranges??
            )
            if (get_indicator | plot_indicator) {
                ret$indicator.img <- get_indicator_image(pixel.array, ret$pixel.idx, target.color[[type]], mask_extreme)
            }
            if (plot_indicator) {
                plot_array_as_image_sRGB(HSVtoRGB(ret$indicator.img), main = str_c("indicator for '", type, "'"))
            }
            return_Object[[type]] <- ret
        }
        return(return_Object)
    } else {
        return(rectangularRange_HSV(
            pixel.array = pixel.array,
            lower_bound = lower_bound,
            upper_bound = upper_bound
        ))
    }
}
