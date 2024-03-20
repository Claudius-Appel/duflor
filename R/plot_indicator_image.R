
#' Plot indicator-image for a given spectrum
#'
#' This is a convenience- and clarity-wrapper around [plot_array_as_image_sRGB()]
#'
#' @param indicator.array `pixel.array` with  modified hsv-values of pixels. See [get_indicator_image()] for details. Values are expected to be in HSV color-space.
#' @param spectrum_name name of plotted spectrum
#'
#' @return /
#' @export
#'
#' @examples
#' \dontrun{
#' load a subset of an image
#' lower_bound <- c(H,S,V)
#' upper_bound <- c(H,S,V)
#' pixel.array <- load_image(image.path,
#'                           subset_only = T,
#'                           crop_left = 0,
#'                           crop_right = 0,
#'                           crop_bottom = 0,
#'                           crop_top = 0
#'                           )
#' # get pixels which belong to a given mask, and return the results for it
#' ret <- extract_pixels_HSV(pixel.array = pixel.array,
#'                           lower_bound = lower_bound,
#'                           upper_bound = upper_bound,
#'                           fast_eval = TRUE
#'                           )
#' # create indicator-image for 'green' spectrum
#' type <- "green"
#' indicator.array <- get_indicator_image(
#'     pixel.array = pixel.array,
#'     pixel.idx = ret$pixel.idx,
#'     target.color = "orange",
#'     mask_extreme = FALSE
#' )
#' plot_indicator_image(indicator.array,type)
#' }
plot_indicator_image <- function(indicator.array,spectrum_name) {
    plot_array_as_image_sRGB(HSVtoRGB(indicator.array), main = str_c("indicator for '", spectrum_name, "'"))
}
