
##### HELPER #####
#' normalise a vector to range `0-1`
#'
#' @param x vector to normalise to range `0-1`
#'
#' @return vector, normalised to range `0-1`
#' @export
#'
norm_to_range_01 <- function(x){(x-min(x))/(max(x)-min(x))}

#' compare two lists and return all keys that are not present in both.
#'
#' the keys' contents are not considered. Only the presence or absence of the
#' key is relevant.
#'
#' @param a a list
#' @param b a list
#'
#' @return vector of elements not present in either `a` or `b`.
#' @keywords internal
#'
#' @examples duflor:::get_unique_list_elements(list(a = 1,b =2,c= 3),list(a = 1,b = 1,d = 3))
get_unique_list_elements <- function(a,b) {
    n_a <- names(a)
    n_b <- names(b)
    return(
        c(
            setdiff(n_a,n_b),
            setdiff(n_b,n_a)
        )
    )
}

#' pseudo-function to define default parameter documentation
#'
#' @param pixel.array image array as loaded via duflor::load_image()
#' @param pixel.idx list declaring pixels to which target.color.hsv is applied
#' @param target.color color in rgb-format `0-255`, or a member of  `colors()`
#' @param target.color.hsv hsv-formatted color to apply
#' @param image.path path to image-file
#' @param plot_indicator control whether or not the indicator image should be plotted
#' @param get_indicator control whether or not target pixels should get colored in the indicator-images
#' @param mask_extreme by default, only the `hue`- and `saturation`-components
#' are applied to the pixels. Set this argument to `TRUE` to also apply `value`.
#' This will increase the contrast of the image drastically, but might result in
#' less favorable images.)
#' @param return_hsv TRUE by default. Controls whether or not function returns pixel-data in `HSV`-colorspace or in `RGB`
#' @param subset_only do you want to load only a subset range of values
#' @param upper_bound EITHER:
#' - list of upper HSV-bounds, e.g. list(green = c(H_green_lower,S_green_lower,V_green_lower),drought = c(H_drought_lower,S_drought_lower,V_drought_lower))
#' - single vector of length 3 declaring a set of HSV-values
#' @param lower_bound see `upper_bound`
#' @param crop_left number of pixels to crop from the left edge of the image
#' @param crop_right see crop_left
#' @param crop_top see crop_left
#' @param crop_bottom see crop_left
#' @return nothing. This function does *literally nothing at all*
#' @keywords internal
#'
.main_args <- function(pixel.array = NA,
                       pixel.idx = NA,
                       target.color = NA,
                       target.color.hsv = NA,
                       image.path = NA,
                       plot_indicator = NA,
                       get_indicator = NA,
                       mask_extreme = NA,
                       return_hsv = NA,
                       subset_only = NA,
                       upper_bound = NA,
                       lower_bound = NA,
                       crop_left = NA,
                       crop_right = NA,
                       crop_top = NA,
                       crop_bottom = NA) {
# nomenclature:
# central arguments are spaced with '.'
# booleans are spaced with '_'
#

}
















