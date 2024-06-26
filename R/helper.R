
##### HELPER #####
#' normalise a vector to range `0-1`
#'
#' @param x vector to normalise to range `0-1`
#'
#' @return vector, normalised to range `0-1`
#' @keywords internal
#'
#' @seealso [limit_to_range()]
#'
#' @examples
#' x <- runif(n = 50, min = 1, max = 10)
#' max(duflor:::norm_to_range_01(x))
#' min(duflor:::norm_to_range_01(x))
#'
#'
norm_to_range_01 <- function(x){(x-min(x))/(max(x)-min(x))}

#' replace values outside of boundaries with respective  boundary
#'
#' This function limits only values which lie outside of
#' `[replace_lower, replace_upper`]. Values which do not fall outside this range
#' are not modified.
#'
#'
#' @param value vector to normalise
#' @param replace_lower value to insert in place of elements of `value` below this bound.
#' @param replace_upper see `replace_lower`
#'
#' @return vector, normalised to range `[replace_lower, replace_upper]`
#' @keywords internal
#'
#' @seealso [norm_to_range_01()]
#'
#' @examples
#' result <- duflor:::limit_to_range(c(0,100,255),0,255)
#' result2 <- duflor:::limit_to_range(c(0,100,256),0,255)
#' print(result)
#' print(result2)
limit_to_range <- function(x,replace_lower,replace_upper) {
    if (isFALSE(is.numeric(x))) {
        stop(
            simpleError(
                "Input 'x' must be numeric."
            )
        )
    }
    if (isFALSE(is.numeric(replace_lower))) {
        stop(
            simpleError(
                "Input 'replace_lower' must be numeric."
            )
        )
    }
    if (isFALSE(is.numeric(replace_upper))) {
        stop(
            simpleError(
                "Input 'replace_upper' must be numeric."
            )
        )
    }
    pmax(replace_lower, pmin(x, replace_upper))
}
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
#' @param indicator.array `pixel.array` as returned
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
#' @param fast_eval Use `C++`-code where possible to reduce execution time?
#' @param return_hsv TRUE by default. Controls whether or not function returns pixel-data in `HSV`-colorspace or in `RGB`
#' @param subset_only do you want to load only a subset range of values
#' @param upper_bound EITHER:
#' - list of upper HSV-bounds, e.g. `list(green = c(H_green_lower,S_green_lower,V_green_lower),drought = c(H_drought_lower,S_drought_lower,V_drought_lower))`
#' - single vector of length 3 declaring a set of HSV-values
#' @param lower_bound see `upper_bound`
#' @param crop_left number of pixels to crop from the left edge of the image
#' @param crop_right see `crop_left`
#' @param crop_top see `crop_left`
#' @param crop_bottom see `crop_left`
#' @return nothing. This function does *literally nothing at all*
#' @keywords internal
#'
.main_args <- function(pixel.array = NA,
                       indicator.array = NA,
                       pixel.idx = NA,
                       target.color = NA,
                       target.color.hsv = NA,
                       image.path = NA,
                       plot_indicator = NA,
                       get_indicator = NA,
                       mask_extreme = NA,
                       fast_eval = NA,
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
#' takes a cluster-range of integers and reassigns them based on their frequency
#'
#' For a input `clus = c(1,1,1,2,2,3,3,3,3)`, returns `clus = c(2,2,2,3,3,1,1,1,1)`.
#' @param clus vector of integers.
#'
#' @return frequency-based reassigned instance of `clus`
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' clus <- c(1,1,1,2,2,3,3,3,3)
#' clus_new <- reassign_integers_by_frequency(clus)
#' print(clus)
#' print(clus_new)
#' }
reassign_integers_by_frequency <- function(clus) {
    if (is.null(clus)) {
        stop(
            simpleError(
                "input must not be null"
            )
        )
    }
    if (isFALSE(is.vector(clus))) {
        stop(
            simpleError(
                "input is not a vector."
            )
        )
    }
    if (isTRUE(is.list(clus))) {
        stop(
            simpleError(
                "Input must be a vector, not a list."
            )
        )
    }
    # sort by descending frequency
    sorted_clusters <- sort(table(clus), decreasing = TRUE)

    # remap the old cluster numbers to new, frequency-sorted ones
    clus <- match(clus, names(sorted_clusters))
    return(clus)
}
#' wrapper around `object.size()`
#'
#' @param x object to measure
#'
#' @return size of `x` in GB, as numeric
#' @keywords internal
#'
#' @examples duflor:::objs(1:1:500000)
#' @importFrom utils object.size
objs <- function(x) {
    if (any(is.null(x))) {
        stop(
            simpleError(
                "input must not be NULL"
            )
        )
    }
    if (any(is.na(x))) {
        stop(
            simpleError(
                "input must not be NA"
            )
        )
    }
    return(as.numeric(object.size(x)*1e-9 + 0))
}
