
##### HELPER #####
#' normalise a vector to range `0-1`
#'
#' @param x vector to normalise to range `0-1`
#'
#' @return vector, normalised to range `0-1`
#' @keywords internal
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
    # sort by descending frequency
    sorted_clusters <- sort(table(clus), decreasing = TRUE)

    # remap the old cluster numbers to new, frequency-sorted ones
    clus <- match(clus, names(sorted_clusters))
    return(clus)
}
#' add 4D-adjacency-grouping to `pixel.idx`-object
#'
#' The function assigns clusters to all coordinate-pairs in `pixel.idx`.
#' A cluster contains all pixels which share a non-diagonal link with each other.
#'
#' This means that points `(1/1)`, `(1/2)` and `(2/2)` are assigned the same cluster,
#' whereas points  `(5/5)` and `(6/6)` are assigned separate clusters.
#'
#' To consider diagonal matches as well, see [diagonal_adjacency()]
#'
#' Reference: <https://stackoverflow.com/a/37946855>
#' @param pixel.idx pixel.idx-object
#'
#' @return `pixel.idx` with added 3rd column `clus` mapping to a cluster
#' @export
#' @importFrom stats cutree
#' @importFrom stats dist
#' @importFrom stats hclust
#'
adjacency <- function(pixel.idx) {
    #TODO: rename functions to clarify naming conventions
    cbind(pixel.idx, clus = cutree(hclust(dist(x = pixel.idx, method = "manhattan"), "single"), h = 1))
}

#' add 8D-adjacency-grouping to `pixel.idx`-object
#'
#' The function assigns clusters to all coordinate-pairs in `pixel.idx`.
#' A cluster contains all pixels which share a diagonal link with each other.
#'
#' This means that points `(1/1)`, `(1/2)` and `(2/2)` are assigned the same cluster.
#' Additionally, points  `(5/5)` and `(6/6)` are assigned the same cluster.
#'
#' To consider diagonal matches as well, see [adjacency()]
#'
#' Reference: <https://stackoverflow.com/a/37946855>
#' @inheritParams .main_args
#'
#' @return `pixel.idx` with added 3rd column `clus` mapping to a cluster
#' @export
#' @importFrom stats cutree
#' @importFrom stats dist
#' @importFrom stats hclust
#'
diagonal_adjacency <- function(pixel.idx) {
    cbind(pixel.idx, clus = cutree(hclust(dist(x = pixel.idx, method = "maximum"), "single"), h = 1))
}
#' return coordinates by cluster_id from `pixel.idx`
#'
#' @inheritParams .main_args
#' @param cluster_id index of the to-be-retrieved cluster
#'
#' @return `pixel.idx` with added 3rd column `clus` mapping to a cluster
#' @export
#'
#' @examples
#' \dontrun{
#' pixels <- extract_pixels_HSV(...) # extract pixels of a certain color-range
#' adjacency <- adjacency(pixels$identifier$pixel.idx) # assign clusters
#' coords <- retrieve_adjacency_coords(adjacency,1) # retrieve coordinates of first cluster
#'
#' plot_array_as_image_sRGB( # display result
#'     HSVtoRGB(
#'         change_pixel_color_HSV(
#'             pixel.array,
#'             coords,
#'             target.color = "white",
#'             mask_extreme = T
#'             )
#'         )
#'     )
#' }
retrieve_adjacency_coords <- function(pixel.idx,cluster_id) {
    pixel.idx[pixel.idx[,"clus"]==cluster_id,]
}

#' wrapper around `object.size()`
#'
#' @param x object to retrive memory size of
#'
#' @return size of `x` in GB
#' @keywords internal
#'
#' @examples duflor:::objs(1:1:500000)
#' @importFrom utils object.size
objs <- function(x) {
    cat(as.numeric(object.size(x)*1e-9 + 0),"GB")
}
