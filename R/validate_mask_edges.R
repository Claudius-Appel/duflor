#' Check for a given mask pixel.idx-object if its edges are clean
#'
#' Check for a given spectrum's mask if the pixels on its edges are part of its
#' masked spectrum. That would suggest that the bounding area was set too restrictively, cutting
#' off pixels which are potentially valid for this mask.
#'
#'
#' @inheritParams .main_args
#' @param mask.idx pixel-locations of pixels detected for this mask
#' @param mask_name the mask's name, to print useful warnings
#' @param array_dim dimensions of the loaded `pixel.array`
#'
#' @return list of booleans for each edge. `TRUE` values signify that a given boundary was infringed upon.
#' @export
#'
#' @examples
#' \dontrun{
#' # load a subset of an image
#' pixel.array <- duflor::load_image(image.path,
#'                                   subset_only = T,
#'                                   crop_left = 20,
#'                                   crop_right = 20,
#'                                   crop_bottom = 20,
#'                                   crop_top = 20
#'                                   )
#'
#' # get pixels which belong to a given mask, and return the results for it
#' ret <- extract_pixels_HSV(pixel.array, lower_bound, upper_bound, F)
#'
#' # check for each mask if its edges are valid.
#' for (mask in names(ret)) {
#'     duflor:::validate_mask_edges(ret[[mask]]$pixel.idx,mask, dim(pixel.array)[1:2],image.path)
#' }
#' }
validate_mask_edges <- function(mask.idx,mask_name,array_dim,image.path) {
    ## for an extracted mask, check if it touches the boundaries of the array loaded
    # get the matched pixels that are as close to the edges as possible
    # array_dim[1]: width of pixel.array
    # array_dim[2]: height of pixel.array
    y_bottom <- max(mask.idx[,2])
    y_top <- min(mask.idx[,2])
    x_right<- max(mask.idx[,1])
    x_left <- min(mask.idx[,1])
    ret <- list(left = FALSE,
                right = FALSE,
                top = FALSE,
                bottom = FALSE
                )
    if (y_bottom >= array_dim[2]) {
        # mask touches the bottom edge. Are pixels missing?
        ret$bottom <- TRUE
        warning("The provided mask '",
                mask_name,
                "' extends up to the bottom edge of the available pixel grid for image '",image.path,"'. ",
                "\nIt is possible that some pixels which might qualify under this mask lie outside the loaded range.",
                "\nConsider changing the 'crop_*'-parameters when loading an image.",immediate. = T)
    }
    if (y_top <= 1) {
        ret$top <- TRUE
        # mask touches the top edge. Are pixels missing?
        warning("The provided mask '",
                mask_name,
                "' extends up to the top edge of the available pixel grid for image '",image.path,"'. ",
                "\nIt is possible that some pixels which might qualify under this mask lie outside the loaded range.",
                "\nConsider changing the 'crop_*'-parameters when loading an image.",immediate. = T)
    }
    if (x_left <= 1) {
        ret$left <- TRUE
        # mask touches left edge. Are pixels missing?
        warning("The provided mask '",
                mask_name,
                "' extends up to the left edge of the available pixel grid for image '",image.path,"'. ",
                "\nIt is possible that some pixels which might qualify under this mask lie outside the loaded range.",
                "\nConsider changing the 'crop_*'-parameters when loading an image.",immediate. = T)
    }
    if (x_right >= array_dim[1]) {
        # mask touches right edge. Are pixels missing?
        ret$right <- TRUE
        warning("The provided mask '",
                mask_name,
                "' extends up to the right edge of the available pixel grid for image '",image.path,"'. ",
                "\nIt is possible that some pixels which might qualify under this mask lie outside the loaded range.",
                "\nConsider changing the 'crop_*'-parameters when loading an image.",immediate. = T)
    }
    return(ret)
}
