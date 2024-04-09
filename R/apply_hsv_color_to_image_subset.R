#' apply hsv-color to index-located pixels in array
#'
#' @inheritParams .main_args
#'
#' @importFrom imager as.cimg
#' @return modified pixel.array
#' @keywords internal
#'
apply_hsv_color_to_image_subset <- function(pixel.array,pixel.idx,target.color.hsv, mask_extreme = FALSE) {
    pa_t <- as.array(pixel.array)
    for (i in 1:nrow(pixel.idx)) {
        pa_t[pixel.idx[i, "x"], pixel.idx[i, "y"],1,1] <- target.color.hsv[1]
        pa_t[pixel.idx[i, "x"], pixel.idx[i, "y"],1,2] <- target.color.hsv[2]
    }
    if (isTRUE(as.logical(mask_extreme))) {
        for (i in 1:nrow(pixel.idx)) {
            pa_t[pixel.idx[i, "x"], pixel.idx[i, "y"],1,3] <- target.color.hsv[3]
        }
    }
    pixel.array <- as.cimg(pa_t)
    return(pixel.array)
}
