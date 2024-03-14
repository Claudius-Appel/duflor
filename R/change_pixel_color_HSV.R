#' handles color-translation from character-name to hsv and returns results of `duflor::apply_hsv_color_to_image_subset()`
#'
#' @inheritParams .main_args
#' @return `pixel.array` with hsv-values of pixels at positions `pixel.idx` modified.
#' @export
#'
change_pixel_color_HSV <- function(pixel.array, pixel.idx, target.color, mask_extreme) {
    if (is.character(target.color)) {
        target.color_ <- as.vector(grDevices::col2rgb(target.color)) # col in RGB, ranged 0-255
        if (length(target.color_) != 3) {
            stop("'target.color_' must be a numeric vector of length 3 with\n             values between 0 and 1 or one of the colors listed by 'colors()'")
        }

        # H-range: 0-360
        # S-range: 0-1
        # V-range: 0-1
        target.color.hsv <- rgb2hsv(target.color_[1], target.color_[2], target.color_[3])

    } else if (is.vector(target.color)) {
        if (length(target.color) != 3) {
            stop("'target.color' must be a numeric vector of length 3 with\n             RGB-values between 0 and 255 or one of the colors listed by 'colors()'")
        }
        if (range(target.color)[2] > 255) {
            target.color <- target.color/255
        }
        target.color.hsv <- rgb2hsv(target.color[1], target.color[2], target.color[3])
    }
    ## colors are in HSV format here
    return(apply_hsv_color_to_image_subset(pixel.array, pixel.idx, target.color.hsv, mask_extreme))
}
