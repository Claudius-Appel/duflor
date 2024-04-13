#' apply color to pixel.array by mask.
#'
#' If `target.color` is a character-string (e.g. `"red1"`) are members of `colors()`,
#' it will be translated into its HSV-representation.
#' If `target.color` is a vector of RGB-values, it gets translated into its
#' HSV-representation.
#' The resulting HSV-color will be applied to all elements of `pixel.array`
#' which are members of `pixel.idx` are translated to their HSV-representation.
#'
#' Values for `target.color` which lie outside the RGB-range of `[0-255,0-255,0-255]` will be
#' constricted to limits of the range `[0,255]`
#' @inheritParams .main_args
#' @return `pixel.array` with hsv-values of pixels at positions `pixel.idx` modified.
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb2hsv
#' @importFrom grDevices colors
#' @export
#'
apply_HSV_color_by_mask <- function(pixel.array, pixel.idx, target.color, mask_extreme) {

    if (is.character(target.color)) {
        if (sum(is.element(colors(),target.color))==0) { # color is not part of the available color set from grDevices, thus error out descriptively
            stop("The provided color '", target.color,"' could not be converted to RGB color-space.\nSee 'grDevices::colors()' for allowed colors.")
        }
        target.color_ <- as.vector(col2rgb(target.color)) # col in RGB, ranged 0-255
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
        if (any(range(target.color) > 255)) {
            target.color <- limit_to_range(target.color,replace_lower = 0,replace_upper = 255)
            warning("Elements of 'target.color' not in range [0,255] were normalised to [0,255], without scaling non-infringing values accordingly.")
        }
        target.color.hsv <- rgb2hsv(target.color[1], target.color[2], target.color[3])
    }
    ## colors are in HSV format here
    # it seems that converting the cImg to a normal arr speeds up the following operation
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
