#' load image as HSV or RGB-array
#'
#' @inheritParams .main_args
#'
#' @note
#' `load_image()` allows loading subsets of images using `imager::load.image()`.
#'
#' @return hsv-formatted pixel.array, unless `HSV==false`.
#' @export
#' @importFrom stringr str_c
#' @importFrom imager as.cimg
#' @importFrom imager load.image
#'
load_image <- function(image.path, subset_only = FALSE, return_hsv = TRUE, crop_left=0, crop_right=0, crop_top=0, crop_bottom=0) {
    ## if we subset, we use RBioFormats. Thus, we first have to mount a java-VM
    ## (or confirm one was mounted already)
    if (file.exists(image.path)) {
        if (isTRUE(as.logical(subset_only))) {
            # get image dimensions from loaded object
            ig_ret <-load.image(image.path)
            xdim <- dim(ig_ret)[1]
            ydim <- dim(ig_ret)[2]
            # determine the offset coordinates in the array
            if (crop_left>0) {
                x1 <- crop_left
            } else {
                x1 <- 1
            }
            if (crop_right>0) {
                x2 <- xdim - crop_right
            } else {
                x2 <- xdim
            }
            if (crop_top>0) {
                y1 <- crop_top
            } else {
                y1 <- 1
            }
            if (crop_bottom>0) {
                y2 <- ydim - crop_bottom
            } else {
                y2 <- ydim
            }
            message("duflor.load_image(): Image loaded via imager, then subset manually")
            if (as.logical(return_hsv)) {
                return(
                    RGBtoHSV(
                        sRGBtoRGB(
                            as.cimg(
                                ig_ret[x1:x2,y1:y2,,],dim = c(x2-x1,y2-y1,1,3)
                                )
                            )
                        )
                    )
            } else {
                return(  # subsetting, HSV
                    sRGBtoRGB(
                        as.cimg(
                            ig_ret[x1:x2,y1:y2,,],dim = c(x2-x1,y2-y1,1,3)
                        )
                    )
                )
            }
        } else {
            if (as.logical(return_hsv)) {
                return(  # no subsetting, HSV
                    RGBtoHSV(
                        sRGBtoRGB(
                            load.image(image.path)
                        )
                    )
                )
            } else {
                return(  # no subsetting, RGB
                    sRGBtoRGB(
                        load.image(image.path)
                    )
                )
            }
        }
    } else {
        stop(
            simpleError(
                str_c("The image file '",image.path,"' does not exist or cannot be accessed. The file is skipped.")
            )
        )
    }
}
