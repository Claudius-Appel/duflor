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
    if (file.exists(image.path)) {
        if (isTRUE(as.logical(subset_only))) {
            if (crop_left<0) {
                stop(
                    simpleError(
                        str_c(
                            "Parameter '"
                            ,crop_left
                            ,"' cannot be below 0. Function will exit"
                            )
                        )
                    )
            }
            if (crop_right<0) {
                stop(
                    simpleError(
                        str_c(
                            "Parameter '"
                            ,crop_right
                            ,"' cannot be below 0. Function will exit"
                            )
                        )
                    )
            }
            if (crop_top<0) {
                stop(
                    simpleError(
                        str_c(
                            "Parameter '"
                            ,crop_top
                            ,"' cannot be below 0. Function will exit"
                            )
                        )
                    )
            }
            if (crop_bottom<0) {
                stop(
                    simpleError(
                        str_c(
                            "Parameter '"
                            ,crop_bottom
                            ,"' cannot be below 0. Function will exit"
                            )
                        )
                    )
            }
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
            if ((x2<x1) || (x2==x1)) {
                stop(
                    simpleError(
                        str_c(
                            "Overlapping cropping: Right Boundary '",x2,"'is smaller than left boundary '", x1,"'."
                        )
                    )
                )
            }
            if ((y2<y1) || (y2==y1)) {
                stop(
                    simpleError(
                        str_c(
                            "Overlapping cropping: Bottom Boundary '",y2,"'is smaller than top boundary '", y1,"'."
                        )
                    )
                )
            }
            if (as.logical(return_hsv)) {
                return( # subsetting, HSV
                    RGBtoHSV(
                        sRGBtoRGB(
                            # suppress known-warning about assuming 3rd dimension
                            #  of return value of `as.cimg` to be "time/depth":
                            #  'Assuming third dimension corresponds to time/depth'
                            #  structure of return value: [x,y,t/d,[r,g,b]]
                            suppressWarnings(as.cimg(
                                ig_ret[x1:x2,y1:y2,,],dim = c(x2-x1,y2-y1,1,3)
                                )
                            ))
                        )
                    )
            } else {
                return(  # subsetting, RGB
                    sRGBtoRGB(
                        suppressWarnings(as.cimg(
                            ig_ret[x1:x2,y1:y2,,],dim = c(x2-x1,y2-y1,1,3)
                        ))
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
