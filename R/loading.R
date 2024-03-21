#' Set java-parameters required for VM-setup.
#'
#' Used internally by [load_image()].
#' If the returned heapspace size used for initialisation is less than 8 gb,
#' issues a warning suggesting to re-initialise the value before loading the package.
#'
#' @param gb integer number of gigabytes of heapspace to assign to the Java environment.
#'
#' @return call to `check_javaVM_setup()`
#' @export
#' @importFrom stringr str_c
#'
setup_javaVM <- function(gb = 8) {

    return(check_javaVM_setup())
}
#' Determine whether or not the java-VM required for "RBioFormats" is set up
#'
#' @return boolean value whether or not the JAVA-VM is set up or not
#' @keywords internal
#'
check_javaVM_setup <- function() {
    java_opt <- getOption("java.parameters")
    ret <- is.null(java_opt)
    print(java_opt)
    print(ret)
    return(ret)
}
#' load image as HSV or RGB-array
#'
#' @inheritParams .main_args
#'
#' @note
#' `load_image()` allows loading subsets of images using `RBioFormats::read.image()`, which in turn relies upon the package `rJava`.
#' For the setup of `RBioFormats`, refer to <https://github.com/aoles/RBioFormats?tab=readme-ov-file#installation>.
#'
#' @return hsv-formatted pixel.array, unless `HSV==false`.
#' @export
#' @importFrom stringr str_c
#' @importFrom imager as.cimg
#' @importFrom imager load.image
#'
load_image <- function(image.path, subset_only = FALSE, return_hsv = TRUE, crop_left=0, crop_right=0, crop_top=0, crop_bottom=0) {
# TODO IMPORTANT:
#
# GB <- 8
# options(java.parameters = paste0("-Xmx", GB, "g")) ## this works.
# library(duflor) # remember to set up the java heap-size prior to loading duflor. This is _important_.
#
# THESE LINES MUST BE DOCUMENTED TO BE EXECUTED PRIOR TO CALLING `library(duflor)`. OTHERWHISE `RBioFormats::read.image()` will most most most likely fail
    ## if we subset, we use RBioFormats. Thus, we first have to mount a java-VM
    ## (or confirm one was mounted already)
    if (isTRUE(as.logical(subset_only))) {
        if (getOption("duflor.java_available")) { ## assert that rJava is installed when attempting to load Image
            chk <- check_javaVM_setup()
            r_chk <- setup_javaVM(8)
            if (chk) {
                if (!r_chk) {
                    stop(
                        simpleError(
                            str_c(
                                "The java-VM could not be mounted successfully.",
                                "\nWithout the VM, images cannot be subset prior to analysis"
                                )
                        )
                    )
                }
            }
            ## we confirmed that rJava is installed, but in truth we just need RBioFormats.
            if (getOption("duflor.RBF_available")) { ## assert that RBF is installed when attempting to load Image
                subset_only <- TRUE
            } else {
                subset_only <- FALSE
            }
        } else {
            # rJava is not installed, thus we need to fallback to non-subsetting
            # without rJava, we don't need to check for RBF.
            subset_only <- FALSE
        }
    }
    if (file.exists(image.path)) {
        if (isTRUE(as.logical(subset_only))) {
            ## load via RBioFormats::read.image
            ## EXPERIMENTAL: VALUES DIFFER SLIGHTLY. We will have to see how
            ## that affects the algos.

            # get image dimensions from metadata, then calculate the cropping-
            # offsets
            meta <- RBioFormats::read.metadata(image.path)$coreMetadata
            xdim <- meta$sizeX
            ydim <- meta$sizeY
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
            if (as.logical(return_hsv)) {
                return(  # subsetting, HSV
                    RGBtoHSV(
                        sRGBtoRGB(
                            imager::as.cimg(
                                RBioFormats::read.image(
                                    file = image.path,
                                    filter.metadata = T,
                                    proprietary.metadata = F,
                                    normalize = T,
                                    read.metadata = F,
                                    subset = list(x=x1:x2, y=y1:y2)
                                )
                            )
                        )
                    )
                )
            } else {
                return(  # subsetting, RGB
                    sRGBtoRGB(
                        imager::as.cimg(
                            RBioFormats::read.image(
                                file = image.path,
                                filter.metadata = T,
                                proprietary.metadata = F,
                                normalize = T,
                                read.metadata = F,
                                subset = list(x=x1:x2, y=y1:y2)
                            )
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
