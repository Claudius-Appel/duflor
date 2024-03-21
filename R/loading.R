#' Reinitalises JVM with previously used parameters
#'
#' Used internally by [load_image()].
#' If [get_javaVM_exceptions()] found any exceptions, this function is called
#' and re-initialises the JVM with the provided values.
#' issues a warning suggesting to re-initialise the value before loading the package.
#' @param heapspace integer heapspace to be allocated to the new JVM. If omitted, the contents of `getOption("duflor..used_JVM_heapspace")` is used.
#' @param unit unit of `heapspace`. Must be either of `c("g","m")`. If omitted, the contents of `getOption("duflor..used_JVM_heap_unit")` is used.
#' @param pkg return-value of `packageName()`
#'
#' @keywords internal
#'
reinitialise_javaVM <- function(heapspace=NA,unit=NA,pkg) {
    if (is.na(heapspace)) {
        heapspace <- getOption("duflor..used_JVM_heapspace")
    }
    if (is.na(unit)) {
        unit <- getOption("duflor..used_JVM_heap_unit")
    }
    rJava::.jpackage(pkg, parameters=str_c("-Xmx",heapspace,unit))
}
#' retrieve exceptions of the JVM
#'
#' check the JVM for any pending exceptions, clear them and return TRUE
#' if exceptions were found.
#'
#' @return boolean value whether or not the JVM contained exceptions.
#' @keywords internal
#' @importFrom stringr str_extract
#'
get_javaVM_exceptions <- function() {
    exception_found <- as.logical(rJava::.jcheck(silent = T))
    return(exception_found)
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
#' @importFrom utils packageName
#'
load_image <- function(image.path, subset_only = FALSE, return_hsv = TRUE, crop_left=0, crop_right=0, crop_top=0, crop_bottom=0) {
    ## if we subset, we use RBioFormats. Thus, we first have to mount a java-VM
    ## (or confirm one was mounted already)
    if (isTRUE(as.logical(subset_only))) {
        if (getOption("duflor.java_available")) { ## assert that rJava is installed when attempting to load Image
            chk <- get_javaVM_exceptions()
            if (chk) {
                # reinit the JVM if it contains exceptions
                r_chk <- reinitialise_javaVM(,,packageName())
                # if this still didn't work, error out
                chk <- get_javaVM_exceptions()
                if (chk) {
                    stop(
                        simpleError(
                            str_c(
                                "The java-VM could not be re-mounted successfully.",
                                "\nWithout the VM, images cannot be subset prior to analysis.",
                                "\nUsed heapspace: ",
                                getOption("duflor..used_JVM_heapspace"),
                                " ",
                                getOption("duflor..used_JVM_heap_unit")
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
