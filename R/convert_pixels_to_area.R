
#' convert pixel counts to an area based on the known area of an identifier-dot.
#'
#' @param pixel.counts A list containing any of the following pixel-counts:
#'
#' - "green"
#' - "drought"
#' - "complete"
#' - "identifier"
#'
#' @description
#' Convert pixel-counts to area based on declared area of the identifier
#'
#' @details
#'
#' The pixel-count for `identifier` must be present, whereas the others are optional.
#' The calculated area is internally unit-less. However, as the default value for
#' `duflor.declared_identifier_area` is `r getOption("duflor.default_identifier_area")` \[cm^2\], the areas computed by this
#' function is also in \[cm^2\].
#'
#' The assumed area of the identifier can be modified by excuting
#' `options(duflor.declared_identifier_area = <value_in_square_centimeters>)` *prior* to calling this function.
#'
#' @return list of computed areas for any key listed in `pixel.counts`.
#'   Additionally, two meta-values are returned as well:
#'   - the identifier-area used for calculating each area (whichever value is set in option `duflor.declared_identifier_area`)
#'   - the area of a singe pixel
#'
#' @note
#' All values are in \[cm^2\].
#'
#' @export
#' @importFrom utils hasName
convert_pixels_to_area <- function(pixel.counts) {
    if (is.null(getOption("duflor.declared_identifier_area"))) {
        used_identifier_area <- getOption("duflor.default_identifier_area")
    } else {
        used_identifier_area <- getOption("duflor.declared_identifier_area")
    }
    if (!hasName(pixel.counts,"identifier")) {
        stop(simpleError("FATAL: Unknown identifier count. Is the identifier missing?"))
    }
    if (hasName(pixel.counts$identifier,"pixel.count")) {
        area_per_pixel = used_identifier_area / pixel.counts$identifier$pixel.count # see .onLoad()
    } else {
        area_per_pixel = used_identifier_area / pixel.counts$identifier # see .onLoad()
    }
    ret <- list(area_per_pixel = area_per_pixel,
                area_identifier = used_identifier_area)
    if (hasName(pixel.counts,"green")) {
        if (hasName(pixel.counts$green,"pixel.count")) {

            ret$plant_area_green <- area_per_pixel * pixel.counts$green$pixel.count
        } else {
            ret$plant_area_green <- area_per_pixel * pixel.counts$green
        }
    }
    if (hasName(pixel.counts,"drought")) {
        if (hasName(pixel.counts$green,"pixel.count")) {
            ret$plant_area_drought <- area_per_pixel * pixel.counts$drought$pixel.count
        } else {
            ret$plant_area_drought <- area_per_pixel * pixel.counts$drought
        }
    }
    if (hasName(pixel.counts,"complete")) {
        if (hasName(pixel.counts$green,"pixel.count")) {
            ret$plant_area_complete <- area_per_pixel * pixel.counts$complete$pixel.count
        } else {
            ret$plant_area_complete <- area_per_pixel * pixel.counts$complete
        }
    }
    if (hasName(pixel.counts,"root")) {
        if (hasName(pixel.counts$green,"pixel.count")) {
            ret$root_area <- area_per_pixel * pixel.counts$root$pixel.count
        } else {
            ret$root_area <- area_per_pixel * pixel.counts$root
        }
    }
    return(ret)
}
