#' onLoad-routine: Initialises package configuration
#'
#' The function initialises various options used by the package upon loading:
#' 1. The following options are initialised:
#'     - `duflor.default_hsv_spectrums`
#'     - `duflor.default_identifier_area`
#'
#' @note
#' When a package `pkg` is attached (see Link below) via `library(pkg)`, or the first time
#' a function of the package `pkg` is called via `pkg::fun()`, this function
#' gets called.
#'
#' @seealso The differences between `attaching` and `loading` a package are somewhat
#' difficult to understand. I found these explanations on
#' - \href{https://r-pkgs.org/dependencies-mindset-background.html#sec-dependencies-attach-vs-load}{r-pkgs.org}
#' - and \href{https://forum.posit.co/t/when-to-use-onload-vs-onattach/21953/2}{forum.posit.co}
#'  useful.
#'
#' @param lib /
#' @param pkg /
#' @keywords internal
#'
.onLoad <- function(lib, pkg) {

    #### Configure default spectrum-definitions ####
    spectrums <- list(
        lower_bound = list(
            bex_drought_HSV = c(20, 0.25, 0.20)
            , bex_green_HSV = c(69, 0.25, 0.20)
            , bex_complete_HSV = c(20, 0.25, 0.20)
            , bex_root_HSV = c(26,0.10,0.45) # TODO: validate these HSV ranges for use in rootarea!!
            , bex_identifier_dot = c(270, 0.2, 0.28)
        ),
        upper_bound = list(
            bex_drought_HSV = c(69, 1, 0.80)
            , bex_green_HSV = c(150, 1, 0.80)
            , bex_complete_HSV = c(150, 1, 0.80)
            , bex_root_HSV = c(52,0.28,0.82) # TODO: validate these HSV ranges for use in rootarea!!
            , bex_identifier_dot = c(359, 1, 1)
        )
    )
    options(duflor.default_hsv_spectrums = spectrums)

    #### Configure default identifier-area ####
    options(duflor.default_identifier_area = 0.503) # cm^2

}
#' onUnLoad-routine: Tears down package configuration
#'
#' The function unsets various options used by the package:
#'
#' - "duflor.default_hsv_spectrums"
#' - "duflor.default_identifier_area"
#'
#' @param lib /
#' @param pkg /
#'
#' @keywords internal
#'
.onUnLoad <- function(lib, pkg) {
    # unset options
    options(duflor.default_hsv_spectrums = NULL)
    options(duflor.default_identifier_area = NULL)
}
#' Title
#'
#' @param libname .
#' @param pkgname .
#' @importFrom utils packageDescription
#' @return .
#' @keywords internal
#'
.onAttach <- function(libname,pkgname) {
    packageStartupMessage("Attaching ",pkgname," version ",
                          packageDescription("duflor")$Version, " from library ",libname,".")
}

