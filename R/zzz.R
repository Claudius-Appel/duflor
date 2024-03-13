#' Title
#'
#' @param lib /
#' @param pkg /
#' @return .
#' @keywords internal
#' @noRd
#'
#' @importFrom utils packageDescription
#'
.onLoad <- function(lib, pkg) {
    # curr_memory <- getOption("java.parameters")
    # options(dfl.java_parameters = curr_memory)
    # GB <- 8
    # if (is.null(curr_memory)) {
    #     options(java.parameters = paste0("-Xmx",GB,"g"))
    #     packageStartupMessage(paste0("1Option <java.parameters> was to '",getOption("java.parameters"),"'"))
    # } else {
    #     matches <- regexec("(\\d+)(.*)", curr_memory)
    #     value <- as.integer(regmatches(curr_memory, matches)[[1]][[2]])
    #     unit <- regmatches(curr_memory, matches)[[1]][[3]]
    #     if (isFALSE(unit=="g")) { # we loaded less than a gigabyte - so we must stock up
    #         options(java.parameters = paste0("-Xmx",GB,"g"))
    #         packageStartupMessage(paste0("2Option <java.parameters> was set to '",getOption("java.parameters"),"'"))
    #     } else {
    #         if (value<GB) {
    #             options(java.parameters = paste0("-Xmx",GB,"g"))
    #             packageStartupMessage(paste0("3Option <java.parameters> was set to '",getOption("java.parameters"),"'"))
    #         } else {
    #             packageStartupMessage(paste0("4<java.parameters> is already set to '",getOption("java.parameters"),"'"))
    #         }
    #     }
    # }
    # library(RBioFormats)
    # invisible()
}
.onUnLoad <- function(lib, pkg) {
    # ## reset Java parameters
    # options(java.parameters = getOption("dfl.java_parameters"))
    # options(dfl.java_parameters = NULL)
}
#' Title
#'
#' @param libname .
#' @param pkgname .
#' @importFrom utils packageDescription
#' @return .
#' @keywords internal
#' @noRd
#'
.onAttach <- function(libname,pkgname) {
    packageStartupMessage("Attaching ",pkgname," version ",
                          packageDescription("duflor")$Version, " from library ",libname,".")
    packageStartupMessage("Currently-set <java_parameters>: '",getOption("java.parameters"),"'")
}

