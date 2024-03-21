#' onLoad-routine: Initialises package configuration
#'
#' The function initialises various options used by the package upon loading:
#' 1. Most notably, it initialises the javaVM required for loading subsets of images.
#' 2. The following options are initialised:
#'     - `duflor.default_hsv_spectrums`
#'     - `duflor.default_identifier_area`
#'     - `duflor.java_available`
#'     - `duflor.RBF_available`
#'
#' **Rules for the initialisation of the JavaVM**:
#' 1. by default, the JVM is initialised with 8GB of heap space
#' 2. values lower than 8GB/8000MB are ignored, and rule 1) takes effect.
#' 3. To set custom values, execute `option(duflor.java_heapspace_in_GB)` **before**
#'    loading the package (see notes below)
#'
#'
#' @note
#' When a package `pkg` is attached (see Link below) via `library(pkg)`, or the first time
#' a function of the package `pkg` is called via `pkg::fun()`, this function
#' gets called.
#'
#' If the package runs into an `java.lang.OutOfMemoryError: Java heap space`-error
#' while loading images via [load_image()], the heapspace of the JVM is not sufficiently large.
#' To resolve this issue,
#'
#' 1. save your work
#' 2. initialise a new R-session
#' 3. execute `option(duflor.java_heapspace_in_GB)` with a value `>8`
#' 4. load _duflor_ via `library(duflor)` or by calling `duflor::any_function()`
#' 5. optionally check the console output to validate that the correct value was set.
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
#' @importFrom utils packageDescription
#' @importFrom utils installed.packages
#' @importFrom stringr str_c
#'
.onLoad <- function(lib, pkg) {
    #### Java VM Configuration and Check for availability of rJava and RBioFormats ####
    if (isTRUE(as.logical(length(find.package("rJava",quiet = T))>0))) {

        # rJava is installed, so set rJava-related stuff and allow image-subsetting
        # **we cannot use `requireNamespace()` to check because it**
        # **immediately loads the package as well.**
        # as we need to configure some options prior to that happening, this would not work.

        #### initialise java VM ####
        current_java_heapspace <- getOption("duflor.java_heapspace_in_GB")
        if (is.null(current_java_heapspace)) {
            current_java_heapspace <- 8
            heap_unit <- "g"
            # packageStartupMessage("no val set, init with 8g")
            rJava::.jpackage(pkg, parameters=str_c("-Xmx8",heap_unit))
            options(java.parameters = str_c("-Xmx8",heap_unit))
        } else {
            if (current_java_heapspace<1) { # given in GB, must be converted to MB
                current_java_heapspace <- current_java_heapspace * 1000
                heap_unit <- "m"
            } else {
                heap_unit <- "g"
            }
            if (heap_unit=="m") {
                if (current_java_heapspace<8000) {
                    # less than 8000MB is to be set, set 8000MB
                    # packageStartupMessage(str_c("val<8000mb set, init with 8000"," ",heap_unit))
                    rJava::.jpackage(pkg, parameters=str_c("-Xmx8000",heap_unit))
                    options(java.parameters = str_c("-Xmx8000",heap_unit))
                } else {
                    # set more than 8000MB
                    # packageStartupMessage(str_c("val>8000mb set, init with ",current_java_heapspace," ",heap_unit))
                    rJava::.jpackage(pkg, parameters=str_c("-Xmx",current_java_heapspace,heap_unit))
                    options(java.parameters = str_c("-Xmx",current_java_heapspace,heap_unit))
                }
            } else if (heap_unit=="g") {
                if (current_java_heapspace<8) {
                    # less than 8GB is to be set, set 8GB
                    # packageStartupMessage(str_c("val<8gb set, init with 8"," ",heap_unit))
                    rJava::.jpackage(pkg, parameters=str_c("-Xmx8",heap_unit))
                    options(java.parameters = str_c("-Xmx8",heap_unit))
                } else {
                    # more than 8GB is to be set
                    # packageStartupMessage(str_c("val>8g set, init with ",current_java_heapspace," ",heap_unit))
                    rJava::.jpackage(pkg, parameters=str_c("-Xmx",current_java_heapspace,heap_unit))
                    options(java.parameters = str_c("-Xmx",current_java_heapspace,heap_unit))
                }
            }
        }
        # As a result, we can now store the currently set values:
        options(duflor..used_JVM_heapspace = current_java_heapspace)
        options(duflor..used_JVM_heap_unit = heap_unit)
        #### check RBioFormats availability ####
        options(duflor.java_available = requireNamespace("rJava",quietly = T))
        options(duflor.RBF_available = requireNamespace("RBioFormats",quietly = T))
    } else {
        # rJava is not installed, we don't have to set rJava-related stuff
        #### check RBioFormats availability ####
        options(duflor.java_available = requireNamespace("rJava",quietly = T))
        options(duflor.RBF_available = requireNamespace("RBioFormats",quietly = T))
    }


    #### Configure default spectrum-definitions ####
    spectrums <- list(
    bex_lower_bound_drought_HSV = c(20, 0.25, 0.20)
    , bex_upper_bound_drought_HSV = c(69, 1, 0.80)
    , bex_lower_bound_green_HSV = c(69, 0.25, 0.20)
    , bex_upper_bound_green_HSV = c(150, 1, 0.80)
    , bex_lower_bound_complete_HSV = c(20, 0.25, 0.20)
    , bex_upper_bound_complete_HSV = c(150, 1, 0.80)
    , bex_lower_bound_identifier_dot = c(270, 0.2, 0.28)
    , bex_upper_bound_identifier_dot = c(359, 1, 1)
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
#' - "duflor.java_available"
#' - "duflor.RBF_available"
#' - "duflor..used_JVM_heapspace"
#' - "duflor..used_JVM_heap_unit"
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
    options(duflor.java_available = NULL)
    options(duflor.RBF_available = NULL)
    options(duflor..used_JVM_heapspace = NULL)
    options(duflor..used_JVM_heap_unit = NULL)
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
    if (!requireNamespace("RBioFormats",quietly = T)) {
        packageStartupMessage("The package RBioFormats is not available. Image subsetting during loading relies on RBioFormats. Image-Loading will default to reading the complete image.")
    } else {
        if (requireNamespace("rJava",quietly = T)) {
            packageStartupMessage(
                "Package 'RBioFormats' is available. Image subsetting during loading is possible. The Java-VM was initialised with ",
                getOption("duflor..used_JVM_heapspace"),
                getOption("duflor..used_JVM_heap_unit")
                ," of heap space")
        } else {
            packageStartupMessage("Package 'RBioFormats' is available, but its dependency 'rJava' is not. Image subsetting relies on RBioFormats. Image-Loading will default to reading the complete image.")

        }
    }
}

