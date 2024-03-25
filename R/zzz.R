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
#' 1. by default, the JVM is initialised with `r getOption("duflor.default_java_heapspace")`MB of heap space
#' 2. values lower than 900MB are ignored, and rule 1) takes effect.
#' 3. To set custom values, execute `option(duflor.java_heapspace_in_MB)` **before**
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
#' 3. execute `option(duflor.java_heapspace_in_MB)` with a value >`r getOption("duflor.default_java_heapspace")`MB
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
        # set default values
        default_java_heapspace <- 900
        default_java_heap_unit <- "m"
        options("duflor.default_java_heapspace" = default_java_heapspace)
        options("duflor.default_java_heap_unit" = default_java_heap_unit)

        current_java_heapspace <- getOption("duflor.java_heapspace_in_MB") # retrieve custom preset value

        if (is.null(current_java_heapspace)) { ## assign default heapspace (see above)
            used_java_heapspace <- default_java_heapspace
            rJava::.jpackage(pkg, parameters=str_c("-Xmx",used_java_heapspace,default_java_heap_unit))
            options(java.parameters = str_c("-Xmx",used_java_heapspace,default_java_heap_unit))
        } else {
            # assumption: values are given in MB
            if (current_java_heapspace<default_java_heapspace) { ## if assigned heapspace in MB is < 900MB, overwrite with this value
                used_java_heapspace <- default_java_heapspace
            } else { ## else assign the custom-set value retrieved from options.
                used_java_heapspace <- current_java_heapspace
            }
            ## and now assign it
            rJava::.jpackage(pkg, parameters=str_c("-Xmx",used_java_heapspace,default_java_heap_unit))
            options(java.parameters = str_c("-Xmx",used_java_heapspace,default_java_heap_unit))
        }
        # As a result, we can now store the currently set values:
        options(duflor..used_JVM_heapspace = used_java_heapspace)
        options(duflor..used_JVM_heap_unit = default_java_heap_unit)
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

