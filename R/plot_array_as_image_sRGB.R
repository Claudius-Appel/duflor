#' plot an rgb-array via graphics-package
#'
#' @param rgb.array rgb-formatted image-array.
#' @param main title to pot above the plot as a sub-title. is usually not rendered properly
#'
#' @return /
#' @export
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics rasterImage
#' @importFrom graphics title
#'
#' @examples
#' \dontrun{
#' plot_array_as_image_sRGB(
#'     HSVtoRGB(
#'         imager::load.example("parrots")
#'         ),
#'     main = str_c("parrots-example")
#' )
#' }
plot_array_as_image_sRGB <- function(rgb.array, main = "title") {
    # plot rgb data as raster
    # HSV>sRGB-data
    rgb.array <- rgb.array[1:dim(rgb.array)[1], 1:dim(rgb.array)[2], , 1:3]
    rgb.array <- aperm(rgb.array, c(2, 1, 3))
    rgb.array[, , 1] <- norm_to_range_01(rgb.array[, , 1])
    rgb.array[, , 2] <- norm_to_range_01(rgb.array[, , 2])
    rgb.array[, , 3] <- norm_to_range_01(rgb.array[, , 3])
    if (length(dim(rgb.array)) != 3) {
        stop("RGB_array must be an array of three dimensions (pixel rows,\n             pixel columns, and color channels)")
    } else if (dim(rgb.array)[3] != 3) {
        warning("Provided array has more than 3 channels; using only the\n                first 3 as R, G, and B channels")
    }
    op <- graphics::par(mar = c(0, 0, 0, 0))
    asp <- dim(rgb.array)[1] / dim(rgb.array)[2]
    graphics::plot(0:10, 0:10, type = "n", ann = F, axes = T, asp = asp)
    graphics::rasterImage(rgb.array, xleft = 0, ybottom = 0, xright = 10, ytop = 10)
    graphics::title(sub = main, line = 0)
    graphics::par(op)
}
