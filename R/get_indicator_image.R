#' wrapper around `change_pixel_color_HVS()` for the intention of clarity.
#'
#' @inheritParams change_pixel_color_HSV
#'
#' @return `pixel.array` with hsv-values of pixels at positions `pixel.idx` modified.
#' @export
#'
get_indicator_image <- function(pixel.array, pixel.idx, target.color, mask_extreme = F) {
    return(change_pixel_color_HSV(
        pixel.array = pixel.array,
        pixel.idx = pixel.idx,
        target.color = target.color,
        mask_extreme = mask_extreme
    ))
}
