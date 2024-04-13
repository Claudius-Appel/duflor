#' return coordinates by cluster_id from `pixel.idx`
#'
#' @inheritParams .main_args
#' @param cluster_id index of the to-be-retrieved cluster
#'
#' @return `pixel.idx` with added 3rd column `clus` mapping to a cluster
#' @export
#'
#' @examples
#' \dontrun{
#' pixels <- extract_pixels_HSV(...) # extract pixels of a certain color-range
#' adjacency <- adjacency(pixels$identifier$pixel.idx) # assign clusters
#' coords <- retrieve_adjacency_coords(adjacency,1) # retrieve coordinates of first cluster
#'
#' plot_array_as_image_sRGB( # display result
#'     HSVtoRGB(
#'         apply_HSV_color_by_mask(
#'             pixel.array,
#'             coords,
#'             target.color = "white",
#'             mask_extreme = T
#'             )
#'         )
#'     )
#' }
retrieve_adjacency_coords <- function(pixel.idx,cluster_id) {
    pixel.idx[pixel.idx[,"clus"]==cluster_id,]
}
