#' add 4D-adjacency-grouping to `pixel.idx`-object
#'
#' The function assigns clusters to all coordinate-pairs in `pixel.idx`.
#' A cluster contains all pixels which share a non-diagonal link with each other.
#'
#' This means that points `(1/1)`, `(1/2)` and `(2/2)` are assigned the same cluster,
#' whereas points  `(5/5)` and `(6/6)` are assigned separate clusters.
#'
#' To consider diagonal matches as well, see [diagonal_adjacency()]
#'
#' Reference: <https://stackoverflow.com/a/37946855>
#' @param pixel.idx pixel.idx-object
#' @param sort_by_frequency
#' logical, control if clusters should be enumerated from 1 > N based on the number of elements in them.
#' For more info, see documentation on [duflor::reassign_integers_by_frequency()]
#'
#' @return `pixel.idx` with added 3rd column `clus` mapping to a cluster
#' @export
#' @importFrom stats cutree
#' @importFrom stats dist
#' @importFrom stats hclust
#'
adjacency <- function(pixel.idx,sort_by_frequency = TRUE) {
    #TODO: rename functions to clarify naming conventions
    clus <- cutree(hclust(dist(x = pixel.idx, method = "manhattan"), "single"), h = 1)
    clus <- reassign_integers_by_frequency(clus)
    return(cbind(pixel.idx, clus = clus))
}
