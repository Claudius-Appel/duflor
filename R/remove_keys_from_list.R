#' remove keys from a list by name
#'
#' @param list A normal list
#' @param keys A vector of keys to be removed from `list`
#'
#' @return `list` without the members of  `keys`
#' @keywords internal
#'
#' @examples
#' list <- list(A = 1, B = "water", C = "0", D = 99)
#' duflor:::remove_key_from_list(list = list,c("C","D"))
remove_key_from_list <- function(list, keys) {
    if (isFALSE(is.list(list))) {
        warning("Parameter 'list': not a list. Returning unmodified input 'list'")
        return(list)
    }
    if (is.null(names(list))) {
        return(list)
    }
    for (key_name in keys) {
        if (key_name %in% names(list)) {
            list[[key_name]] <- NULL
        } else {
            warning("Key '", key_name, "' not found in the list.\n", sep = "")
        }
    }
    return(list)
}
