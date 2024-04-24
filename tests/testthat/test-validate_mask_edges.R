load_extdata <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "duflor"),full.names = T)
    } else {
        system.file("extdata", path, package = "duflor", mustWork = TRUE)
    }
}
test_that("masks touching the edge get reported", {
    file_path <- load_extdata("duflor-icon.png")
    pixel.array <- load_image(file_path,subset_only = F,return_hsv = T)
    spectrums <- getOption("duflor.default_hsv_spectrums")
    spectrums$lower_bound <- duflor:::remove_key_from_list(spectrums$lower_bound,c("bex_root_HSV","bex_green_HSV","bex_drought_HSV"))
    spectrums$upper_bound <- duflor:::remove_key_from_list(spectrums$upper_bound,c("bex_root_HSV","bex_green_HSV","bex_drought_HSV"))
    ## convert spectrums to matrix
    nlb <- do.call(rbind,spectrums$lower_bound)
    nub <- do.call(rbind,spectrums$upper_bound)
    ## strip dimnames-attributes
    dimnames(nlb) <- c()
    dimnames(nub) <- c()
    result <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = spectrums$lower_bound,
                                  upper_bound = spectrums$upper_bound,
                                  fast_eval = T,
                                  bundle_pixelarray = F,
                                  check_value = T,
                                  use_single_iteration_cpp = T
    )
    edge_checks <- list()
    for (mask in names(result)) {
        suppressWarnings(

            edge_checks[[mask]] <-  duflor:::validate_mask_edges(result[[mask]]$pixel.idx,mask, dim(pixel.array)[1:2],file_path)
        )
    }
    expect_true(edge_checks$bex_complete_HSV$left)
    expect_true(edge_checks$bex_complete_HSV$right)
    expect_false(edge_checks$bex_complete_HSV$top)
    expect_true(edge_checks$bex_complete_HSV$bottom)
    expect_false(edge_checks$bex_identifier_dot$left)
    expect_false(edge_checks$bex_identifier_dot$right)
    expect_false(edge_checks$bex_identifier_dot$top)
    expect_false(edge_checks$bex_identifier_dot$bottom)
})
