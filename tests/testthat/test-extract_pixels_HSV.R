load_extdata <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "duflor"),full.names = T)
    } else {
        system.file("extdata", path, package = "duflor", mustWork = TRUE)
    }
}
test_that("single-spectrum: all extraction methods return identical results", {
    #' This test is likely overengineered and overkill, but given that this is
    #' _the_ central step of the pipeline, I'd rather have it overtested than understested

    #### setup ####
    test_path <- load_extdata("duflor-icon.png")
    object_loaded_from_file <- load_image(test_path)
    library(duflor)
    ## load example data
    file_path <- load_extdata("duflor-icon.png")
    pixel.array <- load_image(file_path,F,T)
    spectrums <- getOption("duflor.default_hsv_spectrums")
    spectrums$lower_bound <- duflor:::remove_key_from_list(spectrums$lower_bound,c("bex_root_HSV","bex_complete_HSV","bex_green_HSV","bex_drought_HSV"))
    spectrums$upper_bound <- duflor:::remove_key_from_list(spectrums$upper_bound,c("bex_root_HSV","bex_complete_HSV","bex_green_HSV","bex_drought_HSV"))
    ## convert spectrums to matrix
    nlb <- do.call(rbind,spectrums$lower_bound)
    nub <- do.call(rbind,spectrums$upper_bound)
    ## strip dimnames-attributes
    dimnames(nlb) <- c()
    dimnames(nub) <- c()
    #### get results ####
    # single-iteration-CPP
    result1 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = nlb,
                                  upper_bound = nub,
                                  fast_eval = T,
                                  bundle_pixelarray = F,
                                  check_value = T,
                                  use_single_iteration_cpp = T
    )
    # multi-iteration-CPP
    result2 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = nlb,
                                  upper_bound = nub,
                                  fast_eval = T,
                                  bundle_pixelarray = F,
                                  check_value = T,
                                  use_single_iteration_cpp = F
    )
    # no-CPP
    result3 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = nlb,
                                  upper_bound = nub,
                                  fast_eval = F,
                                  bundle_pixelarray = F,
                                  check_value = T,
                                  use_single_iteration_cpp = F
    )
    #### ensure equivalence ####
    ## validate pixel.idx
    expect_equal(object = result1[[1]]$pixel.idx,expected = result2[[1]]$pixel.idx)
    expect_equal(object = result1[[1]]$pixel.idx,expected = result3[[1]]$pixel.idx)
    expect_true((sum(isFALSE(result1[[1]]$pixel.idx==result2[[1]]$pixel.idx))==0))
    expect_true((sum(isFALSE(result1[[1]]$pixel.idx==result3[[1]]$pixel.idx))==0))
    ## validate pixel.count
    for (name in names(result1)) {
        expect_equal(object = result1[[1]]$pixel.count,expected = result2[[1]]$pixel.count)
        expect_equal(object = result1[[1]]$pixel.count,expected = result3[[1]]$pixel.count)
    }
    expect_true((sum(isFALSE(result1[[1]]$pixel.count==result2[[1]]$pixel.count))==0))
    expect_true((sum(isFALSE(result1[[1]]$pixel.count==result3[[1]]$pixel.count))==0))
    ## validate img.fraction
    for (name in names(result1)) {
        expect_equal(object = result1[[1]]$img.fraction,expected = result2[[1]]$img.fraction)
        expect_equal(object = result1[[1]]$img.fraction,expected = result3[[1]]$img.fraction)
    }
    expect_true((sum(isFALSE(result1[[1]]$img.fraction==result2[[1]]$img.fraction))==0))
    expect_true((sum(isFALSE(result1[[1]]$img.fraction==result3[[1]]$img.fraction))==0))
    ## expect object-size is identical with an arbitrarily set tolerance of 0.1%
    expect_equal(object = object.size(result1),expected = object.size(result2),tolerance = 0.001)
    expect_equal(object = object.size(result1),expected = object.size(result3),tolerance = 0.001)
})
test_that("multi-spectrums: all extraction methods return identical results", {
    #' This test is likely overengineered and overkill, but given that this is
    #' _the_ central step of the pipeline, I'd rather have it overtested than understested

    #### setup ####
    test_path <- load_extdata("duflor-icon.png")
    object_loaded_from_file <- load_image(test_path)
    library(duflor)
    ## load example data
    file_path <- load_extdata("duflor-icon.png")
    pixel.array <- load_image(file_path,F,T)
    spectrums <- getOption("duflor.default_hsv_spectrums")
    ## convert spectrums to matrix
    nlb <- do.call(rbind,spectrums$lower_bound)
    nub <- do.call(rbind,spectrums$upper_bound)
    ## strip dimnames-attributes
    dimnames(nlb) <- c()
    dimnames(nub) <- c()
    #### get results ####
    # single-iteration-CPP
    result1 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = spectrums$lower_bound,
                                  upper_bound = spectrums$upper_bound,
                                  fast_eval = T,
                                  bundle_pixelarray = F,
                                  check_value = T,
                                  use_single_iteration_cpp = T
                                  )
    # multi-iteration-CPP
    result2 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = spectrums$lower_bound,
                                  upper_bound = spectrums$upper_bound,
                                  fast_eval = T,
                                  bundle_pixelarray = F,
                                  check_value = T,

                                  use_single_iteration_cpp = F
                                  )
    # no-CPP
    result3 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = spectrums$lower_bound,
                                  upper_bound = spectrums$upper_bound,
                                  fast_eval = F,
                                  bundle_pixelarray = F,
                                  check_value = T,
                                  use_single_iteration_cpp = F
                                  )
    #### ensure equivalence ####
    ## validate pixel.idx
    for (name in names(result1)) {
        expect_equal(object = result1[[name]]$pixel.idx,expected = result2[[name]]$pixel.idx)
        expect_equal(object = result1[[name]]$pixel.idx,expected = result3[[name]]$pixel.idx)
    }
    expect_true((sum(isFALSE(result1$bex_green_HSV$pixel.idx==result2$bex_green_HSV$pixel.idx))==0))
    expect_true((sum(isFALSE(result1$bex_green_HSV$pixel.idx==result3$bex_green_HSV$pixel.idx))==0))
    ## validate pixel.count
    for (name in names(result1)) {
        expect_equal(object = result1[[name]]$pixel.count,expected = result2[[name]]$pixel.count)
        expect_equal(object = result1[[name]]$pixel.count,expected = result3[[name]]$pixel.count)
    }
    expect_true((sum(isFALSE(result1$bex_green_HSV$pixel.count==result2$bex_green_HSV$pixel.count))==0))
    expect_true((sum(isFALSE(result1$bex_green_HSV$pixel.count==result3$bex_green_HSV$pixel.count))==0))
    ## validate img.fraction
    for (name in names(result1)) {
        expect_equal(object = result1[[name]]$img.fraction,expected = result2[[name]]$img.fraction)
        expect_equal(object = result1[[name]]$img.fraction,expected = result3[[name]]$img.fraction)
    }
    expect_true((sum(isFALSE(result1$bex_green_HSV$img.fraction==result2$bex_green_HSV$img.fraction))==0))
    expect_true((sum(isFALSE(result1$bex_green_HSV$img.fraction==result3$bex_green_HSV$img.fraction))==0))
    ## expect object-size is identical with an arbitrarily set tolerance of 0.1%
    expect_equal(object = object.size(result1),expected = object.size(result2),tolerance = 0.001)
    expect_equal(object = object.size(result1),expected = object.size(result3),tolerance = 0.001)
})
test_that("lower_bound/upper_bound define same sets", {
    library(duflor)
    ## load example data
    spectrums <- getOption("duflor.default_hsv_spectrums")
    spectrums$lower_bound <- remove_key_from_list(spectrums$lower_bound,c("bex_drought_HSV"))
    expect_error(extract_pixels_HSV(pixel.array = c(),
                                    lower_bound = spectrums$lower_bound,
                                    upper_bound = spectrums$upper_bound,
                                    fast_eval = T,
                                    bundle_pixelarray = F,
                                    check_value = T,
                                    use_single_iteration_cpp = T
    ))
    spectrums$lower_bound$non_matched_key <- c(1,2,3)
    expect_error(extract_pixels_HSV(pixel.array = c(),
                                    lower_bound = spectrums$lower_bound,
                                    upper_bound = spectrums$upper_bound,
                                    fast_eval = T,
                                    bundle_pixelarray = F,
                                    check_value = T,
                                    use_single_iteration_cpp = T
    ))
})
test_that("single-spectrum: pixel.array can be bundled into the return-structure successfully", {
    #### setup ####
    ## load example data
    file_path <- load_extdata("duflor-icon.png")
    pixel.array <- load_image(file_path,F,T)
    spectrums <- getOption("duflor.default_hsv_spectrums")
    spectrums$lower_bound <- duflor:::remove_key_from_list(spectrums$lower_bound,c("bex_root_HSV","bex_complete_HSV","bex_green_HSV","bex_drought_HSV"))
    spectrums$upper_bound <- duflor:::remove_key_from_list(spectrums$upper_bound,c("bex_root_HSV","bex_complete_HSV","bex_green_HSV","bex_drought_HSV"))
    ## convert spectrums to matrix
    nlb <- do.call(rbind,spectrums$lower_bound)
    nub <- do.call(rbind,spectrums$upper_bound)
    ## strip dimnames-attributes
    dimnames(nlb) <- c()
    dimnames(nub) <- c()
    #### get results ####
    # single-iteration-CPP
    result1 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = nlb,
                                  upper_bound = nub,
                                  fast_eval = T,
                                  bundle_pixelarray = T,
                                  check_value = T,
                                  use_single_iteration_cpp = T
    )
    # multi-iteration-CPP
    result2 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = nlb,
                                  upper_bound = nub,
                                  fast_eval = T,
                                  bundle_pixelarray = T,
                                  check_value = T,
                                  use_single_iteration_cpp = F
    )
    # no-CPP
    result3 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = nlb,
                                  upper_bound = nub,
                                  fast_eval = F,
                                  bundle_pixelarray = T,
                                  check_value = T,
                                  use_single_iteration_cpp = F
    )

    expect_true(hasName(result1,"pixel.array"))
    expect_true(hasName(result2,"pixel.array"))
    expect_true(hasName(result3,"pixel.array"))
})
test_that("multi-spectrums: pixel.array can be bundled into the return-structure successfully", {
    #### setup ####
    test_path <- load_extdata("duflor-icon.png")
    object_loaded_from_file <- load_image(test_path)
    library(duflor)
    ## load example data
    file_path <- load_extdata("duflor-icon.png")
    pixel.array <- load_image(file_path,F,T)
    spectrums <- getOption("duflor.default_hsv_spectrums")
    spectrums$lower_bound <- duflor:::remove_key_from_list(spectrums$lower_bound,c("bex_complete_HSV","bex_green_HSV","bex_drought_HSV"))
    spectrums$upper_bound <- duflor:::remove_key_from_list(spectrums$upper_bound,c("bex_complete_HSV","bex_green_HSV","bex_drought_HSV"))
    #### get results ####
    # single-iteration-CPP
    result1 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = spectrums$lower_bound,
                                  upper_bound = spectrums$upper_bound,
                                  fast_eval = T,
                                  bundle_pixelarray = T,
                                  check_value = T,
                                  use_single_iteration_cpp = T
    )
    # multi-iteration-CPP
    result2 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = spectrums$lower_bound,
                                  upper_bound = spectrums$upper_bound,
                                  fast_eval = T,
                                  bundle_pixelarray = T,
                                  check_value = T,
                                  use_single_iteration_cpp = F
    )
    # no-CPP
    result3 <- extract_pixels_HSV(pixel.array = pixel.array,
                                  lower_bound = spectrums$lower_bound,
                                  upper_bound = spectrums$upper_bound,
                                  fast_eval = F,
                                  bundle_pixelarray = T,
                                  check_value = T,
                                  use_single_iteration_cpp = F
    )

    expect_true(hasName(result1,"pixel.array"))
    expect_true(hasName(result2,"pixel.array"))
    expect_true(hasName(result3,"pixel.array"))
})
