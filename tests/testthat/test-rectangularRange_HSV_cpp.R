load_extdata <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "duflor"),full.names = T)
    } else {
        system.file("extdata", path, package = "duflor", mustWork = TRUE)
    }
}

test_that("return-value has proper dimensions [n_hits,2]", {
    #### setup ####
    test_path <- load_extdata("duflor-icon.png")
    ## load example data
    pixel.array <- load_image(test_path,F,T)
    spectrums <- getOption("duflor.default_hsv_spectrums")
    for (name in names(spectrums$lower_bound)) {
        a <- rectangularRange_HSV_cpp(
            H = pixel.array[, , , 1],
            S = pixel.array[, , , 2],
            V = pixel.array[, , , 3],
            upper_bound = spectrums$upper_bound[[name]],
            lower_bound = spectrums$lower_bound[[name]],
            image_width = dim(pixel.array)[1],
            check_V = T
        )
        # print(dim(a)[1])
        # print(dim(a)[2])
        expect_equal(dim(a)[[2]],2)
    }

    # expect_error(
    #     rectangularRange_HSV_cpp(
    #         H = pixel.array[, , , 1],
    #         S = pixel.array[, , , 2],
    #         V = pixel.array[, , , 3],
    #         upper_bound = spectrums$upper_bound$bex_drought_HSV,
    #         lower_bound = spectrums$lower_bound$bex_drought_HSV,
    #         image_width = dim(pixel.array)[1],
    #         check_V = T
    #     )
    # )
    # expect_error(
    #     rectangularRange_HSV_cpp(
    #         H = pixel.array[, , , 1],
    #         S = pixel.array[, , , 2],
    #         V = pixel.array[, , , 3],
    #         upper_bound = spectrums$upper_bound,
    #         lower_bound = spectrums$lower_bound,
    #         image_width = dim(pixel.array)[1],
    #         check_V = T
    #     )
    # )
    # expect_error(
    #     rectangularRange_HSV_cpp(
    #         H = pixel.array[, , , 1],
    #         S = pixel.array[, , , 2],
    #         V = pixel.array[, , , 3],
    #         upper_bound = spectrums$upper_bound,
    #         lower_bound = spectrums$lower_bound,
    #         image_width = dim(pixel.array)[1],
    #         check_V = T
    #     )
    # )
    # expect_error(
    #     rectangularRange_HSV_cpp(
    #         H = pixel.array[, , , 1],
    #         S = pixel.array[, , , 2],
    #         V = pixel.array[, , , 3],
    #         upper_bound = spectrums$upper_bound,
    #         lower_bound = spectrums$lower_bound,
    #         image_width = dim(pixel.array)[1],
    #         check_V = T
    #     )
    # )


})
test_that("invalid boundary dimensions return errors", {
    #### setup ####
    test_path <- load_extdata("duflor-icon.png")
    ## load example data
    pixel.array <- load_image(test_path,F,T)
    ## check_V = FALSE
    expect_error(rectangularRange_HSV_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(),
        lower_bound = c(),
        image_width = dim(pixel.array)[1],
        check_V =F))
    expect_error(rectangularRange_HSV_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(1),
        lower_bound = c(1),
        image_width = dim(pixel.array)[1],
        check_V =F))
    # expect_error(rectangularRange_HSV_cpp(
    #     H = pixel.array[, , , 1],
    #     S = pixel.array[, , , 2],
    #     V = pixel.array[, , , 3],
    #     upper_bound = c(1,2),
    #     lower_bound = c(1,2),
    #     image_width = dim(pixel.array)[1],
    #     check_V =F))
    expect_error(rectangularRange_HSV_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(1,2,3,4),
        lower_bound = c(1,2,3,4),
        image_width = dim(pixel.array)[1],
        check_V =F))
    expect_error(rectangularRange_HSV_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(1,2,3),
        lower_bound = c(1),
        image_width = dim(pixel.array)[1],
        check_V =F))
    ## check_V = TRUE
    expect_error(rectangularRange_HSV_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(),
        lower_bound = c(),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(1),
        lower_bound = c(1),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(1,2),
        lower_bound = c(1,2),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(1,2,3),
        lower_bound = c(1,2),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(1,2,3,4),
        lower_bound = c(1,2,3,4),
        image_width = dim(pixel.array)[1],
        check_V =T))
})
# test_that("check_V", {
#   expect_equal(2 * 2, 4)
#   Figure out how to test this parameter even
# })


#' TO BE DONE FOR ALL RECTRANGE-FUNCTIONS.
#' CURRENTLY, THEY ARE ALL ONLY TESTED
#' IMPLICITLY VIA THE `EXTRACT_PIXELS_HSV`-TESTS
#'
#'
# don't like that
#' stuff to test:
#' - missing args
#' - bounds are not of correct dimensions
#' - check_V
