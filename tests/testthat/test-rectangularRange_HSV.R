load_extdata <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "duflor"),full.names = T)
    } else {
        system.file("extdata", path, package = "duflor", mustWork = TRUE)
    }
}

test_that("missing arguments fail", {
    #### setup ####
    test_path <- load_extdata("duflor-icon.png")
    ## load example data
    pixel.array <- load_image(test_path,F,T)
    # spectrums <- getOption("duflor.default_hsv_spectrums")
    expect_error(rectangularRange_HSV())
    expect_error(rectangularRange_HSV(pixel.array = pixel.array))
    expect_error(rectangularRange_HSV(pixel.array = pixel.array,upper_bound = ""))
    expect_error(rectangularRange_HSV(pixel.array = pixel.array,lower_bound = ""))


})
test_that("empty boundaries return empty arraay", {
    #### setup ####
    test_path <- load_extdata("duflor-icon.png")
    ## load example data
    pixel.array <- load_image(test_path,F,T)
    object <- rectangularRange_HSV(pixel.array = pixel.array,upper_bound = "","",check_V = T)
    expected <- matrix(integer(),nrow = 0,ncol = 2)
    dimnames(expected)[[2]] <- list("x","y")
    expect_equal(object, expected)
})
