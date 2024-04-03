test_that("loading errors on invalid path", {
    expect_error(load_image(image.path = ""))
})
test_that("loading errors on negative subsetting dimensions", {
    load_extdata <- function(path = NULL) {
        if (is.null(path)) {
            dir(system.file("extdata", package = "duflor"),full.names = T)
        } else {
            system.file("extdata", path, package = "duflor", mustWork = TRUE)
        }
    }
    test_path <- load_extdata("duflor-icon.png")
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = -1))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = -1))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = -1))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = -1))
})
test_that("loading errors if ((subset_left>subset_right) || (subset_bottom>subset_top)) ", {
    load_extdata <- function(path = NULL) {
        if (is.null(path)) {
            dir(system.file("extdata", package = "duflor"),full.names = T)
        } else {
            system.file("extdata", path, package = "duflor", mustWork = TRUE)
        }
    }
    test_path <- load_extdata("duflor-icon.png")
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = 280,crop_right = 250))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = 250,crop_left = 300))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = 350,crop_bottom = 300))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = 350,crop_top = 300))
})
test_that("loading errors on invalid subsetting types", {
    load_extdata <- function(path = NULL) {
        if (is.null(path)) {
            dir(system.file("extdata", package = "duflor"),full.names = T)
        } else {
            system.file("extdata", path, package = "duflor", mustWork = TRUE)
        }
    }
    test_path <- load_extdata("duflor-icon.png")
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = NA))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = NA))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = NA))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = NA))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = ""))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = ""))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = ""))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = ""))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = "0"))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = "0"))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = "0"))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = "0"))
})
