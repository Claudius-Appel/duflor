test_that("loading errors on invalid path", {
    expect_error(load_image(image.path = ""))
})
test_that("loading errors on negative subsetting dimensions", {
    test_path <- load_extdata("duflor-icon.png")
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = -1))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = -1))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = -1))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = -1))
})
test_that("loading errors if ((subset_left>subset_right) || (subset_bottom>subset_top)) ", {
    test_path <- load_extdata("duflor-icon.png")
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = 280,crop_right = 250))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = 250,crop_left = 300))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = 350,crop_bottom = 300))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = 350,crop_top = 300))
})
test_that("loading errors on invalid subsetting types", {
    test_path <- load_extdata("duflor-icon.png")
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = NA))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = NA))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = NA))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = NA))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = ""))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = ""))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = ""))
    expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = ""))
    # expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_left = "0"))
    # expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_right = "0"))
    # expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_top = "0"))
    # expect_error(load_image(image.path = test_path,subset_only = T,return_hsv = T,crop_bottom = "0"))
})
test_that("images must be loaded with 3 channels", {
    test_path_j <- load_extdata("duflor-icon.jpg")
    test_path_p <- load_extdata("duflor-icon.png")
    jpg <- load_image(test_path_j)
    png <- load_image(test_path_p)
    expect_equal(dim(jpg),dim(png))
})
test_that("loaded image is of correct size in HSV color-space", {
    # test that fullsized image is of expected dimensions
    test_path <- load_extdata("duflor-icon.png")
    object_loaded_from_file <- load_image(test_path,return_hsv = T)
    expected_dimensional_info <- c(518,600,1,3)
    retrieved_dimensional_info <- dim(object_loaded_from_file)
    expect_equal(expected_dimensional_info,retrieved_dimensional_info)

    # test that cropped image gets cropped to the right dimensions
    cropped_object_loaded_from_file <- load_image(test_path,subset_only = T,return_hsv = T,crop_left = 219,crop_top = 401)
    cropped_expected_dimensional_info <- c(300,200,1,3)
    cropped_retrieved_dimensional_info <- dim(cropped_object_loaded_from_file)
    expect_equal(cropped_expected_dimensional_info,cropped_retrieved_dimensional_info)
})
test_that("loaded image is of correct size in RGB color-space", {
    # test that fullsized image is of expected dimensions
    test_path <- load_extdata("duflor-icon.png")
    object_loaded_from_file <- load_image(test_path,return_hsv = F)
    expected_dimensional_info <- c(518,600,1,3)
    retrieved_dimensional_info <- dim(object_loaded_from_file)
    expect_equal(expected_dimensional_info,retrieved_dimensional_info)

    # test that cropped image gets cropped to the right dimensions
    cropped_object_loaded_from_file <- load_image(test_path,subset_only = T,return_hsv = F,crop_left = 219,crop_top = 401)
    cropped_expected_dimensional_info <- c(300,200,1,3)
    cropped_retrieved_dimensional_info <- dim(cropped_object_loaded_from_file)
    expect_equal(cropped_expected_dimensional_info,cropped_retrieved_dimensional_info)

})
