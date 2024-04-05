test_that("target.color must be a valid name", {
    test_path <- load_extdata("duflor-icon.png")
    spectrums <- getOption("duflor.default_hsv_spectrums")
    test_arr <- load_image(test_path)
    ret <- extract_pixels_HSV(
        pixel.array = test_arr,
        lower_bound = spectrums$lower_bound,
        upper_bound = spectrums$upper_bound
        )

    expect_error(
        change_pixel_color_HSV(
            pixel.array = test_arr,
            pixel.idx = ret$bex_identifier_dot$pixel.idx,
            target.color = "red99",
            mask_extreme = F
            )
        )
})
test_that("target.color must be a valid vector", {
    test_path <- load_extdata("duflor-icon.png")
    spectrums <- getOption("duflor.default_hsv_spectrums")
    test_arr <- load_image(test_path)
    ret <- extract_pixels_HSV(
        pixel.array = test_arr,
        lower_bound = spectrums$lower_bound,
        upper_bound = spectrums$upper_bound
        )
  expect_error(
        change_pixel_color_HSV(
            pixel.array = test_arr,
            pixel.idx = ret$bex_identifier_dot$pixel.idx,
            target.color = c(255,0,0,1),
            mask_extreme = F
            )
        )
})
