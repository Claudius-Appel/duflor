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
        apply_HSV_color_by_mask(
            pixel.array = test_arr,
            pixel.idx = ret$bex_identifier_dot$pixel.idx,
            target.color = "red99",
            mask_extreme = F
            )
        )
    expect_no_error(
        apply_HSV_color_by_mask(
            pixel.array = test_arr,
            pixel.idx = ret$bex_identifier_dot$pixel.idx,
            target.color = "red1",
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
        apply_HSV_color_by_mask(
            pixel.array = test_arr,
            pixel.idx = ret$bex_identifier_dot$pixel.idx,
            target.color = c(255,0,0,1),
            mask_extreme = F
            )
        )
})
test_that("target.color must be a length-3-vector of numerics in range [0,255]", {
    test_path <- load_extdata("duflor-icon.png")
    spectrums <- getOption("duflor.default_hsv_spectrums")
    test_arr <- load_image(test_path)
    ret <- extract_pixels_HSV(
        pixel.array = test_arr,
        lower_bound = spectrums$lower_bound,
        upper_bound = spectrums$upper_bound
        )
  expect_no_error(apply_HSV_color_by_mask(
            pixel.array = test_arr,
            pixel.idx = ret$bex_identifier_dot$pixel.idx,
            target.color = c(255,255,255),
            mask_extreme = F
            )
        )
})
test_that("elements of target.color outside of range [0,255] get set to respective boundaries", {
    test_path <- load_extdata("duflor-icon.png")
    spectrums <- getOption("duflor.default_hsv_spectrums")
    test_arr <- load_image(test_path)
    ret <- extract_pixels_HSV(
        pixel.array = test_arr,
        lower_bound = spectrums$lower_bound,
        upper_bound = spectrums$upper_bound
        )
  expect_warning(
        apply_HSV_color_by_mask(
            pixel.array = test_arr,
            pixel.idx = ret$bex_identifier_dot$pixel.idx,
            target.color = c(256,255,255),
            mask_extreme = F
            )
        )
  expect_warning(
        apply_HSV_color_by_mask(
            pixel.array = test_arr,
            pixel.idx = ret$bex_identifier_dot$pixel.idx,
            target.color = c(-50,255,256),
            mask_extreme = F
            )
        )
})
