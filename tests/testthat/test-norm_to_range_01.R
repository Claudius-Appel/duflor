test_that("normalised to range [0,1]", {
    x <- runif(n = 50, min = 1, max = 10)
    expect_equal(max(norm_to_range_01(x)),1)
    expect_equal(min(norm_to_range_01(x)),0)
})
test_that("invalid input types error", {
    expect_error(norm_to_range_01(list()))
    expect_error(norm_to_range_01(c("A","B")))
    expect_error(norm_to_range_01(c("2.12","1","0.3")))
})
