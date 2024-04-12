test_that("reassign_integers_by_frequency can be sorted properly", {
    clus <- c(1,1,1,2,2,3,3,3,3)
    expect_equal(reassign_integers_by_frequency(clus),c(2,2,2,3,3,1,1,1,1))
})
test_that("reassign_integers_by_frequency fails on NULL input", {
    expect_error(reassign_integers_by_frequency(NULL))
    expect_error(reassign_integers_by_frequency(c()))
})
test_that("fails on input-type 'list()'",{
    expect_error(reassign_integers_by_frequency(list(a = 2, b = 3)))
})
