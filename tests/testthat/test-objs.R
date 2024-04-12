test_that("invalid inputs error", {
  expect_error(objs(NA))
  expect_error(objs(NULL))
})
test_that("valid inputs do not error", {
  expect_no_error(objs(rnorm(99)))
  expect_no_error(objs(list(a = rnorm(99), b = rnorm(12))))
})
