test_that("empty list returns empty list()", {
    expect_equal(suppressWarnings(remove_key_from_list(list(),c("key"))),list())
})
test_that("empty keys returns input list", {
    expect_equal(remove_key_from_list(list(a = 1),c()),expected = list(a = 1))
})
test_that("non-member key issues warning", {
    l <- list(a=1,b = "C")
    expect_warning(remove_key_from_list(l,c("c")))
})
test_that("keys are case-sensitive", {
    l <- list(a=1,b = "C")
    # we mute the following warning
    # 'In remove_key_from_list(l, c("A")) : Key 'A' not found in the list.'
    # because it is checked for in the test above. Here, we only care for the
    # return-value being equal to the input value
    expect_equal(suppressWarnings(remove_key_from_list(l,c("A"))),expected = l)
})
