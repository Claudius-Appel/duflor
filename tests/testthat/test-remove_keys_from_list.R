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
test_that("non-list inputs are returned as-is", {
    matrix <- matrix(1:9, nrow = 3, ncol = 3)
    vector <- vector(mode = "logical", length = 0)
    L3 <- LETTERS[1:3]
    df <- data.frame(1, 1:10, sample(L3, 10, replace = TRUE))
    expect_equal(suppressWarnings(remove_key_from_list(matrix,"A")),matrix)
    expect_equal(suppressWarnings(remove_key_from_list(vector,"A")),vector)
})
test_that("list inputs are returned with supplied keys missing",{
    L3 <- LETTERS[1:3]
    df <- data.frame(1, 1:10, sample(L3, 10, replace = TRUE))
    df2 <- data.frame(1, 1:10, sample(L3, 10, replace = TRUE))
    df2 <- df2[1:2]
    expect_equal(remove_key_from_list(df,"sample.L3..10..replace...TRUE."),df2)
})

