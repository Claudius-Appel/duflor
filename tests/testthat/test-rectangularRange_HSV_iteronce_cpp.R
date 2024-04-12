load_extdata <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "duflor"),full.names = T)
    } else {
        system.file("extdata", path, package = "duflor", mustWork = TRUE)
    }
}
test_that("return-value has proper dimensions [n_hits,2]",{
    #### setup ####
    file_path <- load_extdata("duflor-icon.png")
    pixel.array <- load_image(file_path,F,T)
    spectrums <- getOption("duflor.default_hsv_spectrums")

    ## convert spectrums to matrix
    nlb <- do.call(rbind,spectrums$lower_bound)
    nub <- do.call(rbind,spectrums$upper_bound)

    ## strip dimnames-attributes
    dimnames(nlb) <- c()
    dimnames(nub) <- c()
    ## extract matches
    result <- rectangularRange_HSV_iteronce_cpp(H = pixel.array[,,,1],
                                                S = pixel.array[,,,2],
                                                V = pixel.array[,,,3],
                                                upper_bound = nub,
                                                lower_bound = nlb,
                                                image_width = dim(pixel.array)[1],
                                                check_V = T)
    result2 <- rectangularRange_HSV_iteronce_cpp(H = pixel.array[,,,1],
                                                 S = pixel.array[,,,2],
                                                 V = pixel.array[,,,3],
                                                 upper_bound = nub,
                                                 lower_bound = nlb,
                                                 image_width = dim(pixel.array)[1],
                                                 check_V = F)
    ## add names to results-matrix.
    names(result) <- names(spectrums$lower_bound)
    names(result2) <- names(spectrums$lower_bound)
    for (name in names(result)) {
        # print(dim(result[[name]]$pixel.idx)[[1]])
        # print(dim(result[[name]]$pixel.idx)[[2]])
        expect_equal(dim(result[[name]]$pixel.idx)[[2]],2)
    }
    for (name in names(result2)) {
        # print(dim(result[[name]]$pixel.idx)[[1]])
        # print(dim(result[[name]]$pixel.idx)[[2]])
        expect_equal(dim(result2[[name]]$pixel.idx)[[2]],2)
    }
})
test_that("invalid boundary dimensions return errors", {
    #### setup ####
    test_path <- load_extdata("duflor-icon.png")
    ## load example data
    pixel.array <- load_image(test_path,F,T)
    #### check_V = FALSE ####
    expect_error(rectangularRange_HSV_iteronce_cpp( ##
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(),
        lower_bound = c(),
        image_width = dim(pixel.array)[1],
        check_V =F))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1),c(1)),
        lower_bound = rbind(c(1),c(1)),
        image_width = dim(pixel.array)[1],
        check_V =F))
    # expect_error(rectangularRange_HSV_iteronce_cpp(
    #     H = pixel.array[, , , 1],
    #     S = pixel.array[, , , 2],
    #     V = pixel.array[, , , 3],
    #     upper_bound = rbind(c(1,2),c(1,2)),
    #     lower_bound = rbind(c(1,2),c(1,2)),
    #     image_width = dim(pixel.array)[1],
    #     check_V =F))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2,3),c(1,2,3)),
        lower_bound = rbind(c(1,2),c(1,2)),
        image_width = dim(pixel.array)[1],
        check_V =F))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2),c(1,2)),
        lower_bound = rbind(c(1,2,3),c(1,2,3)),
        image_width = dim(pixel.array)[1],
        check_V =F))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2,3),c(1,2,3)),
        lower_bound = rbind(c(1,2,3,4),c(1,2,3,4)),
        image_width = dim(pixel.array)[1],
        check_V =F))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2,3,4),c(1,2,3,4)),
        lower_bound = rbind(c(1,2,3),c(1,2,3)),
        image_width = dim(pixel.array)[1],
        check_V =F))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2,3,4),c(1,2,3,4)),
        lower_bound = rbind(c(1,2,3),c(1,2,3)),
        image_width = dim(pixel.array)[1],
        check_V =F))
    #### check_V = TRUE ####
    expect_error(rectangularRange_HSV_iteronce_cpp( ##
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = c(),
        lower_bound = c(),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1),c(1)),
        lower_bound = rbind(c(1),c(1)),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2),c(1,2)),
        lower_bound = rbind(c(1,2),c(1,2)),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2,3),c(1,2,3)),
        lower_bound = rbind(c(1,2),c(1,2)),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2),c(1,2)),
        lower_bound = rbind(c(1,2,3),c(1,2,3)),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2,3),c(1,2,3)),
        lower_bound = rbind(c(1,2,3,4),c(1,2,3,4)),
        image_width = dim(pixel.array)[1],
        check_V =T))
    expect_error(rectangularRange_HSV_iteronce_cpp(
        H = pixel.array[, , , 1],
        S = pixel.array[, , , 2],
        V = pixel.array[, , , 3],
        upper_bound = rbind(c(1,2,3,4),c(1,2,3,4)),
        lower_bound = rbind(c(1,2,3),c(1,2,3)),
        image_width = dim(pixel.array)[1],
        check_V =T))
    # expected <- matrix(integer(),nrow = 0,ncol = 2)
    # dimnames(expected)[[2]] <- list("x","y")
    # expect_equal(object, expected)
})
# to test
# check_V = F
#
