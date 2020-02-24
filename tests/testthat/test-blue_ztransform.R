test_that("Make a simple linear transformation", {

  data_example <- data.frame( ID = c(1,2,3,4,5,6,7,78,7,7,7,7,7,7,7,7,8,8,8,8,8),
                              puntaje = c(1,2,3,4,5,6,7,78,7,7,7,7,7,7,7,7,8,8,8,8,8))
  blue_ztransform <- function(x, data, meanvar = 500,sdvar = 100) {
    standard <- scale(x, center = T, scale = T)
    conversion <- data.frame((standard*sdvar)+meanvar)
    data <- data.frame(data, Transf_500 = conversion[,1])
    data}
expect_equal(blue_ztransform())


})


