test_that("Make a simple linear transformation", {

  data_example <- data.frame(ID = c(1,2,3,4,5),
                              puntaje = c(1,2,3,4,5))

  data_result <- data.frame(ID = c(1,2,3,4,5),
                            puntaje = c(1,2,3,4,5))
  meanvar <- 500
  sdvar <- 100
  z <- (scale(data_example$puntaje,
                                  center = T, scale = T)*sdvar)+meanvar
  data_result$Transf_500 <- as.numeric(z)
  expect_equal(blue_ztransform(data_example$puntaje, data_example),
                     data_result)
  expect_error(blue_ztransform(data_example$puntaje))
})



