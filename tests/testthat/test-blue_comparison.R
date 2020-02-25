test_that("Comparison of two scales", {
  data_example <- data.frame(ID = c(1,2,3,4,5),
                             puntaje = c(1,2,3,4,5))

  data_result <- data.frame(ID = c(1,2,3,4,5),
                            puntaje = c(1,2,3,4,5))
  parfmean <- 1
  parfsd <- 0.5
  meanvar <- 500
  sdvar <- 100

  data_result$Transf_500 <- (data_example$puntaje-parfmean)/parfsd
  data_result$Transf_500 <- (data_result$Transf_500*sdvar)+meanvar
  expect_equal(blue_comparison(data_example$puntaje,
                               data_example,
                               parfmean = 1,
                               parfsd = 0.5),
               data_result)
  expect_error(blue_comparison(data_example$puntaje,
                               data_example))
})



