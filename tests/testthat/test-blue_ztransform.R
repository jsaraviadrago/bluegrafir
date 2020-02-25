context("running linear transformation")

test_that("Make a simple linear transformation", {

  data_example <- data.frame( ID = c(1,2,3,4,5),
                              puntaje = c(1,2,3,4,5))
  data_result <- data.frame(ID = c(1,2,3,4,5),
                            puntaje = c(1,2,3,4,5),
                            Transf_500 = c(445.4499, 451.7789,
                                           458.1079, 464.4370, 470.7660))
   expect_equivalent(blue_ztransform(data_example$puntaje, data_example),data_result)
})



