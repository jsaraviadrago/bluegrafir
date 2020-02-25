test_that("Make a simple linear transformation", {

  data_example <- data.frame(ID = c(1,2,3,4,5),
                              puntaje = c(1,2,3,4,5))
  data_result <- data.frame(ID = c(1,2,3,4,5),
                            puntaje = c(1,2,3,4,5),
                            Transf_500 = c(373.5089, 436.7544,
                                           500.0000, 563.2456,
                                           626.4911))
   expect_equivalent(blue_ztransform(data_example$puntaje, data_example),
                     data.frame(data_result))
})



