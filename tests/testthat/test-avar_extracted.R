test_that("Calculating average variance extracted", {
  set.seed(123456)
  data_pcfa <- data.frame(replicate(10,sample(1:5,1000,rep=TRUE)))

  m1 <- 'f=~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10'
  m1.cfa <- lavaan::cfa(m1, data= data_pcfa)

  # Extracting distribution of items per factor
  z<- lavaan::parameterEstimates(m1.cfa)
  z <- z[,1:3]

  z <- z %>%
    filter(op == "=~") %>%
    select(-op)
  # Creating sum of error variance per factor
  y <- data.frame(lavaan::inspect(m1.cfa,what="std")$theta)
  y$max <- apply(y,2,max)
  y <- y %>%
    select(max)
  y$rhs <- rownames(y)
  yz <- dplyr::left_join(z, y ,by = "rhs")
  yz_sum <- yz %>%
    dplyr::group_by(lhs) %>%
    dplyr::summarise(sum_error = sum(max),
                     Item_number = dplyr::n())
  yz_sum$AVE <- yz_sum$sum_error/yz_sum$Item_number
  yz_sum <- yz_sum %>%
    select(lhs, AVE)
  yz_sum

  expect_equal(avar_extracted(m1.cfa), yz_sum)
})
