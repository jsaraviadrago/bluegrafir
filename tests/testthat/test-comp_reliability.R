test_that("Calculating composite reliability", {
  set.seed(123456)
  data_pcfa <- data.frame(replicate(10,sample(1:5,1000,rep=TRUE)))

  m1 <- 'f=~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10'
  m1.cfa <- lavaan::cfa(m1, data= data_pcfa)
  # Creating sum of beta loadings per factor
  x1 <- data.frame(lavaan::inspect(m1.cfa,what="est")$lambda)
  sum_loadings <- data.frame(apply(x1, 2, sum)^2)
  colnames(sum_loadings) <- "Sum_beta_loadings"
  sum_loadings$lhs <- rownames(sum_loadings)
  # Extracting latent factor variance
  v <- lavaan::parameterEstimates(m1.cfa)
  v <- v[,1:4]
  v$variance <- dplyr::if_else(v$lhs == v$rhs,1,0)

  v <- v %>%
    filter(op == "~~",
           variance == "1") %>%
    select(lhs, est)
  # Extracting distribution of items per factor
  z<- lavaan::parameterEstimates(m1.cfa)
  z <- z[,1:3]
  z <- z %>%
    filter(op == "=~") %>%
    select(-op)
  z <- left_join(z,v, by = "lhs")
  # Creating sum of error variance per factor
  y <- data.frame(lavaan::inspect(m1.cfa,what="est")$theta)
  y$max <- apply(y,2,max)
  y <- y %>%
    select(max)
  y$rhs <- rownames(y)
  yz <- left_join(z, y ,by = "rhs")
  yz_sum <- yz %>%
    dplyr::group_by(lhs) %>%
    dplyr::summarise(sum_error = sum(max),
                     Item_number = dplyr::n(),
                     variance_latent = mean(est))
  CR <- left_join(yz_sum,sum_loadings, by = "lhs")
  # Amount of latent variables
  var <- lavaan::inspect(m1.cfa)$psi
  var <- ncol(var)
  # Specify if there are error correlations
  er <- lavaan::parameterEstimates(m1.cfa)
  er <- er[,1:3]
  er$covariance <- if_else(er$lhs != er$rhs,1,0)
  er <- er %>%
    filter(op == "~~",
           covariance == "1")
  eval <- dim(er)[1] == var
  if (eval == TRUE) {
    CR$composite_reliability <-(CR$Sum_beta_loadings*CR$variance_latent) / (CR$Sum_beta_loadings*CR$variance_latent + CR$sum_error)
  } else {
    CR$composite_reliability_ec <-(CR$Sum_beta_loadings*CR$variance_latent) / (CR$Sum_beta_loadings*CR$variance_latent + CR$sum_error + 2*CR$sum_error)
  }
  CR <- CR[,c(1,6)]
  CR
  expect_equal(comp_reliability(m1.cfa), CR)
})
