test_that("Table of Mcdonald's Omega", {

  set.seed(123456)
  data_pcfa <- data.frame(replicate(10,sample(1:5,1000,rep=TRUE)))

  data.rel <- psych::omegaSem(data_pcfa)

  # Extracting Omega reliability coeficients
  omegacoef <- data.frame(data.rel$omegaSem$omega.group)
  factor.names <- rownames(omegacoef)
  factor.names <- factor.names[!factor.names %in%
                                 "g"]

  omegacoef <- omegacoef %>%
    select(total)
  omegacoef$names <- rownames(omegacoef)
  omegacoef <- data.frame(omegacoef[2:nrow(omegacoef[1]),])

  omegacoef$names <- gsub("[*]", "", omegacoef$names)
  # Check which reliability stands for which factor.
  factors <- data.rel$omegaSem$schmid$sl
  factors <- factors[,c(factor.names)]
  factors <- data.frame(factors)
  factors <- abs(factors)
  factors$varmax <- colnames(factors)[max.col(factors,
                                              ties.method="first")]
  factors$names <- rownames(factors)
  factors <- factors %>%
    select(names, varmax)
  factors$varmax <- gsub("[.]", "", factors$varmax)
  factors <- dplyr::left_join(factors, omegacoef,
                              by = c("varmax" = "names"))
  factors <- factors %>%
    select(Factor = varmax,
           Omega = total)  %>%
    dplyr::group_by(Factor) %>%
    dplyr::summarise(Omega = mean(Omega))
  factors

  expect_equal(grafi_reliability(data.rel), factors)
})
