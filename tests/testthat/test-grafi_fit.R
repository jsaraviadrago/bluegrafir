test_that("Table of fit statistics", {
  set.seed(123456)
  data_pcfa <- data.frame(replicate(10,sample(1:5,1000,rep=TRUE)))

  m1 <- 'f=~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10'
  m1.cfa <- lavaan::cfa(m1, data= data_pcfa)

  mi.nrows <- 5
  tabla <- data.frame(lavaan::fitMeasures(m1.cfa))
  colnames(tabla)[1] <- "Fit Measures"
  names <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
             "chisq.scaled", "df.scaled",
             "pvalue.scaled", "cfi.scaled",
             "tli.scaled", "rmsea.scaled")
  tabla$indices <- rownames(tabla)
  library(dplyr)
  tabla <- tabla %>%
    filter(indices %in% names)
  estimador <- nrow(tabla)
  names2 <- c("Chi2", "DF", "P-VALUE", "CFI", "TLI", "RMSEA", "SRMR")
  if (estimador == 7) {
    tabla$indices <-  recode(tabla$indices,
                             chisq = "Chi2",
                             df = "DF",
                             pvalue = "P-VALUE",
                             cfi = "CFI",
                             tli = "TLI",
                             rmsea = "RMSEA",
                             srmr = "SRMR")
  } else {
    tabla$indices <-  recode(tabla$indices,
                             chisq.scaled = "Chi2",
                             df.scaled = "DF",
                             pvalue.scaled = "P-VALUE",
                             cfi.scaled = "CFI",
                             tli.scaled = "TLI",
                             rmsea.scaled = "RMSEA",
                             srmr = "SRMR")
    tabla <- tabla %>%
      filter(indices %in% names2)
  }
  tabla <- tabla[,c(2,1)]
  analysis <- c("~", "~~", ":=")
  tabla1 <- lavaan::parameterEstimates(m1.cfa, standardized=TRUE) %>%
    filter(op %in% analysis) %>%
    select('Dep variable'=lhs,
           Relationship=op,
           'Ind variable'=rhs,
           B=est,
           SE=se,
           Z=z,
           'p-value'=pvalue,
           Beta=std.all)
  tabla2 <- lavaan::parameterEstimates(m1.cfa,
                                       standardized=TRUE) %>%
    filter(op == "=~") %>%
    select('Latent factor'=lhs,
           'Variable'=rhs,
           B=est,
           SE=se,
           Z=z,
           'p-value'=pvalue,
           Beta=std.all)
  mi <- lavaan::inspect(m1.cfa,"mi")
  mi.order <- mi[order(-mi$mi),]
  tabla3 <- mi.order[1:mi.nrows,] %>%
    select(Variable_1 = lhs,
           relationship = op,
           Variable_2 = rhs,
           MI = mi)
  tabla <- as.tbl(tabla)
  tabla1 <- as.tbl(tabla1)
  tabla2 <- as.tbl(tabla2)
  tabla3 <- as.tbl(tabla3)
  lista_general <- list(tabla,tabla1, tabla2, tabla3)
  lista_general

  expect_equal(grafi_fit(m1.cfa), lista_general)
})
