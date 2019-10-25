#' Table of overall fit measures for a CFA and SEM models
#'
#' This function needs a lavaan object with a model fit to work. It creates a table of all fit indices for a CFA and SEM model.
#'
#' @name grafi
#' @param x lavaan object: The name of the model fit that was calculated from the specified CFA model in the lavaan package.
#' @param y specify the amount of rows to show in the modification indexes table. The default is 5 rows.
#' @return The output is a list of tibbles that gives you overall fit indices, parameter estimates and modification indices for CFA models.
#' Example of fit indices: Chi square, degrees of freedom, p-values of Chi square, CFI, TLI, RMSEA, SRMR.
#' Example of parameter estimates: undstandardized betas, standardized betas, standard error, z values and p values
#' I want to thank Rose Hartmann for the code in her web page thank helped me finish one part of the function.
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom  dplyr as.tbl
#' @author Juan Carlos Saravia
#' @examples \donttest{grafi(fit)}
#' @export
#'
globalVariables (c("indices", "rhs", "se",
                "z", "pvalue", "std.all"))
grafi <- function(x, mi.nrows = 5) {
  tabla <- data.frame(lavaan::fitMeasures(x))
  colnames(tabla)[1] <- "Fit Measures"
  names <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")
  tabla$indices <- rownames(tabla)
  tabla <- tabla %>%
    dplyr::filter(indices %in% names)
  tabla$indices <-  dplyr::recode(tabla$indices,
                                  chisq = "Chi2",
                                  df = "df",
                                  pvalue = "sig",
                                  cfi = "CFI",
                                  tli = "TLI",
                                  rmsea = "RMSEA",
                                  srmr = "SRMR")
  tabla <- tabla[,c(2,1)]
  tabla1 <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
    filter(op == "=~") %>%
    select('Latent Factor'=lhs,
           Indicator=rhs, B=est,
           SE=se, Z=z,
           'p-value'=pvalue,
           Beta=std.all)
  mi <- lavaan::inspect(x,"mi")
  mi.order <- mi[order(-mi$mi),]
  tabla2 <- mi.order[1:mi.nrows,] %>%
    select(Variable_1 = lhs,
           relationship = op,
           Variable_2 = rhs,
           MI = mi)
  tabla <- as.tbl(tabla)
  tabla1 <- as.tbl(tabla1)
  tabla2 <- as.tbl(tabla2)
  tabla_general <- list(tabla,tabla1,tabla2)
  tabla_general
}

#' Table of reliability coeficientes for Mcdonald's Omega
#'
#' This function needs a psych and GPArotation object from the omegaSem function to work. It creates a table of all fit indices for a CFA and SEM model.
#'
#' @name blue
#' @param x McDonalds Omega calculated with the omegaSem function. The name of the McDonald's Omega specified with the psych and GPArotation packages.
#' @return The output is a data.frame that gives you fit McDonalds Omega coefficients in a table.
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @author Juan Carlos Saravia
#' @examples \donttest{blue(fit)}
#' @export
globalVariables(c("total", "omegarel", "varmax",
                  "Factor", "Omega"))
blue <- function(x) {
  # Extracting Omega reliability coeficients
  omegacoef <- data.frame(x$omegaSem$omega.group)
  factor.names <- rownames(omegacoef)
  factor.names <- factor.names[!factor.names %in%
                                 "g"]
  omegacoef <- omegacoef %>%
    select(total)
  omegacoef$names <- rownames(omegacoef)
  omegacoef <- data.frame(omegacoef[2:nrow(omegacoef[1]),])

  omegacoef$names <- gsub("[*]", "", omegacoef$names)
  # Check which reliability stands for which factor.
  factors <- omegarel$omegaSem$schmid$sl
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
}





