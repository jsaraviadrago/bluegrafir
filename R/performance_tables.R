#' Table of overall fit measures for a CFA and SEM models
#'
#'This function needs a lavaan object with a model fit to work. It creates a table of all fit indices for a CFA and SEM model.
#'
#' @name grafi
#' @param x lavaan object: The name of the model fit that was calculated from the specified CFA model in the lavaan package.
#' @return The output is a data.frame that gives you overall fit indices for CFA and SEM models.
#' For example: Chi square, degrees of freedom, p-values of Chi square, CFI, TLI, RMSEA, SRMR.
#' @importFrom dplyr "%>%"
#' @author Juan Carlos Saravia
#' @examples \donttest{grafi(fit)}
#'
#'
globalVariables("indices")
grafi <- function(x) {
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
  tabla
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
#'
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




