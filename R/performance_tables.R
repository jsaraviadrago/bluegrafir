#'Table of overall fit measures for a CFA and SEM models
#'
#'This function needs a lavaan object with a model fit to work. It creates a table of all fit indices for a SEM model.
#' @name grafit
#' @param x lavaan object: The name of the model fit that was calculated from the specified CFA or SEM model in the lavaan package.
#' @param mi.nrows specify the amount of rows to show in the modification indexes table. The default is 5 rows.
#' @return The output is a list of tibbles that gives you overall fit indices, parameter estimates and modification indices for CFA or SEM models.
#' Example of fit indices: Chi square, degrees of freedom, p-values of Chi square, CFI, TLI, RMSEA, SRMR.
#' Example of parameter estimates: undstandardized betas, standardized betas, standard error, z values and p values.
#' I want to thank Rose Hartmann because with her web page I was able to make one chunk of this function.
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom dplyr as.tbl
#' @importFrom dplyr filter
#' @importFrom dplyr recode
#' @importFrom lavaan parameterEstimates
#' @author Juan Carlos Saravia
#' @examples \donttest{grafi2(fit)}
#' @export
#'
#'
#'
globalVariables (c("indices", "rhs", "se",
                   "z", "pvalue", "std.all"))
grafit <- function(x, mi.nrows = 5) {
  tabla <- data.frame(lavaan::fitMeasures(x))
  colnames(tabla)[1] <- "Fit Measures"
  names <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
             "chisq.scaled", "df.scaled",
             "pvalue.scaled", "cfi.scaled",
             "tli.scaled", "rmsea.scaled")
  tabla$indices <- rownames(tabla)
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
  tabla1 <- parameterEstimates(x, standardized=TRUE) %>%
    filter(op %in% analysis) %>%
    select('Dep variable'=lhs,
           Relationship=op,
           'Ind variable'=rhs,
           B=est,
           SE=se,
           Z=z,
           'p-value'=pvalue,
           Beta=std.all)
  tabla2 <- parameterEstimates(x, standardized=TRUE) %>%
    filter(op == "=~") %>%
    select('Latent factor'=lhs,
           'Variable'=rhs,
           B=est,
           SE=se,
           Z=z,
           'p-value'=pvalue,
           Beta=std.all)
  mi <- lavaan::inspect(x,"mi")
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
}


#' Table of reliability coeficientes for Mcdonald's Omega
#'
#' This function needs a psych and GPArotation object from the omegaSem function to work. It creates a table of all fit indices for a CFA and SEM model.
#'
#' @name grafirel
#' @param x McDonalds Omega calculated with the omegaSem function. The name of the McDonald's Omega specified with the psych and GPArotation packages.
#' @return The output is a data.frame that gives you fit McDonalds Omega coefficients in a table.
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @author Juan Carlos Saravia
#' @examples \donttest{grafi2(fit)}
#' @export
globalVariables(c("total", "omegarel", "varmax",
                  "Factor", "Omega"))
grafirel <- function(x) {
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


#' Table of distributions of categories in a vector
#'
#' This function just needs a vector and it is useful if you want to make a tidy table of distributions of items. It works with the map function from the purrr package which enables you to calculate multiple ordered tables of items.
#'
#' @name graficat
#' @param y vector of values that show a table of frequencies, proportions and cumulative frequencies.
#' @return a tibble of 4 columns with categories, frequencies, proportions and cumulative frequencies.
#' @importFrom dplyr "%>%"
#' @author Juan Carlos Saravia
#' @examples \donttest{grafi3(x)}
#' @export
#'
#'
globalVariables(c("Freq.x", "Freq.y", "x",
                  "Frequencies"))
graficat <- function(x) {
  freq <- data.frame(table(x))
  prop <- data.frame(prop.table(table(x)))
  tabla <- dplyr::left_join(freq,prop, by = "x")
  tabla <- tabla %>%
    dplyr::select(Categories = x,
           Frequencies = Freq.x,
           Proportions = Freq.y)
  tabla <- transform(tabla, CumFreq = cumsum(Frequencies))
  tabla <- dplyr::as.tbl(tabla)
  tabla
}
