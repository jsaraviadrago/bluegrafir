#'Table of overall fit measures for a CFA and SEM models
#'
#'This function needs a lavaan object with a model fit to work. It creates a table of all fit indices for a SEM model.
#' @name grafi_fit
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
#' @importFrom rlang .data
#' @author Juan Carlos Saravia
#' @examples
#'set.seed(123456)
#'data <- data.frame(replicate(10,sample(1:5,1000,rep=TRUE)))
#'
#'m2 <- 'f=~ X1 + X2 + X3+X4+X5+X6+X7+X8+X9+X10'
#'fit2 <- lavaan::cfa(m2, data = data)
#'lavaan::summary(fit2, fit.measures = TRUE,
#'        standardized = TRUE)
#'grafi_fit(fit2)
#' @export
#'

grafi_fit <- function(x, mi.nrows = 5) {
  tabla <- data.frame(lavaan::fitMeasures(x))
  colnames(tabla)[1] <- "Fit Measures"
  names <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
             "chisq.scaled", "df.scaled",
             "pvalue.scaled", "cfi.scaled",
             "tli.scaled", "rmsea.scaled")
  tabla$indices <- rownames(tabla)
  tabla <- tabla %>%
    filter(.data$indices %in% names)
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
      filter(.data$indices %in% names2)
  }
  tabla <- tabla[,c(2,1)]
  analysis <- c("~", "~~", ":=")
  tabla1 <- parameterEstimates(x, standardized=TRUE) %>%
    filter(.data$op %in% analysis) %>%
    select('Dep variable'=.data$lhs,
           Relationship=.data$op,
           'Ind variable'=.data$rhs,
           B=.data$est,
           SE=.data$se,
           Z=.data$z,
           'p-value'=.data$pvalue,
           Beta=.data$std.all)
  tabla2 <- parameterEstimates(x, standardized=TRUE) %>%
    filter(.data$op == "=~") %>%
    select('Latent factor'=.data$lhs,
           'Variable'=.data$rhs,
           B=.data$est,
           SE=.data$se,
           Z=.data$z,
           'p-value'=.data$pvalue,
           Beta=.data$std.all)
  mi <- lavaan::inspect(x,"mi")
  mi.order <- mi[order(-mi$mi),]
  tabla3 <- mi.order[1:mi.nrows,] %>%
    select(Variable_1 = .data$lhs,
           relationship = .data$op,
           Variable_2 = .data$rhs,
           MI = .data$mi)
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
#' @name grafi_reliability
#' @param x McDonalds Omega calculated with the omegaSem function. The name of the McDonald's Omega specified with the psych and GPArotation packages.
#' @return The output is a data.frame that gives you fit McDonalds Omega coefficients in a table.
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @author Juan Carlos Saravia
#' @examples
#' set.seed(123456)
#'data <- data.frame(replicate(10,sample(1:5,1000,rep=TRUE)))
#'data.rel <- psych::omegaSem(data)
#'
#'grafi_reliability(data.rel)
#' @export

grafi_reliability <- function(x) {
  # Extracting Omega reliability coeficients
  omegacoef <- data.frame(x$omegaSem$omega.group)
  factor.names <- rownames(omegacoef)
  factor.names <- factor.names[!factor.names %in%
                                 "g"]
  omegacoef <- omegacoef %>%
    select(.data$total)
  omegacoef$names <- rownames(omegacoef)
  omegacoef <- data.frame(omegacoef[2:nrow(omegacoef[1]),])

  omegacoef$names <- gsub("[*]", "", omegacoef$names)
  # Check which reliability stands for which factor.
  factors <- x$omegaSem$schmid$sl
  factors <- factors[,c(factor.names)]
  factors <- data.frame(factors)
  factors <- abs(factors)
  factors$varmax <- colnames(factors)[max.col(factors,
                                              ties.method="first")]
  factors$names <- rownames(factors)
  factors <- factors %>%
    select(names, .data$varmax)
  factors$varmax <- gsub("[.]", "", factors$varmax)
  factors <- dplyr::left_join(factors, omegacoef,
                       by = c("varmax" = "names"))
  factors <- factors %>%
    select(Factor = .data$varmax,
           Omega = .data$total)  %>%
    dplyr::group_by(.data$Factor) %>%
    dplyr::summarise(Omega = mean(.data$Omega))
  factors
}


#' Table of distributions of categories in a vector
#'
#' This function just needs a vector and it is useful if you want to make a tidy table of distributions of items. It works with the map function from the purrr package which enables you to calculate multiple ordered tables of items.
#'
#' @name grafi_distribution
#' @param x vector of values that show a table of frequencies, proportions and cumulative frequencies.
#' @return a tibble of 4 columns with categories, frequencies, proportions and cumulative frequencies.
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data
#' @author Juan Carlos Saravia
#' @examples
#' data_prueba <- c(1,2,3,4,5,6,7,78,7,7,7,7,7,7,7,7,8,8,8,8,8)
#' grafi_distribution(data_prueba)
#' @export
#'
#'
grafi_distribution <- function(x) {
  freq <- data.frame(table(x))
  prop <- data.frame(prop.table(table(x)))
  tabla <- dplyr::left_join(freq,prop, by = "x")
  names(tabla)[1] <- "Vector1"
  tabla <- tabla %>%
    dplyr::select(Categories = .data$Vector1,
                  Frequencies = .data$Freq.x,
                  Proportions = .data$Freq.y)
  tabla <- tabla %>% dplyr::as_tibble() %>%
    dplyr::mutate(
      CumFreq = cumsum(.data$Frequencies))
  tabla
}

#' Wright Map for items difficulty and person ability
#'
#' This function needs two vectors (person's abilities and items difficulties). With that information it will make a nice wright map
#'
#' @name grafi_wrightmap
#' @param persons.measure Person's abilities
#' @param items.measure Item's difficulties
#' @param items.names Item's names
#' @param groups amounts of groups that the items are seperated
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom ggrepel geom_text_repel
#' @importFrom gridExtra grid.arrange
#' @return a ggplot histogram of amount of people and a grapha of item difficulty
#' @author Juan Carlos Saravia
#' @examples
#'
#' personas.data <- data.frame(personas=sort(round(rnorm(500, 0,1),3)))
#'
#' items.data <- data.frame(
#'   items_n = as.numeric(seq(1:36)),
#'   items_g = ceiling(runif(36, 0, 5)),
#'   items = sort(round(runif(36, -3,3),3)),
#'   stringsAsFactors=F)
#'items.data <- cbind(items.data,
#'                     items_c = dplyr::if_else(items.data$items_n < 10,
#'                      paste("IT_EX",items.data$items_n, sep="_0"),
#'                                      paste("IT_EX",items.data$items_n, sep="_")))
#'
#'
#'grafi_wrightmap(personas.data$personas,items.data$items,items.data$items_c)
#'
#'
#' @export

grafi_wrightmap <- function(persons.measure, items.measure, items.names, groups = 5){
  persons.measure <- data.frame(persons.measure)
  names(persons.measure)[1] <- "personas"
  items.measurement <- data.frame(items.measure)
  names(items.measurement)[1] <- "items.measure2"
  items.data <- data.frame(
    items_n = as.numeric(1:nrow(items.measurement)),
    items_g = ceiling(stats::runif(nrow(items.measurement),0,groups)),
    items.measure2 = items.measurement$items.measure2,
    items.names,
    stringsAsFactors=F)
  groups <- 1:groups
  groups <- paste0("D",groups)


  pe <-
    ggplot2::ggplot(persons.measure, ggplot2::aes(x=.data$personas)) +
    ggplot2::coord_flip() +
    ggplot2::geom_histogram(ggplot2::aes(y = (.data$..count..)),
                   binwidth=0.5, fill="#33b8ff",
                   color = "#339cff") +
    ggplot2::labs(x = "", y = "Personas") +
    ggplot2::theme(axis.title = ggplot2::element_text(color = "black"),
          axis.title.x = ggplot2::element_text(vjust = 0, angle = 0,
                                      face = "italic", size = 14)) +
    ggplot2::theme(panel.background = ggplot2::element_blank())+
    ggplot2::scale_x_continuous(limits=c(-4, 4), breaks=seq(-4,4,by=0.5))

  it <-
    ggplot2::ggplot(items.data, ggplot2::aes(x=.data$items.measure2, y=.data$items_g)) +
    ggplot2::coord_flip() +
    ggplot2::geom_jitter(position = ggplot2::position_jitter(height = .1), size=2,
                         ggplot2::aes(color = "#339cff")) +
    geom_text_repel(ggplot2::aes(.data$items.measure2, .data$items_g, label = .data$items.names), size = 2) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(panel.background = ggplot2::element_blank())+
    ggplot2::theme(axis.title = ggplot2::element_text(color = "black"),
          axis.title.x = ggplot2::element_text(vjust = 0, angle = 0,
                                      face = "italic", size = 14)) +
    ggplot2::labs(x = "", y = "Items") +
    ggplot2::scale_x_continuous(limits=c(-4, 4), breaks=seq(-4,4,by=0.5)) +
    ggplot2::scale_y_continuous(labels = groups)
  grafica <- grid.arrange(it,pe, ncol=2, widths=c(1,1), top = " ")

}


