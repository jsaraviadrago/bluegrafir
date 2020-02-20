#' Function to calculate Composite reliability
#'
#' This function needs a lavaan object with a model fit to work. It calculates Composite reliability for CFA models.
#' There are two formulas inside the function and they run depending on the fit characteristics.
#' The composite reliability or factor rho coefficient is the ratio of explained variance over total variance.
#' With no error correlations the CR is just calculated as previously defined.
#' A different formula is needed for when indicators share at least one error covariance.
#' In this case the total variance is calculated by adding the sum of the unstandarized error variance multiplied by 2.
#'
#' @name comp_reliability
#' @seealso Kline, R. (2016). Principles and Practice of Structural Equation Modeling. Fourth Edition. Guilford press. NY.
#' @seealso Raykov, T. (2004). Behavioral scale realiability and measurement invariance evaluation using latent variable modeling. Behavior therapy, 35, 299-331.
#' @param x lavaan object: The name of the model fit that was calculated from the specified CFA model with the lavaan package.
#' @return The output is a data.frame specifying the latent factor in the first column and the CR in the second column.
#' @importFrom dplyr "%>%"
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr if_else
#' @importFrom lavaan parameterEstimates
#' @importFrom lavaan inspect
#' @importFrom rlang .data
#' @author Juan Carlos Saravia
#' @examples
#' #'set.seed(123456)
#'data <- data.frame(replicate(10,sample(1:5,1000,rep=TRUE)))
#'
#'m2 <- 'f=~ X1 + X2 + X3+X4+X5+X6+X7+X8+X9+X10'
#'fit2 <- lavaan::cfa(m2, data = data)
#'lavaan::summary(fit2, fit.measures = TRUE,
#'        standardized = TRUE)
#'comp_reliability(fit2)
#'
#' @export


comp_reliability <- function(x) {
  # Creating sum of beta loadings per factor
  x1 <- data.frame(inspect(x,what="est")$lambda)
  sum_loadings <- data.frame(apply(x1, 2, sum)^2)
  colnames(sum_loadings) <- "Sum_beta_loadings"
  sum_loadings$lhs <- rownames(sum_loadings)
  # Extracting latent factor variance
  v <- parameterEstimates(x)
  v <- v[,1:4]
  v$variance <- if_else(v$lhs == v$rhs,1,0)
  v <- v %>%
    filter(.data$op == "~~",
           .data$variance == "1") %>%
    select(.data$lhs, .data$est)
  # Extracting distribution of items per factor
  z<- parameterEstimates(x)
  z <- z[,1:3]
  z <- z %>%
    filter(.data$op == "=~") %>%
    select(-.data$op)
  z <- left_join(z,v, by = "lhs")
  # Creating sum of error variance per factor
  y <- data.frame(inspect(x,what="est")$theta)
  y$max <- apply(y,2,max)
  y <- y %>%
    select(max)
  y$rhs <- rownames(y)
  yz <- left_join(z, y ,by = "rhs")
  yz_sum <- yz %>%
    dplyr::group_by(.data$lhs) %>%
    dplyr::summarise(sum_error = sum(max),
              Item_number = dplyr::n(),
              variance_latent = mean(.data$est))
  CR <- left_join(yz_sum,sum_loadings, by = "lhs")
  # Amount of latent variables
  var <- inspect(x)$psi
  var <- ncol(var)
  # Specify if there are error correlations
  er <- parameterEstimates(x)
  er <- er[,1:3]
  er$covariance <- if_else(er$lhs != er$rhs,1,0)
  er <- er %>%
    filter(.data$op == "~~",
           .data$covariance == "1")
  eval <- dim(er)[1] == var
  if (eval == TRUE) {
    CR$composite_reliability <-(CR$Sum_beta_loadings*CR$variance_latent) / (CR$Sum_beta_loadings*CR$variance_latent + CR$sum_error)
  } else {
    CR$composite_reliability_ec <-(CR$Sum_beta_loadings*CR$variance_latent) / (CR$Sum_beta_loadings*CR$variance_latent + CR$sum_error + 2*CR$sum_error)
  }
  CR <- CR[,c(1,6)]
  CR
}

#' Function to calculate Average variance extracted (AVE)
#'
#' This function needs a lavaan object with a model fit to work. It calculates the Average variance extracted for CFA models.
#' It is the average of the squared standardized pattern coefficients for indicators that depend on the same factor but are specified to measure no other factors.
#'
#' @name avar_extracted
#' @seealso Kline, R. (2016). Principles and Practice of Structural Equation Modeling. Fourth Edition. Guilford press. NY.
#' @seealso Raykov, T. (2004). Behavioral scale realiability and measurement invariance evaluation using latent variable modeling. Behavior therapy, 35, 299-331.
#' @param x lavaan object: The name of the model fit that was calculated from the specified CFA model in the lavaan package.
#' @return The output is a data.frame specifying the latent factor in the first column and the AVE in the second column.
#' @importFrom dplyr "%>%"
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @author Juan Carlos Saravia
#' @examples
#'#' #'set.seed(123456)
#'data <- data.frame(replicate(10,sample(1:5,1000,rep=TRUE)))
#'
#'m2 <- 'f=~ X1 + X2 + X3+X4+X5+X6+X7+X8+X9+X10'
#'fit2 <- lavaan::cfa(m2, data = data)
#'lavaan::summary(fit2, fit.measures = TRUE,
#'        standardized = TRUE)
#'avar_extracted(fit2)
#'
#'
#   \donttest{ave(fit)}
#' @export

avar_extracted <- function(x) {
  # Extracting distribution of items per factor
  z<- lavaan::parameterEstimates(x)
  z <- z[,1:3]
  z <- z %>%
    filter(.data$op == "=~") %>%
    select(-.data$op)
  # Creating sum of error variance per factor
  y <- data.frame(lavaan::inspect(x,what="std")$theta)
  y$max <- apply(y,2,max)
  y <- y %>%
    select(max)
  y$rhs <- rownames(y)
  yz <- dplyr::left_join(z, y ,by = "rhs")
  yz_sum <- yz %>%
    dplyr::group_by(.data$lhs) %>%
    dplyr::summarise(sum_error = sum(max),
              Item_number = dplyr::n())
  yz_sum$AVE <- yz_sum$sum_error/yz_sum$Item_number
  yz_sum <- yz_sum %>%
    select(.data$lhs, .data$AVE)
  yz_sum
}


