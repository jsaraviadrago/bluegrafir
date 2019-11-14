#' Linear transformations to compare measurements between years
#'
#' This function is used to standardize your results and transform them with a set mean and standard deviation.
#'
#' @name blue
#' @param x vector that you want to change scale.
#' @param meanvar parameter for the mean, the default is 500. This is used for the linear transformation and setting a mean different from 0
#' @param sdvar parameter for the standard deviation, the default is 100. This is used for the linear transformation and setting a standard deviation different from 1
#' @return The output is a vector of a transformed value with the mean and standard deviation set with the parameters
#' @author Juan Carlos Saravia
#' @examples \donttest{blue(x,y,z)}
#' @export
blue <- function(x,meanvar = 500,sdvar = 100) {
  standard <- scale(x, center = T, scale = T)
  conversion <- (standard*sdvar)+meanvar
}

#' This function is used to compare results between groups or years with parameters that you establish.
#'
#' @name blue2
#' @param lmes vector with the most recent measurement or the group you want to compare against the original parameters
#' @param parfmean mean with the baseline parameter to compare against.
#' @param parfsd sd with the baseline parameter to compare against.
#' @param meanvar parameter for the mean, the default is 500. This is used for the linear transformation and setting a mean different from 0
#' @param sdvar parameter for the standard deviation, the default is 100. This is used for the linear transformation and setting a standard deviation different from 1.
#' @return The output is a vector of a transformed value that compares a recent measurement against a baseline measurement.
#' @author Juan Carlos Saravia
#' @examples \donttest{blue2(x,y,z,a)}
#' @export

blue2 <- function(lmes, parfmean, parfsd,
                  meanvar = 500, sdvar = 100) {
  equate <- (lmes-parfmean)/parfsd
  conversion <- (equate*sdvar)+meanvar
}
