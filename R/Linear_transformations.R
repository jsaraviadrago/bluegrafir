#' Linear transformations to compare measurements between years
#'
#' This function is used to standardize your results and transform them with a set mean and standard deviation.
#'
#' @name bluebase
#' @param x vector that you want to change scale.
#' @param meanvar parameter for the mean, the default is 500. This is used for the linear transformation and setting a mean different from 0
#' @param sdvar parameter for the standard deviation, the default is 100. This is used for the linear transformation and setting a standard deviation different from 1
#' @return The output is a vector of a transformed value with the mean and standard deviation set with the parameters
#' @author Juan Carlos Saravia
#' @examples \donttest{blue(x,y,z)}
#' @export
bluebase <- function(x,meanvar = 500,sdvar = 100) {
  standard <- scale(x, center = T, scale = T)
  conversion <- (standard*sdvar)+meanvar
}

#' This function is used to compare results between groups or years with parameters that you establish.
#'
#' @name bluecomp
#' @param lmes vector with the most recent measurement or the group you want to compare against the original parameters
#' @param parfmean mean with the baseline parameter to compare against.
#' @param parfsd sd with the baseline parameter to compare against.
#' @param meanvar parameter for the mean, the default is 500. This is used for the linear transformation and setting a mean different from 0
#' @param sdvar parameter for the standard deviation, the default is 100. This is used for the linear transformation and setting a standard deviation different from 1.
#' @return The output is a vector of a transformed value that compares a recent measurement against a baseline measurement.
#' @author Juan Carlos Saravia
#' @examples \donttest{blue2(x,y,z,a)}
#' @export

bluecomp <- function(lmes, parfmean, parfsd,
                  meanvar = 500, sdvar = 100) {
  equate <- (lmes-parfmean)/parfsd
  conversion <- (equate*sdvar)+meanvar
}

#' This function can be used to do linear percentile scaling. It helps to create thresholds for surveys and continous variables.
#'
#'It calculates the cumulative frequency of a continous variable, transforms this variable into percentiles by using a constant of .50 in order to calculate values over the 50% and under it based on the frequency of values.
#' After that, it standardize scores and then transforms values with a mean of 500 and standard deviation of 100 (default).
#' @name bluebare
#' @param x vector of continous values
#' @param sdev standard deviation which is to be assigned for the transformation, default value is 100.
#' @param means mean which is to be assigned for the linear transformation. Default value is 500.
#' @param threshold logical parameter were TRUE assigns three thresholds. It is based on a normal distribution were values lower than -1 standard deviation from the mean are low, values higher than 1 standard deviation are high and values in between are medium (default).
#' if logical parameter is FALSE, 5 thresholds are assigned where values bigger than 2 standard deviations are very high, higher than 1 standard deviation are high, higher than -1 are medium, higher than -2 are low and lower than -2 standard deviations are very low.
#' @return The output is a tibble with raw scores, zscores and thresholds
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @author Juan Carlos Saravia
#' @examples \donttest{bluebare(x,sdev,means,threshold)}
#' @export
globalVariables (c("x", "Zscore", "cortes",
                   "Z", "pvalue", "std.all",
                   "Puntajes_Brutos", "Threshold"))
bluebare <- function(x, sdev = 100, means = 500, threshold = TRUE){
  order.freq <- data.frame(table(x))
  order.freq$CumFreq <- cumsum(order.freq$Freq)
  val1 <- data.frame((0.5*order.freq$Freq)/(sum(order.freq$Freq))*100)
  val1 <- val1[1,]
  order.freq$RP <- NA
  order.freq[1,4] <- val1
  freqrp <-  c(order.freq[2:nrow(order.freq),2],0)
  order.freq$freqrp <- freqrp
  order.freq$RPcalc <- (order.freq$CumFreq + 0.5*order.freq$freqrp)/(sum(order.freq$Freq))*100
  dimminus <- nrow(order.freq)-1
  RPcalc <- order.freq$RPcalc[1:dimminus]
  order.freq[2:nrow(order.freq),4] <-  RPcalc
  order.freq <- order.freq %>%
    select(-freqrp, -RPcalc)
  order.freq
  order.freq$RPdiv100 <- (order.freq$RP)/100

  order.freq$Zscore <- scale(order.freq$RPdiv100,
                             center = T,
                             scale = T)
  order.freq$Tscore <- ((order.freq$Zscore)*sdev)+means

  if (threshold == TRUE) {
    order.freq <- order.freq %>% mutate(
      cortes = case_when(
        Zscore > 1 ~ "Alto",
        Zscore > -1 ~ "Medio",
        TRUE ~ "Bajo"))
  } else {
    order.freq <- order.freq %>% mutate(
      cortes = case_when(
        Zscore > 2 ~ "Muy alto",
        Zscore > 1 ~ "Alto",
        Zscore > -1 ~ "Medio",
        Zscore > -2 ~ "Bajo",
        TRUE ~ "Muy bajo"))
  }
  baremos <- order.freq %>%
    select(Puntajes_Brutos = x,
           Z = Zscore,
           Threshold = cortes)
  baremos <- dplyr::as.tbl(baremos)
  summarytab <- baremos %>%
    dplyr::group_by(Threshold) %>%
    dplyr::summarise(MinPB = dplyr::first(Puntajes_Brutos),
              MaxPB = dplyr::last(Puntajes_Brutos),
              MinZ = min(Z, na.rm = T),
              MaxZ = max(Z, na.rm = T))
  list(baremos,summarytab)
}

#' This functions calculates parameters to equate measurements in a IRT framework
#'
#' It calculates mean-mean parameters in order to do a linear transformation of a scale to make it comparable with another one.
#' The function does the mean-mean transformation for test in a IRT framework, a good reference is:
#' Kolen, M & Brennan, R. (2014) Test equating, scaling and linking. Methods and practice. Third edition. NY: Springer.
#' @name bluequate
#' @param x vector of continous variable that you want to equate
#' @param y vector of continous variable that you want to equate to. This variable will have the same metric as parameter x.
#' @return The output is a list of tibbles with the calculated parameters, the items and the difficulty of items of one group and the other. Finally a the plot with control bands to check item displacement.
#'
#' @author Juan Carlos Saravia
#' @examples \donttest{bluequate(x,y, err.x, err.y)}
#' @export

