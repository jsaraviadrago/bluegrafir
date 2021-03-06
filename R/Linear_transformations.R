#' Linear transformations to compare measurements between years
#'
#' This function is used to standardize your results and transform them with a set mean and standard deviation.
#'
#' @name blue_ztransform
#' @param x vector that you want to change scale.
#' @param data dataframe that goes with continous vector
#' @param meanvar parameter for the mean, the default is 500. This is used for the linear transformation and setting a mean different from 0
#' @param sdvar parameter for the standard deviation, the default is 100. This is used for the linear transformation and setting a standard deviation different from 1
#' @return The output is a vector of a transformed value with the mean and standard deviation set with the parameters
#' @author Juan Carlos Saravia
#' @examples
#'
#'data_example <- data.frame( ID = c(1,2,3,4,5,6,7,78,7,7,7,7,7,7,7,7,8,8,8,8,8),
#'                           puntaje = c(1,2,3,4,5,6,7,78,7,7,7,7,7,7,7,7,8,8,8,8,8))
#'blue_ztransform(data_example$puntaje,data_example,
#' meanvar = 500, sdvar = 100)
#' @export
blue_ztransform <- function(x, data, meanvar = 500,sdvar = 100) {
  standard <- scale(x, center = T, scale = T)
  conversion <- data.frame((standard*sdvar)+meanvar)
  data <- data.frame(data, Transf_500 = conversion[,1])
  data}

#' This function is used to compare results between groups or years with parameters that you establish.
#'
#' @name blue_comparison
#' @param lmes vector with the most recent measurement or the group you want to compare against the original parameters
#' @param data data.frame from the vector with the most recent measurement
#' @param parfmean mean with the baseline parameter to compare against.
#' @param parfsd sd with the baseline parameter to compare against.
#' @param meanvar parameter for the mean, the default is 500. This is used for the linear transformation and setting a mean different from 0
#' @param sdvar parameter for the standard deviation, the default is 100. This is used for the linear transformation and setting a standard deviation different from 1.
#' @return The output is a vector of a transformed value that compares a recent measurement against a baseline measurement.
#' @author Juan Carlos Saravia
#' @examples
#'
#'data_example <- data.frame( ID = c(1,2,3,4,5,6,7,78,7,7,7,7,7,7,7,7,8,8,8,8,8),
#'                           puntaje = c(1,2,3,4,5,6,7,78,7,7,7,7,7,7,7,7,8,8,8,8,8))
#' blue_comparison(data_example$puntaje, data_example,
#'  parfmean = 2, parfsd = 0.5)
#' @export

blue_comparison <- function(lmes,data, parfmean, parfsd,
                            meanvar = 500, sdvar = 100) {
  equate <- (lmes-parfmean)/parfsd
  conversion <- data.frame((equate*sdvar)+meanvar)
  data_final <- data.frame(data, Transf_500 = conversion[,1])
  data_final}

#' This function can be used to do linear percentile scaling. It helps to create thresholds for surveys and continous variables.
#'
#'It calculates the cumulative frequency of a continous variable, transforms this variable into percentiles by using a constant of .50 in order to calculate values over the 50% and under it based on the frequency of values.
#' After that, it standardize scores and then transforms values with a mean of 500 and standard deviation of 100 (default).
#' @name blue_zscaling
#' @param x vector of continous values
#' @param data data from the continous values of x parameter
#' @param sdev standard deviation which is to be assigned for the transformation, default value is 100.
#' @param means mean which is to be assigned for the linear transformation. Default value is 500.
#' @param type_scale logical parameter were TRUE assigns three thresholds. It is based on a normal distribution were values lower than -1 standard deviation from the mean are low, values higher than 1 standard deviation are high and values in between are medium (default).
#' if logical parameter is FALSE, 5 thresholds are assigned where values bigger than 2 standard deviations are very high, higher than 1 standard deviation are high, higher than -1 are medium, higher than -2 are low and lower than -2 standard deviations are very low.
#' @return The output is a tibble with raw scores, zscores and thresholds
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @author Juan Carlos Saravia
#' @examples
#'data_example <- data.frame(ID = c(1,2,3,4,5,6,7,78,7,7,7,7,7,7,7,7,8,8,8,8,8),
#'puntaje = c(1,2,3,4,5,6,7,78,7,7,7,7,7,7,7,7,8,8,8,8,8))

#'blue_zscaling(data_example$puntaje,data_example,
#' type_scale = "CumulativeZ")
#' @export

blue_zscaling <- function(x, data, sdev = 100, means = 500,
                          type_scale = "CumulativeZ"){
  x2 <- data.frame(x)
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
  order.freq$RPdiv100 <- (order.freq$RP)/100
  if (type_scale == "CumulativeZ") {
    order.freq$Zscore <- stats::qnorm(order.freq$RPdiv100)
  } else if(type_scale == "Zscores"){
    order.freq$Zscore <- scale(order.freq$RPdiv100,
                               center = T,
                               scale = T)
  } else {
    order.freq$Zscore <- stats::qnorm(order.freq$RPdiv100)
  }
  order.freq$Tscore <- ((order.freq$Zscore)*sdev)+means
  order.freq <- order.freq  %>%
    mutate( Threshold =
              case_when( Zscore < -1 ~ "Bajo",
                         Zscore < 1 ~ "Medio",
                         Zscore >= 1 ~ "Alto"))
  baremos <- order.freq %>%
    select(Puntajes_Brutos = .data$x,
           Z = .data$Zscore,
           .data$Threshold)
  baremos <- dplyr::as.tbl(baremos)
  summarytab <- baremos %>%
    dplyr::group_by(.data$Threshold) %>%
    dplyr::summarise(ThresholdPB = dplyr::first(.data$Puntajes_Brutos))
  summarytab<-  summarytab %>%
    dplyr::filter(.data$Threshold != "Bajo")
  Alto <- summarytab %>%
    dplyr::filter(.data$Threshold == "Alto") %>%
    dplyr::select(.data$ThresholdPB) %>%
    as.numeric()
  Medio <- summarytab %>%
    dplyr::filter(.data$Threshold  == "Medio") %>%
    dplyr::select(.data$ThresholdPB) %>%
    as.numeric()
  x2$Levels <- dplyr::if_else(x2$x >= Alto, "Alto",
                              dplyr::if_else(x2$x >= Medio, "Medio",
                                             "Bajo"))
  data <- data.frame(data,Levels = x2[,2])
  list(summarytab,data)}

#' This functions calculates parameters to equate measurements in a IRT framework
#'
#' It calculates mean-mean parameters in order to do a linear transformation of a scale to make it comparable with another one.
#' The function does the mean-mean transformation for test in a IRT framework, a good reference is:
#' Kolen, M & Brennan, R. (2014) Test equating, scaling and linking. Methods and practice. Third edition. NY: Springer.
#' @name blue_equate
#' @param x vector of continous variable that you want to equate
#' @param y vector of continous variable that you want to equate to. This variable will have the same metric as parameter x.
#' @param x_ee vector of continous variable with estimated error.
#' @param y_ee vector of continous variable with estimated error.
#' @return The output is a list of tibbles with the calculated parameters, the items and the difficulty of items of one group and the other. Finally a the plot with control bands to check item displacement.
#' @author Juan Carlos Saravia
#' @examples
#'x1 <- c(1,2,3,4,5,6,7,8,9,10)
#'y1 <- c(2,3,4,5,6,7,9,8,6,5)
#'x.ee1 <- c(1,2,3,5,6,7,8,9,20,10)
#'y.ee1 <- c(9,8,7,6,5,4,3,2,1,2)
#'equate_table <- data.frame(x1,x.ee1,y1,y.ee1)
#'blue_equate(equate_table$x1,equate_table$y1,
#'equate_table$x.ee1,
#'equate_table$y.ee1)
#' @export

blue_equate <- function(x,y,x_ee,y_ee){
  media.x <- mean(x, na.rm = T)
  std.x <- stats::sd(x, na.rm = T)
  media.x_ee <- mean(x_ee, na.rm = T)
  std.x_ee <- stats::sd(x_ee)
  media.y <- mean(y, na.rm = T)
  std.y <- stats::sd(y, na.rm = T)
  media.y_ee <- mean(y_ee, na.rm =T)
  std.y_ee <- stats::sd(y_ee, na.rm=T)
  Di_M1_M2 <- media.x - media.y
  FirstM_in_secondM_a <-  std.y/std.x
  FirstM_in_secondM_b <- (media.y-(FirstM_in_secondM_a*media.x))
  SecondM_in_FirstM_a <- std.x/std.y
  SecondM_in_FirstM_b <- (media.x-(SecondM_in_FirstM_a*media.y))
  Equate <- data.frame(FirstM_in_secondM_a,
                       FirstM_in_secondM_b,
                       SecondM_in_FirstM_a,
                       SecondM_in_FirstM_b)
  Equate
}
