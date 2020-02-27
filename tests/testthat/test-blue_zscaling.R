test_that("Cumulative Z scaling", {
  data_example <- data.frame(ID = c(1,2,3,4,5,6,7,7,7,7,7,7,7,7,7,7,
                                    8,8,8,8,8),
  puntaje = c(1,2,3,4,5,6,7,7,7,7,7,7,7,7,7,7,
              8,8,8,8,8))

  data_prueba <- data.frame(ID = c(1,2,3,4,5,6,7,7,7,7,7,7,7,7,7,7,
                                    8,8,8,8,8),
                             puntaje = c(1,2,3,4,5,6,7,7,7,7,7,7,7,7,7,7,
                                         8,8,8,8,8))
  sdev <- 100
  means <- 500
  x2 <- data.frame(data_example$puntaje)
  order.freq <- data.frame(table(data_example$puntaje))
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
  order.freq$Zscore <- stats::qnorm(order.freq$RPdiv100)
  order.freq$Tscore <- ((order.freq$Zscore)*sdev)+means
  library(dplyr)

  order.freq <- order.freq  %>%
    dplyr::mutate( Threshold =
                     dplyr::case_when( Zscore < -1 ~ "Bajo",
                                       Zscore < 1 ~ "Medio",
                                       Zscore >= 1 ~ "Alto"))
  baremos <- order.freq %>%
    select(Puntajes_Brutos = Var1,
           Z = Zscore,
           Threshold)
  baremos <- dplyr::as.tbl(baremos)
  summarytab <- baremos %>%
    dplyr::group_by(Threshold) %>%
    dplyr::summarise(ThresholdPB = dplyr::first(Puntajes_Brutos))

  summarytab<-  summarytab %>%
    dplyr::filter(Threshold != "Bajo")
  Alto <- summarytab %>%
    dplyr::filter(Threshold == "Alto") %>%
    dplyr::select(ThresholdPB) %>%
    as.numeric()
  Medio <- summarytab %>%
    dplyr::filter(Threshold  == "Medio") %>%
    dplyr::select(ThresholdPB) %>%
    as.numeric()
  x2$Levels <- dplyr::if_else(x2$data_example.puntaje >= Alto,
                              "Alto",
                              dplyr::if_else(x2$data_example.puntaje >= Medio,
                                             "Medio",
                                             "Bajo"))
  data_example <- data.frame(data_example,Levels = x2[,2])
  data_final <- list(summarytab,data_example)
  expect_equal(blue_zscaling(data_prueba$puntaje,data_prueba),
               data_final)
  expect_error(blue_zscaling(data_prueba$puntaje))
})
