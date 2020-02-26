test_that("Create a tabla of distributions", {

  data_prueba <- c(1,2,3,4,5,6,7,7,7,7,7,7,7,7,7,8,8,8,8,8,9)
  freq <- data.frame(table(data_prueba))
  prop <- data.frame(prop.table(table(data_prueba)))
  tabla <- dplyr::left_join(freq,prop, by = "data_prueba")
  names(tabla)[1] <- "Vector1"
  tabla <- tabla %>%
    dplyr::select(Categories = Vector1,
                  Frequencies = Freq.x,
                  Proportions = Freq.y)
  tabla <- tabla %>% dplyr::as_tibble() %>%
    dplyr::mutate(
      CumFreq = cumsum(Frequencies))
  tabla

  expect_equal(grafi_distribution(data_prueba),tabla)
})
