test_that("Wright map", {
  library(ggplot2)
  library(ggrepel)
  library(gridExtra)

  set.seed(1234)
  personas.data <- data.frame(individuos=sort(round(rnorm(500, 0,1),3)))

set.seed(9876)
  items.data1 <- data.frame(
    items_n1 = as.numeric(seq(1:36)),
    items_g1 = ceiling(runif(36, 0, 5)),
    items1 = sort(round(runif(36, -3,3),3)),
    stringsAsFactors=F)

  items.data1 <- cbind(items.data1,
                       items_c = ifelse(items.data1$items_n < 10,
                                        paste("IT_EX",
                                              items.data1$items_n,
                                              sep="_0"),
                                        paste("IT_EX",
                                              items.data1$items_n,
                                              sep="_")))

  groups <- 5

  persons.measure <- data.frame(personas.data)
  names(persons.measure)[1] <- "personas"
  items.measurement <- data.frame(items.data1$items1)
  names(items.measurement)[1] <- "items.measure2"
  items.data <- data.frame(
    items_n = as.numeric(1:nrow(items.measurement)),
    items_g = ceiling(stats::runif(nrow(items.measurement),0,groups)),
    items.measure2 = items.measurement$items.measure2,
    items.names = items.data1$items_c,
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
  grafica2 <- grid.arrange(it,pe, ncol=2, widths=c(1,1), top = " ")

  expect_equal(grafi_wrightmap(personas.data$individuos,
                               items.data1$items1,
                               items.data1$items_c),
               grafica2)
})
