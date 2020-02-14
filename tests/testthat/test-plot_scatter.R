# test-plot_scatter.R

test_that("plot_scatterplot_amplitude() maps color to condition", {
  n <- 198
  conditions <- c("control", "wash", "drug_1", "wash", "drug_2", "wash")
  n_conditions <- length(conditions)

  set.seed(451)
  simple_df <-
    data.frame(
      sweep = rep(1:n, each = 1),
      condition = rep(conditions, each = n / n_conditions),
      amplitude = rnorm(n, mean = 75, sd = 1),
      stimulus = rep(1, n)
    )

  simple_df <-
    simple_df %>%
    dplyr::mutate(amplitude = ifelse(condition == "drug_1", (amplitude - 50), amplitude)) %>%
    dplyr::filter(condition %in% c("control", "drug_1", "drug_2"))

  test_plot <-
    plot_scatterplot_amplitude(
      df = simple_df,
      sweep_duration = 5,
      filename = "file_01",
      experiment_id = "experiment",
      monochrome = FALSE,
      ymax = 100,
      y_label = "custom y"
    ) + ggplot2::scale_color_manual(values = c("black", "#00B0F6", "#00BF7D"))


  plot_data <- ggplot2::layer_data(test_plot)
  plot_scales <- ggplot2::layer_scales(test_plot)
  chunk <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))
  plot_means <- as.numeric(sapply(chunk(plot_data$y, 3), mean))

  expect_equal(round(plot_means), c(75, 25, 75))
  expect_equal(plot_data$colour, rep(c("black", "#00B0F6", "#00BF7D"), each = n / n_conditions))
})

test_that("plot_scatterplot_amplitude() has no color mapping", {
  n <- 198
  conditions <- c("control", "wash", "drug_1", "wash", "drug_2", "wash")
  n_conditions <- length(conditions)

  simple_df <-
    data.frame(
      sweep = rep(1:n, each = 1),
      condition = rep(conditions, each = n / n_conditions),
      amplitude = rnorm(n, mean = 75, sd = 1),
      stimulus = rep(1, n)
    )

  simple_df <-
    simple_df %>%
    dplyr::mutate(amplitude = ifelse(condition == "drug_1", (amplitude - 50), amplitude)) %>%
    dplyr::filter(condition %in% c("control", "drug_1", "drug_2"))

  test_plot_monochrome <-
    plot_scatterplot_amplitude(
      df = simple_df,
      sweep_duration = 5,
      filename = "file_01",
      experiment_id = "experiment",
      monochrome = TRUE,
      truncate_x = TRUE,
      ymax = 100
    )

  # test color mapping
  expect_null(test_plot_monochrome[["layers"]][[1]][["mapping"]])
})
