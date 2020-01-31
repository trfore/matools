# test-plot_spike_raster.R

test_that("plot_spike_raster accepts data frames", {
  n <- 40 # sweeps
  n_stim <- 3
  conditions <- c("control", "drug_1", "drug_2")
  n_conditions <- length(conditions)

  expected_condition_starts <- c(-1, -41, -81)
  expected_limit <- c(-120)

  simple_df <-
    data.frame(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      time_ms = rep(c(280, 290, 300), times = (n * n_conditions)),
      condition = rep(conditions, rep((n * n_stim), n_conditions))
    )

  test_plot_1 <-
    plot_spike_raster(
      df = simple_df,
      filename = "example",
      id = "experiment_id",
      time_of_stim = c(278.18, 288.18, 298.18),
      condition_names = conditions,
      auto_window = TRUE,
      auto_window_size = 30,
      auto_window_symmetric = FALSE,
      plot_conditions_only = TRUE
    )
  plot_1_data <- ggplot2::layer_data(test_plot_1)
  plot_1_scales <- ggplot2::layer_scales(test_plot_1)

  test_plot_2 <-
    plot_spike_raster(
      df = simple_df,
      filename = "example",
      id = "experiment_id",
      time_of_stim = c(278.18, 288.18, 298.18),
      condition_names = conditions,
      auto_window = TRUE,
      auto_window_size = 30,
      auto_window_symmetric = FALSE,
      plot_conditions_only = FALSE
    )
  plot_2_data <- ggplot2::layer_data(test_plot_2)
  plot_2_scales <- ggplot2::layer_scales(test_plot_2)

  expect_equal(plot_1_data$linetype, rep("dotted", times = n_conditions))
  # y-axis breaks / condition starts
  expect_equal(plot_1_data$yintercept, expected_condition_starts)
  # y-axis limit, last "sweep"
  expect_equal(plot_1_scales[["y"]][["range"]][["range"]][[1]], expected_limit)
  expect_equal(plot_1_data$yintercept, plot_2_data$ymin)
})

test_that("plot_spike_raster accepts tibbles", {
  n <- 40 # sweeps
  n_stim <- 3
  conditions <- c("control", "drug_1", "drug_2")
  n_conditions <- length(conditions)

  expected_condition_starts <- c(-1, -41, -81)
  expected_limit <- c(-120)

  simple_tbl <-
    tibble::tibble(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      time_ms = rep(c(280, 290, 300), times = (n * n_conditions)),
      condition = rep(conditions, rep((n * n_stim), n_conditions))
    )

  test_plot <-
    plot_spike_raster(
      df = simple_tbl,
      filename = "example",
      id = "experiment_id",
      time_of_stim = c(278.18, 288.18, 298.18),
      condition_names = conditions,
      auto_window = TRUE,
      auto_window_size = 30,
      auto_window_symmetric = FALSE,
      plot_conditions_only = TRUE
    )

  plot_data <- ggplot2::layer_data(test_plot)
  plot_scales <- ggplot2::layer_scales(test_plot)

  expect_equal(plot_data$linetype, rep("dotted", times = n_conditions))
  # y-axis breaks / condition starts
  expect_equal(plot_data$yintercept, expected_condition_starts)
  # y-axis limit
  expect_equal(plot_scales[["y"]][["range"]][["range"]][[1]], expected_limit)
})

test_that("plot_spike_raster plots all events and highlights conditions", {
  n <- 40 # sweeps
  n_stim <- 3
  conditions <- c("control", "wash", "drug_1", "wash", "drug_2", "wash")
  n_conditions <- length(conditions)

  # auto_condition_color for 3 conditions
  expected_colors <- c("black", "#F8766D", "#00BFC4")
  expected_condition_starts <- c(-1, -81, -161)
  expected_condition_ends <- c(-40, -120, -200)
  expected_range <- c(-n * n_conditions, -1)

  simple_df <-
    tibble::tibble(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      time_ms = rep(c(280, 290, 300), times = (n * n_conditions)),
      condition = rep(conditions, rep((n * n_stim), n_conditions))
    )

  test_plot <-
    plot_spike_raster(
      df = simple_df,
      filename = "example",
      id = "experiment_id",
      time_of_stim = c(278.18, 288.18, 298.18),
      condition_names = c("control", "drug_1", "drug_2"),
      auto_window = TRUE,
      auto_window_size = 30,
      auto_window_symmetric = FALSE,
      plot_conditions_only = FALSE
    )

  plot_data <- ggplot2::layer_data(test_plot)
  plot_scales <- ggplot2::layer_scales(test_plot)

  expect_equal(plot_data$fill, expected_colors)
  expect_equal(plot_data$ymin, expected_condition_starts)
  expect_equal(plot_data$ymax, expected_condition_ends)
  expect_equal(plot_scales[["y"]][["range"]][["range"]], expected_range)
})

test_that("plot_spike_raster only plots conditions and truncates data", {
  n <- 40 # sweeps
  n_stim <- 3
  conditions <- c("control", "wash", "drug_1", "wash", "drug_2", "wash")
  n_conditions <- length(conditions)

  expected_condition_starts <- c(-1, -41, -81)
  expected_limit <- c(-120)

  simple_df <-
    tibble::tibble(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      time_ms = rep(c(280, 290, 300), times = (n * n_conditions)),
      condition = rep(conditions, rep((n * n_stim), n_conditions))
    )

  test_plot <-
    plot_spike_raster(
      df = simple_df,
      filename = "example",
      id = "experiment_id",
      time_of_stim = c(278.18, 288.18, 298.18),
      condition_names = c("control", "drug_1", "drug_2"),
      auto_window = TRUE,
      auto_window_size = 30,
      auto_window_symmetric = FALSE,
      plot_conditions_only = TRUE
    )

  plot_data <- ggplot2::layer_data(test_plot)
  plot_scales <- ggplot2::layer_scales(test_plot)

  # y-axis breaks / condition starts
  expect_equal(plot_data$yintercept, expected_condition_starts)
  # y-axis limit, last "sweep"
  expect_equal(plot_scales[["y"]][["range"]][["range"]][[1]], expected_limit)
})

test_that("plot_spike_raster returns expected color values", {
  n <- 40 # sweeps
  n_stim <- 3
  conditions <- c("control", "wash", "bdrug", "ddrug", "cdrug")
  n_conditions <- length(conditions)

  expected_colors <- c("black", "#00B0F6", "#00BF7D", "#F8766D")
  expected_condition_starts <- c(-1, -41, -81, -121)
  expected_limit <- c(-160)

  simple_df <-
    tibble::tibble(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      time_ms = rep(c(280, 290, 300), times = (n * n_conditions)),
      condition = rep(conditions, rep((n * n_stim), n_conditions))
    )

  test_plot <-
    plot_spike_raster(
      df = simple_df,
      filename = "example",
      id = "experiment_id",
      time_of_stim = c(278.18, 288.18, 298.18),
      condition_names = c("control", "bdrug", "ddrug", "cdrug"),
      auto_window = TRUE,
      auto_window_size = 30,
      auto_window_symmetric = FALSE,
      manual_condition_color = c("black", "#00B0F6", "#00BF7D", "#F8766D"),
      plot_conditions_only = TRUE
    )

  plot_data <- ggplot2::layer_data(test_plot)
  plot_scales <- ggplot2::layer_scales(test_plot)

  # y-axis breaks / condition starts
  expect_equal(plot_data$yintercept, expected_condition_starts)
  # y-axis limit, last "sweep"
  expect_equal(plot_scales[["y"]][["range"]][["range"]][[1]], expected_limit)
  # y-axis labels
  expect_equal(test_plot[["layers"]][[4]][["aes_params"]][["colour"]], expected_colors)
  # returns error when manual_condition_color does not match the number of condition_names
  expect_error(
    plot_spike_raster(
      df = simple_df,
      filename = "example",
      id = "experiment_id",
      time_of_stim = c(278.18, 288.18, 298.18),
      condition_names = c("control", "bdrug", "ddrug", "cdrug"),
      auto_window = TRUE,
      auto_window_size = 30,
      auto_window_symmetric = FALSE,
      manual_condition_color = c("black", "#00B0F6", "#00BF7D"),
      plot_conditions_only = TRUE
    )
  )
})
