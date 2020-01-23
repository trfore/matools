# test-plot_heatmap.R

test_that("plot_heatmap accepts data frames", {
  n <- 10 # sweeps
  n_stim <- 2 # stimuli per sweep
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_df <-
    data.frame(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      amplitude = c(rnorm(n * n_stim, 100, 20), rnorm(n * n_stim, 50, 10)),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      stimulus = rep(c(1:n_stim), n * n_conditions),
      event_index = rep(1, ((n * n_stim * n_conditions))),
      extra_column = 2
    )

  plot_test <-
    plot_heatmap(
      df = simple_df,
      filename = "example",
      condition_names = conditions,
      max_amplitude = 200
    )

  plot_data <- ggplot2::layer_data(plot_test)
  plot_scales <- ggplot2::layer_scales(plot_test)

  expect_equal(as.numeric(plot_data$x), rep(c(1, 2), times = (n * n_conditions)))
  expect_equal(as.numeric(plot_data$y), rep(-1:-n, each = n_stim, times = n_conditions))
  # returns warning when 'max_amplitude' is not defined
  expect_warning(plot_heatmap(
    df = simple_df,
    filename = "example",
    condition_names = conditions
  ))
  # returns warning when 'max_amplitude' is below the df's maximum amplitude
  expect_warning(plot_heatmap(
    df = simple_df,
    filename = "example",
    condition_names = conditions,
    max_amplitude = 1
  ))
})

test_that("plot_heatmap accepts tibbles", {
  n <- 10 # sweeps
  n_stim <- 2 # stimuli per sweep
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_tbl <-
    tibble::tibble(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      amplitude = c(rnorm(n * n_stim, 100, 20), rnorm(n * n_stim, 50, 10)),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      stimulus = rep(c(1:n_stim), n * n_conditions),
      event_index = rep(1, ((n * n_stim * n_conditions))),
      extra_column = 2
    )

  plot_test <-
    plot_heatmap(
      df = simple_tbl,
      filename = "example",
      condition_names = conditions,
      max_amplitude = 200
    )

  plot_data <- ggplot2::layer_data(plot_test)
  plot_scales <- ggplot2::layer_scales(plot_test)

  expect_equal(as.numeric(plot_data$x), rep(c(1, 2), times = (n * n_conditions)))
  expect_equal(as.numeric(plot_data$y), rep(-1:-n, each = n_stim, times = n_conditions))
})
