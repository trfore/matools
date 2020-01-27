# test_plot_jitter.R

test_that("plot_jitter accepts data frames", {
  n <- 100 # sweeps
  n_stim <- 3 # stimuli per sweep
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_df <-
    data.frame(
      experiment_id = rep("example", ((n * n_stim * n_conditions))),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      stimulus = rep(c(1:n_stim), n * n_conditions),
      jitter = c(rnorm((n * n_stim), 2, 0.5), rnorm((n * n_stim), 3, 0.5)),
      event_index = rep(1, ((n * n_stim * n_conditions)))
    )

  plot_test <-
    plot_jitter(
      simple_df,
      ymax = 4,
      sd_value = "sd"
    )

  plot_data <- ggplot2::layer_data(plot_test)
  plot_scales <- ggplot2::layer_scales(plot_test)
  sd_error_bars <- environment(plot_scales[["y"]][["super"]])[["self"]][["range"]][["range"]]

  expect_equal(plot_data$x, c(0.875, 1.875, 2.875, 1.125, 2.125, 3.125))
  expect_equal(round(plot_data$y), rep(c(2, 3), each = 3))

  plot_test <-
    plot_jitter(
      simple_df,
      ymax = 4,
      sd_value = "sem"
    )
  plot_scales <- ggplot2::layer_scales(plot_test)
  sem_error_bars <- environment(plot_scales[["y"]][["super"]])[["self"]][["range"]][["range"]]

  expect_lt(min(sd_error_bars), min(sem_error_bars))
  expect_gt(max(sd_error_bars), max(sem_error_bars))
  # returns error when passed incorrect 'sd_value'
  expect_error(
    plot_jitter(
      simple_df,
      ymax = 4,
      sd_value = "not_a_value"
    )
  )
})

test_that("plot_jitter accepts tibbles", {
  n <- 100 # sweeps
  n_stim <- 3 # stimuli per sweep
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_tbl <-
    tibble::tibble(
      experiment_id = rep("example", ((n * n_stim * n_conditions))),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      stimulus = rep(c(1:n_stim), n * n_conditions),
      jitter = c(rnorm((n * n_stim), 2, 0.5), rnorm((n * n_stim), 3, 0.5)),
      event_index = rep(1, ((n * n_stim * n_conditions)))
    )

  plot_test <-
    plot_jitter(
      simple_tbl,
      ymax = 4,
      sd_value = "sd"
    )

  plot_data <- ggplot2::layer_data(plot_test)
  plot_scales <- ggplot2::layer_scales(plot_test)
  sd_error_bars <- environment(plot_scales[["y"]][["super"]])[["self"]][["range"]][["range"]]

  expect_equal(plot_data$x, c(0.875, 1.875, 2.875, 1.125, 2.125, 3.125))
  expect_equal(round(plot_data$y), rep(c(2, 3), each = 3))

  plot_test <-
    plot_jitter(
      simple_tbl,
      ymax = 4,
      sd_value = "sem"
    )
  plot_scales <- ggplot2::layer_scales(plot_test)
  sem_error_bars <- environment(plot_scales[["y"]][["super"]])[["self"]][["range"]][["range"]]

  expect_lt(min(sd_error_bars), min(sem_error_bars))
  expect_gt(max(sd_error_bars), max(sem_error_bars))
})

test_that("plot_jitter accepts vargs", {
  n <- 100 # sweeps
  n_stim <- 3 # stimuli per sweep
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_df <-
    data.frame(
      experiment_id = rep("example", ((n * n_stim * n_conditions))),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      stimulus = rep(c(1:n_stim), n * n_conditions),
      jitter = c(rnorm((n * n_stim), 2, 0.5), rnorm((n * n_stim), 3, 0.5)),
      event_index = rep(1, ((n * n_stim * n_conditions)))
    )

  plot_test <-
    plot_jitter(
      simple_df,
      group_value = "condition",
      ymax = 4,
      sd_value = "sd",
      ggplot2::scale_color_manual(values = c("black", "blue"))
    )

  plot_data <- ggplot2::layer_data(plot_test)
  expect_equal(plot_data$colour, rep(c("black", "blue"), each = 3))
  expect_equal(plot_test[["labels"]][["title"]], NULL)
})

test_that("plot_jitter accepts multiple vargs", {
  n <- 100 # sweeps
  n_stim <- 3 # stimuli per sweep
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_df <-
    data.frame(
      experiment_id = rep("example", ((n * n_stim * n_conditions))),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      stimulus = rep(c(1:n_stim), n * n_conditions),
      jitter = c(rnorm((n * n_stim), 2, 0.5), rnorm((n * n_stim), 3, 0.5)),
      event_index = rep(1, ((n * n_stim * n_conditions)))
    )

  plot_test <-
    plot_jitter(
      simple_df,
      group_value = "condition",
      ymax = 4,
      sd_value = "sd",
      ggplot2::scale_color_manual(values = c("black", "blue")),
      ggplot2::ggtitle("title")
    )

  plot_data <- ggplot2::layer_data(plot_test)
  expect_equal(plot_data$colour, rep(c("black", "blue"), each = 3))
  expect_equal(plot_test[["labels"]][["title"]], "title")
})

test_that("plot_jitter accepts ordered stimuli", {
  n <- 100 # sweeps
  n_stim <- 3 # stimuli per sweep
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_df <-
    data.frame(
      experiment_id = rep("example", ((n * n_stim * n_conditions))),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      stimulus = as.ordered(rep(c(1:n_stim), n * n_conditions)),
      jitter = c(rnorm((n * n_stim), 2, 0.5), rnorm((n * n_stim), 3, 0.5)),
      event_index = rep(1, ((n * n_stim * n_conditions)))
    )

  plot_test <-
    plot_jitter(
      simple_df,
      ymax = 4,
      sd_value = "sd"
    )

  plot_data <- ggplot2::layer_data(plot_test)
  plot_scales <- ggplot2::layer_scales(plot_test)

  expect_equal(as.numeric(plot_data$x), c(0.875, 1.875, 2.875, 1.125, 2.125, 3.125))
})
