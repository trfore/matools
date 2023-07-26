# test-plot_event_avg.R

test_that("plot_event_avg() plots sem and sd", {
  simple_tbl <-
    data.frame(
      recording_id = rep(1:2, each = 3),
      experiment_id = rep("example", 6),
      condition = rep(c("control", "drug_1", "drug_2"), 2),
      amplitude = c(1, 0.5, 1, 2, 1, 2)
    )

  simple_plot_sem <-
    plot_event_avg(
      df = simple_tbl,
      x_axis = condition,
      y_axis = amplitude,
      plot_grouping = "recording_id",
      plot_mean = TRUE,
      sd_value = "sem",
      experiment_name = "example",
      x_label = "condition",
      y_label = "amplitude",
      ymax = 3
    )

  expect_equal(rlang::quo_get_expr(simple_plot_sem[["layers"]][[5]][["mapping"]][["ymax"]])[[3]][[3]], "sem")

  simple_plot_sd <-
    plot_event_avg(
      df = simple_tbl,
      x_axis = condition,
      y_axis = amplitude,
      plot_grouping = "recording_id",
      plot_mean = TRUE,
      sd_value = "sd",
      experiment_name = "example",
      x_label = "condition",
      y_label = "amplitude",
      ymax = 3
    )

  expect_equal(rlang::quo_get_expr(simple_plot_sd[["layers"]][[5]][["mapping"]][["ymax"]])[[3]][[3]], "sd")
})

test_that("plot_event_avg() errors when weights are missing", {
  simple_tbl <-
    data.frame(
      recording_id = rep(1:2, each = 3),
      experiment_id = rep("example", 6),
      condition = rep(c("control", "drug_1", "drug_2"), 2),
      amplitude = c(1, 0.5, 1, 2, 1, 2)
    )

  expect_error(
    plot_event_avg(
      df = simple_tbl,
      x_axis = condition,
      y_axis = amplitude,
      plot_grouping = "recording_id",
      plot_mean = TRUE,
      weighted_mean = TRUE,
      sd_value = "sd",
      experiment_name = "example",
      x_label = "condition",
      y_label = "amplitude",
      ymax = 3
    )
  )
})

test_that("plot_event_avg() plots arithmetic mean", {
  weighted_tbl <-
    data.frame(
      recording_id = rep(1:2, each = 3),
      experiment_id = rep("example", 6),
      condition = rep(c("control", "drug_1", "drug_2"), 2),
      amplitude = c(1, 0.5, 1, 2, 1, 2),
      w = rep(c(1, 100), each = 3)
    )

  expected_arithmetic_mean <-
    weighted_tbl %>%
    dplyr::group_by(condition) %>%
    dplyr::summarise(
      avg = mean(amplitude),
      sd = sd(amplitude)
    )

  # expect original weight values to be untouched
  plot_arithmetic_mean <-
    plot_event_avg(
      df = weighted_tbl,
      x_axis = condition,
      y_axis = amplitude,
      plot_grouping = "recording_id",
      plot_mean = TRUE,
      weighted_mean = FALSE,
      sd_value = "sd",
      experiment_name = "plot arithmetic mean",
      x_label = "condition",
      y_label = "amplitude",
      ymax = 3
    )
  expect_equal(plot_arithmetic_mean[["layers"]][[5]][["data"]][["avg"]], expected_arithmetic_mean$avg)
  expect_equal(plot_arithmetic_mean[["layers"]][[5]][["data"]][["sd"]], expected_arithmetic_mean$sd)
})

test_that("plot_event_avg() plots weighted mean", {
  weighted_tbl <-
    data.frame(
      recording_id = rep(1:2, each = 3),
      experiment_id = rep("example", 6),
      condition = rep(c("control", "drug_1", "drug_2"), 2),
      amplitude = c(1, 0.5, 1, 2, 1, 2),
      w = rep(c(1, 100), each = 3)
    )

  expected_weighted_mean <-
    weighted_tbl %>%
    dplyr::group_by(condition) %>%
    dplyr::summarise(
      avg = weighted.mean(amplitude, w),
      sd = sqrt(Hmisc::wtd.var(amplitude, w))
    )

  plot_weighted_mean <-
    plot_event_avg(
      df = weighted_tbl,
      x_axis = condition,
      y_axis = amplitude,
      plot_grouping = "recording_id",
      plot_mean = TRUE,
      weighted_mean = TRUE,
      sd_value = "sd",
      experiment_name = "plot weighted mean",
      x_label = "condition",
      y_label = "amplitude",
      ymax = 3
    )
  expect_equal(plot_weighted_mean[["layers"]][[5]][["data"]][["avg"]], expected_weighted_mean$avg)
  expect_equal(plot_weighted_mean[["layers"]][[5]][["data"]][["sd"]], expected_weighted_mean$sd)
  expect_error(
    plot_event_avg(
      df = weighted_tbl,
      x_axis = condition,
      y_axis = amplitude,
      plot_grouping = "recording_id",
      plot_mean = TRUE,
      weighted_mean = FALSE,
      sd_value = "not_a_value",
      experiment_name = "plot arithmetic mean",
      x_label = "condition",
      y_label = "amplitude",
      ymax = 3
    )
  )
})
