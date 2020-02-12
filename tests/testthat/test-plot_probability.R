# test-plot_probability.R

# plot_probability ----
test_that("plot_probability works without an inset", {
  # example output from create_histogram_df()
  bins <- seq(0.5, 39.5, by = 1)
  n_bins <- length(bins)
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_tbl <-
    tibble::tibble(
      condition = rep(conditions, each = n_bins),
      h_bin_centers = rep(bins, n_conditions),
      probability = 0,
      sd = 0.1
    ) %>%
    dplyr::mutate(probability = dplyr::case_when(
      condition == "control" & h_bin_centers %in% c(1.5, 11.5) ~ 1,
      condition == "drug_1" & h_bin_centers %in% c(1.5, 11.5) ~ 0.5,
      .default = 0
    ))

  test_plot <-
    plot_probability(
      df = simple_tbl,
      plot_stim_times = c(0, 10, 20),
      plot_value = "probability"
    )

  plot_data <- ggplot2::layer_data(test_plot)
  plot_scales <- ggplot2::layer_scales(test_plot)

  expect_equal(plot_data$x, simple_tbl$h_bin_centers)
  expect_equal(plot_data$y, simple_tbl$probability)
})

test_that("plot_probability works with an inset", {
  # example output from create_histogram_df()
  bins <- seq(0.5, 39.5, by = 1)
  n_bins <- length(bins)
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_tbl <-
    tibble::tibble(
      condition = rep(conditions, each = n_bins),
      h_bin_centers = rep(bins, n_conditions),
      probability = 0,
      sd = 0.1
    ) %>%
    dplyr::mutate(probability = dplyr::case_when(
      condition == "control" & h_bin_centers %in% c(1.5, 11.5) ~ 1,
      condition == "drug_1" & h_bin_centers %in% c(1.5, 11.5) ~ 0.5,
      .default = 0
    ))

  simple_inset <-
    simple_tbl %>%
    dplyr::filter(.data$condition == "drug_1") %>%
    dplyr::mutate(condition = "drug_2")

  test_plot <-
    plot_probability(
      df = simple_tbl,
      plot_stim_times = c(0, 10, 20),
      plot_value = "probability",
      df_inset = simple_inset,
      sd_value = "sd"
    )

  plot_data <- ggplot2::layer_data(test_plot)
  plot_scales <- ggplot2::layer_scales(test_plot)

  expect_equal(plot_data$x, simple_tbl$h_bin_centers)
  expect_equal(plot_data$y, simple_tbl$probability)
})

test_that("plot_probability accepts vargs", {
  bins <- seq(0.5, 39.5, by = 1)
  n_bins <- length(bins)
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)

  simple_tbl <-
    tibble::tibble(
      condition = rep(conditions, each = n_bins),
      h_bin_centers = rep(bins, n_conditions),
      probability = 0,
      sd = 0.1
    ) %>%
    dplyr::mutate(probability = dplyr::case_when(
      condition == "control" & h_bin_centers %in% c(1.5, 11.5) ~ 1,
      condition == "drug_1" & h_bin_centers %in% c(1.5, 11.5) ~ 0.5,
      .default = 0
    ))

  simple_inset <-
    simple_tbl %>%
    dplyr::filter(.data$condition == "drug_1") %>%
    dplyr::mutate(condition = "drug_2")

  test_plot <-
    plot_probability(
      df = simple_tbl,
      plot_stim_times = c(0, 10, 20),
      plot_value = "probability",
      xmax = 40,
      ymax = 1,
      y_label = "spike probability",
      df_inset = simple_inset,
      sd_value = "sd",
      manual_condition_color = NULL,
      ggplot2::ggtitle("title")
    )

  expect_equal(test_plot[["labels"]][["title"]], "title")
})

test_that("plot_probability without inset, plot colors match legend colors", {
  bins <- seq(0.5, 39.5, by = 1)
  n_bins <- length(bins)
  conditions <- c("control", "adrug", "bdrug", "ddrug", "cdrug")
  n_conditions <- length(conditions)

  expected_values <- data.frame(
    condition = conditions,
    probability = c(1, 0.75, 0.50, 0.25, 0.10),
    color = c("black", "#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
  )

  simple_tbl <-
    tibble::tibble(
      condition = rep(conditions, each = n_bins),
      h_bin_centers = rep(bins, n_conditions),
      probability = 0,
      sd = 0.1
    ) %>%
    dplyr::mutate(probability = dplyr::case_when(
      condition == "control" & h_bin_centers == 1.5 ~ 1,
      condition == "adrug" & h_bin_centers == 2.5 ~ 0.75,
      condition == "bdrug" & h_bin_centers == 4.5 ~ 0.50,
      condition == "ddrug" & h_bin_centers == 6.5 ~ 0.25,
      condition == "cdrug" & h_bin_centers == 8.5 ~ 0.10,
      .default = 0
    ))

  test_plot <-
    plot_probability(
      df = simple_tbl,
      plot_stim_times = c(0, 10, 20),
      plot_value = "probability"
    )

  plot_data <- ggplot2::layer_data(test_plot) %>% dplyr::filter(y %in% expected_values$probability)
  plot_scales <- ggplot2::layer_scales(test_plot)

  expect_equal(plot_data$colour, expected_values$color)
  expect_equal(plot_data$y, expected_values$probability)
})

test_that("plot_probability with inset, plot colors match legend colors", {
  bins <- seq(0.5, 39.5, by = 1)
  n_bins <- length(bins)
  conditions <- c("control", "adrug", "bdrug", "ddrug", "cdrug")
  n_conditions <- length(conditions)

  expected_values <- data.frame(
    condition = conditions,
    probability = c(1, 0.75, 0.50, 0.25, 0.10),
    color = c("black", "#F8766D", "#A3A500", "#00BF7D", "#00B0F6")
  )
  expected_inset_values <- data.frame(
    condition = "edrug",
    probability = c(1),
    color = c("#E76BF3")
  )

  simple_tbl <-
    tibble::tibble(
      condition = rep(conditions, each = n_bins),
      h_bin_centers = rep(bins, n_conditions),
      probability = 0,
      sd = 0.1
    ) %>%
    dplyr::mutate(probability = dplyr::case_when(
      condition == "control" & h_bin_centers == 1.5 ~ 1,
      condition == "adrug" & h_bin_centers == 2.5 ~ 0.75,
      condition == "bdrug" & h_bin_centers == 4.5 ~ 0.50,
      condition == "ddrug" & h_bin_centers == 6.5 ~ 0.25,
      condition == "cdrug" & h_bin_centers == 8.5 ~ 0.10,
      .default = 0
    ))

  simple_inset <-
    simple_tbl %>%
    dplyr::filter(.data$condition == "control") %>%
    dplyr::mutate(condition = "edrug")

  test_plot <-
    plot_probability(
      df = simple_tbl,
      plot_stim_times = c(0, 10, 20),
      plot_value = "probability",
      df_inset = simple_inset
    )

  plot_data <-
    ggplot2::layer_data(test_plot) %>%
    dplyr::filter(y %in% expected_values$probability)
  plot_scales <- ggplot2::layer_scales(test_plot)
  inlay_data <-
    ggplot2::layer_data(test_plot$plot_env$plot_inlay) %>%
    dplyr::filter(y %in% expected_inset_values$probability)
  inlay_scales <- ggplot2::layer_scales(test_plot$plot_env$plot_inlay)

  expect_equal(plot_data$colour, expected_values$color)
  expect_equal(plot_data$y, expected_values$probability)
  expect_equal(inlay_data$colour, expected_inset_values$color)
  expect_equal(inlay_data$y, expected_inset_values$probability)
})

test_that("plot_probability arg manual_condition_color accepts color values", {
  bins <- seq(0.5, 39.5, by = 1)
  n_bins <- length(bins)
  conditions <- c("control", "adrug", "bdrug", "ddrug", "cdrug")
  n_conditions <- length(conditions)

  expected_values <- data.frame(
    condition = conditions,
    probability = c(1, 0.75, 0.50, 0.25, 0.10),
    color = c("black", "#00B0F6", "#00BF7D", "#F8766D", "#E76BF3")
  )
  expected_inset_values <- data.frame(
    condition = "edrug",
    probability = c(1),
    color = c("orange")
  )

  simple_tbl <-
    tibble::tibble(
      condition = rep(conditions, each = n_bins),
      h_bin_centers = rep(bins, n_conditions),
      probability = 0,
      sd = 0.1
    ) %>%
    dplyr::mutate(probability = dplyr::case_when(
      condition == "control" & h_bin_centers == 1.5 ~ 1,
      condition == "adrug" & h_bin_centers == 2.5 ~ 0.75,
      condition == "bdrug" & h_bin_centers == 4.5 ~ 0.50,
      condition == "ddrug" & h_bin_centers == 6.5 ~ 0.25,
      condition == "cdrug" & h_bin_centers == 8.5 ~ 0.10,
      .default = 0
    ))

  simple_inset <-
    simple_tbl %>%
    dplyr::filter(.data$condition == "control") %>%
    dplyr::mutate(condition = "edrug")

  test_plot <-
    plot_probability(
      df = simple_tbl,
      plot_stim_times = c(0, 10, 20),
      plot_value = "probability",
      xmax = 40,
      ymax = 1,
      y_label = "spike probability",
      df_inset = simple_inset,
      sd_value = "sd",
      manual_condition_color = c("black", "#00B0F6", "#00BF7D", "#F8766D", "#E76BF3", "orange")
    )
  plot(test_plot)

  plot_data <-
    ggplot2::layer_data(test_plot) %>%
    dplyr::filter(y %in% expected_values$probability)
  plot_scales <- ggplot2::layer_scales(test_plot)
  inlay_data <-
    ggplot2::layer_data(test_plot$plot_env$plot_inlay) %>%
    dplyr::filter(y %in% expected_inset_values$probability)
  inlay_scales <- ggplot2::layer_scales(test_plot$plot_env$plot_inlay)

  expect_equal(plot_data$colour, expected_values$color)
  expect_equal(plot_data$y, expected_values$probability)
  expect_equal(inlay_data$colour, expected_inset_values$color)
  expect_equal(inlay_data$y, expected_inset_values$probability)
  # returns error when manual_condition_color does not match the number of condition_names
  expect_error(
    plot_probability(
      df = simple_tbl,
      plot_stim_times = c(0, 10, 20),
      plot_value = "probability",
      xmax = 40,
      ymax = 1,
      y_label = "spike probability",
      df_inset = simple_inset,
      sd_value = "sd",
      manual_condition_color = c("black", "#00B0F6", "#00BF7D", "#F8766D", "#E76BF3")
    )
  )
})

# plot_max_probability ----
test_that("plot_max_probability handles a single experiment", {
  # example output from create_histogram_df()
  bins <- seq(0.5, 39.5, by = 1)
  n_bins <- length(bins)
  conditions <- c("control", "drug")
  n_conditions <- length(conditions)

  simple_tbl <-
    tibble::tibble(
      cell_id = "cell",
      experiment_id = "example",
      condition = rep(conditions, each = n_bins),
      h_bin_centers = rep(bins, n_conditions),
      probability = 0,
      density = 1
    ) %>%
    dplyr::mutate(probability = dplyr::case_when(
      condition == "control" & h_bin_centers %in% c(1.5, 11.5) ~ 0.75,
      condition == "drug" & h_bin_centers %in% c(1.5, 11.5) ~ 0.5,
      .default = 0
    ))

  test_plot_01 <-
    plot_max_probability(
      simple_tbl,
      x_axis_condition = "control",
      y_axis_condition = "drug",
      metric = "probability",
      xmax = 1,
      ymax = 1
    )
  plot_01_scales <- ggplot2::layer_scales(test_plot_01)
  expect_equal(plot_01_scales[["x"]][["range"]][["range"]][1], 0.75)
  expect_equal(plot_01_scales[["y"]][["range"]][["range"]][1], 0.50)
  expect_error(
    plot_max_probability(
      simple_tbl,
      x_axis_condition = "control",
      y_axis_condition = "not_a_value",
      metric = "probability"
    )
  )

  # handles user-defined metric
  test_plot_02 <-
    plot_max_probability(
      simple_tbl,
      x_axis_condition = "control",
      y_axis_condition = "drug",
      metric = "density",
      xmax = 2,
      ymax = 2,
      x_label = "custom x",
      y_label = "custom y"
    )
  plot_02_scales <- ggplot2::layer_scales(test_plot_02)
  expect_equal(plot_02_scales[["x"]][["range"]][["range"]][1], 1)
  expect_equal(plot_02_scales[["y"]][["range"]][["range"]][1], 1)
  # expect error for invalid 'metric' arg
  expect_error(
    plot_max_probability(
      simple_tbl,
      x_axis_condition = "control",
      y_axis_condition = "drug",
      metric = "not_a_value"
    )
  )
  # test xmax and ymax
  expect_equal(plot_02_scales[["x"]][["limits"]][2], 2)
  expect_equal(plot_02_scales[["y"]][["limits"]][2], 2)
  # test labels
  expect_equal(test_plot_02[["labels"]][["x"]], "custom x")
  expect_equal(test_plot_02[["labels"]][["y"]], "custom y")
})

test_that("plot_max_probability handles aggregated data", {
  bins <- seq(0.5, 39.5, by = 1)
  n_bins <- length(bins)
  conditions <- c("control", "drug")
  n_conditions <- length(conditions)
  cell_ids <- c("cell01", "cell02")
  n_cells <- length(cell_ids)
  experiment_ids <- c("example01", "example02")
  n_experiments <- length(cell_ids)

  simple_tbl <-
    tibble::tibble(
      cell_id = rep(cell_ids, each = n_bins * n_conditions),
      experiment_id = rep(experiment_ids, each = n_bins * n_conditions),
      condition = rep(conditions, each = n_bins, times = n_cells),
      h_bin_centers = rep(bins, n_conditions * n_cells),
      probability = 0,
      density = 1
    ) %>%
    dplyr::mutate(probability = dplyr::case_when(
      cell_id == "cell01" & condition == "control" & h_bin_centers %in% c(1.5, 11.5) ~ 0.75,
      cell_id == "cell01" & condition == "drug" & h_bin_centers %in% c(1.5, 11.5) ~ 0.5,
      cell_id == "cell02" & condition == "control" & h_bin_centers %in% c(1.5, 11.5) ~ 0.25,
      cell_id == "cell02" & condition == "drug" & h_bin_centers %in% c(1.5, 11.5) ~ 0.75,
      .default = 0
    ))

  test_plot <-
    plot_max_probability(
      simple_tbl,
      x_axis_condition = "control",
      y_axis_condition = "drug",
      metric = "probability"
    )

  plot_scales <- ggplot2::layer_scales(test_plot)
  expect_equal(plot_scales[["x"]][["range"]][["range"]], c(0.25, 0.75))
  expect_equal(plot_scales[["y"]][["range"]][["range"]], c(0.50, 0.75))
})
