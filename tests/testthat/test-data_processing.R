# test-data_processing.R

# add_condition_tag ----
test_that("add_condition_tag accepts and returns data frames", {
  expected_columns <- c("sweep", "test_column_a", "test_column_b", "condition")
  expected_values <- rep(c("control", NA, "drug_1"), c(20, 28, 12))
  user_drug_args <-
    tibble::tribble(
      ~condition_start, ~condition_end, ~condition,
      1, 10, "control",
      25, 30, "drug_1"
    )

  simple_df <-
    data.frame(
      sweep = as.ordered(rep(1:30, each = 2)),
      test_column_a = 1,
      test_column_b = 2
    )

  df_output <- add_condition_tag(
    df = simple_df,
    parameters = user_drug_args
  )

  expect_type(df_output[["condition"]], "character")
  expect_equal(df_output[["condition"]], expected_values)
  expect_equal(colnames(df_output), expected_columns)
  expect_identical(class(simple_df), class(df_output))
})

test_that("add_condition_tag accepts and returns tibbles", {
  expected_columns <- c("sweep", "test_column_a", "test_column_b", "condition")
  expected_values <- rep(c("control", NA, "drug_1"), c(20, 28, 12))
  user_drug_args <-
    tibble::tribble(
      ~condition_start, ~condition_end, ~condition,
      1, 10, "control",
      25, 30, "drug_1"
    )

  simple_tbl <-
    tibble::tibble(
      sweep = as.ordered(rep(1:30, each = 2)),
      test_column_a = 1,
      test_column_b = 2
    )

  tbl_output <- add_condition_tag(
    df = simple_tbl,
    parameters = user_drug_args
  )

  expect_type(tbl_output[["condition"]], "character")
  expect_equal(tbl_output[["condition"]], expected_values)
  expect_equal(colnames(tbl_output), expected_columns)
  expect_identical(class(simple_tbl), class(tbl_output))
})

test_that("add_condition_tag accepts asymmetric inputs", {
  # asymmetric - the "recording" has more 'sweeps' than the drug condition
  asymmetric_expected_values <- rep(c("control", NA, "drug_1", NA), c(20, 28, 12, 20))
  user_drug_args <-
    tibble::tribble(
      ~condition_start, ~condition_end, ~condition,
      1, 10, "control",
      25, 30, "drug_1"
    )

  asymmetric_tbl <- tibble::tibble(sweep = as.ordered(rep(1:40, each = 2)))

  asymmetric_output <- add_condition_tag(
    df = asymmetric_tbl,
    parameters = user_drug_args
  )

  expect_equal(asymmetric_output[["condition"]], asymmetric_expected_values)
})

# add_event_index ----
test_that("add_event_index accepts and returns data frames", {
  expected_columns <- c("sweep", "time_ms", "amplitude", "stimulus", "extra_column", "event_index")
  expected_values <- c(1, 2, NA, 1, 1, 1, 1, 2, 1)

  simple_df <- data.frame(
    sweep = as.ordered(c(1, 1, 1, 1, 2, 2, 2, 2, 2)),
    time_ms = c(405, 408, NA, 445, seq(405, 445, 10)),
    amplitude = c(100, 100, NA, 100, 100, 100, 100, 100, 100),
    stimulus = ordered(c(1, 1, 2, "r", 1, 2, "o", "o", "r"), levels = c(1, 2, "o", "r")),
    extra_column = 1
  )

  df_output <- add_event_index(simple_df)

  expect_type(df_output[["event_index"]], "integer")
  expect_equal(df_output[["event_index"]], expected_values)
  expect_equal(colnames(df_output), expected_columns)
  expect_error(add_event_index(subset(df_output, select = -stimulus)))
  expect_identical(class(simple_df), class(df_output))
})

test_that("add_event_index accepts and returns tibbles", {
  expected_columns <- c("sweep", "time_ms", "amplitude", "stimulus", "extra_column", "event_index")
  expected_values <- c(1, 2, NA, 1, 1, 1, 1, 2, 1)

  simple_tbl <- tibble::tibble(
    sweep = as.ordered(c(1, 1, 1, 1, 2, 2, 2, 2, 2)),
    time_ms = c(405, 408, NA, 445, seq(405, 445, 10)),
    amplitude = c(100, 100, NA, 100, 100, 100, 100, 100, 100),
    stimulus = ordered(c(1, 1, 2, "r", 1, 2, "o", "o", "r"), levels = c(1, 2, "o", "r")),
    extra_column = 1
  )

  tbl_output <- add_event_index(simple_tbl)

  expect_type(tbl_output[["event_index"]], "integer")
  expect_equal(tbl_output[["event_index"]], expected_values)
  expect_equal(colnames(tbl_output), expected_columns)
  expect_error(add_event_index(subset(simple_tbl, select = -stimulus)))
  expect_identical(class(simple_tbl), class(tbl_output))
})

# add_event_jitter ----
test_that("add_event_jitter accepts and returns data frames", {
  expected_columns <- c("sweep", "time_ms", "rise_ms", "stimulus", "stim_times", "jitter")
  expected_values <- c(5, 8, NA, 5, 5, 5, NA, NA, 5)

  simple_df <- data.frame(
    sweep = as.ordered(rep(1:2, c(4, 5))),
    time_ms = c(405, 408, NA, 445, seq(405, 445, 10)),
    rise_ms = rep(0.5, 9),
    stimulus = ordered(c(1, 1, 2, "r", 1, 2, "o", "o", "r"), levels = c(1, 2, "o", "r")),
    stim_times = c(400, 400, 410, 440, 400, 410, NA, NA, 440)
  )

  df_output <- add_event_jitter(simple_df)

  expect_type(df_output[["jitter"]], "double")
  expect_equal(colnames(df_output), expected_columns)
  expect_equal(df_output[["jitter"]], expected_values)
  expect_equal(add_event_jitter(simple_df, to_rise = TRUE)[["jitter"]], (expected_values - 0.5))
  expect_identical(class(simple_df), class(df_output))
  # returns error when missing 'stim_times' column
  expect_error(add_event_jitter(simple_df %>% dplyr::select(-"stim_times")))
})

test_that("add_event_jitter accepts and returns tibbles", {
  expected_columns <- c("sweep", "time_ms", "rise_ms", "stimulus", "stim_times", "jitter")
  expected_values <- c(5, 8, NA, 5, 5, 5, NA, NA, 5)

  simple_tbl <- tibble::tibble(
    sweep = as.ordered(rep(1:2, c(4, 5))),
    time_ms = c(405, 408, NA, 445, seq(405, 445, 10)),
    rise_ms = rep(0.5, 9),
    stimulus = ordered(c(1, 1, 2, "r", 1, 2, "o", "o", "r"), levels = c(1, 2, "o", "r")),
    stim_times = c(400, 400, 410, 440, 400, 410, NA, NA, 440)
  )

  tbl_output <- add_event_jitter(simple_tbl)

  expect_type(tbl_output[["jitter"]], "double")
  expect_equal(colnames(tbl_output), expected_columns)
  expect_equal(tbl_output[["jitter"]], expected_values)
  expect_equal(add_event_jitter(simple_tbl, to_rise = TRUE)[["jitter"]], (expected_values - 0.5))
  expect_identical(class(simple_tbl), class(tbl_output))
})

# add_normalized_amplitude ----
test_that("add_normalized_amplitude accepts and returns data frames", {
  n <- 10 # sweeps
  n_stim <- 1 # stimuli per sweep
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)
  expected_columns <-
    c("sweep", "amplitude", "condition", "stimulus", "event_index", "extra_column", "amplitude_normalized")
  expected_values <- rep(c(1.0, 0.5), each = n)

  simple_df <-
    data.frame(
      sweep = c(1:((n * n_stim) * n_conditions)),
      amplitude = rep(c(100, 50), each = n),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      stimulus = rep(c(1:n_stim), n * n_conditions),
      event_index = rep(1, ((n * n_stim * n_conditions))),
      extra_column = 2
    )

  df_output <-
    add_normalized_amplitude(simple_df,
      normalize_condition = "control",
      normalize_stimulus = 1
    )

  expect_equal(colnames(df_output), expected_columns)
  expect_type(df_output[["amplitude_normalized"]], "double")
  expect_equal(df_output[["amplitude_normalized"]], expected_values)
  expect_identical(class(simple_df), class(df_output))
})

test_that("add_normalized_amplitude accepts and returns tibbles", {
  n <- 10 # sweeps
  n_stim <- 1 # stimuli per sweep
  conditions <- c("control", "drug_1")
  n_conditions <- length(conditions)
  expected_columns <-
    c("sweep", "amplitude", "condition", "stimulus", "event_index", "extra_column", "amplitude_normalized")
  expected_values <- rep(c(1.0, 0.5), each = n)

  simple_tbl <-
    tibble::tibble(
      sweep = c(1:((n * n_stim) * n_conditions)),
      amplitude = rep(c(100, 50), each = n),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      stimulus = rep(c(1:n_stim), n * n_conditions),
      event_index = rep(1, ((n * n_stim * n_conditions))),
      extra_column = 2
    )

  tbl_output <-
    add_normalized_amplitude(simple_tbl,
      normalize_condition = "control",
      normalize_stimulus = 1
    )

  expect_equal(colnames(tbl_output), expected_columns)
  expect_type(tbl_output[["amplitude_normalized"]], "double")
  expect_equal(tbl_output[["amplitude_normalized"]], expected_values)
  expect_identical(class(simple_tbl), class(tbl_output))
})

test_that("add_normalized_amplitude calculates amplitude based upon input args", {
  simple_tbl <-
    tibble::tibble(
      sweep = rep(1:4, each = 2),
      amplitude = c(100, 200, 100, 200, 50, 50, 50, 50),
      condition = rep(c("control", "drug_1"), each = 4),
      stimulus = rep(c(1:2), 4),
      event_index = 1
    )

  tbl_output_1 <-
    add_normalized_amplitude(simple_tbl,
      normalize_condition = "control",
      normalize_stimulus = 1
    )
  expect_equal(tbl_output_1[["amplitude_normalized"]], c(1, 2, 1, 2, 0.5, 0.5, 0.5, 0.5))

  tbl_output_2 <-
    add_normalized_amplitude(simple_tbl,
      normalize_condition = "control",
      normalize_stimulus = 2
    )
  expect_equal(tbl_output_2[["amplitude_normalized"]], c(0.5, 1, 0.5, 1, 0.25, 0.25, 0.25, 0.25))

  tbl_output_3 <-
    add_normalized_amplitude(simple_tbl,
      normalize_condition = "drug_1",
      normalize_stimulus = 1
    )
  expect_equal(tbl_output_3[["amplitude_normalized"]], c(2, 4, 2, 4, 1, 1, 1, 1))
})

test_that("add_normalized_amplitude does not override original amplitude or event_index values", {
  expected_amp <- c(100, NA, 100, NA, 50, 50, 50, 50)
  expected_amp_norm <- c(1, NA, 1, NA, 0.5, 0.5, 0.5, 0.5)
  expected_event_index <- c(1, NA, 1, NA, 1, 1, 1, 1)

  simple_tbl <-
    tibble::tibble(
      sweep = rep(1:4, each = 2),
      amplitude = expected_amp,
      condition = rep(c("control", "drug_1"), each = 4),
      stimulus = rep(c(1:2), 4),
      event_index = expected_event_index
    )

  tbl_output <-
    add_normalized_amplitude(simple_tbl,
      normalize_condition = "control",
      normalize_stimulus = 1
    )

  expect_equal(tbl_output[["amplitude"]], expected_amp)
  expect_equal(tbl_output[["event_index"]], expected_event_index)
  expect_equal(tbl_output[["amplitude_normalized"]], expected_amp_norm)
})

# add_ppr ----
test_that("add_ppr accepts and returns data frames", {
  expected_columns <- c("sweep", "condition", "amplitude", "stimulus", "event_index", "amplitude_normalized", "ppr")
  expected_values <- c(NA, 0.5, NA, 0.5, NA, 0.5, NA, 0.5, NA, NA, NA, NA, NA, 0.0, NA, 0.0)
  n <- 2

  simple_df <-
    data.frame(
      sweep = rep(1:(4 * n), each = 2),
      condition = rep(c(NA, "control", "drug_1", "drug_2"), each = (2 * n)),
      amplitude = c(rep(c(40, 20), n), rep(c(40, 20), n), rep(c(10, NA), n), rep(c(NA, 30), n)),
      stimulus = rep(1:2, 4 * n),
      event_index = 1,
      amplitude_normalized = 1
    )

  df_output <- add_ppr(df = simple_df)

  expect_equal(colnames(df_output), expected_columns)
  expect_equal(df_output[["ppr"]], expected_values)
  expect_identical(class(simple_df), class(df_output))
  # test user-defined arg
  expect_error(
    add_ppr(
      df = simple_df,
      column_ref = "not_a_column"
    )
  )
  df_output_ud <- add_ppr(
    df = simple_df,
    column_ref = "amplitude_normalized"
  )
  expect_equal(df_output_ud[["ppr"]], rep(c(NA, 1), 8))
  expect_equal(colnames(df_output_ud), expected_columns)
})

test_that("add_ppr accepts and returns tibbles", {
  expected_columns <- c("sweep", "condition", "amplitude", "stimulus", "event_index", "amplitude_normalized", "ppr")
  expected_values <- c(NA, 0.5, NA, 0.5, NA, 0.5, NA, 0.5, NA, NA, NA, NA, NA, 0.0, NA, 0.0)
  n <- 2

  simple_tbl <-
    tibble::tibble(
      sweep = rep(1:(4 * n), each = 2),
      condition = rep(c(NA, "control", "drug_1", "drug_2"), each = (2 * n)),
      amplitude = c(rep(c(40, 20), n), rep(c(40, 20), n), rep(c(10, NA), n), rep(c(NA, 30), n)),
      stimulus = rep(1:2, 4 * n),
      event_index = 1,
      amplitude_normalized = 1
    )

  tbl_output <- add_ppr(simple_tbl)

  expect_equal(colnames(tbl_output), expected_columns)
  expect_equal(tbl_output[["ppr"]], expected_values)
  expect_identical(class(simple_tbl), class(tbl_output))
  # test user-defined arg
  expect_error(
    add_ppr(
      df = simple_tbl,
      column_ref = "not_a_column"
    )
  )
  df_output_ud <- add_ppr(
    df = simple_tbl,
    column_ref = "amplitude_normalized"
  )
  expect_equal(df_output_ud[["ppr"]], rep(c(NA, 1), 8))
  expect_equal(colnames(df_output_ud), expected_columns)
})

test_that("add_ppr accounts for ordered 'stimulus' columns", {
  expected_columns <- c("sweep", "condition", "amplitude", "stimulus", "event_index", "amplitude_normalized", "ppr")
  expected_values <- c(NA, 0.5, NA, 0.5, NA, 0.5, NA, 0.5, NA, NA, NA, NA, NA, 0.0, NA, 0.0)
  n <- 2

  tbl_ordered <-
    tibble::tibble(
      sweep = rep(1:(4 * n), each = 2),
      condition = rep(c(NA, "control", "drug_1", "drug_2"), each = (2 * n)),
      amplitude = c(rep(c(40, 20), n), rep(c(40, 20), n), rep(c(10, NA), n), rep(c(NA, 30), n)),
      stimulus = ordered(rep(1:2, 4 * n)),
      event_index = 1,
      amplitude_normalized = 1
    )

  tbl_ordered_ppr <- add_ppr(tbl_ordered)

  expect_equal(colnames(tbl_ordered_ppr), expected_columns)
  expect_equal(tbl_ordered_ppr[["ppr"]], expected_values)
})

# add_stimulus_index ----
test_that("add_stimulus_index accepts and returns data frames without recovery stim", {
  simple_df <- data.frame(
    sweep = as.ordered(rep(1:2, c(1, 2))),
    time_ms = c(405, 405, 415)
  )

  df_output <- add_stimulus_index(
    df = simple_df,
    time_of_stim = c(400, 410),
    isi = 10,
    sweep_count = 2,
    recovery_stim = NULL,
    add_missing = TRUE
  )

  expect_equal(df_output[["stimulus"]], ordered(c(1, 2, 1, 2), levels = c(1, 2)))
  expect_equal(df_output[["stim_times"]], rep(c(400, 410), 2))
  expect_identical(class(simple_df), class(df_output))
})

test_that("add_stimulus_index accepts and returns tibbles without recovery stim", {
  simple_tbl <- tibble::tibble(
    sweep = as.ordered(rep(1:2, c(1, 2))),
    time_ms = c(405, 405, 415)
  )

  tbl_output <- add_stimulus_index(
    df = simple_tbl,
    time_of_stim = c(400, 410),
    isi = 10,
    sweep_count = 2,
    recovery_stim = NULL,
    add_missing = TRUE
  )

  expect_equal(tbl_output[["stimulus"]], ordered(c(1, 2, 1, 2), levels = c(1, 2)))
  expect_equal(tbl_output[["stim_times"]], rep(c(400, 410), 2))
  expect_identical(class(simple_tbl), class(tbl_output))
  expect_error(
    add_stimulus_index(
      df = simple_tbl,
      time_of_stim = NULL,
      isi = 10,
      sweep_count = 2,
      recovery_stim = NULL,
      add_missing = TRUE
    )
  )
})

test_that("add_stimulus_index accepts inputs with a recovery stimulus", {
  # replicates a paired stimulus w/ recovery experiment, to test if f() returns
  # a index with missing (1st sweep), "orphan" (2nd sweep), and recovery events
  expected_idx <- ordered(c(1, 2, "r", 1, 2, "o", "o", "r"),
    levels = c(1, 2, "o", "r")
  )
  expected_times <- c(400, 410, 440, 400, 410, NA, NA, 440)

  simple_tbl <- tibble::tibble(
    sweep = as.ordered(rep(1:2, c(2, 5))),
    time_ms = c(405, 445, seq(405, 445, 10))
  )

  tbl_output <- add_stimulus_index(
    df = simple_tbl,
    time_of_stim = c(400, 410),
    isi = 10,
    sweep_count = 2,
    recovery_stim = 440,
    add_missing = TRUE
  )

  expect_equal(tbl_output[["stim_times"]], expected_times)
  expect_s3_class(tbl_output[["stimulus"]], "factor")
  expect_equal(levels(tbl_output[["stimulus"]]), levels(expected_idx))
  expect_equal(tbl_output[["stimulus"]], expected_idx)
})

test_that("add_stimulus_index discovers missing sweeps and runs insert_rows_for_missing_sweeps()", {
  simple_tbl <- tibble::tibble(
    sweep = ordered(1:2, levels = 1:3),
    time_ms = c(405, 405)
  )
  expected_sweeps <- ordered(1:3, levels = 1:3)

  tbl_output <- add_stimulus_index(
    df = simple_tbl,
    time_of_stim = c(400),
    isi = 10,
    sweep_count = 3,
    recovery_stim = NULL,
    add_missing = FALSE
  )

  expect_equal(tbl_output[["sweep"]], expected_sweeps)

  # TODO how to test trys and stops
  # tbl_output <- add_stimulus_index(
  #   df = simple_tbl,
  #   time_of_stim = c(400),
  #   isi = 10,
  #   sweep_count = 4,
  #   recovery_stim = NULL,
  #   add_missing = FALSE
  # )
})

test_that("add_stimulus_index discovers missing time_ms and tries standardize_event_time()", {
  simple_tbl <- tibble::tibble(
    sweep = ordered(1:2, levels = 1:2)
  )

  # error due to missing time_ms and rec_time_ms
  expect_error(add_stimulus_index(
    df = simple_tbl,
    time_of_stim = c(400),
    isi = 10,
    sweep_count = 2,
    recovery_stim = NULL,
    add_missing = FALSE
  ))

  simple_tbl <- tibble::tibble(
    rec_time_ms = c(400, 4400),
    sweep = ordered(1:2, levels = 1:2)
  )

  # non-recoverable error due to missing matools_env$sweep_duration_sec
  expect_error(add_stimulus_index(
    df = simple_tbl,
    time_of_stim = c(400),
    isi = 10,
    sweep_count = 2,
    recovery_stim = NULL,
    add_missing = FALSE
  ))
})

# add_sweep_number_to_rows ----
test_that("add_sweep_number_to_rows accepts and returns data frames", {
  simple_df <- data.frame(rec_time_ms = c("400.50", "4,400.50"))

  df_output <- add_sweep_number_to_rows(
    df = simple_df,
    sweep_duration = 1,
    sweep_count = 5,
    time_ref = "rec_time_ms"
  )

  expect_equal(droplevels(df_output[["sweep"]]), as.ordered(c(1, 5)))
  expect_s3_class(df_output[["sweep"]], c("ordered", "factor"))
  expect_identical(class(simple_df), class(df_output))
})

test_that("add_sweep_number_to_rows accepts and returns tibbles", {
  simple_tbl <- tibble::tibble(rec_time_ms = seq(400.5, 4400.5, by = 1000))
  tbl_output <- add_sweep_number_to_rows(
    df = simple_tbl,
    sweep_duration = 1,
    sweep_count = 5,
    time_ref = "rec_time_ms"
  )

  expect_equal((tbl_output[["sweep"]]), as.ordered(1:5))
  expect_s3_class(tbl_output[["sweep"]], c("ordered", "factor"))
  expect_identical(class(simple_tbl), class(tbl_output))
  expect_error(
    add_sweep_number_to_rows(
      df = simple_tbl,
      sweep_duration = 1,
      sweep_count = 5,
      time_ref = "not_a_column"
    )
  )
})

# align_condition_sweeps ----
test_that("align_condition_sweeps accepts and returns data frames", {
  n <- 40 # sweeps
  stim <- c(280)
  n_stim <- length(stim)
  conditions <- c("control", "wash", "drug_1", "wash", "drug_2", "wash")
  n_conditions <- length(conditions)

  simple_df <-
    data.frame(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      stimulus = stim,
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      extra_column = 1
    )

  df_output <- align_condition_sweeps(
    df = simple_df,
    condition_names = c("control", "drug_1", "drug_2")
  )

  expect_identical(class(simple_df), class(df_output))
  expect_equal(colnames(df_output), c(colnames(simple_df), "trial"))
  expect_equal(unique(df_output$condition), unique(conditions))

  align_values <- rep(1:n, each = n_stim)
  na_values <- rep(NA, each = n_stim, times = n)
  expected_values <- rep(c(align_values, na_values), times = 3)

  expect_equal(df_output$trial, expected_values)
})

test_that("align_condition_sweeps accepts and returns tibbles", {
  n <- 40 # sweeps
  stim <- c(280)
  n_stim <- length(stim)
  conditions <- c("control", "wash", "drug_1", "wash", "drug_2", "wash")
  n_conditions <- length(conditions)

  simple_tbl <-
    tibble::tibble(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      stimulus = stim,
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      extra_column = 1
    )

  tbl_output <- align_condition_sweeps(
    df = simple_tbl,
    condition_names = c("control", "drug_1", "drug_2")
  )

  expect_identical(class(simple_tbl), class(tbl_output))
  expect_equal(colnames(tbl_output), c(colnames(simple_tbl), "trial"))
  expect_equal(unique(tbl_output$condition), unique(conditions))

  align_values <- rep(1:n, each = n_stim)
  na_values <- rep(NA, each = n_stim, times = n)
  expected_values <- rep(c(align_values, na_values), times = 3)

  expect_equal(tbl_output$trial, expected_values)
})

# sequential_condition_sweeps ----
test_that("sequential_condition_sweeps accepts and returns data frames", {
  n <- 40 # sweeps
  stim <- c(280)
  n_stim <- length(stim)
  conditions <- c("control", "wash", "drug_1")
  n_conditions <- length(conditions)

  target_conditions <- c("control", "drug_1")
  expected_output <-
    data.frame(
      condition = rep(conditions, each = n),
      trial = c(1:40, rep(NA, n), 41:80)
    )

  simple_df <-
    data.frame(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      stimulus = stim,
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      extra_column = 1
    )

  df_output <- sequential_condition_sweeps(
    df = simple_df,
    condition_names = target_conditions
  )

  expect_identical(class(simple_df), class(df_output))
  expect_equal(colnames(df_output), c(colnames(simple_df), "trial"))
  expect_equal(df_output$condition, expected_output$condition)
  expect_equal(df_output$trial, expected_output$trial)
})

test_that("sequential_condition_sweeps accepts and returns tibbles", {
  n <- 40 # sweeps
  stim <- c(280)
  n_stim <- length(stim)
  conditions <- c("control", "wash", "drug_1")
  n_conditions <- length(conditions)

  target_conditions <- c("control", "drug_1")
  expected_output <-
    data.frame(
      condition = rep(conditions, each = n),
      trial = c(1:40, rep(NA, n), 41:80)
    )

  simple_tbl <-
    tibble::tibble(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      stimulus = stim,
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      extra_column = 1
    )

  tbl_output <- sequential_condition_sweeps(
    df = simple_tbl,
    condition_names = target_conditions
  )

  expect_identical(class(simple_tbl), class(tbl_output))
  expect_equal(colnames(tbl_output), c(colnames(simple_tbl), "trial"))
  expect_equal(tbl_output$condition, expected_output$condition)
  expect_equal(tbl_output$trial, expected_output$trial)
})

test_that("sequential_condition_sweeps errors when sweeps are missing", {
  n <- 40 # sweeps
  stim <- c(280)
  n_stim <- length(stim)
  conditions <- c("control", "wash", "drug_1")
  n_conditions <- length(conditions)

  # drop several rows in each condition
  simple_df <-
    data.frame(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      stimulus = stim,
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      extra_column = 1
    ) %>%
    dplyr::filter(!row_number() %in% c(35:40, 81:89))

  expect_error(sequential_condition_sweeps(
    df = simple_df,
    condition_names = target_conditions
  ))
})

test_that("sequential_condition_sweeps handles conditions with the same name", {
  n <- 40 # sweeps
  stim <- c(280)
  n_stim <- length(stim)
  conditions <- c("control", "wash", "drug_1", "skip_me", "wash")
  n_conditions <- length(conditions)

  target_conditions <- c("control", "wash", "drug_1", "wash")
  expected_output <-
    data.frame(
      condition = rep(conditions, each = n),
      trial = c(1:120, rep(NA, n), 121:160)
    )

  simple_df <-
    data.frame(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      stimulus = stim,
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      extra_column = 1
    )

  df_output <- sequential_condition_sweeps(
    df = simple_df,
    condition_names = target_conditions
  )

  expect_equal(df_output$condition, expected_output$condition)
  expect_equal(df_output$trial, expected_output$trial)
})

# calculate_stimulus_times ----
test_that("calculate stimulus times from file parameters", {
  test_param_path <- system.file(
    "extdata/demo_data",
    "demo_data_project_parameters.csv",
    package = "matools",
    mustWork = TRUE
  )

  set_pkg_environment(force_new = TRUE)
  set_data_directory(getwd())
  import_experiment_parameters(test_param_path)
  set_experiment_parameters(matools_env$parameters, "cell1")

  calculate_stimulus_times(
    matools_env$stimulus_time_of_first,
    matools_env$stimulus_count,
    matools_env$stimulus_isi
  )

  expect_true(exists("time_of_stimuli", envir = matools_env))
  expect_type(matools_env$time_of_stimuli, "double")
  expect_equal(c(415.5, 425.5, 435.5), matools_env$time_of_stimuli)
})

# condition_sweep_lut ----
test_that("condition_sweep_lut accepts data frames", {
  n <- 40 # sweeps
  n_stim <- 3
  conditions <- c("control", "wash", "drug_1", "wash", "drug_2", "wash")
  n_conditions <- length(conditions)

  simple_df <-
    data.frame(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      time_ms = rep(c(280, 290, 300), times = (n * n_conditions)),
      condition = rep(conditions, rep((n * n_stim), n_conditions)),
      trial = rep(1:(n * n_conditions), each = n_stim)
    )

  conditions <- c("control", "drug_1", "drug_2")
  n_conditions <- length(conditions)

  df_output_1 <- condition_sweep_lut(df = simple_df, condition_names = conditions)
  df_output_2 <- condition_sweep_lut(df = simple_df, condition_names = conditions, column_ref = "trial")

  expect_error(condition_sweep_lut(df = simple_df, condition_names = conditions, column_ref = "fake"))
  expect_equal(df_output_1$condition, conditions)
  expect_equal(df_output_1$condition_start, c(1, 81, 161))
  expect_equal(df_output_1$condition_end, c(40, 120, 200))
  expect_equal(df_output_1$delta, c(39, 39, 39))
  expect_equal(df_output_1, df_output_2)
})

test_that("condition_sweep_lut accepts and returns tibbles", {
  n <- 40 # sweeps
  n_stim <- 3
  conditions <- c("control", "wash", "drug_1", "wash", "drug_2", "wash")
  n_conditions <- length(conditions)

  simple_tbl <-
    tibble::tibble(
      sweep = rep(1:(n * n_conditions), each = n_stim),
      time_ms = rep(c(280, 290, 300), times = (n * n_conditions)),
      condition = rep(conditions, rep((n * n_stim), n_conditions))
    )

  conditions <- c("control", "drug_1", "drug_2")
  n_conditions <- length(conditions)

  tbl_output <- condition_sweep_lut(df = simple_tbl, condition_names = conditions)

  expect_identical(class(simple_tbl), class(tbl_output))
  expect_equal(tbl_output$condition, conditions)
  expect_equal(tbl_output$condition_start, c(1, 81, 161))
  expect_equal(tbl_output$condition_end, c(40, 120, 200))
  expect_equal(tbl_output$delta, c(39, 39, 39))
})

# create_histogram_df ----
test_that("create_histogram_df accepts data frames", {
  n <- 10
  expected_columns <- c(
    "condition", "count_events", "count_sweeps", "h_counts", "h_density", "h_bin_centers", "probability",
    "h_count_norm_max", "h_prob_norm_max"
  )

  simple_df <- data.frame(
    time_ms = rep(c(417, 427), n),
    sweep = rep(c(1:n), each = 2),
    condition = rep("control", n * 2),
    stimulus = rep(c(1, 2), n),
    event_index = 1
  )

  df_output <-
    create_histogram_df(
      df = simple_df,
      time_of_stim = c(415.5, 425.5),
      condition_names = c("control", "drug1"),
      bin_size = 1,
      window_size = 20,
      first_bin_on_stim = FALSE,
      one_event_per_bin = FALSE
    )

  df_output_expanded <- df_output %>%
    dplyr::select(condition, h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max) %>%
    tidyr::unnest(c(h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max))

  expect_equal(colnames(df_output), expected_columns)
  expect_equal(sum(df_output_expanded[["h_density"]]), 1)
  expect_equal(sum(df_output_expanded[["h_counts"]]), 20)
  expect_equal(df_output_expanded[["h_bin_centers"]], seq(from = 0.5, to = 19.5, by = 1))
  expect_equal(df_output_expanded[["probability"]], c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0))

  # Drop half of the rows for stimulus 2
  drop50 <- simple_df[-seq(12, 20, by = 2), ]
  drop50_output <-
    create_histogram_df(
      df = drop50,
      time_of_stim = c(415.5, 425.5),
      condition_names = c("control", "drug1"),
      bin_size = 1,
      window_size = 20,
      first_bin_on_stim = FALSE,
      one_event_per_bin = FALSE
    )

  drop50_output_expanded <- drop50_output %>%
    dplyr::select(condition, h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max) %>%
    tidyr::unnest(c(h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max))

  expect_equal(sum(drop50_output_expanded[["h_density"]]), 1)
  expect_equal(drop50_output_expanded[["probability"]], c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0))
})

test_that("create_histogram_df accepts tibbles", {
  n <- 10
  expected_columns <- c(
    "condition", "count_events", "count_sweeps", "h_counts", "h_density", "h_bin_centers", "probability",
    "h_count_norm_max", "h_prob_norm_max"
  )

  simple_tbl <- tibble::tibble(
    time_ms = rep(c(417, 427), n),
    sweep = rep(c(1:n), each = 2),
    condition = rep("control", n * 2),
    stimulus = rep(c(1, 2), n),
    event_index = 1
  )

  tbl_output <-
    create_histogram_df(
      df = simple_tbl,
      time_of_stim = c(415.5, 425.5),
      condition_names = c("control", "drug1"),
      bin_size = 1,
      window_size = 20,
      first_bin_on_stim = FALSE,
      one_event_per_bin = FALSE
    )

  tbl_output_expanded <- tbl_output %>%
    dplyr::select(condition, h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max) %>%
    tidyr::unnest(c(h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max))

  expect_equal(colnames(tbl_output), expected_columns)
  expect_equal(sum(tbl_output_expanded[["h_density"]]), 1)
  expect_equal(sum(tbl_output_expanded[["h_counts"]]), 20)
  expect_equal(tbl_output_expanded[["h_bin_centers"]], seq(from = 0.5, to = 19.5, by = 1))
  expect_equal(tbl_output_expanded[["probability"]], c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0))
})

test_that("create_histogram_df function args work as expected", {
  n <- 10
  simple_df <-
    data.frame(
      time_ms = rep(c(417, 419), n),
      sweep = rep(c(1:n), each = 2),
      condition = rep("control", n * 2),
      stimulus = 1,
      event_index = rep(c(1, 2), n)
    )

  # only count a single event per bin
  df_output <-
    create_histogram_df(
      df = simple_df,
      time_of_stim = c(415.5, 425.5),
      condition_names = c("control", "drug1"),
      bin_size = 10,
      window_size = 10,
      first_bin_on_stim = FALSE,
      one_event_per_bin = TRUE
    )

  df_output_expanded <- df_output %>%
    dplyr::select(condition, h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max) %>%
    tidyr::unnest(c(h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max))

  expect_equal(sum(df_output[["count_events"]]), 20)
  expect_equal(sum(df_output[["count_sweeps"]]), 10)
  expect_equal(sum(df_output_expanded[["h_counts"]]), 10)
  expect_equal(sum(df_output_expanded[["h_density"]]), 0.1)
  expect_equal(sum(df_output_expanded[["probability"]]), 1)

  # bin_size > 2 and will create bins centered on the stimulus
  expect_warning(create_histogram_df(
    df = simple_df,
    time_of_stim = c(415.5, 425.5),
    condition_names = c("control", "drug1"),
    bin_size = 10,
    window_size = 20,
    first_bin_on_stim = TRUE
  ))
})

# insert_rows_for_missing_sweeps ----
test_that("insert_rows_for_missing_sweeps accepts and returns data frames", {
  simple_df <- data.frame(
    a_value = c(400, 4400),
    sweep = ordered(c(1, 5), levels = c(1:5))
  )

  df_output <- insert_rows_for_missing_sweeps(
    df = simple_df,
    sweep_count = 5
  )

  expect_identical(class(simple_df), class(df_output))
  expect_equal((df_output[["a_value"]]), c(400, NA, NA, NA, 4400))
  expect_equal((df_output[["sweep"]]), as.ordered(1:5))
  expect_s3_class(df_output[["sweep"]], c("ordered", "factor"))
})

test_that("insert_rows_for_missing_sweeps handles duplicate sweeps", {
  simple_df <- data.frame(
    some_value = 400,
    sweep = rep(c(1, 5), each = 2)
  )

  expected_output <- c(1, 1, 2, 3, 4, 5, 5)

  df_output <- insert_rows_for_missing_sweeps(
    df = simple_df,
    sweep_count = 5
  )

  expect_equal(df_output[["sweep"]], expected_output)
})

test_that("insert_rows_for_missing_sweeps accepts and returns tibbles", {
  simple_tbl <- tibble::tibble(
    a_value = c(400, 4400),
    sweep = ordered(c(1, 5), levels = c(1:5))
  )

  tbl_output <- insert_rows_for_missing_sweeps(
    df = simple_tbl,
    sweep_count = 5
  )

  expect_identical(class(simple_tbl), class(tbl_output))
  expect_equal((tbl_output[["a_value"]]), c(400, NA, NA, NA, 4400))
  expect_equal((tbl_output[["sweep"]]), as.ordered(1:5))
  expect_s3_class(tbl_output[["sweep"]], c("ordered", "factor"))
  expect_error(insert_rows_for_missing_sweeps(tibble(a_value = c(400, 4400)), 5))
})

# standardize_event_time ----
test_that("standardize_event_time accepts and returns data frames", {
  simple_df <- data.frame(
    rec_time_ms = c(400, 4400),
    sweep = ordered(c(1, 5), levels = c(1:5))
  )

  df_output <- standardize_event_time(
    df = simple_df,
    sweep_duration = 1,
    time_ref = "rec_time_ms"
  )

  expect_identical(class(simple_df), class(df_output))
  expect_type(df_output[["time_ms"]], "double")
  expect_equal((df_output[["time_ms"]]), c(400, 400))
})

test_that("standardize_event_time accepts and returns tibbles", {
  simple_tbl <- tibble::tibble(
    rec_time_ms = c(400, 4400),
    sweep = ordered(c(1, 5), levels = c(1:5))
  )

  tbl_output <- standardize_event_time(
    df = simple_tbl,
    sweep_duration = 1,
    time_ref = "rec_time_ms"
  )

  expect_identical(class(simple_tbl), class(tbl_output))
  expect_type(tbl_output[["time_ms"]], "double")
  expect_equal((tbl_output[["time_ms"]]), c(400, 400))
  # Relative time should never exceed the sweep duration in milliseconds
  expect_lte(max(tbl_output[["time_ms"]], na.rm = TRUE), 1000)
  expect_error(standardize_event_time(simple_tbl,
    sweep_duration = 1,
    time_ref = "not_a_column"
  ))

  no_sweep_tbl <- tibble::tibble(
    rec_time_ms = c(400, 4400)
  )
  # TODO this test does not throw an error when the try call is used
  # expect_error(standardize_event_time(no_sweep_tbl,
  #   sweep_duration = 1,
  #   time_ref = "rec_time_ms"
  # ))
})
