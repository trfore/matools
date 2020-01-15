# test-utils.R

# test setup ----
test_file_path <-
  system.file("extdata/demo_data",
    "cell1.ASC",
    package = "matools",
    mustWork = TRUE
  )

test_data_dir <-
  system.file("extdata/demo_data", package = "matools", mustWork = TRUE)

test_param_path <-
  system.file(
    "extdata/demo_data",
    "demo_data_project_parameters.csv",
    package = "matools",
    mustWork = TRUE
  )

asc_file_column_names <-
  c(
    "ma_row", "rec_time_ms", "amplitude", "rise_ms", "decay_ms", "area",
    "baseline", "noise", "group", "channel", "x10_90_rise", "halfwidth",
    "rise50", "peak_direction", "burst", "burste", "x10_90slope",
    "rel_time"
  )

# set_pkg_environment ----
test_that("Can create matools_env directory variable", {
  set_pkg_environment(force_new = TRUE)
  set_data_directory(getwd())
  expect_identical(matools_env$directory_data, getwd())
})

# asc_to_df ----
test_that("acs_to_df converts ASC file to df", {
  expected_nrows <- 275
  expected_types <- c(
    "integer", "character", "double", "double", "double", "character", "double", "double", "integer", "integer",
    "double", "double", "double", "integer", "integer", "integer", "character", "double"
  )

  test_df <- asc_to_df(test_file_path, FALSE)
  expect_true(is.data.frame(test_df))
  expect_equal(colnames(test_df), asc_file_column_names)
  expect_true(all(test_df$ma_row == cummax(test_df$ma_row)))
  expect_equal(unname(sapply(test_df, typeof)), expected_types)
  expect_equal(expected_nrows, nrow(test_df))
})

test_that("asc_to_tibble handles merging iei files", {
  expected_nrows <- 275
  test_df <- asc_to_df(test_file_path, TRUE)

  expect_equal(colnames(test_df), c(asc_file_column_names, "iei"))
  expect_equal(expected_nrows, nrow(test_df))
})

# asc_to_tibble ----
test_that("asc_to_tibble converts ASC file to tibble", {
  expected_nrows <- 275
  expected_types <- c(
    "integer", "double", "double", "double", "double", "double", "double", "double", "integer", "integer",
    "double", "double", "double", "integer", "integer", "integer", "double", "double"
  )


  test_tbl <- asc_to_tibble(test_file_path, FALSE)

  expect_s3_class(test_tbl, "tbl_df")
  expect_equal(colnames(test_tbl), asc_file_column_names)
  expect_true(all(test_tbl$ma_row == cummax(test_tbl$ma_row)))
  expect_equal(unname(sapply(test_tbl, typeof)), expected_types)
  expect_equal(expected_nrows, nrow(test_tbl))
})

test_that("asc_to_tibble handles merging iei files", {
  expected_nrows <- 275
  test_tbl <- asc_to_tibble(file_path = test_file_path, merge_iei = TRUE)

  expect_equal(colnames(test_tbl), c(asc_file_column_names, "iei"))
  expect_equal(expected_nrows, nrow(test_tbl))
})

# asc_to_df / asc_to_tibble ----
test_that("df and tibble imports are equivalent dimensions", {
  test_df <- asc_to_df(test_file_path, FALSE)
  test_tbl <- asc_to_tibble(test_file_path, FALSE)

  expect_equal(dim(test_df), dim(test_tbl))
})

# import_experiment_parameters ----
test_that("import_experiment_parameters accepts a file PATH input", {
  expected_rownames <- c("cell1", "cell2", "cell3")

  set_pkg_environment(force_new = TRUE)
  import_experiment_parameters(file = test_param_path)
  expect_type(matools_env$parameters, "list")
  expect_equal(expected_rownames, rownames(matools_env$parameters))
})

test_that("import_experiment_parameters accepts a data.frame/tibble input", {
  df_input <- utils::read.csv(test_param_path)
  expected_rownames <- c("cell1", "cell2", "cell3")

  set_pkg_environment(force_new = TRUE)
  import_experiment_parameters(file = df_input)
  expect_type(matools_env$parameters, "list")
  expect_equal(expected_rownames, rownames(matools_env$parameters))
})

# set_experiment_parameters ----
test_that("Can set matools_env parameter vars for a single experiment", {
  set_pkg_environment(force_new = TRUE)
  import_experiment_parameters(test_param_path)
  # Pull package demo data for "cell1"
  set_experiment_parameters(matools_env$parameters, "cell1")

  expected_parameters <-
    c("experiment_id", "sweep_total", "sweep_duration_sec")
  expect_true(all(expected_parameters %in% names(matools_env)))
})

# import_check_missing_info ----
test_that("import_check_missing_info prompts on success", {
  test_parameters <- data.frame(x = 1:2, row.names = c("cell1", "cell2"))
  test_files_in_folder <- c("cell1", "cell2")

  expect_message(import_check_missing_info(test_files_in_folder, test_parameters, skip_prompt = TRUE), "created")
  expect_equal(matools_env[["files_to_process"]], c("cell1", "cell2"))
})

test_that("import_check_missing_info errors when files are missing", {
  test_parameters <- data.frame(x = 1:2, row.names = c("cell1", "cell2"))
  test_files_in_folder <- c("cell1")

  expect_warning(
    import_check_missing_info(test_files_in_folder, test_parameters, skip_prompt = TRUE),
    "missing:cell2"
  )
  expect_equal(matools_env[["files_to_process"]], "cell1")
})
