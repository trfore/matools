library(matools)

profvis::profvis({
  test_file_path <- system.file("extdata/demo_data",
    "cell1.ASC",
    package = "matools",
    mustWork = TRUE
  )
  test_param_path <- system.file("extdata/demo_data",
    "demo_data_project_parameters.csv",
    package = "matools",
    mustWork = TRUE
  )

  set_pkg_environment(force_new = TRUE)
  set_data_directory(getwd())

  test_df <- asc_to_df(test_file_path, FALSE)
  import_experiment_parameters(test_param_path)
  set_experiment_parameters(matools.env$parameters, "cell1")

  calculate_stimulus_times(
    matools.env$stimulus_time_of_first,
    matools.env$stimulus_count,
    matools.env$stimulus_isi
  )

  test_df <- add_sweep_number_to_rows(
    test_df,
    filename,
    matools.env$sweep_duration_sec,
    matools.env$sweep_total,
    "rec_time_ms"
  )

  test_df <- insert_rows_for_missing_sweeps(
    test_df,
    matools.env$sweep_total
  )

  test_df <- standardize_event_time(
    test_df,
    matools.env$sweep_duration_sec,
    "rec_time_ms"
  )

  test_df <- add_stimulus_value(
    test_df,
    matools.env$time_of_stimuli,
    matools.env$stimulus_isi,
    matools.env$sweep_total,
    matools.env$stimulus_time_of_recovery
  )

  test_df <- add_event_index(test_df)

  test_df <- add_condition_tag(test_df,
    filename = filename,
    matools.env$condition_names,
    parameters_manual = FALSE,
    parameters = matools.env$parameters
  )

  test_df <- calculate_event_jitter(test_df,
    matools.env$time_of_stimuli,
    from_rise = FALSE
  )
})
