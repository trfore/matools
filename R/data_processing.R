# data_processing.R

utils::globalVariables(c("matools_env", ":="))

#### Data Wrangling Functions ####

#' @title Add Condition Tag To Data Frame
#'
#' @description Adds a condition tag to the data frame allowing analysis based upon the tag
#'
#' @param df Data Frame or Tibble, with the following columns:
#' * sweep
#' @param parameters Data Frame or Tibble with experiment parameters, expected columns are as follows:
#' * condition_start - Integer
#' * condition_end - Integer
#' * condition - Character
#'
#' @details Uncategorized events will default to \code{NA}.
#'
#' @return Data Frame or Tibble with the additional column: \code{condition}.
#'
#' @seealso
#' * \link{add_sweep_number_to_rows}
#' * \link{insert_rows_for_missing_sweeps}
#'
#' @examples
#' simple_tbl <- tibble::tibble(sweep = as.ordered(rep(1:5, each = 2)))
#' user_drug_args <-
#'   tibble::tribble(
#'     ~condition_start, ~condition_end, ~condition,
#'     1, 2, "control",
#'     4, 5, "drug_1"
#'   )
#'
#' add_condition_tag(
#'   df = simple_tbl,
#'   parameters = user_drug_args
#' )
#'
#' @importFrom dplyr left_join mutate rowwise select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @importFrom utils globalVariables
#' @export
#' @md
add_condition_tag <- function(df,
                              parameters) {
  max_sweep <- max(parameters$condition_end)
  parameters <-
    parameters %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sweep = list(ordered(
      c(.data$condition_start:.data$condition_end),
      levels = c(1:max_sweep)
    )))

  drug_condition_lut <-
    parameters %>%
    dplyr::select("condition", "sweep") %>%
    tidyr::unnest(cols = c(sweep))

  all_levels <- union(levels(df$sweep), levels(drug_condition_lut$sweep))

  df <- dplyr::left_join(
    dplyr::mutate(df, sweep = factor(sweep, levels = all_levels)),
    dplyr::mutate(drug_condition_lut, sweep = factor(sweep, levels = all_levels)),
    by = "sweep"
  )
  return(df)
}

#' @title Add Event Index To Data Frame
#'
#' @description Several events may occur in response to a experimental stimulus, this function will add a sub-index
#'   allowing for segregation by the i-th event to a n-th stimulus.
#'
#' @param df Data Frame or Tibble with the following columns:
#' * amplitude
#' * sweep
#' * stimulus
#'
#' @details The first event for a given stimulus will always start with an index value of 1. If multiple events occur,
#' and iterative index is created. If no events occur, the index takes a \code{NA} value.
#'
#' @return Data Frame or Tibble with event index numbers in column the \code{event_index}.
#'
#' @seealso
#' * \link{add_stimulus_index}
#'
#' @examples
#' simple_tbl <- tibble::tibble(
#'   sweep = as.ordered(c(1, 1, 1, 1, 2, 2, 2, 2, 2)),
#'   time_ms = c(405, 408, NA, 445, seq(405, 445, 10)),
#'   amplitude = c(100, 100, NA, 100, 100, 100, 100, 100, 100),
#'   stimulus = ordered(c(1, 1, 2, "r", 1, 2, "o", "o", "r"), levels = c(1, 2, "o", "r"))
#' )
#' add_event_index(simple_tbl)
#'
#' @importFrom dplyr select case_when group_by mutate row_number ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @md
add_event_index <- function(df) {
  if (!"stimulus" %in% colnames(df)) {
    df <- try(add_stimulus_index(
      df = df,
      time_of_stim = matools_env$time_of_stimuli,
      isi = matools_env$stimulus_isi,
      sweep_count = matools_env$sweep_total,
      recovery_stim = matools_env$stimulus_time_of_recovery
    ), silent = TRUE)

    if ("try-error" %in% class(df)) {
      stop(
        "df is missing 'stimulus' column, unsuccessfully tried add_stimulus_index() using default values. ",
        "Manually run add_stimulus_index() before this function."
      )
    }
  }

  tmp <-
    df %>%
    dplyr::select("sweep", "stimulus", "amplitude") %>%
    dplyr::group_by(.data$sweep, .data$stimulus) %>%
    dplyr::mutate(event_index = dplyr::case_when(!is.na(.data$amplitude) ~ dplyr::row_number(.data$stimulus))) %>%
    dplyr::ungroup()

  df["event_index"] <- tmp["event_index"]
  return(df)
}

#' @title Calculate Event Jitter From Stimulus
#'
#' @description The jitter of an synaptic event (i.e. action potential, post-synaptic event) is calculated using
#' difference between the stimulus time and the peak amplitude time (or start of the 20/80 rise).
#'
#' @param df Data Frame or Tibble, with the following columns:
#' * time_ms
#' * stim_times
#' @param to_rise Logical, default is \code{FALSE} and jitter is calculated between the stimulus time to the peak
#'   amplitude, i.e. \code{time_ms}. If \code{TRUE} the rise time, \code{rise_ms}, will be subtracted from the time
#'   of peak amplitude, thus calculating jitter to the start the 20/80 rise time.
#'
#' @return Data Frame or Tibble with numeric column, \code{jitter}, in milliseconds.
#'
#' @examples
#' simple_df <- data.frame(
#'   sweep = as.ordered(rep(1:2, c(4, 5))),
#'   time_ms = c(405, 408, NA, 445, seq(405, 445, 10)),
#'   rise_ms = rep(0.5, 9),
#'   stimulus = ordered(c(1, 1, 2, "r", 1, 2, "o", "o", "r"), levels = c(1, 2, "o", "r")),
#'   stim_times = c(400, 400, 410, 440, 400, 410, NA, NA, 440)
#' )
#'
#' add_event_jitter(simple_df)
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @md
add_event_jitter <- function(df,
                             to_rise = FALSE) {
  if (!"stim_times" %in% colnames(df)) {
    df <- try(add_stimulus_index(
      df = df,
      time_of_stim = matools_env$time_of_stimuli,
      isi = matools_env$stimulus_isi,
      sweep_count = matools_env$sweep_total,
      recovery_stim = matools_env$stimulus_time_of_recovery
    ), silent = TRUE)

    if ("try-error" %in% class(df)) {
      stop(
        "df is missing 'stim_times' column, unsuccessfully tried add_stimulus_index() using default values. ",
        "Manually run add_stimulus_index() before this function."
      )
    }
  }

  if (to_rise) {
    df <- df %>% dplyr::mutate(jitter = (.data$time_ms - .data$rise_ms) - .data$stim_times)
  } else {
    df <- df %>% dplyr::mutate(jitter = .data$time_ms - .data$stim_times)
  }

  return(df)
}

#' @title Calculate The Normalized Amplitude
#'
#' @description This will calculate a normalized amplitude based upon a user defined baseline - set using the args:
#'   \code{normalize_condition} and \code{normalize_stimulus}. The baseline is calculated from the mean amplitude of
#'   first "event" after the user-defined \code{normalize_stimulus} value.
#'
#' @param df Data Frame or Tibble, with the following columns:
#' * sweep
#' * amplitude
#' * condition
#' * stimulus
#' * event_index
#'
#' @param normalize_condition Character, the condition to normalize all amplitudes against, defaults to 'control'.
#' @param normalize_stimulus Numeric, the stimulus to normalize against, defaults to '1'.
#'
#' @return Data Frame or Tibble with the additional column: \code{amplitude_normalized}.
#'
#' @examples
#' simple_df <-
#'   data.frame(
#'     sweep = c(1:20),
#'     amplitude = rep(c(100, 50), each = 10),
#'     condition = rep(c("control", "drug_1"), c(10, 10)),
#'     stimulus = 1,
#'     event_index = 1
#'   )
#'
#' add_normalized_amplitude(simple_df,
#'   normalize_condition = "control",
#'   normalize_stimulus = 1
#' )
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter group_by summarise pull mutate left_join join_by
#' @importFrom rlang .data
#' @export
#' @md
add_normalized_amplitude <- function(df,
                                     normalize_condition = "control",
                                     normalize_stimulus = 1) {
  tmp <- df %>%
    dplyr::select("sweep", "amplitude", "condition", "stimulus", "event_index") %>%
    dplyr::mutate(
      amplitude = ifelse(is.na(.data$amplitude), 0, .data$amplitude),
      event_index = ifelse(is.na(.data$event_index), 1, .data$event_index)
    )

  condition_mean <-
    tmp %>%
    dplyr::filter(.data$condition == normalize_condition &
      .data$stimulus == normalize_stimulus &
      .data$event_index == 1) %>%
    dplyr::group_by(.data$condition, .data$stimulus) %>%
    dplyr::summarise(mean_amp = mean(.data$amplitude)) %>%
    dplyr::pull()

  tmp <- tmp %>% dplyr::mutate(amplitude_normalized = .data$amplitude / condition_mean)

  tmp <-
    dplyr::left_join(df, tmp,
      by = dplyr::join_by("sweep", "amplitude", "condition", "stimulus", "event_index")
    )
  return(tmp)
}

#' @title Add Pair-Pulse Ratio
#'
#' @description
#' This will add the pair-pulse ratio value to rows where the stimulus == 2 and event_index == 1. For paired stimuli
#' with no event on the 2nd stimulus, \code{0} is returned; for stimuli with no event on the 1st stimulus, \code{NA} is
#' returned.
#'
#' @param df Data Frame or Tibble with the following columns:
#' * sweep
#' * condition
#' * stimulus
#' * event_index
#' * amplitude OR another reference column
#' @param column_ref Character, column to use for ppr calculation. Defaults to \code{"amplitude"}.
#'
#' @return Data Frame or Tibble with the additional column: \code{condition}.
#'
#' @examples
#' n <- 2
#' simple_df <-
#'   data.frame(
#'     sweep = rep(1:(4 * n), each = 2),
#'     condition = rep(c(NA, "control", "drug_1", "drug_2"), each = (2 * n)),
#'     amplitude = c(rep(c(40, 20), n), rep(c(40, 20), n), rep(c(10, NA), n), rep(c(NA, 30), n)),
#'     stimulus = rep(1:2, 4 * n),
#'     event_index = 1
#'   )
#'
#' add_ppr(df = simple_df, column_ref = "amplitude")
#'
#' @importFrom dplyr select mutate arrange filter mutate_at vars left_join join_by
#' @importFrom rlang .data
#' @importFrom tidyr spread gather
#' @export
#' @md
add_ppr <- function(df,
                    column_ref = "amplitude") {
  required_cols <- c("sweep", "condition", "stimulus", "event_index", column_ref)
  if (!all(required_cols %in% colnames(df))) {
    stop("df missing columns, see 'add_ppr' help")
  }

  tmp <-
    df %>%
    dplyr::select("sweep", "condition", "stimulus", "event_index", !!column_ref) %>%
    dplyr::mutate(
      !!column_ref := ifelse(is.na(.data[[!!column_ref]]), 0, .data[[!!column_ref]]),
      event_index = ifelse(is.na(.data$event_index), 1, .data$event_index)
    ) %>%
    tidyr::spread("stimulus", !!column_ref) %>%
    dplyr::mutate(ppr = .data[["2"]] / .data[["1"]]) %>%
    dplyr::mutate(
      ppr = ifelse(is.nan(.data$ppr), 0, .data$ppr),
      ppr = ifelse(is.infinite(.data$ppr), 0, .data$ppr)
    ) %>%
    tidyr::gather(key = "stimulus", value = !!column_ref, .data[["1"]], .data[["2"]], convert = TRUE) %>%
    dplyr::arrange(.data$sweep)

  stim_class <- class(df[["stimulus"]])[1]
  if (stim_class == "ordered") {
    tmp <- tmp %>% dplyr::mutate_at(dplyr::vars(.data$stimulus), list(stim_class))
  }

  tmp <-
    dplyr::left_join(df, (tmp %>% dplyr::filter(.data$stimulus == 2)),
      by = dplyr::join_by("sweep", "stimulus", "event_index", "condition", !!column_ref)
    )

  return(tmp)
}

#' @title Add Stimulus Index To Data Frame
#'
#' @description Categorizes events by relationship to a given stimulus, creating a stimulus index.
#'
#' @param df Data Frame or Tibble with the following columns:
#' * sweep
#' * time_ms
#' @param time_of_stim List, with event times in milliseconds.
#' @param isi Numeric, interstimulus interval in milliseconds.
#' @param sweep_count Numeric, total number of experiment sweeps/trials.
#' @param recovery_stim Numeric, default is \code{NULL}. Time of the recovery stimulus in milliseconds, useful in
#'   paired-pulse ratio experiments when a recovery stimulus is given to measure the rate of recovery.
#' @param add_missing Logical, default is \code{TRUE}. Rows for missing events will be inserted with corresponding
#'   sweep and stimulus number.
#'
#' @details Determines which events fall within a stimulus window, defined by the interstimulus interval (isi), and
#' adds rows with \code{NA} for missing events. The \code{stimulus} column can take on several values:
#' * Character int, e.g. \code{1}, representing an event occurring within a stimulus window.
#' * \code{NA}, representing events that do not correspond to a stimulus.
#' * Character, \code{r}, representing the recovery stimulus. If \code{recovery_stim} is not \code{NULL}.
#'
#' @return Data Frame or Tibble with the following columns:
#' * \code{stimulus}, Factor column with the stimulus index.
#' * \code{stim_time}, Numeric column with stimulus times.
#'
#' @note Run \link{insert_rows_for_missing_sweeps} before this function, as it will only insert NA for missing events on
#' sweep numbers already present the data frame. A check occurs in this function and notifies the user.
#'
#' @seealso
#' * \link{insert_rows_for_missing_sweeps}
#' * \link{add_event_index}
#'
#' @examples
#' simple_df <- data.frame(
#'   sweep = as.ordered(rep(1:2, c(1, 2))),
#'   time_ms = c(405, 405, 415)
#' )
#'
#' # add rows for missing events
#' add_stimulus_index(
#'   df = simple_df,
#'   time_of_stim = c(400, 410),
#'   isi = 10,
#'   sweep_count = 2,
#'   recovery_stim = NULL,
#'   add_missing = TRUE
#' )
#'
#' @importFrom dplyr last bind_rows left_join
#' @importFrom magrittr %>%
#' @importFrom plyr ddply .
#' @importFrom tibble tibble
#' @export
#' @md
add_stimulus_index <- function(df,
                               time_of_stim,
                               isi,
                               sweep_count,
                               recovery_stim = NULL,
                               add_missing = TRUE) {
  if (is.null(time_of_stim)) {
    stop(
      "time_of_stim arg is null"
    )
  }

  if (length(unique(df$sweep)) != sweep_count) {
    df <- try(insert_rows_for_missing_sweeps(df, sweep_count), silent = TRUE)

    if ("try-error" %in% class(df)) {
      stop(
        "add_stimulus_index() df is missing sweep/trial(s), ",
        "unsuccessfully tried insert_rows_for_missing_sweeps() using default values. ",
        "Manually run insert_rows_for_missing_sweeps() before this function."
      )
    } else if (length(unique(df$sweep)) != sweep_count) {
      stop(
        "add_stimulus_index() df was missing sweep/trial(s), ",
        "tried insert_rows_for_missing_sweeps() using default values, ",
        "but sweep column does not match expected sweep_count"
      )
    }
  }

  if (!"time_ms" %in% names(df)) {
    df <- try(standardize_event_time(
      df = df,
      sweep_duration = matools_env$sweep_duration_sec,
      time_ref = "rec_time_ms"
    ), silent = TRUE)

    if ("try-error" %in% class(df)) {
      stop(
        "df is missing time_ms column, unsuccessfully tried standardize_event_time() using default values ",
        "and matools_env$sweep_duration_sec. Manually run standardize_event_time() before this function."
      )
    }
  }

  if (!is.null(recovery_stim)) {
    # This adds a orphan bin between the last stimulus window and the recovery
    # window. Events within the orphan bin are changed to \code{"o"} Events in
    # the recovery are converted to the value \code{"r"}.
    bin_breaks <-
      append(
        time_of_stim,
        c((dplyr::last(time_of_stim) + isi), recovery_stim, (recovery_stim + isi))
      )

    bin_num_code <-
      c(seq_along(time_of_stim), length(time_of_stim) + 2)
    bin_labels <- c(seq_along(time_of_stim), "o", "r")
    stimulus_times <- c(time_of_stim, NA, recovery_stim)
  } else {
    bin_breaks <-
      append(time_of_stim, (dplyr::last(time_of_stim) + isi))
    bin_num_code <- c(seq_along(time_of_stim))
    bin_labels <- bin_num_code
    stimulus_times <- c(time_of_stim)
  }

  df$stimulus <- .bincode(df$time_ms, bin_breaks)

  if (add_missing) {
    # Not every stimulus is expected to cause an event, this will allow a user
    # to analyse missing events via grouping (e.g. stimulus &/or sweep) as the
    # other column values will be NA.
    df_missing_stimuli <-
      plyr::ddply(df, plyr::.(sweep), function(x) {
        data.frame(
          stimulus =
            bin_num_code[!(bin_num_code %in% x[, "stimulus"])]
        )
      })
    df <- dplyr::bind_rows(df, df_missing_stimuli)

    # If a user inserted missing sweeps using insert_rows_for_missing_sweeps()
    # this will trim the excess rows, as they are superseded by this f()'s
    # insertion of missing events.
    if (nrow(df[which(is.na(df$time_ms) &
      is.na(df$stimulus)), ]) > 0) {
      df <- df[-which(is.na(df$time_ms) & is.na(df$stimulus)), ]
    }
  }

  df <- df[order(df$sweep, df$stimulus, df$time_ms), ]
  rownames(df) <- NULL

  df$stimulus <- ordered(df$stimulus, labels = bin_labels)

  # Insert the stimulus time in case user wishes to calculate jitter or group by
  # this time value.
  stim_time_lut <- tibble::tibble(
    stim_times = stimulus_times,
    stimulus = ordered(bin_labels, levels = bin_labels)
  )
  df <- dplyr::left_join(df, stim_time_lut, by = "stimulus")

  return(df)
}

#' @title Add Sweep Number To Rows
#'
#' @description Determines the sweep/trial number based upon the event time, \code{rec_time_ms}.
#'
#' @param df Data Frame or Tibble
#' @param sweep_duration Numeric, sweep/trial duration in seconds.
#' @param sweep_count Numeric, total number of experiment sweeps/trials.
#' @param time_ref Character, event time reference point column name, i.e. \code{rec_time_ms}.
#'
#' @return Data Frame or Tibble with the following columns:
#' * \code{sweep}, Ordered Factor
#'
#' @seealso
#' * \link{insert_rows_for_missing_sweeps}
#'
#' @examples
#' simple_df <- data.frame(rec_time_ms = c("400.50", "4,400.50"))
#'
#' add_sweep_number_to_rows(
#'   df = simple_df,
#'   sweep_duration = 1,
#'   sweep_count = 5,
#'   time_ref = "rec_time_ms"
#' )
#'
#' \dontrun{
#' df <- add_sweep_number_to_rows(
#'   df = data,
#'   sweep_duration = matools_env$sweep_duration_sec,
#'   sweep_count = matools_env$sweeps_total,
#'   time_ref = "rec_time_ms"
#' )
#' }
#'
#' @importFrom dplyr ensym
#' @export
#' @md
add_sweep_number_to_rows <- function(df,
                                     sweep_duration,
                                     sweep_count,
                                     time_ref = "rec_time_ms") {
  if (!time_ref %in% colnames(df)) {
    stop(time_ref, " column not present in df")
  }

  length_in_milliseconds <- sweep_duration * 1000
  bin_width <-
    (seq(1, (length_in_milliseconds * (sweep_count + 1)), by = (length_in_milliseconds))) - 1

  if (!tibble::is_tibble(df)) {
    # Imported data.frames will likely have the "rec_time_ms" column as a char that includes commas and periods,
    # if the recording lasted past 1000 ms.
    df[, time_ref] <- as.numeric(gsub(",", "", df[, time_ref]))
  }
  df$sweep <- cut(df[[dplyr::ensym(time_ref)]],
    bin_width,
    labels = c(1:sweep_count),
    ordered_result = TRUE
  )

  return(df)
}

#' @title Align Condition Sweeps
#'
#' @description
#' Creates a 'trial' column and aligns the first sweep in each condition to trial '1'. The original
#' sweep value is not overwritten.
#'
#' @param df Data Frame or Tibble, with the following columns:
#' * sweep
#' * condition
#' @param condition_names Character vector with condition names.
#'
#' @return Data Frame or Tibble, with the additional column:
#' * trial
#'
#' @seealso
#' * \link{sequential_condition_sweeps}
#' * \link{plot_heatmap}
#'
#' @importFrom dplyr filter group_by join_by left_join mutate select slice_head
#' @importFrom magrittr %>%
align_condition_sweeps <- function(df,
                                   condition_names) {
  offset <-
    df %>%
    dplyr::filter(.data$condition %in% condition_names) %>%
    dplyr::group_by(.data$condition) %>%
    dplyr::slice_head() %>%
    dplyr::mutate(sweep_offset = .data$sweep - 1) %>%
    dplyr::select("condition", "sweep_offset")

  tmp <-
    dplyr::left_join(df, offset, by = dplyr::join_by("condition")) %>%
    dplyr::mutate(trial = .data$sweep - .data$sweep_offset) %>%
    dplyr::select(!"sweep_offset")

  return(tmp)
}

#' @title Sequential Condition Sweeps
#'
#' @details
#' Creates a 'trial' column with sequential trial numbers across all conditions. The original
#' sweep value is not overwritten.
#'
#' @param df Data Frame or Tibble, with the following columns:
#' * sweep
#' * condition
#' @param condition_names Character vector with condition names.
#'
#' @return Data Frame or Tibble, with the additional column:
#' * trial
#'
#' @seealso
#' * \link{align_condition_sweeps}
#' * \link{plot_spike_raster}
#'
#' @importFrom dplyr filter left_join join_by
#' @importFrom magrittr %>%
sequential_condition_sweeps <- function(df,
                                        condition_names) {
  # check for continuous sweeps
  missing_sweeps <- setdiff(c(1:max(df$sweep)), df[["sweep"]])

  if (!length(missing_sweeps) == 0) {
    df <- try(insert_rows_for_missing_sweeps(df, matools_env$sweep_total), silent = TRUE)

    if ("try-error" %in% class(df)) {
      stop(
        "Sweeps are not continuous, unsuccessfully tried insert_rows_for_missing_sweeps() using default values. ",
        "Manually run insert_rows_for_missing_sweeps() before this function."
      )
    }
  }

  tmp <-
    df %>%
    dplyr::filter(.data$condition %in% condition_names)

  sweep_values <- unique(tmp$sweep)
  lookup_table <-
    data.frame(
      sweep = sweep_values,
      trial = seq_along(sweep_values)
    )

  tmp <-
    dplyr::left_join(df, lookup_table, by = dplyr::join_by("sweep"))

  return(tmp)
}

#' @title Calculate Stimulus Times
#'
#' @description The output can be used to determine the # of stimuli and
#'   timing for subsequent functions.
#'
#' @param stimulus_time_first Numeric, time of the first stimulus in ms
#' @param stimulus_count Numeric, number of stimuli
#' @param stimulus_isi Numeric, interstimulus interval in ms, e.g. 25 Hz = 40 ms
#'
#' @return returns a vector of stimulus times to the \code{matools_env}:
#' * \code{time_of_stimuli} vector with stimulus times
#'
#' @examples
#' \dontrun{
#' library(matools)
#'
#' calculate_stimulus_times(
#'   matools_env$stimulus_time_of_first,
#'   matools_env$stimulus_count,
#'   matools_env$stimulus_isi
#' )
#' }
#'
#' @export
#' @md
calculate_stimulus_times <- function(stimulus_time_first,
                                     stimulus_count,
                                     stimulus_isi) {
  matools_env$time_of_stimuli <-
    seq(
      stimulus_time_first,
      (stimulus_time_first + (stimulus_isi * (stimulus_count - 1))),
      stimulus_isi
    )
}

#' Generate Condition Sweep Lookup Table (LUT)
#'
#' @details
#' This function will determine the first and last sweep numbers for all conditions, returning a
#' LUT with the columns, \code{condition_start} and \code{condition_end}. It also calculates the
#' number of sweeps for each condition in the column, \code{delta}.
#'
#' @param df Data Frame or Tibble, with the following columns:
#' * sweep OR another reference column, e.g. trial
#' * condition
#' @param condition_names Character vector with condition names.
#' @param column_ref Character, reference column to generate LUT. Defaults to \code{"sweep"}.
#'
#' @return Tibble, with the following columns:
#' * \code{condition} Character.
#' * \code{condition_start} Integer, first sweep for a condition.
#' * \code{condition_end} Integer, last sweep for a condition.
#' * \code{delta} Integer, number of sweeps in a condition.
#'
#' @importFrom dplyr filter group_by mutate row_number select ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
condition_sweep_lut <- function(df, condition_names, column_ref = "sweep") {
  tmp <-
    df %>%
    dplyr::filter(.data$condition %in% condition_names) %>%
    dplyr::group_by(.data$condition) %>%
    dplyr::filter(dplyr::row_number() %in% c(1, n())) %>%
    dplyr::select("condition", !!column_ref) %>%
    dplyr::mutate(tag = c("condition_start", "condition_end")) %>%
    tidyr::pivot_wider(names_from = .data$tag, values_from = .data[[!!column_ref]]) %>%
    dplyr::mutate(delta = .data$condition_end - .data$condition_start) %>%
    dplyr::ungroup()

  return(tmp)
}

#' @title Create Data Frame With Histogram Values
#'
#' @description Generates a data.frame of binned values, binning starts at the first stimulus, e.g. 0 ms, and ends at
#'   \code{window_size}.
#'
#' @param df Data Frame or Tibble with the following columns:
#' * time_ms
#' * sweep
#' * condition
#' * stimulus
#' * event_index
#' @param time_of_stim List, with event times in milliseconds.
#' @param condition_names Character, vector with condition titles.
#' @param bin_size Integer, histogram bin width size in milliseconds. Default is '1'.
#' @param window_size Integer, in milliseconds the size of the window to use for the histogram, the start of the window
#'   will be at the first stimulus.
#' @param first_bin_on_stim Logical, if \code{TRUE} the first bin center will occur on the first stimulus, i.e. 0.
#'   Default is \code{FALSE}.
#' @param one_event_per_bin Logical, if \code{TRUE} only a single event will be counted for each bin. Default is
#'   \code{FALSE}.
#'
#' @return Tibble with the following columns:
#' * \code{condition} - character
#' * \code{count_events} - integer
#' * \code{count_sweeps} - integer
#' * \code{h_counts} - nested list of integers
#' * \code{h_density} - nested list of doubles
#' * \code{h_bin_centers} - nested list of doubles
#' * \code{probability} - nested list of doubles
#' * \code{h_count_norm_max} - nested list of doubles
#' * \code{h_prob_norm_max} - nested list of doubles
#'
#' @note This only returns a tibble. Histogram related columns are nested, which facilitates
#'  aggregating the output into a tibble for future comparison across experiments. See code example
#'  using \code{dplyr::select} and \code{tidyr::unnest} for working with this functions output.
#'
#' @seealso
#' * [analysis_evoked_ap()] for a workflow example that aggregates this functions output.
#'
#' @examples
#' library(magrittr, include.only = "%>%")
#'
#' simple_df <- data.frame(
#'   time_ms = rep(c(417, 427), 10),
#'   sweep = rep(c(1:10), each = 2),
#'   condition = rep("control", 10),
#'   stimulus = rep(c(1, 2), 10),
#'   event_index = 1
#' )
#'
#' df_output <-
#'   create_histogram_df(
#'     df = simple_df,
#'     time_of_stim = c(415.5, 425.5),
#'     condition_names = c("control", "drug1"),
#'     bin_size = 1,
#'     window_size = 20,
#'     first_bin_on_stim = FALSE,
#'     one_event_per_bin = FALSE
#'   )
#'
#' df_output %>%
#'   dplyr::select(
#'     condition, h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max
#'   ) %>%
#'   tidyr::unnest(c(
#'     h_counts, h_density, h_bin_centers, probability, h_count_norm_max, h_prob_norm_max
#'   ))
#'
#' @importFrom dplyr filter group_by mutate arrange
#' @importFrom dplyr summarise n_distinct pull select
#' @importFrom graphics hist
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr drop_na
#' @export
#' @md
create_histogram_df <- function(df,
                                time_of_stim,
                                condition_names,
                                bin_size = 1,
                                window_size,
                                first_bin_on_stim = FALSE,
                                one_event_per_bin = FALSE) {
  if (first_bin_on_stim == TRUE) {
    # start the first bin before the stimulus
    first_bin <- -(bin_size / 2)
    if (bin_size > 2) {
      warning(
        "histogram bin is centered on the first stimulus will group events occuring ", (bin_size / 2),
        "ms before and after the stimuli. Consider setting bin_size <= 2ms or first_bin_on_stim = FALSE"
      )
    }
  } else {
    first_bin <- 0
  }

  histogram_df <-
    df %>%
    dplyr::select("time_ms", "stimulus", "sweep", "condition", "event_index") %>%
    dplyr::filter(.data$condition %in% condition_names) %>%
    dplyr::group_by(.data$condition) %>%
    # normalize time_ms to the first stimulus
    dplyr::mutate(time_ms_norm = .data$time_ms - time_of_stim[1]) %>%
    # drop events greater than window_size, keep 0 and NA
    dplyr::filter(.data$time_ms_norm >= 0 & .data$time_ms_norm < (window_size + first_bin) | is.na(.data$time_ms_norm)) %>%
    dplyr::group_by(.data$condition) %>%
    dplyr::summarise(
      count_events = sum(!is.na(.data$time_ms)),
      count_sweeps = dplyr::n_distinct(.data$sweep),
      h_counts = if (.data$count_events == 0) {
        list(rep(0, (length(seq(first_bin, window_size, bin_size)) - 1)))
      } else if (one_event_per_bin == TRUE) {
        list(graphics::hist(.data$time_ms_norm[.data$event_index == 1], plot = FALSE, breaks = c(seq(first_bin, window_size, bin_size)))$counts)
      } else {
        list(graphics::hist(.data$time_ms_norm, plot = FALSE, breaks = c(seq(first_bin, window_size, bin_size)))$counts)
      },
      h_density = if (.data$count_events == 0) {
        list(rep(0, (length(seq(first_bin, window_size, bin_size)) - 1)))
      } else if (one_event_per_bin == TRUE) {
        list(graphics::hist(.data$time_ms_norm[.data$event_index == 1],
          plot = FALSE,
          breaks = c(seq(first_bin, window_size, bin_size))
        )$density)
      } else {
        list(graphics::hist(.data$time_ms_norm,
          plot = FALSE,
          breaks = c(seq(first_bin, window_size, bin_size))
        )$density)
      },
      h_bin_centers = list(graphics::hist(.data$time_ms_norm,
        plot = FALSE,
        breaks = c(seq(first_bin, window_size, bin_size))
      )$mids),
      probability = list(unlist(.data$h_counts) / .data$count_sweeps)
    ) %>%
    dplyr::arrange(match(.data$condition, condition_names))

  # normalizing the histogram values to the max value in control
  histogram_df <-
    histogram_df %>%
    dplyr::group_by(.data$condition) %>%
    dplyr::mutate(
      h_count_norm_max = list(unlist(.data$h_counts) /
        max(histogram_df %>%
          dplyr::filter(.data$condition == "control") %>%
          dplyr::pull(.data$h_counts) %>%
          unlist())),
      h_prob_norm_max = list(unlist(.data$probability) /
        max(histogram_df %>%
          dplyr::filter(.data$condition == "control") %>%
          dplyr::pull(.data$probability) %>%
          unlist()))
    )

  return(histogram_df)
}

#' @title Add Blank Rows To Data Frame For Missing Sweeps/Trials
#'
#' @description Events may not occur on every sweep/trial, this will add a blank row for sweeps without events.
#' Thus, creating continuous sweep data allowing for addition of condition tags and/or analysis by sweep number.
#'
#' @param df Data Frame or Tibble, with the following columns:
#' * sweep
#' @param sweep_count Numeric, total number of sweeps.
#'
#' @return Appended data.frame with rows for sweeps/trials without events, only the ordered factor column, \code{sweep},
#' will contain a integer values, all other row values are \code{NA}.
#'
#' @seealso
#' * \link{add_sweep_number_to_rows}
#'
#' @examples
#' simple_df <- data.frame(
#'   rec_time_ms = c(400, 4400),
#'   sweep = ordered(c(1, 5), levels = c(1:5))
#' )
#'
#' insert_rows_for_missing_sweeps(
#'   df = simple_df,
#'   sweep_count = 5
#' )
#'
#' @export
#' @md
insert_rows_for_missing_sweeps <- function(df,
                                           sweep_count) {
  if (!"sweep" %in% colnames(df)) {
    df <- try(add_sweep_number_to_rows(
      df = df,
      sweep_duration = matools_env$sweep_duration_sec,
      sweep_count = sweep_count,
      time_ref = "rec_time_ms"
    ), silent = TRUE)

    if ("try-error" %in% class(df)) {
      stop(
        "df is missing 'sweep' column, unsuccessfully tried add_sweep_number_to_rows() using default values. ",
        "Manually run add_sweep_number_to_rows() before this function."
      )
    }
  }

  missing_sweeps_in_df <- setdiff(c(1:sweep_count), df[["sweep"]])
  df_temp <- matrix(
    nrow = (length(missing_sweeps_in_df)),
    ncol = ncol(df)
  )
  colnames(df_temp) <- colnames(df)
  df_temp[, "sweep"] <- missing_sweeps_in_df
  df_temp <- rbind(df, df_temp)
  df_temp <-
    df_temp[order(df_temp[["sweep"]]), ] # Resort by sweep number
  row.names(df_temp) <- NULL # Reset row indexing

  return(df_temp)
}

#' @title Standardize Event Time To Discrete Time
#'
#' @description For converting continuous time into discrete time series for functions that plot by sweeps or calculate
#' events by stimuli within sweeps.
#'
#' @param df Data Frame or Tibble with a \code{sweep} column
#' @param sweep_duration Numeric, sweep/trial length in seconds.
#' @param time_ref Character, event time reference point column name. Defaults to "rec_time_ms".
#'
#' @return Data Frame or Tibble with transformed time column, \code{time_ms}.
#'
#' @seealso
#' * \link{add_sweep_number_to_rows}
#'
#' @examples
#' simple_df <- data.frame(
#'   rec_time_ms = c(400, 4400),
#'   sweep = ordered(c(1, 5), levels = c(1:5))
#' )
#'
#' standardize_event_time(
#'   df = simple_df,
#'   sweep_duration = 1,
#'   time_ref = "rec_time_ms"
#' )
#' \dontrun{
#' df <- standardize_event_time(
#'   df,
#'   matools_env$sweep_duration_sec,
#'   "rec_time_ms"
#' )
#' }
#'
#' @importFrom rlang ensym
#' @export
#' @md
standardize_event_time <- function(df,
                                   sweep_duration,
                                   time_ref = "rec_time_ms") {
  if (!time_ref %in% colnames(df)) {
    stop(time_ref, " column not present in df")
  } else if (!"sweep" %in% colnames(df)) {
    df <- try(add_sweep_number_to_rows(
      df = df,
      sweep_duration = matools_env$sweep_duration_sec,
      sweep_count = matools_env$sweep_total,
      time_ref = "rec_time_ms"
    ), silent = TRUE)

    if ("try-error" %in% class(df)) {
      stop(
        "df is missing 'sweep' column, unsuccessfully tried add_sweep_number_to_rows() using default values. ",
        "Manually run add_sweep_number_to_rows() before this function."
      )
    }
  } else {
    time_in_ms <- sweep_duration * 1000
    df$time_ms <-
      df[[rlang::ensym(time_ref)]] - ((as.numeric(df[["sweep"]]) - 1) * time_in_ms)
  }
  return(df)
}
