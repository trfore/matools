# utils.R

utils::globalVariables(c("matools_env"))

#### Utility Functions ####

#' @title ASC File LUT
#' @description
#' A lookup table with the column names and classes for ASC files generated by mini analysis
#'
#' @return tibble with three columns:
#' * name
#' * df_class - classes for data frames
#' * tbl_class - classes for tibbles
#'
#' @note Several data frame columns are set to the base type "character" to avoid generating \code{NA} on number values
#' containing commas and periods, e.g. \code{1,200.00}, on import.
#'
#' @importFrom tibble tribble
asc_lookup_table <- function() {
  lut <-
    tibble::tribble(
      ~name, ~df_class, ~tbl_class,
      "ma_row", "integer", "i",
      "rec_time_ms", "character", "d",
      "amplitude", "double", "d",
      "rise_ms", "double", "d",
      "decay_ms", "double", "d",
      "area", "character", "d",
      "baseline", "double", "d",
      "noise", "double", "d",
      "group", "integer", "i",
      "channel", "integer", "i",
      "x10_90_rise", "double", "d",
      "halfwidth", "double", "d",
      "rise50", "double", "d",
      "peak_direction", "integer", "i",
      "burst", "integer", "i",
      "burste", "integer", "i",
      "x10_90slope", "character", "d",
      "rel_time", "double", "d"
    )

  return(lut)
}

#' @title Convert .ASC File Into A Tibble Data Frame
#'
#' @description Exported files from Synaptosoft's Mini Analysis software (MA, see ref) are saved as ASCII text file
#' (.asc), this function will convert (.asc) files into a \code{tibble}.
#'
#' @param file_path Character, path to file. User can call \code{file.choose} to interactively select the file.
#' @param merge_iei Logical, if TRUE .asc and .txt files with matching names will be merged. Default is \code{FALSE}.
#'
#' @details Mini Analysis software (MA) independently analyzes inter-event interval from the events file, this function
#'   will combine the event file (.asc) with the corresponding inter-event interval (IEI) text file (.txt) if arg
#'   \code{merge_iei} is set to TRUE.
#'
#' @return
#' A tibble with column names that match default columns within Synaptosoft's Mini Analysis software. Column names are
#' as follows:
#' * \code{ma_row}: Integer, corresponds to the row number generated in Mini Analysis. It is not recommended for use
#'   as a reference/key value.
#' * \code{rec_time_ms}: Double, continuous time value that is determined from the recording digitization value.
#' * \code{amplitude} Double, amplitude of the detected event.
#' * \code{rise_ms} Double, rise time in milliseconds.
#' * \code{decay_ms} Double, decay time in milliseconds.
#' * \code{area} Double
#' * \code{baseline} Double
#' * \code{noise} Double
#' * \code{group}: Integer (0 to 255), user-assigned group number within MA.
#' * \code{channel}: Integer, corresponds to the recording channel as defined by the user's DAC and digitizer.
#' * \code{x10_90_rise} Double
#' * \code{halfwidth} Double
#' * \code{rise50} Double
#' * \code{peak_direction} Integer
#' * \code{burst} Integer
#' * \code{burste} Integer
#' * \code{x10_90slope} Numeric
#' * \code{rel_time} Double
#' * \code{iei}: Double, inter-event interval in milliseconds calculated in MA. Optional column, if arg \code{merge_iei}
#'   is set to \code{TRUE}.
#'
#' @references \url{http://www.synaptosoft.com/MiniAnalysis/}
#'
#' @importFrom readr col_double col_integer col_number read_tsv
#' @importFrom dplyr bind_cols
#' @export
#' @md
asc_to_tibble <- function(file_path,
                          merge_iei = FALSE) {
  temp <- readr::read_tsv(
    file_path,
    col_names = asc_lookup_table()$name,
    col_types = list(
      ma_row = readr::col_integer(),
      rec_time_ms = readr::col_number(),
      amplitude = readr::col_double(),
      rise_ms = readr::col_double(),
      decay_ms = readr::col_double(),
      area = readr::col_number(),
      baseline = readr::col_double(),
      noise = readr::col_double(),
      group = readr::col_integer(),
      channel = readr::col_integer(),
      x10_90_rise = readr::col_double(),
      halfwidth = readr::col_double(),
      rise50 = readr::col_double(),
      peak_direction = readr::col_integer(),
      burst = readr::col_integer(),
      burste = readr::col_integer(),
      x10_90slope = readr::col_number(),
      rel_time = readr::col_double()
    )
  )

  if (merge_iei == TRUE) {
    temp_iei <- suppressWarnings(readr::read_tsv(
      paste(strsplit(file_path, ".(ASC)"), "txt", sep = "."),
      col_select = 1,
      col_names = "iei",
      col_types = list(iei = readr::col_double()),
      skip = 6 # file header
    ))
    temp_iei <-
      rbind(NA, temp_iei) # add NA value at the start to equalize size
    temp <- dplyr::bind_cols(temp, temp_iei)
  }
  return(temp)
}

#' @title Convert .ASC File Into Data Frame
#'
#' @description Exported files from Synaptosoft's Mini Analysis software (MA, see ref) are saved as ASCII text file
#'   (.asc), this function will convert (.asc) files into a \code{data.frame}.
#'
#' @inheritParams asc_to_tibble
#'
#' @details Mini Analysis software (MA) independently analyzes inter-event interval from the events file, this function
#'   will combine the event file (.asc) with the corresponding inter-event interval (IEI) text file (.txt) if arg
#'   \code{merge_iei} is set to TRUE.
#'
#' @return data.frame, column names match default columns within Synaptosoft's Mini Analysis software.
#' Column names are as follows:
#' * \code{ma_row}: Integer, corresponds to the row number generated in Mini Analysis. It is not recommended for use
#'   as a reference/key value.
#' * \code{rec_time_ms}: Character, continuous time value that is determined from the recording digitization value.
#' * \code{amplitude} Double, amplitude of the detected event.
#' * \code{rise_ms} Double, rise time in milliseconds.
#' * \code{decay_ms} Double, decay time in milliseconds.
#' * \code{area} Character
#' * \code{baseline} Double
#' * \code{noise} Double
#' * \code{group}: Integer (0 to 255), user-assigned group number within MA.
#' * \code{channel}: Integer, corresponds to the recording channel as defined by the user's DAC and digitizer.
#' * \code{x10_90_rise} Double
#' * \code{halfwidth} Double
#' * \code{rise50} Double
#' * \code{peak_direction} Integer
#' * \code{burst} Integer
#' * \code{burste} Integer
#' * \code{x10_90slope} Character
#' * \code{rel_time} Double
#' * \code{iei}: Double, inter-event interval in milliseconds calculated in MA. Optional column, if arg \code{merge_iei}
#'   is set to \code{TRUE}.
#'
#' @note Several columns are set to the base type "character" to avoid generating \code{NA} on number values containing
#' commas and periods, e.g. \code{1,200.00}, on import.
#'
#' @references \url{http://www.synaptosoft.com/MiniAnalysis/}
#'
#' @importFrom utils read.delim
#' @export
#' @md
asc_to_df <- function(file_path,
                      merge_iei = FALSE) {
  temp <- utils::read.delim(
    file_path,
    header = FALSE,
    col.names = asc_lookup_table()$name,
    colClasses = asc_lookup_table()$df_class
  )

  if (merge_iei == TRUE) {
    temp_iei <- utils::read.delim(
      paste(strsplit(file_path, ".(ASC)"), "txt", sep = "."),
      colClasses = c(NA, NA),
      col.names = c("iei", NA),
      header = FALSE,
      sep = "\t",
      skip = 6
    )[1]

    temp_iei <-
      rbind(NA, temp_iei) # add NA value at the start to equalize size
    temp <- cbind(temp, temp_iei)
  }
  return(temp)
}


#' Create A CSV Template For Project Parameters
#'
#' @return 'parameter_template.csv' file in working directory
#'
#' @importFrom tibble as_tibble
#' @export
create_parameter_template <- function() {
  column_names <- c(
    "file_name", "experiment_id", "genotype_id", "general_notes", "total_sweeps", "sweep_duration_sec",
    "event_threshold", "first_stim", "num_of_stim", "stim_isi", "condition_1_name", "condition_1_start",
    "condition_1_end", "condition_2_name", "condition_2_start", "condition_2_end", "condition_3_name",
    "condition_3_start", "condition_3_end", "condition_4_name", "condition_4_start", "condition_4_end",
    "condition_5_name", "condition_5_start", "condition_5_end", "condition_6_name", "condition_6_start",
    "condition_6_end"
  )

  tmp <- matrix(ncol = length(column_names))
  colnames(tmp) <- column_names

  write.csv(tibble::as_tibble(tmp), "parameter_template.csv", row.names = FALSE)
}

#' @title Importing Experiment Parameters
#'
#' @description Importing experiment parameters from a static file allows for batch processing of experiments that may
#'   have asymmetries, e.g. drug, stimulus, experiment id. User should manually fill in a "experiment_parameters.csv"
#'   file. The function, \link{create_parameter_template}, can be called to generate a template file.
#'
#' @param file Data.Frame or Character (PATH to .csv file). Users can pass a data frame or tibble into this function or
#'   the full PATH to a .csv file. This allows for the use of a remote/cloud-based file, e.g. \code{googlesheets}, or
#'   local file. Default is \code{NULL} and will prompt the user to interactively select the file.
#'
#' @return
#' Environment objects (variables) to \code{matools_env}:
#' * \code{parameters}
#' * \code{experiment_types}
#'
#' @examples
#' \dontrun{
#' library(matools)
#'
#' ### Passing the PATH to a local file ###
#' local_file_path <- "/path/to/my/parameters.csv"
#' import_experiment_parameters(file = local_file_path)
#'
#' ### Using a publicly shared Google Sheet ###
#' googlesheets4::gs4_deauth()
#' public_sheet <- "https://docs.google.com/spreadsheets/d/YOUR_SHEET_HASH_HERE/edit?usp=sharing"
#'
#' # Convert the sheet to a data frame
#' parameters <-
#'   googlesheets4::read_sheet(public_sheet)
#' import_experiment_parameters(file = parameters)
#' }
#'
#' @seealso \link{create_parameter_template} to generate a csv template.
#'
#' @importFrom dplyr distinct filter
#' @importFrom plyr .
#' @importFrom magrittr %>%
#' @importFrom rstudioapi selectFile
#' @importFrom stringr str_detect
#' @importFrom tibble column_to_rownames
#' @importFrom utils read.csv
#' @export
#' @md
import_experiment_parameters <- function(file = NULL) {
  if (is.data.frame(file)) {
    matools_env$parameters <- file %>%
      dplyr::filter(!stringr::str_detect(.data$file_name, "#")) %>%
      tibble::column_to_rownames(var = "file_name")
  } else if (is.null(file) || utils::file_test("-f", file)) {
    matools_env$parameters <-
      utils::read.csv(
        if (is.null(file)) {
          rstudioapi::selectFile(
            caption = "Select Parameters File",
            filter = "CSV Files (*.csv)",
            label = "parameters (.csv)",
            path = matools_env$directory_parent
          )
        } else {
          file
        },
        header = TRUE,
        comment.char = "#",
        blank.lines.skip = TRUE,
        row.names = 1
      )
  } else {
    stop("Unknown 'file' value passed, pass data.frame/tibble or path to file")
  }

  matools_env$experiment_types <-
    levels(matools_env$parameters %>% dplyr::distinct(experiment_id) %>% .[["experiment_id"]])
}

#' @title Import Check For Matching Data Files To Experimental Parameters
#'
#' @description Indicates a disparity between files present and/or parameters. Returns warning message with missing
#'   file information and allows user to exclude processing data that are missing either the data file or experimental
#'   parameters. This is important, as helper functions, \link{import_experiment_parameters}, can generate environment
#'   variables from these parameters, and analysis function arguments can easily point to those values.
#'
#' @param dir_filenames Character vector of files within the working directory
#' @param parameters Data frame with experimental parameters
#' @param skip_prompt Logical, skips warning messages and user prompts, automatically will remove files missing
#'   parameters from the \code{files_to_process} list. Defaults to \code{FALSE}.
#'
#' @return
#' Character vector \code{files_to_process} in the \code{matools_env}.
#'
#' @examples
#' \dontrun{
#' library(matools)
#'
#' set_pkg_environment()
#' set_data_directory()
#' import_experiment_parameters()
#' import_check_missing_info(
#'   matools_env$files_in_folder,
#'   matools_env$parameters
#' )
#'
#' print(matools_env$files_to_process)
#' }
#'
#' @seealso
#' * \link{set_pkg_environment} to generate the required \code{matools_env}.
#' * \link{set_data_directory} to auto generate a vector of file names \code{matools_env$files_in_folder}.
#' * \link{import_experiment_parameters} to auto generate a data.frame of parameters \code{matools_env$parameters}.
#' * \link{analysis_evoked_ap} or \link{analysis_evoked_psp} for example workflows.
#'
#' @importFrom utils menu
#' @export
#' @md
import_check_missing_info <- function(dir_filenames,
                                      parameters,
                                      skip_prompt = FALSE) {
  if (exists("matools_env") == FALSE) {
    warning("matools_env does not exist, running set_pkg_environment()", call. = FALSE, immediate. = TRUE)
    set_pkg_environment()
  }

  missing_files <- setdiff(rownames(parameters), dir_filenames)
  missing_parameters <- setdiff(dir_filenames, rownames(parameters))
  missing_parameters_or_file <- c(missing_files, missing_parameters)

  if (length(missing_parameters) > 0) {
    warning("the following files are missing parameters:", missing_parameters, call. = FALSE, immediate. = TRUE)
  }
  if (length(missing_files) > 0) {
    warning("the following data files (.asc) are missing:", missing_files, call. = FALSE, immediate. = TRUE)
  }

  # check and prompt for user choice
  if (skip_prompt == FALSE) {
    if (length(missing_parameters) > 0 || length(missing_files) > 0) {
      continue_analysis <-
        utils::menu(c("Yes", "No"),
          title = "Would you like to exclude these files and continue?"
        )
    } else {
      continue_analysis <- 1
      message("All files and experiment parameters present")
    }

    if (continue_analysis == 2) {
      stop("User halted analysis, please update parameters file")
    } else if (continue_analysis == 0) {
      stop("User exited selection menu, please update parameters file")
    }
  }

  message("matools_env$files_to_process created")
  matools_env[["files_to_process"]] <-
    setdiff(dir_filenames, missing_parameters_or_file)
}

#' @title Set Data Directory
#'
#' @description Extracts data directory, it's parent and names of data files (*.asc) within the directory. This will
#'   aid with creating an analysis pipeline.
#'
#' @param file_path Optional path to data directory. Defaults to \code{NULL} and opens GUI for directory selection.
#'
#' @return Environment variables to \code{matools_env}:
#' * \code{directory_data}
#' * \code{directory_parent}
#' * \code{files_in_folder}
#'
#' @seealso \link{set_pkg_environment} to generate the required matools_env
#'
#' @examples
#' library(matools)
#'
#' set_pkg_environment()
#' set_data_directory(file_path = getwd())
#' print(matools_env$directory_data)
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom tools file_path_sans_ext
#' @export
#' @md
set_data_directory <- function(file_path = NULL) {
  if (exists("matools_env") == TRUE) {
    if (is.null(matools_env$directory_data)) {
      if (is.null(file_path)) {
        message("Select Data Directory")
        matools_env$directory_data <-
          rstudioapi::selectDirectory(
            caption = "Select Directory",
            label = "Data Directory"
          )
      } else {
        matools_env$directory_data <- file_path
      }
    } else {
      message("Data directory is set to:", matools_env$directory_data)
    }

    matools_env$directory_parent <- dirname(matools_env$directory_data)

    # get the file names within the data directory
    matools_env$files_in_folder <- tools::file_path_sans_ext(
      list.files(
        path = matools_env$directory_data, pattern = ".ASC",
        full.names = FALSE, ignore.case = TRUE
      )
    )
  } else {
    stop("matools_env does not exist, please run set_pkg_environment() first")
  }
}

#' @title Set Experiment Parameters
#'
#' @description Pulls file specific parameters (i.e. drug start times) from the parameters data frame, allowing for
#'   analysis f() based upon individual experiment settings.
#'
#' @param parameters Experiment parameters df imported using
#'   \code{import_experiment_parameters} function.
#' @param filename Character, single file name
#' @param condition_names
#'   \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}:Character
#'   vector with condition titles
#'
#' @return environment variables to \code{matools_env}:
#' * \code{experiment_id} chr
#' * \code{stimulus_time_of_first} dbl
#' * \code{stimulus_count} int
#' * \code{stimulus_isi} int
#' * \code{stimulus_time_of_recovery} dbl
#' * \code{event_threshold} int
#' * \code{sweep_total} int
#' * \code{sweep_duration_sec} int
#' * \code{condition_names} chr
#' * \code{condition_starts} list
#' * \code{condition_ends} list
#' * \code{condition_drug_starts}{lifecycle::badge("deprecated")}
#'
#' @importFrom lifecycle deprecated deprecate_warn
#' @export
#' @md
set_experiment_parameters <- function(parameters,
                                      filename,
                                      condition_names = lifecycle::deprecated()) {
  matools_env$experiment_id <- parameters[filename, "experiment_id"]
  matools_env$stimulus_time_of_first <- parameters[filename, "first_stim"]
  matools_env$stimulus_count <- parameters[filename, "num_of_stim"]
  matools_env$stimulus_isi <- parameters[filename, "stim_isi"]
  matools_env$stimulus_time_of_recovery <- parameters[filename, "recovery_stim"]
  matools_env$event_threshold <- parameters[filename, "event_threshold"]
  matools_env$sweep_total <- parameters[filename, "total_sweeps"]
  matools_env$sweep_duration_sec <- parameters[filename, "sweep_duration_sec"]

  if (missing(condition_names)) {
    conditions <- parameters[filename, grep(
      "^condition_.*_name$",
      colnames(parameters)
    )]
    conditions[] <- lapply(conditions, as.character)
    matools_env$condition_names <- unlist(conditions[, conditions != "NULL"])
  } else {
    lifecycle::deprecate_warn("0.3.1",
      "set_experiment_parameters(condition_names = )",
      details = "now auto generated from csv file"
    )
  }

  condition_starts <- list()
  condition_ends <- list()
  for (i in seq_along(matools_env$condition_names)) {
    condition_starts[[i]] <- parameters[filename, paste("condition",
      i, "start",
      sep = "_"
    )]
    condition_ends[[i]] <- parameters[filename, paste("condition",
      i, "end",
      sep = "_"
    )]
  }
  matools_env$condition_starts <- unlist(condition_starts)
  matools_env$condition_ends <- unlist(condition_ends)
}
