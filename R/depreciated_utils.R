# depreciated_utils.R

utils::globalVariables(c("experiment_id", "matools_env"))

#### Depreciated Utility Functions ####

#' @title Convert .ASC Files To .cvs Files
#'
#' @description Exported files from Synaptosoft's Mini Analysis software (MA,
#'   see ref) are saved as ASCII text file (.asc), this function provides an
#'   user-friendly .csv file for data analysis and viewing. CSV files are
#'   automatically generated in a '\code{WORKING_DIR/data_csv/}' folder.
#'
#' @param file_list Character, vector of files to process, e.g. \code{matools_env$files_in_folder}.
#' @param merge_iei Logical, if TRUE asc and txt files with matching names will be merged.
#'   Default is \code{FALSE}.
#' @param path Character, with the location of data directory containing .asc files. Default is NULL
#'   and matools_env$directory_data will be used.
#'
#' @return Exports *.csv file to directory specified in \code{path}, column
#'   names match default columns within Synaptosoft's Mini Analysis software.
#'
#' @seealso \link{set_data_directory} to auto generate \code{directory_data} in
#'   \code{matools_env}
#'
#' @importFrom utils read.table write.csv
#' @md
batch_convert_asc_to_csv <- function(file_list,
                                     merge_iei = FALSE,
                                     path = NULL) {
  if (is.null(path)) {
    if (is.null(matools_env$directory_data)) {
      stop("arg 'path' and 'matools_env$directory_data' is NULL")
    } else {
      path <- matools_env$directory_data
    }
  }

  dir.create(paste(path, "data_csv", sep = "/"))

  asc_file_column_names <-
    c(
      "ma_row", "rec_time_ms", "amplitude", "rise_ms", "decay_ms", "area",
      "baseline", "noise", "group", "channel", "x10_90_rise", "halfwidth",
      "rise50", "peak_direction", "burst", "burste", "x10_90slope", "rel_time"
    )

  for (i in file_list) {
    file <- paste(i, "ASC", sep = ".")
    temp <- utils::read.table(paste(path, file, sep = "/"),
      sep = "\t",
      col.names = asc_file_column_names
    )

    if (merge_iei == TRUE) {
      file_txt <- paste(i, "txt", sep = ".")
      temp_iei <- utils::read.table(paste(path, file_txt, sep = "/"),
        colClasses = c(NA, NA),
        col.names = c("iei", NA),
        sep = "\t", skip = 6
      )[1]
      temp_iei <- rbind(temp_iei, NA) # add NA value at the end to equalize size
      temp <- cbind(temp, temp_iei)
    }
    utils::write.csv(temp, file = file.path(path, "data_csv", paste(i, "csv",
      sep = "."
    )))
  }
}

#' Creates plot folders within the parent directory
#'
#' @description Automatic plotting functions save outputs, e.g. *.pdf, into these folders.
#'
#' @param experiments Character vector with types of experiments, i.e.
#'   experiment_types.
#'
#' @return System folders with the following names:
#' * \code{boxplots}
#' * \code{scatterplots}
#' * \code{histograms}
#' * \code{rasterplots}
#' * \code{heatplots}
#'
#' @note Deprecated, use tibble structure.
#'
#' @export
#' @md
create_project_folders <- function(experiments) {
  plot_types <- c(
    "boxplots", "scatterplots", "histograms", "rasterplots",
    "heatplots"
  )

  suppressWarnings(dir.create(paste(matools_env$directory_parent, "report",
    sep = "/"
  )))

  for (folder in c("figures", "output")) {
    for (i in experiments) {
      suppressWarnings(dir.create(paste(matools_env$directory_parent, folder, i,
        sep = "/"
      ), recursive = TRUE))

      if (folder == "figures") {
        path <- paste(matools_env$directory_parent, folder, i, sep = "/")
        for (type in plot_types) {
          suppressWarnings(dir.create(paste(path, type, sep = "/")))
        }
      }
    }
  }
}

#' Creates empty experiment summary arrays
#'
#' @description Used to store outputs from individual data files (.asc) as they
#'   are processed, allowing for grouped analysis and/or plotting functions at a
#'   later time. Used in conjunction with \link{fill_summary_array}.
#'
#' @param parameters The output from \code{\link{set_experiment_parameters}} Experiment parameters
#'   data frame.
#' @param y Character, user defined name(s) of output (i.e. "analysis_summary")
#' @param z Character, vector with types of experiments (i.e. matools_env$experiment_types).
#'
#' @return Nested empty lists segregated by experiment type with corresponding filenames. Created
#'   in the global environment.
#'
#' @seealso \link{fill_summary_array}
#'
#' @examples
#' \dontrun{
#' library(matools)
#'
#' summary_array_names <- c("analysis_summary", "spike_counts_all_cells")
#'
#' for (name in summary_array_names) {
#'   create_summary_array(
#'     matools_env$parameters,
#'     name,
#'     matools_env$experiment_types
#'   )
#' }
#'
#'
#' rm(list = summary_array_names)
#' }
#'
#' @export
#' @md
create_summary_array <- function(parameters, y, z) {
  filenames_by_experiment_type <- list()
  for (i in z) {
    filenames_by_experiment_type[[i]] <-
      rownames(parameters[which(parameters$experiment_id == i), ])
  }

  experiment_summary_array <- list()
  for (i in z) {
    experiment_summary_array[[i]] <-
      vector(
        "list",
        length(filenames_by_experiment_type[[i]])
      )
    names(experiment_summary_array[[i]]) <- c(filenames_by_experiment_type[[i]])
  }

  message("Summary array created in the global environment")
  assign(y, experiment_summary_array, envir = matools_env)
}

#' Fill summary array
#'
#' @description Used to store modified dataframes and/or analysis summaries in a
#'   nested list structure - allows the user to generate plots based on
#'   individual or groups of experiments. This is function is soft depreciated
#'
#' @param id Character, with Experiment id
#' @param filename Character, with single filename
#' @param event_summary Data Frame
#' @param array_blank Object, summary array to be filled. Generated using
#'   \code{create_summary_array}.
#' @param array_name String, name of output, e.g "analysis_summary"
#'
#' @return Appended nested lists segregated by experiment type with corresponding filenames.
#'   Revised in the global environment.
#'
#' @seealso \link{create_summary_array}
#'
#' @examples
#' \dontrun{
#' library(matools)
#'
#' summary_array_names <- c("analysis_summary", "spike_counts_all_cells")
#'
#' for (name in summary_array_names) {
#'   create_summary_array(
#'     matools_env$parameters,
#'     name,
#'     matools_env$experiment_types
#'   )
#' }
#'
#' fill_summary_array(
#'   matools_env$experiment_id,
#'   "file_name",
#'   df,
#'   analysis_summary,
#'   "analysis_summary"
#' )
#'
#'
#' rm(list = summary_array_names)
#' }
#'
#' @export
#' @md
fill_summary_array <- function(id,
                               filename,
                               event_summary,
                               array_blank,
                               array_name) {
  first_column_name <- colnames(event_summary[1])
  array_blank[[id]][filename] <- with(
    event_summary,
    split(
      event_summary,
      list(first_column_name)
    )
  )
  assign(array_name, array_blank, envir = matools_env)
}
