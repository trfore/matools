library("microbenchmark")
library("readr",
  include.only = c("col_double", "col_integer", "col_number", "read_tsv")
)

test_file_path <- system.file("extdata/demo_data",
  "cell1.ASC",
  package = "matools",
  mustWork = TRUE
)

test_file_iei_path <- system.file("extdata/demo_data",
  "cell1.txt",
  package = "matools",
  mustWork = TRUE
)

asc_file_column_names <-
  c(
    "ma_row",
    "rec_time_ms",
    "amplitude",
    "rise_ms",
    "decay_ms",
    "area",
    "baseline",
    "noise",
    "group",
    "channel",
    "x10_90_rise",
    "halfwidth",
    "rise50",
    "peak_direction",
    "burst",
    "burste",
    "x10_90slope",
    "rel_time"
  )

test_readtbl <- function(file_path, column_names) {
  temp <- read.table(file_path,
    sep = "\t",
    col.names = column_names
  )

  return(temp)
}

test_readdelim <- function(file_path, column_names) {
  temp <- read.delim(
    file_path,
    col.names = column_names,
    colClasses = c(
      "integer",
      "character", # "5,438.05" rec_time_ms
      "numeric",
      "numeric",
      "numeric",
      "character", # "18.22" area
      "numeric",
      "numeric",
      "integer",
      "integer",
      "numeric",
      "numeric",
      "numeric",
      "integer",
      "integer",
      "integer",
      "character", # "-400.50"  x10_90slope
      "numeric"
    )
  )
  return(temp)
}

test_fread <- function(file_path, column_names) {
  temp <- data.table::fread(file_path,
    col.names = column_names
  )
  return(temp)
}

#' @importFrom readr col_double col_integer col_number read_tsv
#' @importFrom dplyr bind_cols
test_tibble <- function(file_path, column_names, merge_iei = FALSE) {
  temp <- readr::read_tsv(
    file_path,
    col_names = column_names,
    col_types = list(
      ma_row = col_integer(),
      rec_time_ms = col_number(),
      amplitude = col_double(),
      rise_ms = col_double(),
      decay_ms = col_double(),
      area = col_number(),
      baseline = col_double(),
      noise = col_double(),
      group = col_integer(),
      channel = col_integer(),
      x10_90_rise = col_double(),
      halfwidth = col_double(),
      rise50 = col_double(),
      peak_direction = col_integer(),
      burst = col_integer(),
      burste = col_integer(),
      x10_90slope = col_number(),
      rel_time = col_double()
    )
  )

  if (merge_iei == TRUE) {
    temp_iei <- readr::read_tsv(
      test_file_iei_path,
      col_select = 1,
      col_names = "iei",
      col_types = list(iei = col_double()),
      skip = 6
    )
    temp_iei <-
      rbind(NA, temp_iei) # add NA value at the start to equalize size
    temp <- dplyr::bind_cols(temp, temp_iei)
  }

  return(temp)
}

microbenchmark(
  times = 50,
  unit = "ms",
  test_readtbl(test_file_path, asc_file_column_names),
  test_readdelim(test_file_path, asc_file_column_names),
  test_fread(test_file_path, asc_file_column_names),
  test_tibble(test_file_path, asc_file_column_names)
)
