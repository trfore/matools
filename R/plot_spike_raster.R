# plot_spike_raster.R

utils::globalVariables(c("ymin"))

#' @title Generate Spike Raster Plot
#'
#' @param df Data Frame or Tibble with the following columns:
#' * sweep
#' * time_ms
#' * condition
#' @param filename Character, a single file name.
#' @param id Character, experiment id.
#' @param time_of_stim List, stimulus times in milliseconds.
#' @param condition_names Character vector with condition titles.
#' @param auto_window Boolean, automatically set the x-axis values based upon the first and last
#'   stimuli. Default is \code{TRUE} and will use the args \code{auto_window_size} and
#'   \code{auto_window_symmetric} to set the x-axis. If \code{FALSE}, the entire x-axis is plotted
#'   using the values c(0, \code{NA}).
#' @param auto_window_size Integer, the amount of time in milliseconds to plot after the last
#'   stimulus. Default is \code{10} ms.
#' @param auto_window_symmetric Boolean, will set the x-axis to plot \code{auto_window_size} before
#'   and after the first and last stimuli. Default is \code{FALSE}, and will start the x-axis at the
#'   first stimuli.
#' @param manual_condition_color Character, vector of color values. Default is \code{NULL}, which
#'    will force arg \code{auto_condition_color} to \code{TRUE}, automatically generating color
#'    values.
#' @param plot_conditions_only Boolean, Default is \code{TRUE}.
#'
#' @return ggplot object
#'
#' @seealso
#' * \link{standardize_event_time} to auto create \code{time_ms} column.
#' * \link{ggplot_color_hue} to see how 'auto_condition_color' is determined.
#' * \link{condition_sweep_lut}
#'
#' @examples
#' conditions <- c("control", "drug_1", "drug_2")
#' simple_df <-
#'   data.frame(
#'     sweep = rep(1:120, each = 3),
#'     time_ms = rep(c(280, 290, 300), times = 120),
#'     condition = rep(conditions, each = 120)
#'   )
#'
#' plot_spike_raster(
#'   df = simple_df,
#'   filename = "example",
#'   id = "experiment_id",
#'   time_of_stim = c(278.18, 288.18, 298.18),
#'   condition_names = conditions,
#'   auto_window = TRUE,
#'   auto_window_size = 30,
#'   auto_window_symmetric = FALSE,
#'   plot_conditions_only = TRUE
#' )
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes labs xlim scale_y_reverse geom_rect geom_point geom_vline geom_text theme_minimal
#' @importFrom ggplot2 xlim geom_hline geom_point scale_y_continuous theme_classic
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats complete.cases
#' @importFrom utils head tail
#' @export
#' @md
plot_spike_raster <- function(df,
                              filename,
                              id,
                              time_of_stim,
                              condition_names,
                              auto_window = TRUE,
                              auto_window_size = 10,
                              auto_window_symmetric = FALSE,
                              manual_condition_color = NULL,
                              plot_conditions_only = TRUE) {
  if (is.null(manual_condition_color)) {
    condition_colors <- c(
      "black",
      ggplot_color_hue(length(condition_names) - 1)
    )
  } else {
    condition_colors <- manual_condition_color

    if (length(manual_condition_color) != length(condition_names)) {
      error_mesg <- paste0(
        "the values in manual_condition_color (", length(manual_condition_color),
        ") does not match the number of conditions (", length(condition_names), ")"
      )
      stop(error_mesg, call. = FALSE)
    }
  }

  # plot x-axis size f()
  if (auto_window == TRUE && auto_window_symmetric == TRUE) {
    axis_xmin <-
      as.numeric(sub("\\.[0-9].", "", head(time_of_stim, n = 1))) - auto_window_size
    axis_xmax <-
      as.numeric(sub("\\.[0-9].", "", tail(time_of_stim, n = 1))) + auto_window_size
  } else if (auto_window == TRUE && auto_window_symmetric == FALSE) {
    axis_xmin <-
      as.numeric(sub("\\.[0-9].", "", head(time_of_stim, n = 1)))
    axis_xmax <-
      as.numeric(sub("\\.[0-9].", "", tail(time_of_stim, n = 1))) + auto_window_size
  } else {
    axis_xmin <- 0
    axis_xmax <- NA
  }

  if (plot_conditions_only) {
    df <-
      sequential_condition_sweeps(
        df = df,
        condition_names = condition_names
      ) %>%
      dplyr::filter(.data$condition %in% condition_names)
    lut <- condition_sweep_lut(df, condition_names, column_ref = "trial")
  } else {
    lut <- condition_sweep_lut(df, condition_names)
  }

  condition_windows <-
    data.frame(
      xmin = -Inf,
      xmax = Inf,
      ymin = lut$condition_start,
      ymax = lut$condition_end,
      condition = condition_names
    )

  if (plot_conditions_only) {
    plot_raster <-
      ggplot2::ggplot() +
      ggplot2::labs(
        title = filename,
        subtitle = id,
        x = "time (ms)",
        y = "trial"
      ) +
      ggplot2::xlim(axis_xmin, axis_xmax) +
      ggplot2::geom_hline(
        yintercept = condition_windows$ymin,
        color = "grey",
        linetype = "dotted"
      ) +
      ggplot2::geom_point(
        data = df,
        aes(.data$time_ms, .data$trial),
        shape = 20,
        size = .5,
        stroke = .4,
        color = "black"
      ) +
      ggplot2::geom_vline(
        xintercept = time_of_stim,
        color = "red"
      ) +
      ggplot2::geom_text(
        data = condition_windows,
        ggplot2::aes(
          x = (axis_xmax - 5), y = (ymin + 4.5),
          label = .data$condition
        ),
        size = 3.2,
        color = condition_colors
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, tail(condition_windows$ymax, 1), 30),
        trans = "reverse"
      ) +
      ggplot2::theme_classic()
  } else {
    plot_raster <-
      ggplot2::ggplot() +
      ggplot2::labs(
        title = filename,
        subtitle = id,
        x = "time (ms)",
        y = "sweep"
      ) +
      ggplot2::xlim(axis_xmin, axis_xmax) +
      ggplot2::scale_y_reverse() +
      ggplot2::geom_rect(
        data = condition_windows,
        mapping = ggplot2::aes(
          xmin = .data$xmin, xmax = .data$xmax,
          ymin = .data$ymin, ymax = .data$ymax
        ),
        fill = condition_colors, alpha = 0.4
      ) +
      ggplot2::geom_point(
        data = df,
        ggplot2::aes(.data$time_ms, .data$sweep),
        size = 0.1
      ) +
      ggplot2::geom_vline(xintercept = time_of_stim, color = "red") +
      ggplot2::geom_text(
        data = condition_windows,
        ggplot2::aes(
          x = (axis_xmax - 2), y = (.data$ymin + 4),
          label = .data$condition
        ),
        cex = 2.8,
        color = "blue"
      ) +
      ggplot2::theme_minimal()
  }
  return(plot_raster)
}
