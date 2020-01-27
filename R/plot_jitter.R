# plot_jitter.R

#' @title Plot Spike Jitter
#'
#' @param df Data frame or Tibble, a single experiment data set with the following columns:
#'   * condition
#'   * stimulus
#'   * jitter
#'   * event_index
#' @param group_value Character, column name to use for grouping. Default is \code{"condition"}.
#' @param ymax Optional integer, maximum y-value to plot in milliseconds (ms). Default is \code{8}.
#' @param sd_value Optional character, std deviation measurement to use, accepts "sd" or "sem". Default is \code{NULL}.
#' @param ... Optional vargs, to apply additional ggplot function calls, e.g. \code{ylim()}, \code{ylab()}.
#'
#' @return
#' A ggplot2 object with the following features:
#'
#' * X-axis of binned events by stimulus value.
#' * Y-axis of action potential latency (jitter) in milliseconds.
#'
#' @examples
#' n <- 100 # sweeps
#' n_stim <- 2 # stimuli per sweep
#' conditions <- c("control", "drug_1")
#' n_conditions <- length(conditions)
#'
#' simple_tbl <-
#'   data.frame(
#'     experiment_id = rep("example", ((n * n_stim * n_conditions))),
#'     condition = rep(conditions, rep((n * n_stim), n_conditions)),
#'     stimulus = rep(c(1:n_stim), n * n_conditions),
#'     jitter = rep(rnorm((n * n_stim), 2, 1), n_conditions),
#'     event_index = rep(1, ((n * n_stim * n_conditions)))
#'   )
#'
#' plot_jitter(
#'   simple_tbl,
#'   ymax = 4,
#'   sd_value = "sd"
#' )
#'
#' # change the default color scheme ----
#' plot_jitter(
#'   simple_tbl,
#'   group_value = "condition",
#'   ymax = 4,
#'   sd_value = "sem",
#'   ggplot2::scale_color_manual(values = c("black", "blue"))
#' )
#'
#' @importFrom dplyr group_by filter summarise n
#' @importFrom ggplot2 ggplot aes geom_point xlab scale_y_continuous geom_errorbar position_dodge
#' @importFrom ggplot2 theme theme_minimal element_blank element_line element_text unit
#' @importFrom rlang .data ensym
#' @importFrom stats na.omit sd
#' @export
#' @md
plot_jitter <- function(df,
                        group_value = "condition",
                        ymax = 8,
                        sd_value = NULL,
                        ...) {
  group_value <- rlang::ensym(group_value)
  stimulus_num <- as.numeric(na.omit(unique(df$stimulus)))

  # NOTE: na.rm is redundant in the summary calculation, as the filter on event_index will remove 'na' values
  plot_data <-
    df %>%
    dplyr::group_by(.data$experiment_id, !!group_value, .data$stimulus) %>%
    dplyr::filter(.data$event_index == 1 & .data$stimulus %in% stimulus_num) %>%
    dplyr::summarise(
      mean = mean(.data$jitter, na.rm = TRUE),
      sd = sd(.data$jitter, na.rm = TRUE),
      sem = sd(.data$jitter, na.rm = TRUE) / sqrt(dplyr::n())
    )

  plot_base <-
    ggplot2::ggplot(plot_data, ggplot2::aes(.data$stimulus, mean, color = !!group_value)) +
    ggplot2::geom_point(
      size = 1.25,
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::xlab("stimulus") +
    ggplot2::scale_y_continuous(
      "first spike latency (ms)",
      breaks = seq(1, ymax, 1),
      labels = seq(1, ymax, 1),
      limits = c(0, ymax)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      legend.position = c(0.95, 0.9),
      legend.key.size = ggplot2::unit(10, units = "points"),
      axis.line = ggplot2::element_line(colour = "black", size = 0.25),
      axis.ticks = ggplot2::element_line(size = 0.25),
      axis.text = ggplot2::element_text(size = 8),
      text = ggplot2::element_text(size = 8)
    )

  # Add SE value to plot
  if (!is.null(sd_value)) {
    if (!sd_value %in% c("sd", "sem")) {
      stop("Incorrect 'sd_value' passed, please use 'sd' or 'sem'")
    }

    sd_value <- rlang::ensym(sd_value)
    plot_base <-
      plot_base +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = mean - !!sd_value,
          ymax = mean + !!sd_value
        ),
        size = 0.2,
        width = 0.4,
        position = ggplot2::position_dodge(width = 0.5)
      )
  }

  if (length(list(...)) > 0) {
    for (i in list(...)) {
      plot_base <- plot_base + i
    }
  }

  return(plot_base)
}
