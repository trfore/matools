# plot_probability.R

#' @title Plot Event Probability Over Time
#'
#' @description
#' Compliments \code{create_histogram_df()} to generate P() plots over time.
#'
#' @param df Data Frame or Tibble with the following columns:
#' * h_bin_centers
#' * P() metric from \code{create_histogram_df()}, e.g. density, probability.
#' @param plot_stim_times Integer, stimulus times in milliseconds.
#' @param plot_value Character, \code{df} column to use for plotting. Defaults to
#'   "probability".
#' @param xmax Optional integer, maximum x-value to plot in milliseconds (ms), values are rounded to
#'   the nearest 10. Default is \code{40}, xmin is fixed at \code{0}.
#' @param ymax Optional integer, maximum y-value to plot, values are rounded the nearest 1. Default
#'   is \code{1}, ymin is fixed at \code{0}.
#' @param y_label Optional character, custom label for the main y-axis. Default \code{NULL}, will use
#'   the column name from \code{plot_value}.
#' @param df_inset Optional Data Frame, if present a inset is added to the plot in the upper right
#'   corner. Default is \code{NULL}.
#' @param sd_value Character, \code{df} column to use for plotting error bars. Default is
#'   \code{NULL}.
#' @param manual_condition_color Character, vector of color values. Default is \code{NULL}, which
#'    will automatically generating color values.
#' @param ... Optional vargs, to apply additional ggplot function calls, e.g. \code{ylim()},
#'   \code{ylab()}. If \code{df_inset} is \code{NULL}, see note below.
#'
#' @return ggplot2 object
#'
#' @note When adding an inset to the plot, if you want to add additional ggplot function calls, you
#'   must add these using the varg, \code{...}. As this function will return a \code{grob}
#'   object. However, if the \code{df_inset} arg is left at the default value of \code{NULL},
#'   then a ggplot object is returned and additional ggplot function calls can added post hoc, e.g.
#'   \code{plot_probability() + ylab("y-axis label")}.
#'
#' @seealso
#' * [create_histogram_df()]
#' * [plot_max_probability()]
#'
#' @examples
#' library(magrittr, include.only = "%>%")
#'
#' # example setup
#' bins <- seq(0.5, 39.5, by = 1)
#' n_bins <- length(bins)
#' conditions <- c("control", "drug_1")
#' n_conditions <- length(conditions)
#'
#' # example output from create_histogram_df()
#' simple_tbl <-
#'   tibble::tibble(
#'     condition = rep(conditions, each = n_bins),
#'     h_bin_centers = rep(bins, n_conditions),
#'     probability = 0
#'   ) %>%
#'   dplyr::mutate(probability = dplyr::case_when(
#'     condition == "control" & h_bin_centers %in% c(1.5, 11.5) ~ 1,
#'     condition == "drug_1" & h_bin_centers %in% c(1.5, 11.5) ~ 0.5,
#'     .default = 0
#'   ))
#'
#' plot_probability(
#'   df = simple_tbl,
#'   plot_stim_times = c(0, 10, 20),
#'   plot_value = "probability"
#' )
#'
#' @importFrom ggplot2 ggplot aes xlab ylab geom_line geom_vline geom_errorbar
#' @importFrom ggplot2 theme theme_minimal element_blank element_line element_text unit
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous ggplotGrob annotation_custom
#' @importFrom grDevices pdf
#' @importFrom plyr round_any
#' @importFrom rlang .data ensym
#' @export
#' @md
plot_probability <- function(df,
                             plot_stim_times,
                             plot_value = "probability",
                             xmax = 40,
                             ymax = 1,
                             y_label = NULL,
                             df_inset = NULL,
                             sd_value = NULL,
                             manual_condition_color = NULL,
                             ...) {
  # set condition_names and color values
  condition_names <- c(unique(df$condition), if (!is.null(df_inset)) unique(df_inset$condition))

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

  if (!is.null(df_inset)) {
    condition_names_inset <- unique(df_inset$condition)
    n_conditions_inset <- length(condition_names_inset)
    df_inset$condition <- factor(df_inset$condition, levels = condition_names)

    main_colors <- head(condition_colors, -n_conditions_inset)
    inset_colors <- tail(condition_colors, n_conditions_inset)
  } else {
    main_colors <- condition_colors
  }

  df$condition <- factor(df$condition, levels = unique(condition_names))

  # set axis values
  xmax <- plyr::round_any(xmax, 10)
  xaxis_breaks <- seq(from = 0, to = xmax, by = 10)
  xaxis_labels <- xaxis_breaks
  xaxis_labels[c(FALSE, TRUE)] <- ""

  ymax <- plyr::round_any(ymax, 1)
  yaxis_breaks <- seq(from = 0, to = ymax, by = 0.5)
  yaxis_labels <- yaxis_breaks
  yaxis_labels[c(FALSE, TRUE)] <- ""

  plot_value <- ensym(plot_value)
  plot_base <-
    ggplot2::ggplot(
      df %>% dplyr::group_by(.data$condition),
      ggplot2::aes(.data$h_bin_centers, !!plot_value, color = .data$condition)
    ) +
    ggplot2::xlab("time (ms)") +
    {
      if (!is.null(y_label)) ggplot2::ylab(y_label)
    } +
    ggplot2::geom_line(size = 0.2, inherit.aes = TRUE) +
    ggplot2::scale_x_continuous(
      breaks = xaxis_breaks,
      labels = xaxis_labels,
      limits = c(0, xmax)
    ) +
    ggplot2::scale_y_continuous(
      breaks = yaxis_breaks,
      labels = yaxis_labels,
      limits = c(0, ymax)
    ) +
    ggplot2::geom_vline(xintercept = plot_stim_times, color = "grey", size = 0.4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black", size = 0.25),
      axis.ticks = ggplot2::element_line(size = 0.25),
      axis.text = ggplot2::element_text(size = 6),
      text = ggplot2::element_text(size = 6),
      legend.position = "none"
    ) +
    ggplot2::scale_color_manual(values = main_colors)

  if (!is.null(df_inset)) {
    plot_inlay <-
      ggplot2::ggplot(
        df_inset,
        aes(.data$h_bin_centers, !!plot_value, color = .data$condition)
      ) +
      ggplot2::geom_line(size = 0.2) +
      ggplot2::scale_x_continuous(
        breaks = xaxis_breaks,
        labels = xaxis_labels,
        limits = c(0, xmax)
      ) +
      ggplot2::scale_y_continuous(
        breaks = yaxis_breaks,
        labels = yaxis_labels,
        limits = c(0, ymax)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(color = "black", size = 0.25),
        axis.ticks = ggplot2::element_line(size = 0.25),
        axis.text.x = ggplot2::element_text(vjust = 2),
        axis.ticks.length = ggplot2::unit(0.5, "mm"),
        axis.text = ggplot2::element_text(size = 6),
        text = ggplot2::element_text(size = 6),
        legend.position = "none"
      ) +
      ggplot2::scale_color_manual(values = inset_colors)
  }

  if (!is.null(sd_value)) {
    sd_value <- rlang::ensym(sd_value)

    plot_base <-
      plot_base +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = !!plot_value - !!sd_value,
          ymax = !!plot_value + !!sd_value
        ),
        size = 0.2,
        width = 0.4
      )

    if (!is.null(df_inset)) {
      plot_inlay <-
        plot_inlay +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = !!plot_value - !!sd_value,
            ymax = !!plot_value + !!sd_value
          ),
          size = 0.2,
          width = 0.4
        )
    }
  }

  if (length(list(...)) > 0) {
    for (i in list(...)) {
      plot_base <- plot_base + i
      if (!is.null(df_inset)) {
        plot_inlay <- plot_inlay + i
      }
    }
  }

  if (!is.null(df_inset)) {
    plot_inlay_grob <- ggplot2::ggplotGrob(plot_inlay)
    plot_base <-
      plot_base +
      ggplot2::annotation_custom(plot_inlay_grob,
        xmin = 25, xmax = Inf,
        ymin = 0.5, ymax = Inf
      )
  }

  plot_labels <- data.frame(
    condition = condition_names,
    y = seq(from = 0.5, by = -0.1, length.out = length(condition_names))
  )
  plot_base <-
    plot_base +
    ggplot2::geom_text(
      data = plot_labels,
      ggplot2::aes(
        x = 35, .data$y,
        label = .data$condition
      ),
      size = 2.0,
      color = condition_colors
    )

  if (!interactive()) grDevices::pdf(NULL)
  return(plot_base)
}

#' @title Plot Max Probability Across Experiments
#'
#' @description
#' This will plot the maximum value specified by \code{metric} across two conditions.
#'
#' @param df Data Frame or Tibble with the following columns:
#' * cell_id
#' * experiment_id
#' * condition
#' * P() metric from \code{create_histogram_df()}, e.g. density, probability.
#' @param x_axis_condition Character, condition value to use on x-axis, e.g. "control".
#' @param y_axis_condition Character, condition value to use on y-axis, e.g. "drug".
#' @param metric Character, P() metric to plot, e.g. density, probability. Defaults to
#'   "probability".
#' @param xmax Optional integer, maximum x-value to plot, values are rounded the nearest 1. Default
#'   is \code{1}, ymin is fixed at \code{0}.
#' @param ymax Optional integer, maximum y-value to plot, values are rounded the nearest 1. Default
#'   is \code{1}, ymin is fixed at \code{0}.
#' @param x_label Optional character, custom label for the x-axis. Default \code{NULL}, will use the
#'   column name from \code{x_axis_condition}.
#' @param y_label Optional character, custom label for the y-axis. Default \code{NULL}, will use the
#'   column name from \code{y_axis_condition}.
#'
#' @return ggplot2 object
#'
#' @note This function is intended to use with aggregated data in a long format that is extracted
#'   from \code{create_histogram_df}.
#'
#' @seealso
#' * [create_histogram_df()]
#' * [plot_probability()]
#' * [analysis_evoked_ap()] for a workflow example that aggregates create_histogram_df.
#'
#' @examples
#' library(magrittr, include.only = "%>%")
#'
#' bins <- seq(0.5, 39.5, by = 1)
#' n_bins <- length(bins)
#' conditions <- c("control", "drug")
#' n_conditions <- length(conditions)
#'
#' # example output from create_histogram_df()
#' simple_tbl <-
#'   tibble::tibble(
#'     cell_id = "cell",
#'     experiment_id = "example",
#'     condition = rep(conditions, each = n_bins),
#'     h_bin_centers = rep(bins, n_conditions),
#'     probability = 0,
#'     density = 1
#'   ) %>%
#'   dplyr::mutate(probability = dplyr::case_when(
#'     condition == "control" & h_bin_centers %in% c(1.5, 11.5) ~ 0.75,
#'     condition == "drug" & h_bin_centers %in% c(1.5, 11.5) ~ 0.5,
#'     .default = 0
#'   ))
#'
#' plot_max_probability(
#'   simple_tbl,
#'   x_axis_condition = "control",
#'   y_axis_condition = "drug",
#'   metric = "probability",
#'   xmax = 1,
#'   ymax = 1
#' )
#'
#' @importFrom dplyr group_by filter select summarise
#' @importFrom ggplot2 ggplot aes xlab ylab geom_abline geom_point coord_cartesian scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 theme theme_minimal element_blank element_line element_text
#' @importFrom magrittr %>%
#' @importFrom rlang .data ensym
#' @importFrom tidyr spread
#' @export
#' @md
plot_max_probability <- function(df,
                                 x_axis_condition,
                                 y_axis_condition,
                                 metric = "probability",
                                 xmax = 1,
                                 ymax = 1,
                                 x_label = NULL,
                                 y_label = NULL) {
  # set axis values
  xmax <- plyr::round_any(xmax, 1)
  xaxis_breaks <- seq(from = 0, to = xmax, by = 0.25)
  xaxis_labels <- xaxis_breaks
  xaxis_labels[c(FALSE, TRUE)] <- ""

  ymax <- plyr::round_any(ymax, 1)
  yaxis_breaks <- seq(from = 0, to = ymax, by = 0.25)
  yaxis_labels <- yaxis_breaks
  yaxis_labels[c(FALSE, TRUE)] <- ""

  axis_conditions <- c(x_axis_condition, y_axis_condition)

  required_cols <- c("cell_id", "experiment_id", "condition", metric)
  if (!all(required_cols %in% colnames(df))) {
    stop("df missing columns, see 'plot_max_probability' help and try using
         'create_histogram_df' first.")
  } else if (!all(axis_conditions %in% df$condition)) {
    missing <- which(axis_conditions %in% df$condition == FALSE)
    stop("axis_condition: ", axis_conditions[missing], " not found in 'df'")
  }

  df_max_prob <-
    df %>%
    dplyr::group_by(.data$cell_id, .data$condition, .data$experiment_id) %>%
    dplyr::filter(.data$condition %in% axis_conditions) %>%
    dplyr::select("cell_id", "experiment_id", "condition", !!metric) %>%
    dplyr::summarise(max_value = max(unlist(.data[[metric]])))

  x_axis_condition <- rlang::ensym(x_axis_condition)
  y_axis_condition <- rlang::ensym(y_axis_condition)

  plot_max_prob <-
    ggplot2::ggplot(
      tidyr::spread(df_max_prob, .data$condition, .data$max_value),
      ggplot2::aes(!!x_axis_condition, !!y_axis_condition)
    ) +
    ggplot2::geom_abline(
      intercept = 0, slope = 1, linetype = 2,
      size = .2
    ) +
    ggplot2::geom_point(
      shape = 19, size = 1.5, stroke = 0.4,
      ggplot2::aes(color = experiment_id)
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, xmax), ylim = c(0, ymax),
      expand = TRUE, clip = "off"
    ) +
    {
      if (!is.null(x_label)) ggplot2::xlab(x_label)
    } +
    {
      if (!is.null(y_label)) ggplot2::ylab(y_label)
    } +
    ggplot2::scale_x_continuous(
      breaks = xaxis_breaks,
      labels = xaxis_labels,
      limits = c(0, xmax)
    ) +
    ggplot2::scale_y_continuous(
      breaks = yaxis_breaks,
      labels = yaxis_labels,
      limits = c(0, ymax)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black", size = 0.25),
      axis.ticks = ggplot2::element_line(size = 0.25),
      axis.text = ggplot2::element_text(size = 6),
      text = ggplot2::element_text(size = 6)
    )

  return(plot_max_prob)
}
