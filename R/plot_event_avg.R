# plot_event_avg.R

#' @title Plot Event Value (Average)
#'
#' @description A highly abstracted plot that can be used to plot an average value within and across experiments. This
#'   is useful for examining features of synaptic events, e.g. amplitude, failure rate, potency, paired-pulse ratio.
#'   The plot requires pre-processing of the feature(s) of interest and a column with unique values to group by, i.e.
#'   experiment recording IDs.
#'
#' @param df Data frame or Tibble, grouped experiment data set.
#' @param x_axis Character - unquoted, non-standard evaluation. Column name to use for x-axis.
#' @param y_axis Character - unquoted, non-standard evaluation. Column name to use for y-axis.
#' @param plot_grouping Character, column name to use for grouping.
#' @param plot_mean Boolean, add group mean to plot. Default is \code{FALSE}.
#' @param weighted_mean Boolean, plot weighted mean, requires df column labelled \code{'w'} of weights \eqn{w}.
#'   Default is \code{FALSE}.
#' @param sd_value Optional character, std deviation measurement to use, accepts "sd" or "sem". Default is \code{NULL}.
#' @param experiment_name Optional character, add a subtitle with experiment name. Default is \code{NULL}.
#' @param x_label Optional character, custom label for x-axis. Default \code{NULL}, will use the x_axis column name.
#' @param y_label Optional character, custom label for y-axis. Default \code{NULL}, will use the y_axis column name.
#' @param ymax Optional integer, set a y-axis limit. Default is \code{NULL} and the y-axis will be automatically
#'   determined by ggplot.
#' @param ... Optional vargs for additional ggplot function calls, e.g. \code{ylim()}, \code{ylab()}.
#'
#' @details
#' The weight column, \code{w}, is assumed to frequency weights - where a weight equals the number of occurrences.
#' Weighted mean and sd are calculated using the following formulas:
#'
#' * weighted mean:
#'   \eqn{\bar{x}=\frac{\sum\limits_{i=1}^n w_i x_i}{\sum\limits_{i=1}^n w_i}}
#' * weighted sd: \eqn{\sqrt{\hat{\sigma}^2_\mathrm{w}}}
#' * unbiased variance:
#'   \eqn{\hat{\sigma}^2_\mathrm{w}=\frac{\sum\limits_{i=1}^N w_i \left(x_i - \mu^{*}\right)^2 }{\sum_{i=1}^N w_i - 1}}
#'
#' @return
#' A ggplot2 object with user defined features.
#'
#' @references
#' * \href{https://en.wikipedia.org/wiki/weighted_arithmetic_mean}{Wikipedia: Weighted arithmetic mean}
#' * \href{https://github.com/harrelfe/Hmisc/issues/22#issuecomment-94307602}{GitHub: harrelfe/Hmisc - issues #22}
#'
#' @seealso
#' * [stats::weighted.mean()]
#' * [Hmisc::wtd.var()]
#'
#' @examples
#' simple_tbl <-
#'   data.frame(
#'     recording_id = rep(1:2, each = 3),
#'     experiment_id = rep("example", 6),
#'     condition = rep(c("control", "drug_1", "drug_2"), 2),
#'     amplitude = c(1, 0.5, 1, 2, 1, 2)
#'   )
#'
#' plot_event_avg(
#'   df = simple_tbl,
#'   x_axis = condition,
#'   y_axis = amplitude,
#'   plot_grouping = "recording_id",
#'   plot_mean = TRUE,
#'   sd_value = "sem",
#'   experiment_name = "example",
#'   ymax = 3
#' ) + ggplot2::coord_fixed(ratio = 3)
#'
#' # remove average and add custom labels
#' plot_event_avg(
#'   df = simple_tbl,
#'   x_axis = condition,
#'   y_axis = amplitude,
#'   plot_grouping = "recording_id",
#'   plot_mean = FALSE,
#'   experiment_name = "example",
#'   x_label = "custom x-label",
#'   y_label = "custom y-label",
#'   ymax = 3
#' )
#'
#' # plot a weighted average
#' weighted_tbl <-
#'   data.frame(
#'     recording_id = rep(1:2, each = 3),
#'     experiment_id = rep("example", 6),
#'     condition = rep(c("control", "drug_1", "drug_2"), 2),
#'     amplitude = c(1, 0.5, 1, 2, 1, 2),
#'     w = rep(c(1, 100), each = 3)
#'   )
#'
#' plot_event_avg(
#'   df = weighted_tbl,
#'   x_axis = condition,
#'   y_axis = amplitude,
#'   plot_grouping = "recording_id",
#'   plot_mean = TRUE,
#'   weighted_mean = TRUE,
#'   sd_value = "sd",
#'   experiment_name = "plot weighted mean",
#'   x_label = "condition",
#'   y_label = "amplitude",
#'   ymax = 3,
#' )
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_errorbar labs xlab ylab ylim coord_fixed scale_y_continuous
#' @importFrom ggplot2 theme_minimal theme element_blank element_line element_text
#' @importFrom rlang .data enquo
#' @importFrom dplyr group_by summarise
#' @importFrom Hmisc wtd.mean wtd.var
#' @export
#' @md
plot_event_avg <- function(df,
                           x_axis,
                           y_axis,
                           plot_grouping,
                           plot_mean = FALSE,
                           weighted_mean = FALSE,
                           sd_value = NULL,
                           experiment_name = NULL,
                           x_label = NULL,
                           y_label = NULL,
                           ymax = NA,
                           ...) {
  x_axis <- rlang::enquo(x_axis)
  y_axis <- rlang::enquo(y_axis)
  n_count <- nrow(unique(df[plot_grouping]))

  plot_base <-
    (ggplot2::ggplot(mapping = ggplot2::aes(!!x_axis, !!y_axis, group = .data[[plot_grouping]])) +
      ggplot2::geom_line(
        data = df,
        colour = "grey",
        linetype = "dashed",
        size = 0.25
      ) +
      ggplot2::geom_point(
        data = df,
        shape = 21,
        size = 1,
        stroke = 0.5,
        colour = "grey",
        fill = "white"
      ) +
      ggplot2::labs(
        title = experiment_name,
        subtitle = paste("n =", n_count),
        caption = "" # blank to preserve spacing when 'plot_mean' is toggled
      ) +
      {
        if (!is.null(x_label)) ggplot2::xlab(x_label)
      } +
      {
        if (!is.null(y_label)) ggplot2::ylab(y_label)
      } +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, ymax)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black", size = 0.25),
        axis.ticks = ggplot2::element_line(size = 0.25),
        axis.text = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        text = ggplot2::element_text(size = 8)
      ))

  if (plot_mean) {
    if (weighted_mean == TRUE && !("w" %in% colnames(df))) {
      stop("column of weights not found in df! 'weighted_mean' is set to TRUE set to FALSE, or add weight column 'w'.")
    } else if (weighted_mean == FALSE && !("w" %in% colnames(df))) {
      # add equal weights when column is missing, as `case_when` always evaluates the 'w' column and will throw an error
      # if not present
      df$w <- 1
    }

    # case_when always evaluates the 'w' column and will throw an error if not present
    df_avg <-
      df %>%
      dplyr::group_by(!!x_axis) %>%
      dplyr::summarise(
        avg = case_when(weighted_mean == TRUE ~ weighted.mean(!!y_axis, w), .default = mean(!!y_axis)),
        sd = case_when(weighted_mean == TRUE ~ sqrt(Hmisc::wtd.var(!!y_axis, w)), .default = sd(!!y_axis)),
        sem = sd(!!y_axis) / sqrt(length(!!y_axis))
      )
    df_avg[[plot_grouping]] <- "group_average"

    plot_base <- plot_base +
      ggplot2::geom_line(
        data = df_avg, mapping = ggplot2::aes(y = .data$avg),
        colour = "black", linetype = "solid",
        size = 0.25
      ) +
      ggplot2::geom_point(
        data = df_avg, mapping = ggplot2::aes(y = .data$avg),
        shape = 21, size = 1, stroke = 0.5, colour = "black",
        fill = "black"
      )

    if (!is.null(sd_value)) {
      if (!sd_value %in% c("sd", "sem")) {
        stop("Incorrect 'sd_value' passed, please use 'sd' or 'sem'")
      }

      plot_base <- plot_base +
        ggplot2::geom_errorbar(
          data = df_avg, width = 0.2, size = 0.25,
          ggplot2::aes(
            y = .data$avg,
            ymin = .data$avg - .data[[sd_value]],
            ymax = .data$avg + .data[[sd_value]]
          )
        ) +
        ggplot2::labs(
          caption = paste(
            case_when(weighted_mean == TRUE ~ "weighted mean", .default = "mean"), "\u00B1", {{ sd_value }}
          )
        )
    }
  }

  return(plot_base)
}
