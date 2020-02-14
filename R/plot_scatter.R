# plot_scatter.R

#' @title Scatter Plot Of Event Amplitudes Over Time
#'
#' @param df Data frame or Tibble, a single experiment data set with the following columns:
#'   * sweep
#'   * condition
#'   * amplitude
#'   * stimulus
#' @param sweep_duration Required integer, sweep duration in seconds that is used to convert the x-axis into minutes.
#' @param filename Optional character, a single file name to add to the plot title, defaults to "".
#' @param experiment_id Optional character, add a subtitle with experiment id, defaults to "".
#' @param monochrome Boolean, set all plot points to a default black color for all conditions. Default is \code{FALSE},
#'   which will use default ggplot2 color scheme for different conditions.
#' @param truncate_x Boolean, only plot points that occur during a condition. Default is \code{FALSE}, and all x-axis
#'   points will be plotted.
#' @param ymax Optional integer, set a y-axis limit. Default is \code{NULL} and the y-axis will be automatically
#'   determined by ggplot.
#' @param y_label Optional character, custom label for the y-axis. Default \code{NULL}, will use the
#'   df column name, "amplitude".
#'
#' @return
#' A ggplot2 object with the following features:
#'
#'  * X-axis of time (minutes)
#'  * Y-axis of event amplitude (pA)
#'
#' @examples
#' library(dplyr)
#'
#' n <- 198
#' simple_tbl <-
#'   data.frame(
#'     sweep = rep(1:n, each = 1),
#'     condition = rep(c("control", "wash", "drug_1", "wash", "drug_2", "wash"), each = n / 6),
#'     amplitude = rnorm(n, mean = 75, sd = 5),
#'     stimulus = rep(1, n)
#'   )
#'
#' simple_tbl <-
#'   simple_tbl %>%
#'   dplyr::mutate(amplitude = ifelse(condition == "drug_1", (amplitude - 50), amplitude))
#'
#' plot_scatterplot_amplitude(
#'   df = simple_tbl,
#'   filename = "file_01",
#'   experiment_id = "experiment",
#'   sweep_duration = 5,
#'   monochrome = FALSE,
#'   ymax = 100
#' )
#'
#' # Limit the x-axis to segments with a pharmacological condition ----
#' conditions_only_tbl <-
#'   simple_tbl %>%
#'   dplyr::filter(condition %in% c("control", "drug_1", "drug_2"))
#'
#' plot_scatterplot_amplitude(
#'   df = conditions_only_tbl,
#'   filename = "file_01",
#'   experiment_id = "experiment",
#'   sweep_duration = 5,
#'   monochrome = TRUE,
#'   truncate_x = TRUE,
#'   ymax = 100
#' )
#'
#' @importFrom dplyr distinct tally
#' @importFrom ggplot2 ggplot aes labs theme_minimal geom_point scale_y_continuous facet_wrap
#' @importFrom ggplot2 theme_minimal theme element_blank element_line element_text
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @md
plot_scatterplot_amplitude <- function(df,
                                       sweep_duration,
                                       filename = "",
                                       experiment_id = "",
                                       monochrome = FALSE,
                                       truncate_x = FALSE,
                                       ymax = NA,
                                       y_label = NULL) {
  df_na_replaced <- df
  df_na_replaced$amplitude[is.na(df_na_replaced$amplitude)] <- 0

  n_conditions <- length(unique(df$condition))

  amplitude_over_time <-
    ggplot2::ggplot(
      df_na_replaced,
      ggplot2::aes((as.numeric(.data$sweep) * sweep_duration / 60), .data$amplitude)
    ) +
    ggplot2::labs(
      x = "time (min)",
      # y = "amplitude (pA)",
      title = filename,
      subtitle = experiment_id
    ) +
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
      text = ggplot2::element_text(size = 10)
    )

  if (isTRUE(df_na_replaced %>% dplyr::distinct(.data$stimulus) %>% dplyr::tally() > 1)) {
    amplitude_over_time <-
      amplitude_over_time +
      ggplot2::geom_point(ggplot2::aes(shape = as.factor(.data$stimulus), colour = .data$condition)) +
      ggplot2::labs(shape = "stimulus")
  } else {
    amplitude_over_time <-
      amplitude_over_time +
      ggplot2::geom_point(
        {
          if (!monochrome) ggplot2::aes(colour = .data$condition)
        },
        shape = 21,
        size = 1.75,
        stroke = 0.4,
        fill = NA
      ) + {
        if (truncate_x) ggplot2::facet_wrap(~condition, ncol = n_conditions, scales = "free_x")
      }
  }
  return(amplitude_over_time)
}
