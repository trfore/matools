# plot_heatmap.R

#' @title Plot Heat Map of Event Amplitudes
#'
#' @param df  Data Frame or Tibble with the following columns:
#' * sweep
#' * amplitude
#' * condition
#' * stimulus
#' * event index
#' @param filename Character, name of the file.
#' @param condition_names Character vector with condition names.
#' @param max_amplitude Integer, maximum value to use for the heat map. If set less than the maximum
#'   amplitude in the data frame, the value will be changed and a warning is returned. Defaults to
#'   \code{NULL} and will automatically set the value to the df's max value.
#'
#' @details
#' A blank white space is used for missing events (amplitude == NA).
#'
#' @return ggplot object
#'
#' @examples
#' simple_df <-
#'   data.frame(
#'     sweep = rep(1:20, each = 2),
#'     amplitude = c(rnorm(20, 100, 20), rnorm(20, 50, 10)),
#'     condition = rep(c("control", "drug1"), rep(20, 2)),
#'     stimulus = rep(c(1:2), 20),
#'     event_index = rep(1, 40)
#'   )
#'
#' plot_heatmap(
#'   df = simple_df,
#'   filename = "example",
#'   condition_names = c("control", "drug1")
#' )
#'
#' @seealso
#' * \link{align_condition_sweeps}
#'
#' @importFrom dplyr filter group_by slice_head mutate select left_join
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradientn labs facet_wrap scale_x_discrete
#' @importFrom ggplot2 scale_y_reverse theme_minimal theme element_text element_blank unit
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang .data
#' @export
#' @md
plot_heatmap <- function(df,
                         filename,
                         condition_names,
                         max_amplitude = NULL) {
  hm_palette <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")), space = "Lab")

  if (is.null(max_amplitude)) {
    max_amplitude <- max(df["amplitude"])

    warning(paste(
      "plot_heatmap() 'max_amplitude' is NOT defined; 'max_amplitude' set to:",
      max_amplitude
    ), call. = FALSE)
  } else if (max_amplitude < max(df["amplitude"])) {
    max_amplitude <- max(df["amplitude"])

    warning(paste(
      "Warning: plot_heatmap() 'max_amplitude' is BELOW the data frame's max value;",
      "max_amplitude set to:", max_amplitude
    ), call. = FALSE)
  }

  df[which(is.na(df$event_index)), "event_index"] <- 1 # convert event_index=NA into 1
  df$condition_factor <- factor(df$condition, levels = condition_names)

  df <-
    align_condition_sweeps(df, condition_names) %>%
    dplyr::filter(.data$event_index == 1, na.rm = TRUE) %>%
    dplyr::filter(.data$condition %in% condition_names)

  # suppressWarnings is for the scale_x_discrete generated warning
  tmp <-
    suppressWarnings(ggplot2::ggplot(
      df %>% dplyr::filter(.data$event_index == 1, na.rm = TRUE),
      ggplot2::aes(x = .data$stimulus, y = .data$trial)
    ) +
      ggplot2::geom_tile(ggplot2::aes(fill = .data$amplitude), colour = "grey30", size = 0.25) +
      ggplot2::scale_fill_gradientn(
        name = "pA", na.value = "white",
        colours = hm_palette(255),
        limits = c(0.1, max_amplitude)
      ) +
      ggplot2::labs(
        title = filename,
        caption = paste("scale max:", max_amplitude, sep = " "),
        x = "stimulus",
        y = "trial"
      ) +
      ggplot2::facet_wrap(~condition_factor) +
      ggplot2::scale_x_discrete(limits = unique(df$stimulus[!is.na(df$stimulus)])) +
      ggplot2::scale_y_reverse(breaks = c(1, (seq(0, max(df$trial), 5)[-1]))) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        text = ggplot2::element_text(size = 8),
        legend.text = ggplot2::element_text(size = 8),
        strip.text.x = ggplot2::element_text(size = 8),
        strip.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(-1.25, "line")
      ))
  return(tmp)
}
