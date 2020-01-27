# plot_utils.R

#' @title Generate Color Hue List
#'
#' @description Manually generating color values allows for consistent
#'   assignment to individual experiment conditions across all df/experiments -
#'   independent of events occurring within a given condition. ggplot's auto
#'   color can assign different colors across df/experiments if a individual df
#'   is missing events.
#'
#' @param color_count Numeric, number of colors to generate
#'
#' @return Character vector with six digit hex color value
#'
#' @importFrom grDevices hcl
#' @md
ggplot_color_hue <- function(color_count) {
  hues <- seq(15, 375, length = color_count + 1)
  color_values <- hcl(h = hues, l = 65, c = 100)[1:color_count]

  return(color_values)
}
