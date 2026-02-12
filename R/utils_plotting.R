#' Plotting Utilities for pyShinyCell
#'
#' Core utility functions for creating consistent, themed visualizations
#' in pyShinyCell Shiny applications. Includes theming, data preparation,
#' and custom ggplot2 geometries.
#'
#' @keywords internal
#' @name plotting_utils
NULL

#' Apply Factoring and Color Scheme to Data
#'
#' Processes factor levels for a categorical variable and assigns colors
#' based on configuration settings. Integrates with scConf metadata.
#'
#' @param ggData data.frame. Input data frame.
#' @param inpConf data.table. Configuration (scConf) from app settings.
#' @param col Character. Name of column in ggData to factor.
#'   Default: "X"
#' @param inp Character. UI name matching inpConf$UI for color retrieval.
#' @param inpcols Character. Color scheme name ("default" uses fCL from config,
#'   otherwise uses color_generator).
#' @param inporder Character vector. Custom order for factor levels.
#'   If provided, data is subset to match order.
#'
#' @return List with three elements:
#'   - ggData: Ordered data.frame with factored column
#'   - gglvl: Character vector of factor levels (in order)
#'   - ggCol: Named character vector of colors (names = levels)
#'
#' @keywords internal
#' @noRd
doFactoring <- function(ggData, inpConf, col = "X", inp, inpcols, inporder = NULL) {
  if (class(ggData[[col]]) == "factor") {
    gglvl = levels(ggData[[col]])[levels(ggData[[col]]) %in% unique(ggData[[col]])]
    ggData[[col]] %<>% droplevels()
    ggData[[col]] %<>% factor(levels = gglvl)
  } else {
    gglvl = sort(unique(ggData[[col]]))
  }

  ggCol = switch(inpcols,
    "default" = strsplit(inpConf[UI == inp]$fCL, "\\|")[[1]],
    color_generator(inpcols, length(gglvl))
  )
  if (inpConf[UI == inp]$grp) {
    names(ggCol) = switch(inpcols,
      "default" = strsplit(inpConf[UI == inp]$fID, "\\|")[[1]],
      gglvl
    )
  }

  if (length(inporder) > 0) {
    gglvl = inporder
    ggData = ggData[ggData[[col]] %in% gglvl]
    ggData[[col]] %<>% factor(levels = gglvl)
    ggCol = ggCol[gglvl]
  }
  res = list(ggData, gglvl, ggCol)
  names(res) = c("ggData", "gglvl", "ggCol")
  return(res)
}

#' Extract Legend from ggplot2 Object
#'
#' Extracts the legend from a ggplot2 plot as a separate grob for
#' standalone display or arrangement with other plots.
#'
#' @param a.gplot ggplot object. A complete ggplot2 plot with legend.
#'
#' @return grob. The legend component for use with gridExtra or similar.
#'
#' @keywords internal
#' @noRd
g_legend <- function(a.gplot) {
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

#' pyShinyCell Standard ggplot2 Theme
#'
#' Creates a consistent, publication-ready theme for all pyShinyCell visualizations.
#' Customizable for font sizes, axis labels, and text angles.
#'
#' @param base_size Numeric. Base font size. Default: 24
#' @param XYval Logical. Show X and Y axis values/ticks? Default: TRUE
#' @param XYtitle Logical. Show X and Y axis titles? Default: TRUE
#' @param Xang Numeric. Angle (degrees) for X-axis text. Default: 0
#' @param XjusH Numeric. Horizontal justification for X-axis text (0-1).
#'   Default: 0.5 (center)
#'
#' @return ggplot2 theme object. Suitable for addition to ggplot objects.
#'
#' @details
#' Consistent styling across all pyShinyCell plots:
#' - Font: Avenir family (or system default fallback)
#' - Panel: White background with black border
#' - Legend: Bottom position
#' - Colors: Black axis text and lines
#'
#' @keywords internal
#' @noRd
sctheme <- function(base_size = 24, XYval = TRUE, XYtitle = TRUE,
                    Xang = 0, XjusH = 0.5) {
  oupTheme = ggplot2::theme(
    text = ggplot2::element_text(size = base_size, family = "Avenir"),
    panel.background = ggplot2::element_rect(fill = "white", colour = "#000000"),
    axis.line = ggplot2::element_line(colour = "black"),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = base_size / 20),
    axis.title = ggplot2::element_text(colour = "black"),
    axis.text = ggplot2::element_text(size = base_size),
    axis.text.x = ggplot2::element_text(angle = Xang, hjust = XjusH),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = base_size),
    legend.key = ggplot2::element_rect(colour = NA, fill = NA)
  )
  if (!XYval) {
    oupTheme = oupTheme + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }
  if (!XYtitle) {
    oupTheme = oupTheme + ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
  }
  return(oupTheme)
}

#' Draw Rectangle on ggplot2 Plot
#'
#' Utility function to add a rectangle outline to a ggplot2 plot.
#' Used for highlighting regions in scatter plots or selections.
#'
#' @param x_left Numeric. Left X coordinate. Default: -Inf
#' @param x_right Numeric. Right X coordinate. Default: Inf
#' @param y_bottom Numeric. Bottom Y coordinate. Default: -Inf
#' @param y_top Numeric. Top Y coordinate. Default: Inf
#' @param linetype Character. Line type for rectangle edges.
#'   Default: "dotted"
#' @param ... Additional parameters passed to geom_segment.
#'
#' @return List of four geom_segment objects (top, right, bottom, left).
#'
#' @details
#' Returns a list of ggplot2 geom layers suitable for addition with `+`.
#' Each side of the rectangle is drawn as a separate line segment.
#'
#' @keywords internal
#' @noRd
rectangle <- function(x_left = -Inf, x_right = Inf, y_bottom = -Inf, y_top = Inf,
                      linetype = "dotted", ...) {
  list(
    ggplot2::geom_segment(ggplot2::aes(
      x = x_left, xend = x_right, y = y_top, yend = y_top
    ), linetype = linetype, ...),
    ggplot2::geom_segment(ggplot2::aes(
      x = x_right, xend = x_right, y = y_top, yend = y_bottom
    ), linetype = linetype, ...),
    ggplot2::geom_segment(ggplot2::aes(
      x = x_left, xend = x_right, y = y_bottom, yend = y_bottom
    ), linetype = linetype, ...),
    ggplot2::geom_segment(ggplot2::aes(
      x = x_left, xend = x_left, y = y_top, yend = y_bottom
    ), linetype = linetype, ...)
  )
}

#' Extract Brush-Selected Data from Plot
#'
#' Extracts cells/points selected by a Shiny plot brush from a data frame.
#' Handles both fresh brushes and cached brush data from tempData.
#'
#' @param ggData data.frame or data.table. Input data with X and Y columns.
#' @param plot_brush Object. Brush object from Shiny input (plotOutput with
#'   brush parameter). Contains xmin, xmax, ymin, ymax. Can be NULL.
#'
#' @return data.frame or data.table. Subset of ggData within brush bounds.
#'   Empty result if no brush or no data in brush area.
#'
#' @details
#' When plot_brush is NULL, checks for cached brush data in
#' tempData/plot_brush_tmp.rds (allows reactivity across UI elements).
#' Updates cache when new brush provided.
#'
#' Requires columns X and Y in ggData.
#'
#' @keywords internal
#' @noRd
tableBrush <- function(ggData, plot_brush) {
  area = FALSE
  if (is.null(plot_brush) &
    file.exists("tempData/plot_brush_tmp.rds")) {
    plot_brush = readRDS(file = "tempData/plot_brush_tmp.rds")
  }
  if (!is.null(plot_brush)) {
    area = (ggData[, X] > plot_brush$xmin) &
      (ggData[, X] < plot_brush$xmax) &
      (ggData[, Y] > plot_brush$ymin) &
      (ggData[, Y] < plot_brush$ymax)
    # save coordinates inputs
    if (!dir.exists("tempData")) dir.create("tempData")
    saveRDS(plot_brush, file = "tempData/plot_brush_tmp.rds")
    area = (ggData[, X] > plot_brush$xmin) &
      (ggData[, X] < plot_brush$xmax) &
      (ggData[, Y] > plot_brush$ymin) &
      (ggData[, Y] < plot_brush$ymax)
  }
  return(ggData[area])
}

#' Split Violin Plot Geom for ggplot2
#'
#' A custom ggplot2 geom that splits violin plots in half, useful for
#' comparing distributions side-by-side (e.g., treatment vs control).
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data The data to be displayed in this layer.
#' @param stat Character. Statistical transformation ("ydensity").
#' @param position Character. Adjustment to overlapping positions.
#' @param draw_quantiles Numeric vector. Quantiles to draw on violin.
#'   Default: NULL (no quantile lines)
#' @param trim Logical. Trim tails. Default: TRUE
#' @param scale Character. How to scale violins ("area", "count", "width").
#'   Default: "area"
#' @param na.rm Logical. Remove NAs? Default: FALSE
#' @param show.legend Logical. Show legend? Default: NA
#' @param inherit.aes Logical. Inherit plot aesthetics? Default: TRUE
#' @param ... Additional parameters passed to stat/geom.
#'
#' @return ggplot2 layer suitable for addition to plots.
#'
#' @details
#' Each group (even/odd) is drawn on opposite half of the X position.
#' Useful for split violin comparisons where two distributions share space.
#'
#' Credits: Based on jan-glx's implementation from StackOverflow.
#'
#' @keywords internal
#' @noRd
geom_split_violin <- function(
    mapping = NULL,
    data = NULL,
    stat = "ydensity",
    position = "identity",
    ...,
    draw_quantiles = NULL,
    trim = TRUE,
    scale = "area",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {
  return(ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSplitViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      ...
    )
  ))
}

#' GeomSplitViolin ggproto Object
#'
#' Internal ggplot2 Geom object for split violin rendering.
#' Used by geom_split_violin().
#'
#' @keywords internal
#' @noRd
GeomSplitViolin <- ggplot2::ggproto(
  "GeomSplitViolin",
  ggplot2::GeomViolin,
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data$xminv <- data$x - data$violinwidth * (data$x - data$xmin)
    data$xmaxv <- data$x + data$violinwidth * (data$xmax - data$x)
    grp <- data[1, "group"]
    if (grp %% 2 == 1) {
      data$x <- data$xminv
      data.order <- data$y
    } else {
      data$x <- data$xmaxv
      data.order <- -data$y
    }
    newdata <- data[order(data.order), , drop = FALSE]
    newdata <- rbind(
      newdata[1, ],
      newdata,
      newdata[nrow(x = newdata), ]
    )
    newdata[c(1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2::StatYdensity$compute_group(data, ...)$y
      quantiles <- quantile(quantiles, probs = draw_quantiles, na.rm = TRUE)
      quantile_data <- data.frame(
        x = rep(mean(data$x), length(quantiles)),
        y = quantiles,
        group = rep(data$group[1], length(quantiles))
      )
      quantile_data <- transform(quantile_data,
        x = x + violinwidth * (x - xminv[1]) * ifelse(grp %% 2 == 1, -1, 1)
      )

      ggplot2::GeomPath$draw_panel(quantile_data, ...)
    }

    ggplot2::GeomPolygon$draw_panel(newdata, ...)
  }
)
