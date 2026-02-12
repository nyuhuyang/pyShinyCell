#' Color Generation Utilities
#'
#' Internal functions for generating color palettes in pyShinyCell visualizations.
#' These functions provide flexible color palette selection from custom, ggsci, and
#' RColorBrewer sources.
#'
#' @keywords internal
#' @name color_utils
NULL

#' Generate Color Palette
#'
#' Generate a color palette from custom, ggsci, or RColorBrewer sources.
#' Integrates with the pal.info palette metadata to select and expand colors.
#'
#' @param palette.name Character. Name of the palette from pal.info.
#' @param n Integer. Number of colors to generate. If NULL, uses maximum available.
#' @param alpha Numeric. Alpha transparency (0-1). Only applicable for ggsci palettes.
#'
#' @return Character vector of color codes (hex format).
#'
#' @details
#' Palette sources are determined by pal.info metadata:
#' - "custom": Pre-defined color vectors in cList
#' - "RColorBrewer": Via RColorBrewer::brewer.pal()
#' - "ggsci": Via ggsci package functions (pal_npg, pal_aaas, etc.)
#'
#' Colors are expanded to n via colorRampPalette if needed.
#'
#' @keywords internal
#' @noRd
color_generator <- function(palette.name, n=NULL, alpha = 1) {
  if (palette.name == "white") return(rep("white", max(n, 5)))

  # Requires: pal.info (from palette_data.R loaded via use_data)
  # and cList (custom color list)
  if (!exists("pal.info")) {
    stop("pal.info not found. Ensure palette data is loaded.")
  }

  N = as.integer(pal.info[palette.name, "maxcolors"])
  if (is.null(n)) n = N

  switch(pal.info[palette.name, "package"],
    "custom" = {
      if (!exists("cList")) {
        stop("cList not found. Ensure custom palettes are initialized.")
      }
      colorRampPalette(cList[[palette.name]])(n)
    },
    "RColorBrewer" = {
      colorRampPalette(RColorBrewer::brewer.pal(
        n = pal.info[palette.name, "maxcolors"],
        palette.name
      ))(n)
    },
    "ggsci" = {
      ggsci_f <- get(paste0("pal_", tolower(sub("\\..*", "", palette.name))))
      colorRampPalette(
        ggsci_f(
          palette = pal.info[palette.name, "palette"],
          alpha = alpha
        )(pal.info[palette.name, "maxcolors"])
      )(n)
    }
  )
}

#' Bilinear Interpolation Helper
#'
#' Performs bilinear interpolation for 2D color mapping in co-expression plots.
#' Used internally by colorRamp2D.
#'
#' @param x Numeric. X coordinate for interpolation.
#' @param y Numeric. Y coordinate for interpolation.
#' @param xy Numeric. XY scaling factor.
#' @param Q11,Q21,Q12,Q22 Numeric. Values at four corners of interpolation grid.
#'
#' @return Numeric. Interpolated value.
#'
#' @keywords internal
#' @noRd
bilinear <- function(x, y, xy, Q11, Q21, Q12, Q22) {
  oup = (xy - x) * (xy - y) * Q11 + x * (xy - y) * Q21 +
    (xy - x) * y * Q12 + x * y * Q22
  oup = oup / (xy * xy)
  return(oup)
}

#' Create 2D Color Gradient Palette
#'
#' Generate a 2D color gradient grid for use in co-expression visualizations.
#' Creates a matrix of interpolated colors from corner color specifications.
#'
#' @param col1 Character. Color for bottom-left corner (Gene1 low, Gene2 low).
#'   Default: "blue"
#' @param col2 Character. Color for top-right corner (Gene1 high, Gene2 high).
#'   Default: "green"
#' @param col3 Character. Color for top-left corner (Gene2 high).
#'   Default: "red"
#' @param col0 Character. Color for unselected/background cells.
#'   Default: "snow1"
#' @param nTot Integer. Resolution of color grid (nTot x nTot).
#'   Default: 100
#'
#' @return data.table with columns:
#'   - v1: Integer (0 to nTot-1) for Gene1 expression
#'   - v2: Integer (0 to nTot-1) for Gene2 expression
#'   - cMix: Character color code for the cell
#'
#' @details
#' Generates a smooth color gradient across all combinations of two gene
#' expression levels. Used for visualization of co-expressed genes in
#' dimensionality reduction plots.
#'
#' @keywords internal
#' @noRd
colorRamp2D <- function(col1 = "blue", col2 = "green", col3 = "red",
                        col0 = "snow1", nTot = 100) {
  rotate <- function(x) t(apply(x, 2, rev))
  mm <- tcrossprod(seq(1, 0, length.out = nTot))
  tmp1 <- sapply(grDevices::col2rgb(col1) / 255, function(x) 1 - mm * (1 - x))
  tmp2 <- sapply(grDevices::col2rgb(col2) / 255, function(x) 1 - rotate(rotate(mm)) * (1 - x))
  tmp3 <- sapply(grDevices::col2rgb(col3) / 255, function(x) 1 - rotate(mm) * (1 - x))
  tmp4 <- sapply(grDevices::col2rgb(col0) / 255, function(x) 1 - rotate(rotate(rotate(mm))) * (1 - x))

  tmp <- tmp1 * tmp2 * tmp3 * tmp4
  gg <- data.table::data.table(
    v1 = rep((nTot - 1):0, times = nTot),
    v2 = rep(0:(nTot - 1), each = nTot),
    cMix = grDevices::rgb(tmp)
  )
  return(gg)
}
