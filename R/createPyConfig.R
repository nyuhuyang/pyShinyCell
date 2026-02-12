#' Create Extended Configuration for pyShinyCell Apps
#'
#' Creates a shinycell config data.table optimized for pyShinyCell applications.
#' This extends the base ShinyCell createConfig() with additional fields and
#' defaults for Python-based analysis features.
#'
#' @param obj input single-cell object for Seurat (v3+) / SingleCellExperiment
#'   data or input file path for h5ad / loom files
#' @param meta.to.include columns to include from the single-cell metadata.
#'   Default is \code{NA}, which is to use all columns. Users can specify
#'   the columns to include.
#' @param legendCols maximum number of columns allowed when displaying the
#'   legends of categorical metadata. Default is 4.
#' @param maxLevels maximum number of levels allowed for categorical metadata.
#'   Metadata with nlevels > maxLevels will be discarded automatically.
#'   Default is 50.
#' @param enable_py_analysis Logical. If TRUE (default), adds metadata
#'   annotation identifying which metadata can be used for Python-based
#'   differential expression and correlation analysis.
#'
#' @return A shinycell config data.table with pyShinyCell-specific metadata
#'
#' @details
#' This function wraps ShinyCell's createConfig() and adds:
#' - pyShinyCell version metadata
#' - Analysis-ready flags for Python integration
#' - Optimized defaults for differential expression grouping
#'
#' The returned config can be further modified using standard shinycell
#' functions like modColours(), modLabels(), etc.
#'
#' @examples
#' # This is typically called internally or requires setup.
#'
#' @export
createPyConfig <- function(
    obj,
    meta.to.include = NA,
    legendCols = 4,
    maxLevels = 50,
    enable_py_analysis = TRUE) {

  # Call the base ShinyCell createConfig
  scConf <- ShinyCell::createConfig(
    obj = obj,
    meta.to.include = meta.to.include,
    legendCols = legendCols,
    maxLevels = maxLevels
  )

  # Add pyShinyCell metadata as attributes (not columns) to avoid conflicts with ShinyCell
  if (enable_py_analysis) {
    # Identify suitable grouping variables (categorical with reasonable n_levels)
    py_analysis <- rep(FALSE, nrow(scConf))

    if (methods::is(obj, "Seurat")) {
      meta_data <- obj@meta.data
    } else if (methods::is(obj, "SingleCellExperiment")) {
      meta_data <- as.data.frame(SingleCellExperiment::colData(obj))
    } else {
      # For h5ad/loom, we'll be conservative
      meta_data <- NULL
    }

    if (!is.null(meta_data)) {
      for (i in seq_len(nrow(scConf))) {
        var_id <- scConf$ID[i]
        if (var_id %in% colnames(meta_data)) {
          var_col <- meta_data[[var_id]]
          # Mark for analysis if it's categorical with 2-50 levels
          if (is.factor(var_col) || is.character(var_col)) {
            n_levels <- length(unique(var_col))
            if (n_levels >= 2 && n_levels <= 50) {
              py_analysis[i] <- TRUE
            }
          }
        }
      }
    }

    # Store py_analysis mapping as an attribute (keyed by variable ID)
    attr(scConf, "py_analysis") <- setNames(py_analysis, scConf$ID)
  }

  # Add package attribute for tracking
  attr(scConf, "package") <- "pyShinyCell"
  attr(scConf, "version") <- utils::packageVersion("pyShinyCell")

  return(scConf)
}
