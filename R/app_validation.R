#' Validate and Fix Generated App Data
#'
#' Post-processes generated app data to fix common issues:
#' - Invalid default gene values
#' - Out-of-range slider defaults
#' - Missing configuration entries
#'
#' @param prefix File prefix (e.g., "sc1")
#' @param shiny.dir Directory where app files are located
#' @param verbose Print validation messages
#'
#' @return Invisibly returns list of fixes applied
#' @keywords internal
#'
.validateAppData <- function(prefix = "sc1", shiny.dir = ".", verbose = TRUE) {
  msg <- function(...) {
    if (verbose) cat(paste0(..., "\n"))
  }

  fixes_applied <- list()

  # Try to load the generated files
  tryCatch({
    conf_file <- file.path(shiny.dir, paste0(prefix, "conf.rds"))
    meta_file <- file.path(shiny.dir, paste0(prefix, "meta.rds"))
    gene_file <- file.path(shiny.dir, paste0(prefix, "gene.rds"))
    def_file <- file.path(shiny.dir, paste0(prefix, "def.rds"))

    if (!all(file.exists(conf_file, meta_file, gene_file, def_file))) {
      msg("[WARN] Not all required files found. Skipping validation.")
      return(invisible(NULL))
    }

    conf <- readRDS(conf_file)
    meta <- readRDS(meta_file)
    genes <- readRDS(gene_file)
    def <- readRDS(def_file)

    msg("[VALIDATE] Checking app data integrity...")

    # Fix 1: Validate default genes exist
    if (!is.null(def$gene1) && !is.na(def$gene1)) {
      if (!def$gene1 %in% names(genes)) {
        msg("  [FIX] default.gene1 '", def$gene1, "' not found, using first available gene")
        def$gene1 <- names(genes)[1]
        fixes_applied$gene1_fixed <- TRUE
      }
    }

    if (!is.null(def$gene2) && !is.na(def$gene2)) {
      if (!def$gene2 %in% names(genes)) {
        msg("  [FIX] default.gene2 '", def$gene2, "' not found, using second gene or first")
        def$gene2 <- if (length(names(genes)) > 1) names(genes)[2] else names(genes)[1]
        fixes_applied$gene2_fixed <- TRUE
      }
    }

    # Fix 2: Validate dimension reduction ranges
    dimred_cols <- conf[conf$dimred == TRUE]$ID
    for (dr in dimred_cols) {
      if (dr %in% colnames(meta)) {
        min_val <- floor(min(meta[[dr]], na.rm = TRUE))
        max_val <- ceiling(max(meta[[dr]], na.rm = TRUE))

        # Store corrected ranges for use in server.R
        attr(def, paste0(dr, "_min")) <- min_val
        attr(def, paste0(dr, "_max")) <- max_val
      }
    }

    # Fix 3: Ensure configuration has required columns
    required_cols <- c("UI", "ID", "uiType", "default", "fCL", "fRow", "show", "legend", "order")
    missing_cols <- required_cols[!required_cols %in% colnames(conf)]
    if (length(missing_cols) > 0) {
      msg("  [WARN] Missing config columns: ", paste(missing_cols, collapse = ", "))
    }

    # Fix 4: Validate factor levels in metadata
    for (col in colnames(meta)) {
      if (is.factor(meta[[col]])) {
        config_row <- conf[conf$ID == col]
        if (nrow(config_row) > 0 && !is.na(config_row$fID[1])) {
          expected_levels <- strsplit(config_row$fID[1], "\\|")[[1]]
          current_levels <- levels(meta[[col]])
          if (length(current_levels) != length(expected_levels)) {
            msg("  [WARN] Factor level mismatch for '", col,
                "'. Expected ", length(expected_levels), ", got ",
                length(current_levels))
          }
        }
      }
    }

    # Save corrected defaults
    if (length(fixes_applied) > 0) {
      saveRDS(def, def_file)
      msg("  [OK] Saved corrected defaults")
    } else {
      msg("  [OK] All validation checks passed")
    }

    invisible(fixes_applied)
  }, error = function(e) {
    msg("[WARN] Validation failed: ", e$message)
    invisible(NULL)
  })
}
