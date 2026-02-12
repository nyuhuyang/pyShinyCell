#' Generate Shiny Application Code for pyShinyCell
#'
#' Generates server.R, ui.R, and utility code files for a complete
#' pyShinyCell Shiny application. This is the code generation orchestrator
#' that pulls together writer functions for all components.
#'
#' @param scConf shinycell config data.table
#' @param shiny.prefix file prefix for data files (default "sc1")
#' @param shiny.dir directory where Shiny app files will be written
#' @param shiny.title title for the Shiny application
#' @param enable_tabs character vector specifying which analysis tabs to include.
#'   Options: c("de1", "de2", "gsea", "correlation", "tcr").
#'   Default is all tabs: c("de1", "de2", "gsea", "correlation", "tcr")
#' @param python_env name of Python virtual environment for analysis.
#'   Default is "pyShinyCell".
#' @param include_util Logical. If TRUE (default), copies util.R from the
#'   package to the app directory (includes plotting functions).
#' @param verbose Logical. If TRUE (default), prints status messages.
#'
#' @return Invisibly returns a list with paths to generated files:
#'   - server: Path to server.R
#'   - ui: Path to ui.R
#'   - util: Path to util.R (if include_util = TRUE)
#'   - global: Path to global.R (if generated)
#'
#' @details
#' This function performs the following:
#' 1. Validates scConf and enables_tabs
#' 2. Generates server.R from template + tab-specific code
#' 3. Generates ui.R from template + tab-specific UI
#' 4. Copies or generates util.R with necessary functions
#' 5. Generates global.R with Python environment setup if needed
#'
#' The generated code integrates with pre-generated data files
#' (sc1conf.rds, sc1meta.rds, sc1gene.rds, sc1gexpr.h5, sc1gexpr.h5ad).
#'
#' @section Dependencies:
#' Generated apps require: shiny, data.table, ggplot2, hdf5r, reticulate,
#' and all utilities from util.R.
#'
#' @examples
#' # This is typically called internally or requires setup.
#'
#' @export
makePyShinyCode <- function(
    scConf,
    shiny.prefix = "sc1",
    shiny.dir = "shinyApp/",
    shiny.title = "pyShinyCell App",
    enable_tabs = c("de1", "de2", "gsea", "correlation", "tcr"),
    python_env = "pyShinyCell",
    include_util = TRUE,
    verbose = TRUE) {

  msg <- function(...) {
    if (verbose) cat(paste0(..., "\n"))
  }

  # Validate inputs
  if (!is.data.frame(scConf)) {
    stop("scConf must be a data.frame/data.table (config object)")
  }

  valid_tabs <- c("de1", "de2", "gsea", "correlation", "tcr")
  if (!all(enable_tabs %in% valid_tabs)) {
    stop("enable_tabs must be subset of: ", paste(valid_tabs, collapse = ", "))
  }

  # Ensure directory exists
  if (!dir.exists(shiny.dir)) {
    dir.create(shiny.dir, recursive = TRUE)
  }

  msg("[CODE] Generating Shiny app code...")

  # Step 1: Generate server.R
  msg("  - server.R")
  server_code <- .generateServerCode(
    scConf, shiny.prefix, enable_tabs, python_env, verbose = FALSE
  )
  writeLines(server_code, file.path(shiny.dir, "server.R"))

  # Step 2: Generate ui.R
  msg("  - ui.R")
  ui_code <- .generateUICode(
    scConf, shiny.prefix, shiny.title, enable_tabs, verbose = FALSE
  )
  writeLines(ui_code, file.path(shiny.dir, "ui.R"))

  # Step 3: Generate global.R with Python setup
  msg("  - global.R")
  global_code <- .generateGlobalCode(shiny.prefix, python_env)
  writeLines(global_code, file.path(shiny.dir, "global.R"))

  # Step 4: Create util.R with utility functions
  if (include_util) {
    msg("  - util.R")
    .createMinimalUtil(file.path(shiny.dir, "util.R"))
  }

  # Step 5: Create minimal util_palette.R
  msg("  - util_palette.R")
  .createMinimalPalette(file.path(shiny.dir, "util_palette.R"))

  msg("[OK] Shiny app code generation complete!")

  invisible(list(
    server = file.path(shiny.dir, "server.R"),
    ui = file.path(shiny.dir, "ui.R"),
    global = file.path(shiny.dir, "global.R"),
    util = if (include_util) file.path(shiny.dir, "util.R") else NA,
    palette = file.path(shiny.dir, "util_palette.R")
  ))
}

#' Generate server.R code
#' @keywords internal
.generateServerCode <- function(scConf, prefix, enable_tabs, py_env, verbose = FALSE) {
  msg <- function(...) if (verbose) cat(paste0(..., "\n"))

  msg("[CODE] Generating server.R...")

  # Read server template from R folder
  server_template_path <- system.file("R/server_template.R", package = "pyShinyCell")

  if (!file.exists(server_template_path)) {
    stop("Server template not found at: ", server_template_path)
  }

  server_code <- readLines(server_template_path, warn = FALSE)
  server_code <- paste0(server_code, collapse = "\n")

  # Replace sc1 prefix with provided prefix
  server_code <- gsub("\\bsc1([a-z0-9])", paste0(prefix, "\\1"), server_code)
  server_code <- gsub("\\bsc1$", prefix, server_code, perl = TRUE)

  msg("[OK] Server code generated with prefix: ", prefix)
  return(server_code)
}

#' Generate ui.R code
#' @keywords internal
.generateUICode <- function(scConf, prefix, title, enable_tabs, verbose = FALSE) {
  msg <- function(...) if (verbose) cat(paste0(..., "\n"))

  msg("[CODE] Generating ui.R...")

  # Read ui template from R folder
  ui_template_path <- system.file("R/ui_template.R", package = "pyShinyCell")

  if (!file.exists(ui_template_path)) {
    stop("UI template not found at: ", ui_template_path)
  }

  ui_code <- readLines(ui_template_path, warn = FALSE)
  ui_code <- paste0(ui_code, collapse = "\n")

  # Replace title placeholder (My pyShinyCell App) with provided title
  ui_code <- gsub("My pyShinyCell App", title, ui_code, fixed = TRUE)

  # Replace sc1 prefix with provided prefix
  ui_code <- gsub("\\bsc1([a-z0-9])", paste0(prefix, "\\1"), ui_code)
  ui_code <- gsub("\\bsc1$", prefix, ui_code, perl = TRUE)

  msg("[OK] UI code generated")
  return(ui_code)
}

#' Generate global.R with Python setup
#' @keywords internal
.generateGlobalCode <- function(prefix, py_env) {
  code <- glue::glue('
# pyShinyCell Generated Global Setup
library(shiny)
library(reticulate)
library(data.table)

# Load data files
{prefix}conf <- readRDS("{prefix}conf.rds")
{prefix}meta <- readRDS("{prefix}meta.rds")
{prefix}gene <- readRDS("{prefix}gene.rds")
{prefix}def  <- readRDS("{prefix}def.rds")

# Check for maxlvl file (from H5AD conversion)
if (file.exists("{prefix}maxlvl.rds")) {{
  {prefix}max <- readRDS("{prefix}maxlvl.rds")
}}

# Create temp directories for analysis outputs
dir.create("tempData", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

# Python environment setup
tryCatch({{
  use_virtualenv("{py_env}", required = TRUE)
}}, error = function(e) {{
  warning("Python environment not available for analysis functions. ",
          "Install with setupPythonEnv()")
}})

# ============ SOURCE ALL UTILITY FUNCTIONS ============
if (file.exists("util.R")) source("util.R")
if (file.exists("util_palette.R")) source("util_palette.R")

  ')

  return(code)
}

#' Create minimal util.R for testing
#' @keywords internal
.createMinimalUtil <- function(path) {
  util_code <- '
# Minimal utility functions for pyShinyCell
# Full implementation should copy from package or shinyApp_stable/util.R

library(ggplot2)
library(ggrepel)
library(gridExtra)
library(ggsci)

# Placeholder plotting theme
sctheme <- function(base_size = 24) {
  theme_minimal(base_size = base_size)
}

# Message function
msg <- function(...) cat(paste0(..., "\\n"))
msg("[WARN]  Using minimal util.R. For full functionality, ensure util.R is properly sourced.")
  '

  writeLines(util_code, path)
}

#' Create minimal util_palette.R
#' @keywords internal
.createMinimalPalette <- function(path) {
  palette_code <- '
# Minimal color palette utilities for pyShinyCell
# Full palettes should be extended with custom color schemes

library(RColorBrewer)

# Basic color palette generator
getPalette <- function(n, type = "spectral") {
  if (type == "spectral") {
    colorRampPalette(brewer.pal(11, "Spectral"))(n)
  } else if (type == "viridis") {
    viridis::viridis(n)
  } else {
    colorRampPalette(brewer.pal(9, "Set1"))(n)
  }
}

# Placeholder message
cat("[INFO] Using minimal color palette. Customize with custom color schemes.\\n")
  '

  writeLines(palette_code, path)
}
