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

  # Step 4: Copy util.R with utility functions
  if (include_util) {
    msg("  - util.R")
    util_src <- system.file("shinyApp_stable", "util.R", package = "pyShinyCell")
    if (file.exists(util_src)) {
      file.copy(util_src, file.path(shiny.dir, "util.R"), overwrite = TRUE)
    } else {
      # Fallback: create minimal util.R from package resources
      .createMinimalUtil(file.path(shiny.dir, "util.R"))
    }
  }

  # Step 5: Copy util_palette.R if available
  msg("  - util_palette.R")
  palette_src <- system.file("shinyApp_stable", "util_palette.R", package = "pyShinyCell")
  if (file.exists(palette_src)) {
    file.copy(palette_src, file.path(shiny.dir, "util_palette.R"), overwrite = TRUE)
  }

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

  # Base server code
  server_code <- glue::glue('
# pyShinyCell Generated Server Code
shinyServer(function(input, output, session) {{
  # Setup and initialization
  source("util.R")
  source("util_palette.R")
  observe_helpers()

  # Generate gene selection options
  optCrt = "{ option_create: function(data, escape) { return(\"<div class=\\\"create\\\"><strong>\" + escape(data.input) + \"</strong></div>\"); } }"

  # Initialize UI element choices
  for (gene_input in grep("inpg[12]$", names(input), value = TRUE)) {{
    updateSelectizeInput(session, gene_input,
      choices = sort(names({prefix}gene)), server = TRUE,
      options = list(maxOptions = 100, create = TRUE, persist = TRUE, render = I(optCrt)))
  }}

  # ============ STANDARD TABS (a0-a4) ============
  # Placeholder: Standard ShinyCell server logic
  # (Include standard dimension reduction, gene expression plotting, etc.)

  ')

  # Add custom tab server code
  if ("de1" %in% enable_tabs) {
    msg("Adding DE1 (pairwise) tab server code...")
    server_code <- paste0(server_code, "
  # ============ TAB B1: Differential Expression (Pairwise) ============
  # DE analysis comparing two groups
  # TODO: Extract from shinyApp_stable/server.R lines 487-627

    ")
  }

  if ("de2" %in% enable_tabs) {
    msg("Adding DE2 (all-vs-rest) tab server code...")
    server_code <- paste0(server_code, "
  # ============ TAB C1: Differential Expression (All-vs-Rest) ============
  # All-vs-rest DE analysis with clustering
  # TODO: Extract from shinyApp_stable/server.R lines 628-701

    ")
  }

  if ("gsea" %in% enable_tabs) {
    msg("Adding GSEA tab server code...")
    server_code <- paste0(server_code, "
  # ============ TAB D1: Gene Set Enrichment Analysis ============
  # Pathway enrichment visualization
  # TODO: Extract GSEA functionality

    ")
  }

  if ("correlation" %in% enable_tabs) {
    msg("Adding Correlation tab server code...")
    server_code <- paste0(server_code, "
  # ============ TAB E1: Correlation Analysis ============
  # Gene-gene correlation networks
  # TODO: Extract correlation functionality

    ")
  }

  if ("tcr" %in% enable_tabs) {
    msg("Adding TCR tab server code...")
    server_code <- paste0(server_code, "
  # ============ TAB F1: TCR/Repertoire Analysis ============
  # T cell receptor clonotype analysis
  # TODO: Extract TCR functionality

    ")
  }

  # Close shinyServer
  server_code <- paste0(server_code, "\n})\n")

  return(server_code)
}

#' Generate ui.R code
#' @keywords internal
.generateUICode <- function(scConf, prefix, title, enable_tabs, verbose = FALSE) {
  msg <- function(...) if (verbose) cat(paste0(..., "\n"))

  ui_code <- glue::glue('
# pyShinyCell Generated UI Code
source("util.R")
library(shinyhelper)
library(shinycssloaders)
library(bslib)

shinyUI(fluidPage(
  tags$head(tags$style(HTML(".shiny-output-error-validation {{color: red; font-weight: bold;}}"))),
  theme = bs_theme(bootswatch = "default"),
  titlePanel("{title}"),
  navbarPage(
    NULL,
    # ============ STANDARD TABS ============
    tabPanel("CellInfo",
      h4("Cell information on reduced dimensions"),
      p("Visualize cell metadata on dimension reductions"),
      # TODO: Include standard ShinyCell UI
      br()
    ),

    # ============ CUSTOM ANALYSIS TABS ============
    ')

  if ("de1" %in% enable_tabs) {
    msg("Adding DE1 tab UI...")
    ui_code <- paste0(ui_code, '
    tabPanel("Differential Expression",
      h4("Pairwise differential expression analysis"),
      p("Compare gene expression between two cell groups"),
      # TODO: Include DE1 UI elements
      br()
    ),
    ')
  }

  if ("de2" %in% enable_tabs) {
    msg("Adding DE2 tab UI...")
    ui_code <- paste0(ui_code, '
    tabPanel("All-vs-Rest DE",
      h4("All-vs-rest differential expression"),
      p("Identify markers for each group vs all others"),
      # TODO: Include DE2 UI elements
      br()
    ),
    ')
  }

  if ("gsea" %in% enable_tabs) {
    msg("Adding GSEA tab UI...")
    ui_code <- paste0(ui_code, '
    tabPanel("GSEA",
      h4("Gene set enrichment analysis"),
      p("Pathway and biological function enrichment"),
      # TODO: Include GSEA UI elements
      br()
    ),
    ')
  }

  if ("correlation" %in% enable_tabs) {
    msg("Adding Correlation tab UI...")
    ui_code <- paste0(ui_code, '
    tabPanel("Correlation",
      h4("Gene-gene correlation analysis"),
      p("Identify correlated gene expression patterns"),
      # TODO: Include correlation UI elements
      br()
    ),
    ')
  }

  if ("tcr" %in% enable_tabs) {
    msg("Adding TCR tab UI...")
    ui_code <- paste0(ui_code, '
    tabPanel("TCR",
      h4("T cell receptor repertoire"),
      p("TCR clonotype and diversity analysis"),
      # TODO: Include TCR UI elements
      br()
    ),
    ')
  }

  ui_code <- paste0(ui_code, "
  )  # End navbarPage
))  # End fluidPage + shinyUI
  ")

  return(ui_code)
}

#' Generate global.R with Python setup
#' @keywords internal
.generateGlobalCode <- function(prefix, py_env) {
  code <- glue::glue('
# pyShinyCell Generated Global Setup
library(shiny)
library(reticulate)

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
