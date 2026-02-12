#' Create a Complete pyShinyCell Shiny Application
#'
#' Complete end-to-end function that generates a fully-functional pyShinyCell
#' Shiny application. This is the main API function users will call.
#'
#' @param obj input single-cell object for Seurat (v3+) / SingleCellExperiment
#'   data or input file path for h5ad / loom files
#' @param scConf shinycell config data.table. If NULL (default), automatically
#'   created from obj using createPyConfig()
#' @param shiny.dir directory where the Shiny app will be created.
#'   Default is "shinyApp/"
#' @param shiny.title title for the Shiny application.
#'   Default is "pyShinyCell App"
#' @param shiny.prefix file prefix for generated data files.
#'   Default is "sc1"
#' @param enable_tabs character vector of analysis tabs to include.
#'   Options: c("de1", "de2", "gsea", "correlation", "tcr").
#'   Default is all tabs.
#' @param gex.assay assay to use for gene expression. Defaults to "RNA" for
#'   Seurat, "logcounts" for SCE.
#' @param gex.slot slot to use (Seurat only). Default is "data".
#' @param gene.mapping if TRUE, converts Ensembl IDs to gene symbols.
#'   Can also be a named vector for custom mapping.
#' @param default.gene1 primary default gene to display
#' @param default.gene2 secondary default gene to display
#' @param default.multigene character vector of default genes for multi-gene plots
#' @param default.dimred character vector of two default dimension reductions
#' @param python_env name of Python environment to setup/use.
#'   Default is "pyShinyCell"
#' @param setup_python if TRUE (default), runs setupPythonEnv() to ensure
#'   Python is configured. Set to FALSE if already configured.
#' @param generate_h5ad if TRUE (default), generates H5AD file for Python access
#' @param compress_h5ad if TRUE (default), applies gzip compression to H5AD
#' @param remove_assays character vector of Seurat assays to remove before
#'   H5AD conversion (e.g., c("integrated", "SCT"))
#' @param verbose if TRUE (default), prints progress messages
#'
#' @return Invisibly returns a list with paths to all created files and
#'   directories.
#'
#' @details
#' This function performs a complete 4-step workflow:
#' 1. **Config Creation**: Create or validate scConf
#' 2. **Data Generation**: Call makePyShinyFiles() to generate data files
#' 3. **Code Generation**: Call makePyShinyCode() to generate Shiny app code
#' 4. **Python Setup**: Optional Python environment configuration
#'
#' The resulting Shiny application in `shiny.dir` is ready to run with:
#' ```r
#' shiny::runApp("path/to/shinyApp")
#' ```
#'
#' @section Requirements:
#' - Seurat (v3+) or SingleCellExperiment object with metadata and embeddings
#' - OR H5AD file path
#' - Sufficient RAM to load the full object (8-16GB for typical datasets)
#'
#' @section Generated Files:
#' Data files (from makePyShinyFiles):
#' - `<prefix>conf.rds`, `<prefix>meta.rds`, `<prefix>gene.rds`
#' - `<prefix>gexpr.h5` (gene expression matrix)
#' - `<prefix>def.rds` (default settings)
#' - `<prefix>gexpr.h5ad` (H5AD format for Python)
#' - `<prefix>maxlvl.rds` (max expression per gene)
#'
#' Code files (from makePyShinyCode):
#' - `server.R` (Shiny server logic)
#' - `ui.R` (Shiny UI definitions)
#' - `global.R` (initialization and data loading)
#' - `util.R` (plotting and analysis functions)
#' - `util_palette.R` (color palettes)
#'
#' @examples
#' # This is typically called internally or requires setup.
#'
#' @export
makePyShinyApp <- function(
    obj,
    scConf = NULL,
    shiny.dir = "shinyApp/",
    shiny.title = "pyShinyCell App",
    shiny.prefix = "sc1",
    enable_tabs = c("de1", "de2", "gsea", "correlation", "tcr"),
    gex.assay = NA,
    gex.slot = c("data", "scale.data", "counts"),
    gene.mapping = FALSE,
    default.gene1 = NA,
    default.gene2 = NA,
    default.multigene = NA,
    default.dimred = NA,
    python_env = "pyShinyCell",
    setup_python = TRUE,
    generate_h5ad = TRUE,
    compress_h5ad = TRUE,
    remove_assays = NULL,
    verbose = TRUE) {

  msg <- function(...) {
    if (verbose) cat(paste0(..., "\n"))
  }

  msg("[ROCKET] Creating pyShinyCell Shiny App...")
  msg("")

  # ============ STEP 1: Config Creation ============
  msg("--- Step 1: Configuration ---")
  if (is.null(scConf)) {
    msg("  Creating config from object...")
    scConf <- createPyConfig(obj)
  } else {
    msg("  Using provided config")
  }
  msg("  [OK] Config ready")

  # ============ STEP 2: Data File Generation ============
  msg("")
  msg("--- Step 2: Data Generation ---")
  data_files <- makePyShinyFiles(
    obj = obj,
    scConf = scConf,
    gex.assay = gex.assay,
    gex.slot = gex.slot,
    gene.mapping = gene.mapping,
    shiny.prefix = shiny.prefix,
    shiny.dir = shiny.dir,
    default.gene1 = default.gene1,
    default.gene2 = default.gene2,
    default.multigene = default.multigene,
    default.dimred = default.dimred,
    generate_h5ad = generate_h5ad,
    compress_h5ad = compress_h5ad,
    remove_assays = remove_assays,
    calculate_maxlvl = TRUE,
    verbose = verbose
  )

  # ============ STEP 3: Code Generation ============
  msg("")
  msg("--- Step 3: Shiny Code Generation ---")
  code_files <- makePyShinyCode(
    scConf = scConf,
    shiny.prefix = shiny.prefix,
    shiny.dir = shiny.dir,
    shiny.title = shiny.title,
    enable_tabs = enable_tabs,
    python_env = python_env,
    include_util = TRUE,
    verbose = verbose
  )

  # ============ STEP 4: Python Setup (Optional) ============
  msg("")
  msg("--- Step 4: Python Environment ---")
  if (setup_python) {
    msg("  Setting up Python environment: {python_env}")
    tryCatch({
      setupPythonEnv(
        venv_name = python_env,
        verbose = verbose
      )
      msg("  [OK] Python environment ready")
    }, error = function(e) {
      msg("  [WARN]  Python setup failed (optional): {e$message}")
      msg("     Proceed without Python analysis, or run setupPythonEnv() manually")
    })
  } else {
    msg("  Skipping Python setup (setup_python = FALSE)")
  }

  # ============ Summary ============
  msg("")
  msg("[OK] pyShinyCell App Created Successfully!")
  msg("")
  msg("[DIR] App location: {shiny.dir}")
  msg("[LOG] Tabs enabled: {paste(enable_tabs, collapse = ', ')}")
  msg("[PREP] Data prefix: {shiny.prefix}")
  msg("")
  msg("[ROCKET] To run the app:")
  msg("   shiny::runApp('{shiny.dir}')")
  msg("")
  msg("[DATA] To open in RStudio:")
  msg("   - Open {shiny.dir}/ui.R")
  msg("   - Click 'Run App' button")

  # Return all file paths
  invisible(c(data_files, code_files))
}
