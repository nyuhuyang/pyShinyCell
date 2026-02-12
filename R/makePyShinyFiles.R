#' Generate Data Files for pyShinyCell Shiny App
#'
#' Extended version of ShinyCell::makeShinyFiles() that generates all required
#' data files for a pyShinyCell application, including H5AD format for Python
#' analysis integration and maximum expression level calculations.
#'
#' @param obj input single-cell object for Seurat (v3+) / SingleCellExperiment
#'   data or input file path for h5ad / loom files
#' @param scConf shinycell config data.table (created by createConfig() or
#'   createPyConfig())
#' @param gex.assay assay in single-cell data object to use for plotting
#'   gene expression. Defaults to "RNA" for Seurat, "logcounts" for SCE.
#' @param gex.slot slot in single-cell assay to plot. Only used for Seurat
#'   objects. Default is "data".
#' @param gene.mapping specifies whether to convert Ensembl gene IDs to gene
#'   symbols. Set to TRUE for automatic conversion, or provide a named vector
#'   for custom mapping. Default is FALSE.
#' @param shiny.prefix specify file prefix for generated files. Default is "sc1".
#' @param shiny.dir specify directory to create the Shiny app in. Default is
#'   "shinyApp/".
#' @param default.gene1 specify primary default gene to show
#' @param default.gene2 specify secondary default gene to show
#' @param default.multigene character vector specifying default genes to show
#'   in multi-gene plots (bubble plot / heatmap)
#' @param default.dimred character vector specifying the two default dimension
#'   reductions. Default is to use UMAP if available, otherwise TSNE.
#' @param generate_h5ad Logical. If TRUE (default) and obj is a Seurat object,
#'   generates H5AD file for Python analysis. Set to FALSE if working with
#'   h5ad/loom inputs that are already in correct format.
#' @param compress_h5ad Logical. If TRUE (default), applies gzip compression
#'   to generated H5AD file. Requires Python environment with scanpy.
#' @param remove_assays Character vector. Names of Seurat assays to remove
#'   before H5AD conversion to reduce file size. E.g. c("integrated", "SCT").
#' @param calculate_maxlvl Logical. If TRUE (default), calculates and saves
#'   maximum expression levels per gene to maxlvl.rds for efficient visualization.
#'   Requires Python with scipy for H5AD files.
#' @param chunkSize number of genes written to h5file at any one time. Lower
#'   this number to reduce memory consumption. Default is 500.
#' @param verbose Logical. If TRUE (default), prints status messages.
#'
#' @return Invisibly returns a list with paths to generated files:
#'   - conf: Path to config RDS
#'   - meta: Path to metadata RDS
#'   - gene: Path to gene mapping RDS
#'   - gexpr: Path to gene expression file (H5 or H5AD)
#'   - def: Path to defaults RDS
#'   - maxlvl: Path to max levels RDS (if generate_h5ad=TRUE)
#'
#' @details
#' This function generates the following files in \code{shiny.dir}:
#' - `PREFIXconf.rds`: Configuration data table
#' - `PREFIXmeta.rds`: Cell metadata
#' - `PREFIXgene.rds`: Gene mapping
#' - `PREFIXgexpr.h5`: Gene expression matrix (HDF5)
#' - `PREFIXdef.rds`: Default plot settings
#' - `PREFIXgexpr.h5ad`: Gene expression (H5AD, if `generate_h5ad = TRUE`)
#' - `PREFIXmaxlvl.rds`: Max expression per gene (if `calculate_maxlvl = TRUE`)
#'
#' The H5AD file enables direct Python analysis access using scanpy or gseapy.
#'
#' @section Workflow:
#' 1. Calls ShinyCell::makeShinyFiles() to generate standard files
#' 2. If generating H5AD: converts Seurat object to H5AD with gzip compression
#' 3. If calculating maxlvl: computes max expression for each gene from H5AD
#' 4. Generates defaults RDS file with standard plot settings
#'
#' @examples
#' # This is typically called internally or requires setup.
#'
#' @export
makePyShinyFiles <- function(
    obj,
    scConf,
    gex.assay = NA,
    gex.slot = c("data", "scale.data", "counts"),
    gene.mapping = FALSE,
    shiny.prefix = "sc1",
    shiny.dir = "shinyApp/",
    default.gene1 = NA,
    default.gene2 = NA,
    default.multigene = NA,
    default.dimred = NA,
    generate_h5ad = TRUE,
    compress_h5ad = TRUE,
    remove_assays = NULL,
    calculate_maxlvl = TRUE,
    chunkSize = 500,
    verbose = TRUE) {

  msg <- function(...) {
    if (verbose) cat(paste0(..., "\n"))
  }

  # Step 1: Call ShinyCell::makeShinyFiles for standard files
  msg("[FILE] Generating ShinyCell data files...")
  ShinyCell::makeShinyFiles(
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
    chunkSize = chunkSize
  )

  # Prepare file paths
  if (!dir.exists(shiny.dir)) {
    dir.create(shiny.dir, recursive = TRUE)
  }
  h5ad_path <- file.path(shiny.dir, paste0(shiny.prefix, "gexpr.h5ad"))
  maxlvl_path <- file.path(shiny.dir, paste0(shiny.prefix, "maxlvl.rds"))

  # Step 2: Generate H5AD if requested and obj is Seurat
  if (generate_h5ad && methods::is(obj, "Seurat")) {
    msg("[CONV] Generating H5AD file for Python analysis...")

    convertToH5AD(
      obj = obj,
      h5ad.path = h5ad_path,
      compress.gzip = compress_h5ad,
      remove_assays = remove_assays,
      convert_factors = TRUE,
      verbose = verbose
    )

    # Step 3: Calculate max expression levels if requested
    if (calculate_maxlvl && file.exists(h5ad_path)) {
      msg("[DATA] Calculating maximum expression levels...")

      tryCatch({
        if (!requireNamespace("reticulate", quietly = TRUE)) {
          stop("reticulate required for maxlvl calculation")
        }

        sc <- reticulate::import("scanpy", convert = FALSE)
        scipy <- reticulate::import("scipy.sparse", convert = FALSE)

        # Read H5AD
        adata <- sc$read_h5ad(h5ad_path)
        gene_names <- reticulate::py_to_r(adata$var_names$values)

        # Calculate max per gene
        msg("   Computing sparse matrix max...")
        max_exp <- scipy$sparse$csr_matrix$max(adata$X, axis = 0L)
        max_exp_vec <- as.vector(reticulate::py_to_r(max_exp))

        # Create data frame and save
        max_exp_df <- data.frame(val = max_exp_vec, row.names = gene_names)
        saveRDS(max_exp_df, maxlvl_path)

        msg("   [OK] Max levels saved to ", basename(maxlvl_path))

      }, error = function(e) {
        warning("Failed to calculate max levels: ", e$message)
      })
    }
  }

  msg("[OK] All pyShinyCell data files generated successfully!")

  # Return paths invisibly
  invisible(list(
    conf = file.path(shiny.dir, paste0(shiny.prefix, "conf.rds")),
    meta = file.path(shiny.dir, paste0(shiny.prefix, "meta.rds")),
    gene = file.path(shiny.dir, paste0(shiny.prefix, "gene.rds")),
    gexpr = file.path(shiny.dir, paste0(shiny.prefix, "gexpr.h5")),
    def = file.path(shiny.dir, paste0(shiny.prefix, "def.rds")),
    h5ad = if (generate_h5ad) h5ad_path else NA,
    maxlvl = if (calculate_maxlvl) maxlvl_path else NA
  ))
}
