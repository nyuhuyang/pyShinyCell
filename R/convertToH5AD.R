#' Convert Seurat Object to H5AD Format
#'
#' Converts a Seurat object to AnnData (h5ad) format compatible with Python
#' tools like scanpy and gseapy. Uses Python's anndata library for direct
#' conversion without requiring SeuratDisk.
#'
#' @param obj A Seurat (v3+) object to convert
#' @param h5ad.path Character. Path where the h5ad file should be saved.
#'   Must end with ".h5ad"
#' @param compress.gzip Logical. If TRUE (default), uses gzip compression
#'   (compression_opts = 9) when writing H5AD file.
#' @param remove_assays Character vector. Names of assays to remove before
#'   conversion to reduce file size. For example, c("integrated", "SCT")
#' @param convert_factors Logical. If TRUE (default), converts all factor
#'   columns in metadata to character to ensure compatibility with H5AD format.
#' @param verbose Logical. If TRUE (default), prints status messages.
#'
#' @return Invisibly returns the path to the created h5ad file.
#'
#' @details
#' The conversion process:
#' 1. Extracts counts matrix from the default Seurat assay
#' 2. Extracts cell and gene metadata
#' 3. Extracts dimensional reductions (PCA, UMAP, tSNE, etc.)
#' 4. Creates AnnData object using Python's anndata library
#' 5. Writes to H5AD format with optional gzip compression
#'
#' The resulting h5ad file can be read by Python tools:
#' \code{
#'   import scanpy as sc
#'   adata = sc.read_h5ad("path/to/file.h5ad")
#' }
#'
#' @section Dependencies:
#' Requires Seurat (v3+) and reticulate packages.
#' Python must have anndata package installed.
#'
#' @examples
#' # This is typically called internally or requires setup.
#'
#' @export
convertToH5AD <- function(
    obj,
    h5ad.path,
    compress.gzip = TRUE,
    remove_assays = NULL,
    convert_factors = TRUE,
    verbose = TRUE) {

  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Seurat package is required. Install it with: install.packages('Seurat')")
  }
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("reticulate package is required. Install it with: install.packages('reticulate')")
  }

  msg <- function(...) {
    if (verbose) cat(paste0(..., "\n"))
  }

  # Validate input
  if (!methods::is(obj, "Seurat")) {
    stop("obj must be a Seurat object")
  }

  if (!tolower(tools::file_ext(h5ad.path)) == "h5ad") {
    stop("h5ad.path must end with '.h5ad'")
  }

  # Ensure directory exists
  dir.create(dirname(h5ad.path), showWarnings = FALSE, recursive = TRUE)

  # Step 1: Clean metadata
  msg("[PREP] Preparing metadata...")
  if (convert_factors) {
    meta <- obj@meta.data
    for (col in colnames(meta)) {
      if (is.factor(meta[[col]])) {
        obj@meta.data[[col]] <- as.character(meta[[col]])
      }
    }
  }

  # Step 2: Remove assays if specified
  if (!is.null(remove_assays)) {
    msg("[RM] Removing assays: ", paste(remove_assays, collapse = ", "))
    for (assay_name in remove_assays) {
      if (assay_name %in% names(obj@assays)) {
        obj[[assay_name]] <- NULL
      }
    }
  }

  # Step 3: Extract data for AnnData creation
  msg("[EXTR] Extracting data from Seurat object...")

  # Get counts matrix (transposed: genes x cells)
  assay <- Seurat::DefaultAssay(obj)
  counts <- Seurat::GetAssayData(obj, assay = assay, slot = "counts")
  if (nrow(counts) == 0) {
    counts <- Seurat::GetAssayData(obj, assay = assay, slot = "data")
  }

  # Convert to dense matrix if sparse (for some Python compatibility)
  if (!methods::is(counts, "matrix")) {
    counts <- as(counts, "dgCMatrix")  # Ensure sparse format for efficiency
  }

  # Get metadata
  metadata <- as.data.frame(obj@meta.data)

  # Get dimensional reductions
  reductions <- list()
  if (length(obj@reductions) > 0) {
    msg("[DR] Including dimensional reductions: ", paste(names(obj@reductions), collapse = ", "))
    for (red_name in names(obj@reductions)) {
      reductions[[red_name]] <- Seurat::Embeddings(obj[[red_name]])
    }
  }

  # Step 4: Create AnnData object using Python
  msg("[CONV] Creating AnnData object...")

  tryCatch({
    # Import anndata
    anndata <- reticulate::import("anndata", convert = FALSE)
    np <- reticulate::import("numpy", convert = FALSE)
    scipy_sparse <- reticulate::import("scipy.sparse", convert = FALSE)

    # Convert R sparse matrix to Python sparse matrix
    counts_scipy <- scipy_sparse$csr_matrix(Matrix::t(counts))

    # Create AnnData object (genes x cells format)
    adata <- anndata$AnnData(
      X = counts_scipy,
      obs = metadata,
      var = data.frame(gene_names = rownames(counts), row.names = rownames(counts))
    )

    # Add reductions to obsm
    if (length(reductions) > 0) {
      for (red_name in names(reductions)) {
        adata$obsm[[paste0("X_", tolower(red_name))]] <- reductions[[red_name]]
      }
    }

    # Step 5: Write to H5AD with compression
    msg("[WRITE] Writing H5AD file: ", basename(h5ad.path))

    if (compress.gzip) {
      adata$write(h5ad.path, compression = "gzip", compression_opts = 9L)
      msg("[OK] H5AD with gzip compression written")
    } else {
      adata$write(h5ad.path)
      msg("[OK] H5AD file written")
    }

  }, error = function(e) {
    stop("Failed to create/write AnnData: ", e$message)
  })

  msg("[OK] H5AD conversion complete: ", h5ad.path)
  invisible(h5ad.path)
}
