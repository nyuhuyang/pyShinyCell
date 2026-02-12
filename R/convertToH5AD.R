#' Convert Seurat Object to H5AD Format
#'
#' Converts a Seurat object to AnnData (h5ad) format compatible with Python
#' tools like scanpy and gseapy. This function handles the intermediate
#' H5Seurat conversion and compression optimization for efficient storage.
#'
#' @param obj A Seurat (v3+) object to convert
#' @param h5ad.path Character. Path where the h5ad file should be saved.
#'   Must end with ".h5ad"
#' @param compress.gzip Logical. If TRUE (default), compresses the output
#'   using gzip compression level 9 for optimized file size. This requires
#'   Python environment with scanpy.
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
#' 1. Cleans metadata by converting factors to characters
#' 2. Removes specified assays to reduce file size
#' 3. Converts to H5Seurat intermediate format
#' 4. Converts H5Seurat to H5AD
#' 5. Optionally re-compresses with gzip (recommended for large files)
#'
#' The resulting h5ad file can be read by Python tools:
#' \code{
#'   import scanpy as sc
#'   adata = sc.read_h5ad("path/to/file.h5ad")
#' }
#'
#' @section Dependencies:
#' Requires Seurat (v3+), SeuratDisk, and reticulate packages.
#' For gzip compression, requires Python with scanpy.
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
  if (!requireNamespace("SeuratDisk", quietly = TRUE)) {
    stop("SeuratDisk package is required for H5Seurat/H5AD conversion.")
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

  # Step 3: Convert to H5Seurat
  h5seurat_path <- gsub("\\.h5ad$", ".h5Seurat", h5ad.path)
  msg("[FILE] Converting to H5Seurat: ", basename(h5seurat_path))

  tryCatch({
    SeuratDisk::SaveH5Seurat(obj, filename = h5seurat_path)
  }, error = function(e) {
    stop("Failed to save H5Seurat: ", e$message)
  })

  # Step 4: Convert H5Seurat to H5AD
  msg("[CONV] Converting H5Seurat to H5AD...")
  tryCatch({
    SeuratDisk::Convert(h5seurat_path, dest = "h5ad")
  }, error = function(e) {
    stop("Failed to convert to H5AD: ", e$message)
  })

  # Clean up intermediate file
  if (file.exists(h5seurat_path)) {
    file.remove(h5seurat_path)
  }

  # Step 5: Optional gzip compression
  if (compress.gzip && file.exists(h5ad.path)) {
    msg("[PKG] Applying gzip compression (level 9)...")

    tryCatch({
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        stop("reticulate required for gzip compression")
      }

      sc <- reticulate::import("scanpy", convert = FALSE)
      scipy <- reticulate::import("scipy.sparse", convert = FALSE)

      # Read original h5ad
      adata <- sc$read_h5ad(h5ad.path)

      # Write with gzip compression
      adata$write(h5ad.path, compression = "gzip", compression_opts = 9L)

      msg("[OK] Gzip compression complete")
    }, error = function(e) {
      warning("Gzip compression failed, keeping original format: ", e$message)
    })
  }

  msg("[OK] H5AD conversion complete: ", h5ad.path)
  invisible(h5ad.path)
}
