#' Map Ensembl Gene IDs to Gene Symbols
#'
#' Automatically detects organism (human/mouse) from Ensembl IDs
#' and maps to gene symbols using ShinyCell's bundled mapping files.
#'
#' @param gene_ids Character vector of gene IDs (Ensembl format)
#' @param gene.mapping Logical or named vector.
#'   - TRUE: Auto-detect organism and map ENSG/ENMUSG to symbols
#'   - FALSE: No mapping (keep original IDs)
#'   - Named vector: Custom mapping (names = old IDs, values = new names)
#' @param verbose Logical. Print progress messages.
#'
#' @return Named vector mapping gene IDs to symbols
#' @keywords internal
#'
.mapGeneIDs <- function(gene_ids, gene.mapping = TRUE, verbose = TRUE) {
  msg <- function(...) {
    if (verbose) cat(paste0(..., "\n"))
  }

  # If gene.mapping is FALSE, return identity mapping
  if (identical(gene.mapping, FALSE)) {
    msg("  [GENES] Gene mapping disabled, using original IDs")
    names(gene_ids) <- gene_ids
    return(gene_ids)
  }

  # If gene.mapping is already a named vector, use it
  if (is.character(gene.mapping) && !isTRUE(gene.mapping)) {
    msg("  [GENES] Using provided gene mapping vector")
    # Handle partial mapping
    mapped <- gene.mapping[gene_ids]
    unmapped <- gene_ids[is.na(mapped)]
    mapped[is.na(mapped)] <- unmapped
    return(mapped)
  }

  # Auto-detect organism from gene IDs
  msg("  [GENES] Auto-detecting organism from gene IDs...")
  human_count <- sum(grepl("^ENSG", gene_ids, ignore.case = TRUE))
  mouse_count <- sum(grepl("^ENMUSG", gene_ids, ignore.case = TRUE))

  if (human_count > mouse_count) {
    msg("  [GENES] Detected human genes, mapping ENSG IDs to symbols...")
    organism <- "human"
    mapping_file <- "geneMapHS.txt.gz"
  } else if (mouse_count > human_count) {
    msg("  [GENES] Detected mouse genes, mapping ENMUSG IDs to symbols...")
    organism <- "mouse"
    mapping_file <- "geneMapMM.txt.gz"
  } else {
    msg("  [WARN] Could not auto-detect organism, using original IDs")
    names(gene_ids) <- gene_ids
    return(gene_ids)
  }

  # Load mapping file from ShinyCell package
  tryCatch({
    mapping_path <- system.file("extdata", mapping_file, package = "ShinyCell")

    if (!file.exists(mapping_path)) {
      warning("Mapping file not found: ", mapping_file)
      names(gene_ids) <- gene_ids
      return(gene_ids)
    }

    # Read mapping table
    mapping_data <- data.table::fread(mapping_path)
    gene_map <- mapping_data$geneName
    names(gene_map) <- mapping_data$geneID

    # Map gene IDs, keeping unmapped IDs as-is
    mapped <- gene_map[gene_ids]
    unmapped_idx <- is.na(mapped)
    mapped[unmapped_idx] <- gene_ids[unmapped_idx]

    # Reorder to match input gene_ids
    mapped <- mapped[gene_ids]

    msg("  [OK] Mapped ", sum(!unmapped_idx), " of ", length(gene_ids),
        " genes (", organism, ")")

    return(mapped)
  }, error = function(e) {
    warning("Gene mapping failed: ", e$message)
    names(gene_ids) <- gene_ids
    return(gene_ids)
  })
}
