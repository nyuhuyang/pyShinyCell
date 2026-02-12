#' Differential Expression Analysis Functions
#'
#' This module contains functions for performing differential expression analysis
#' on single-cell data using Python-based scanpy via reticulate, including
#' pairwise comparisons and all-vs-rest analysis.
#'
#' @keywords internal
#' @name de_analysis
NULL

#' Find Differentially Expressed Markers Between Groups
#'
#' Performs differential expression analysis between two specified groups
#' using scanpy's rank_genes_groups function. Calls Python via reticulate.
#'
#' @param inpH5ad Character path to H5AD file containing AnnData object
#' @param inpsub1_1 Character column name for first subset dimension
#' @param inpsub1_2 Character vector of values to subset first dimension
#' @param inpsub2_1 Character column name for second subset dimension
#' @param inpsub2_2 Character vector of values to subset second dimension
#' @param inpsub3_1 Character column name for third subset dimension
#' @param inpsub3_2 Character vector of values to subset third dimension
#' @param inpGrp Character column name for grouping variable
#' @param inpident_1 Character vector specifying cells in group 1
#' @param inpident_2 Character vector specifying reference group (default NULL)
#' @param inpDEmethod Character method for DE test (default "wilcoxon")
#' @param min_cells Integer minimum cells per gene (default 100)
#' @param returnFormat Character "uns" to return uns object only (default "uns")
#'
#' @return AnnData object with rank_genes_groups results stored in uns slot
#'
#' @importFrom reticulate import use_virtualenv
#' @importFrom magrittr %>% %<>%
#'
#' @keywords internal
#' @noRd
scFindMarkers <- function(inpH5ad, inpsub1_1, inpsub1_2, inpsub2_1, inpsub2_2, inpsub3_1, inpsub3_2,
                          inpGrp, inpident_1, inpident_2 = NULL, inpDEmethod = "wilcoxon", min_cells = 100, returnFormat = "uns") {
  sc <- reticulate::import("scanpy")
  adata <- sc$read_h5ad(inpH5ad)
  adata <- subsetAnndata(adata, inpsub1_1, inpsub1_2, inpsub2_1, inpsub2_2, inpsub3_1, inpsub3_2)

  # prepare a info  to store different analysis conditions information
  info = paste(c(paste(inpident_1, collapse = "_"), "vs",
                 paste(inpident_2, collapse = "_"), "in",
                 paste(inpsub1_2, collapse = "_"),
                 paste(inpsub2_2, collapse = "_"),
                 paste(inpsub3_2, collapse = "_"),
                 "with", inpDEmethod), collapse = "_")
  adata$obs[, inpGrp] %<>% as.factor()

  if (adata$var_names[0] == "0") adata$var_names = adata$var[["features"]]

  # check if any sample has less than 2 samples.
  df <- as.data.frame(table(adata$obs[, inpGrp]))
  shiny::validate(need(df[df$Var1 %in% inpident_1, "Freq"] > 2, message = paste0("Need more than 2 samples in group one!")))
  shiny::validate(need(df[df$Var1 %in% inpident_2, "Freq"] > 2, message = paste0("Need more than 2 samples in reference group!")))

  sc$pp$filter_genes(adata, min_cells = min_cells, inplace = TRUE)
  print("start DE")
  adata$uns$log1p = NULL

  tryCatch({
    sc$tl$rank_genes_groups(adata, groupby = inpGrp, groups = list(inpident_1), reference = inpident_2, method = inpDEmethod, pts = TRUE, use_raw = FALSE)
  }, error = function(cond) {
    message("rank_genes_groups fail")
    message(cond)
  }, warning = function(cond) {
    message(cond)
  })

  print("DE done")
  adata_uns <- adata$copy()
  adata_uns$X = NULL
  adata_uns$raw = NULL
  adata_uns$obs = data.frame(matrix(ncol = 1, nrow = adata_uns$n_obs))
  adata_uns$obsm = NULL
  adata_uns$varm = NULL
  adata_uns$obsp = NULL
  adata_uns$write_h5ad("tempData/rank_genes_groups_1.h5ad")
  write.table(info, file = "tempData/rank_genes_groups_1.csv", quote = FALSE, row.names = F, col.names = F)

  if (returnFormat == "uns") return(adata_uns)
}

#' Check if Stored Analysis Condition is Identical
#'
#' Internal helper function to check if stored DE analysis conditions
#' match the current parameters by comparing info strings.
#'
#' @param inpsub1_2 Character vector of values for first subset
#' @param inpsub2_2 Character vector of values for second subset
#' @param inpsub3_2 Character vector of values for third subset
#' @param inpident_1 Character group 1 identifiers (default "")
#' @param inpident_2 Character group 2 identifiers (default "")
#' @param min_expr Integer minimum expression threshold (default NULL)
#' @param inpGrp Character grouping column (default NULL)
#' @param inpDEmethod Character DE method (default NULL)
#' @param databases Character vector of database names (default NULL)
#' @param inpdatapath Character data path (default NULL)
#' @param file.name Character path to info file to check (default "tempData/de_info.csv")
#'
#' @return Logical TRUE if stored condition matches current parameters
#'
#' @keywords internal
#' @noRd
.checkIfIdentical <- function(inpsub1_2, inpsub2_2, inpsub3_2, inpident_1 = "", inpident_2 = "",
                              min_expr = NULL, inpGrp = NULL, inpDEmethod = NULL,
                              databases = NULL, inpdatapath = NULL,
                              file.name = "tempData/de_info.csv") {

  info = switch(file.name,
                "tempData/de_info.csv" = paste0(
                  paste(inpident_1, collapse = "_"), " vs ",
                  paste(inpident_2, collapse = "_"), " in ",
                  paste(paste(inpsub1_2, collapse = "_"),
                        paste(inpsub2_2, collapse = "_"),
                        paste(inpsub3_2, collapse = "_"), collapse = "_")),
                "tempData/rank_genes_groups_1.csv" = paste(c(paste(inpident_1, collapse = "_"), "vs",
                                                              paste(inpident_2, collapse = "_"), "in",
                                                              paste(inpsub1_2, collapse = "_"),
                                                              paste(inpsub2_2, collapse = "_"),
                                                              paste(inpsub3_2, collapse = "_"),
                                                              "with", inpDEmethod), collapse = "_"),
                "tempData/cor_info.csv" = paste(c(
                  paste(inpsub1_2, collapse = "_"),
                  paste(inpsub2_2, collapse = "_"),
                  paste(inpsub3_2, collapse = "_"),
                  paste(min_expr, "%")), collapse = "_"),
                "tempData/rank_genes_groups.csv" = paste(c(inpGrp, "vs",
                                                            paste(inpident_2, collapse = "_"), "in",
                                                            paste(inpsub1_2, collapse = "_"),
                                                            paste(inpsub2_2, collapse = "_"),
                                                            paste(inpsub3_2, collapse = "_"),
                                                            "with", inpDEmethod), collapse = "_"),
                "tempData/fgsea_Res.csv" = paste(c(inpGrp, "enriched_in", paste(databases, collapse = "_"),
                                                    inpdatapath,
                                                    paste(inpsub1_2, collapse = "_"),
                                                    paste(inpsub2_2, collapse = "_"),
                                                    paste(inpsub3_2, collapse = "_")), collapse = "_")
  )

  if (!file.exists(file.name)) {
    return(FALSE)
  } else {
    b = read.delim(file.name, header = FALSE)
  }
  return(info == b$V1)
}

#' Load Differential Expression Results
#'
#' Extracts and filters DE results from an AnnData object that has been
#' processed with rank_genes_groups.
#'
#' @param adata_uns AnnData object with rank_genes_groups results in uns slot
#' @param inpGrp Character grouping column name (for documentation)
#' @param inpident_1 Character group 1 identifiers
#' @param inpident_2 Character group 2 identifiers (reference)
#' @param inpcutp Character p-value column to use ("p_val_adj", "p_val", or "FDR")
#' @param inpcutpval Numeric p-value cutoff threshold
#' @param inpcutfc Numeric log2 fold change cutoff
#' @param inpcutpct Numeric percent cells cutoff (0-100)
#' @param inprmgene Character vector of gene categories to remove
#'        (e.g. c("Mitochondrial (MT) genes", "Ribosomal protein large subunit (RPL)"))
#'
#' @return Data frame of filtered DE genes with columns:
#'         genes, scores, avg_log2FC, p_val, p_val_adj, pts, groups
#'
#' @importFrom reticulate import use_virtualenv
#' @importFrom qvalue qvalue
#' @importFrom magrittr %>% %<>%
#'
#' @keywords internal
#' @noRd
loadDEGs <- function(adata_uns, inpGrp, inpident_1, inpident_2, inpcutp, inpcutpval, inpcutfc, inpcutpct, inprmgene = NULL) {
  VIRTUALENV_NAME = 'pyShinyCell'
  reticulate::use_virtualenv(VIRTUALENV_NAME, required = T)
  sc <- reticulate::import("scanpy")
  "loading DEGs"

  markers <- tryCatch({
    sc$get$rank_genes_groups_df(adata_uns, group = NULL)
  }, error = function(cond) {
    message("Anndata does not seem to exist:")
    message(cond)
    return(NA)
  }, warning = function(cond) {
    message(cond)
    return(NULL)
  })

  if ("pct_nz_reference" %in% colnames(markers)) {
    colnames(markers) = c("genes", "scores", "avg_log2FC", "p_val", "p_val_adj", "pts", "pct_nz_reference")
  } else {
    colnames(markers) = c("genes", "scores", "avg_log2FC", "p_val", "p_val_adj", "pts")
  }

  if ("FDR" %in% inpcutp) {
    qobj <- qvalue::qvalue(p = markers[, "p_val"])
    markers[, "FDR"] = qobj$qvalues
    jpeg(file.path("tempData", "hist_pvalues.jpeg"), units = "in", width = 7, height = 10, res = 600)
    print(hist(qobj$pvalues))
    dev.off()
    jpeg(file.path("tempData", "qvalue_plot.jpeg"), units = "in", width = 7, height = 10, res = 600)
    print(plot(qobj))
    dev.off()
  }

  markers$groups = paste(paste(inpident_1, collapse = "_"), "vs.", paste(inpident_2, collapse = "_"))

  # remove MT, RPS and PRL genes
  if (length(inprmgene) != 0) {
    if (markers$genes[1] == toupper(markers$genes[1])) {
      rm_gene = c("^MT-", "^RPL", "^RPS")
    } else {
      rm_gene = c("^mt-", "^Rpl", "^Rps")
    }
    geneGrp = matrix(c("Mitochondrial (MT) genes",
                       "Ribosomal protein large subunit (RPL)",
                       "Ribosomal protein small subunit (RPS)",
                       rm_gene), nrow = 3)
    rmgene = geneGrp[, 1] %in% inprmgene %>% geneGrp[., 2] %>% paste(collapse = "|") %>%
      grep(markers$genes, value = T)
    markers = markers[!markers$genes %in% rmgene, ]
  }

  return(markers)
}

#' Find All Differentially Expressed Markers
#'
#' Performs all-vs-rest differential expression analysis across all groups
#' using scanpy's rank_genes_groups function. Calls Python via reticulate.
#'
#' @param inpH5ad Character path to H5AD file
#' @param inpsub1_1 Character column name for first subset dimension
#' @param inpsub1_2 Character vector of values to subset first dimension
#' @param inpsub2_1 Character column name for second subset dimension
#' @param inpsub2_2 Character vector of values to subset second dimension
#' @param inpsub3_1 Character column name for third subset dimension
#' @param inpsub3_2 Character vector of values to subset third dimension
#' @param inpGrp Character column name for grouping variable
#' @param inpident_2 Character reference group (default "rest" for all-vs-rest)
#' @param inpDEmethod Character DE test method (default "wilcoxon")
#' @param returnFormat Character "full" for full AnnData or "uns" for uns only (default "full")
#'
#' @return AnnData object with rank_genes_groups results in uns slot
#'
#' @importFrom reticulate import
#' @importFrom magrittr %>% %<>%
#'
#' @keywords internal
#' @noRd
scFindAllMarkers <- function(inpH5ad, inpsub1_1, inpsub1_2, inpsub2_1, inpsub2_2, inpsub3_1,
                             inpsub3_2, inpGrp, inpident_2 = "rest", inpDEmethod = "wilcoxon", returnFormat = c("full", "uns")) {
  returnFormat <- match.arg(returnFormat)
  sc <- reticulate::import("scanpy")
  adata <- sc$read_h5ad(inpH5ad)
  adata <- subsetAnndata(adata, inpsub1_1, inpsub1_2, inpsub2_1, inpsub2_2, inpsub3_1, inpsub3_2)

  # prepare a info  to store different analysis conditions information
  info = paste(c(inpGrp, "vs",
                 paste(inpident_2, collapse = "_"), "in",
                 paste(inpsub1_2, collapse = "_"),
                 paste(inpsub2_2, collapse = "_"),
                 paste(inpsub3_2, collapse = "_"),
                 "with", inpDEmethod), collapse = "_")
  print(info)
  adata$obs[, inpGrp] %<>% as.factor()

  if (adata$var_names[1] == "1") adata$var_names = adata$var[["gene_symbols"]]
  print("start DE")

  # check if any sample has less than 2 samples.
  df <- as.data.frame(table(adata$obs[, inpGrp]))
  adata <- subsetAnndata(adata, inpsub1_1 = inpGrp, inpsub1_2 = as.character(df$Var1[df$Freq >= 2]))
  adata$uns$log1p = NULL

  tryCatch({
    sc$tl$rank_genes_groups(adata, groupby = inpGrp, reference = inpident_2, method = inpDEmethod,
                            pts = TRUE, use_raw = FALSE)
  }, error = function(cond) {
    message("rank_genes_groups fail")
    message(cond)
  }, warning = function(cond) {
    message(cond)
  })

  print("DE done")
  adata_uns <- adata$copy()
  adata_uns$raw = NULL
  adata_uns$obsm = NULL
  adata_uns$varm = NULL
  adata_uns$obsp = NULL
  adata_uns$write_h5ad("tempData/rank_genes_groups.h5ad")
  write.table(info, file = "tempData/rank_genes_groups.csv", quote = FALSE, row.names = F, col.names = F)

  if (returnFormat == "full") return(adata)
  adata_uns$X = NULL
  if (returnFormat == "uns") return(adata_uns)
}

#' Load AnnData Object from Temporary Files
#'
#' Reads a pre-computed AnnData object from a temporary H5AD file,
#' typically containing DE results.
#'
#' @param adata_uns_path Character relative path to H5AD file in tempData/
#' @param inpsub1_1 Character column name for first subset dimension
#' @param inpsub1_2 Character vector of values to subset first dimension
#' @param inpsub2_1 Character column name for second subset dimension
#' @param inpsub2_2 Character vector of values to subset second dimension
#' @param inpsub3_1 Character column name for third subset dimension
#' @param inpsub3_2 Character vector of values to subset third dimension
#' @param inpGrp Character grouping column (for documentation)
#' @param inpX Not currently used (default NULL)
#'
#' @return AnnData object read from H5AD file
#'
#' @importFrom reticulate import
#'
#' @keywords internal
#' @noRd
LoadAnndata <- function(adata_uns_path, inpsub1_1, inpsub1_2, inpsub2_1, inpsub2_2, inpsub3_1,
                        inpsub3_2, inpGrp, inpX = NULL) {
  sc <- reticulate::import("scanpy")
  adata_degs = sc$read_h5ad(file.path("tempData", adata_uns_path))
  return(adata_degs)
}

#' Subset AnnData Object by Metadata
#'
#' Subsets an AnnData object by filtering observations based on
#' metadata column values.
#'
#' @param adata AnnData object to subset
#' @param inpsub1_1 Character column name for first subset dimension (default NULL)
#' @param inpsub1_2 Character vector of values to keep in first dimension (default NULL)
#' @param inpsub2_1 Character column name for second subset dimension (default NULL)
#' @param inpsub2_2 Character vector of values to keep in second dimension (default NULL)
#' @param inpsub3_1 Character column name for third subset dimension (default NULL)
#' @param inpsub3_2 Character vector of values to keep in third dimension (default NULL)
#' @param inpsub4_1 Character column name for fourth subset dimension (default NULL, unused)
#' @param inpsub4_2 Character vector of values for fourth dimension (default NULL, unused)
#'
#' @return Subsetted AnnData object (or original if no subset applied)
#'
#' @keywords internal
#' @noRd
subsetAnndata <- function(adata, inpsub1_1 = NULL, inpsub1_2 = NULL, inpsub2_1 = NULL, inpsub2_2 = NULL,
                          inpsub3_1 = NULL, inpsub3_2 = NULL, inpsub4_1 = NULL, inpsub4_2 = NULL) {

  keep = rep(TRUE, adata$n_obs)
  if (!is.null(inpsub1_2)) keep = keep & adata$obs[, inpsub1_1] %in% inpsub1_2
  if (!is.null(inpsub2_2)) keep = keep & adata$obs[, inpsub2_1] %in% inpsub2_2
  if (!is.null(inpsub3_2)) keep = keep & adata$obs[, inpsub3_1] %in% inpsub3_2

  print(table(keep))
  if (all(keep)) {
    return(adata)
  } else {
    return(adata[keep]$copy())
  }
}

#' Generate and Save Rank Genes Groups Visualization
#'
#' Creates visualization plots from rank_genes_groups results using
#' scanpy plotting functions via reticulate.
#'
#' @param adata AnnData object with rank_genes_groups results
#' @param inpPlt Character plot type ("Rank_genes", "Dotplot", "MatrixPlot",
#'               "Stacked Violin", "Heatmap", "TracksPlot", "Correlation Matrix")
#' @param inpGrp Character grouping column name
#' @param inpX Character vector of groups to plot (default NULL for all)
#' @param inpvalToPlot Character value to plot in heatmap
#' @param inpcutfc Numeric log fold change threshold
#' @param inpcutpct Numeric percent cells threshold
#' @param inpZscore Numeric z-score normalization (unused)
#' @param inptop Integer number of top genes to show
#' @param inpcols Character color palette name
#' @param inpcolinv Logical invert color palette
#' @param inpvrange Numeric value range (unused)
#' @param inpdendro Logical show dendrogram
#' @param inpfsz Character font size ("Extra Small", "Small", "Medium", "Large", "Extra Large")
#' @param inpflpxy Logical flip x and y axes
#' @param inpfrt Numeric axis text rotation angle
#'
#' @details Uses scanpy plotting functions which save PNG files to working directory.
#'
#' @importFrom reticulate import use_virtualenv py_run_string
#' @importFrom magrittr %>% %<>%
#'
#' @keywords internal
#' @noRd
saveRankGenesGroupsFigure <- function(adata, inpPlt, inpGrp, inpX,
                                      inpvalToPlot, inpcutfc, inpcutpct, inpZscore,
                                      inptop, inpcols, inpcolinv,
                                      inpvrange, inpdendro,
                                      inpfsz, inpflpxy, inpfrt) {
  sc <- reticulate::import("scanpy")
  sc$set_figure_params(scanpy = TRUE, fontsize = sList[inpfsz] / 2, dpi = 600, dpi_save = 600)
  inptop = as.integer(inptop)

  shiny::validate(need(inptop != 0, message = paste0("Number of genes can't be zero!")))

  inpvalToPlot = switch(inpvalToPlot,
                        "mean expression" = "None",
                        paste0("'", inpvalToPlot, "'"))
  inpGrp = paste0("'", inpGrp, "'")
  if (inpcolinv) inpcols %<>% paste0("_r")
  inpcols = paste0("'", inpcols, "'")

  inpdendro = switch(as.character(inpdendro),
                     "TRUE" = "True",
                     "FALSE" = "False")
  inpflpxy = switch(as.character(inpflpxy),
                    "TRUE" = "True",
                    "FALSE" = "False")
  reticulate::py_run_string("import scanpy as sc")
  assign("adata", adata, envir = globalenv())

  if (inpPlt == "Correlation Matrix") {
    reticulate::py_run_string(paste0("r.adata.obs[", inpGrp, "] = r.adata.obs[", inpGrp, "].astype('category')"))
    reticulate::py_run_string(paste0("sc.tl.dendrogram(r.adata, groupby = ", inpGrp, ")"))
  }

  if (is.null(inpX)) {
    inpX = "None"
  } else {
    inpX = paste0("['", paste(inpX, collapse = "', '"), "']")
  }

  args_list <- list(c(", groups = ", inpX), #1
                    c(", groupby = ", inpGrp), #2
                    c(", n_genes = ", inptop), #3
                    c(", values_to_plot = ", inpvalToPlot), #4
                    c(", min_logfoldchange = ", inpcutfc), #5
                    c(", expression_cutoff = ", inpcutpct / 100), #6
                    c(", cmap = ", inpcols), #7
                    c(", dendrogram = ", inpdendro), #8
                    c(", swap_axes = ", inpflpxy), #9
                    c(", var_group_rotation = ", inpfrt), #10
                    c(", return_fig = False"), #11
                    c(", use_raw = False"), #12
                    c(", save ='.png'", ", show = False)"))

  py_script = switch(inpPlt,
                     "Rank_genes" = paste0("sc.pl.rank_genes_groups(r.adata",
                                           paste(unlist(args_list[1:13]), collapse = "")),
                     "Dotplot" = paste0("sc.pl.rank_genes_groups_dotplot(r.adata",
                                        paste(unlist(args_list[1:13]), collapse = "")),
                     "MatrixPlot" = paste0("sc.pl.rank_genes_groups_matrixplot(r.adata",
                                           paste(unlist(args_list[c(1:5, 7:13)]), collapse = "")),
                     "Stacked Violin" = paste0("sc.pl.rank_genes_groups_stacked_violin(r.adata",
                                               paste(unlist(args_list[1:13]), collapse = "")),
                     "Heatmap" = paste0("sc.pl.rank_genes_groups_heatmap(r.adata",
                                        paste(unlist(args_list[c(1:5, 7:13)]), collapse = "")),
                     "TracksPlot" = paste0("sc.pl.rank_genes_groups_tracksplot(r.adata",
                                           paste(unlist(args_list[1:13]), collapse = "")),
                     "Correlation Matrix" = paste0("sc.pl.correlation_matrix(r.adata",
                                                   paste(unlist(args_list[c(2, 7, 8, 13)]), collapse = "")))

  reticulate::py_run_string(py_script)
}

#' Load All Differential Expression Results
#'
#' Extracts and processes DE results from an all-vs-rest AnnData object.
#' Handles both pairwise and all-vs-rest result formats.
#'
#' @param adata_degs AnnData object with rank_genes_groups results
#' @param inpDEmode Character mode "FindMarkers" or "FindAllMarkers"
#' @param inpGrp Character grouping column name
#' @param inpcutp Character p-value column ("p_val_adj", "p_val")
#' @param inpcutpval Numeric p-value cutoff
#' @param inpcutfc Numeric log fold change cutoff
#' @param inpcutpct Numeric percent cells cutoff (0-100)
#' @param inpident_2 Character reference group (default "rest")
#' @param inprmgene Character vector of gene categories to remove (default NULL)
#' @param verbose Logical print status messages (default FALSE)
#'
#' @return Data frame of filtered DE genes with group column indicating comparisons
#'
#' @importFrom reticulate import
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter
#'
#' @keywords internal
#' @noRd
loadAllDEGs <- function(adata_degs, inpDEmode, inpGrp, inpcutp, inpcutpval, inpcutfc, inpcutpct, inpident_2 = "rest", inprmgene = NULL, verbose = FALSE) {

  sc <- reticulate::import("scanpy")
  markers <- tryCatch({
    sc$get$rank_genes_groups_df(adata_degs, group = NULL)
  }, error = function(cond) {
    message("Anndata does not seem to exist:")
    message(cond)
    return(NA)
  }, warning = function(cond) {
    message(cond)
    return(NULL)
  })

  if (verbose) print("Markers data frame retrieved.")

  if (!is.null(markers) && ncol(markers) > 0) {
    if (verbose) print("Setting column names based on mode...")

    if (inpDEmode == "FindAllMarkers") {
      colnames(markers) = c("group", "genes", "scores", "avg_log2FC", "p_val", "p_val_adj", "pct_nz_group", "pct_nz_reference")
    } else if (ncol(markers) == 6) {
      idents <- levels(adata_degs$obs[, inpGrp])
      inpident_1 <- idents[!idents %in% inpident_2]
      colnames(markers) = c("genes", "scores", "avg_log2FC", "p_val", "p_val_adj", "pct_nz_group")
      markers$group <- paste(inpident_1, "vs", inpident_2)
    } else {
      colnames(markers) = c("group", "genes", "scores", "avg_log2FC", "p_val", "p_val_adj", "pct_nz_group")
    }

    if (verbose) print("Column names set. Filtering markers...")

    markers = markers[markers[, inpcutp] <= inpcutpval, ]
    markers <- markers %>% dplyr::filter(abs(avg_log2FC) > inpcutfc & pct_nz_group > inpcutpct / 100)

    if (verbose) print("Markers filtered. Removing specified genes...")

    # Remove MT, RPS and PRL genes
    if (length(inprmgene) != 0) {
      if (markers$genes[1] == toupper(markers$genes[1])) {
        rm_gene = c("^MT-", "^RPL", "^RPS")
      } else {
        rm_gene = c("^mt-", "^Rpl", "^Rps")
      }
      geneGrp = matrix(c("Mitochondrial (MT) genes",
                         "Ribosomal protein large subunit (RPL)",
                         "Ribosomal protein small subunit (RPS)",
                         rm_gene), nrow = 3)
      rmgene = geneGrp[, 1] %in% inprmgene %>% geneGrp[., 2] %>% paste(collapse = "|") %>%
        grep(markers$genes, value = T)
      markers = markers[!markers$genes %in% rmgene, ]
    }

    if (verbose) print("Specified genes removed. Function complete.")
  } else {
    if (verbose) print("Markers data frame is NULL, NA, or has no columns.")
  }

  return(markers)
}
