#' Gene Set Enrichment Analysis Functions
#'
#' This module contains functions for performing gene set enrichment analysis (GSEA)
#' and pathway enrichment using fgsea and enrichR packages on differential
#' expression results.
#'
#' @keywords internal
#' @name gsea_analysis
NULL

#' Perform Enrichr Enrichment Analysis
#'
#' Generates enrichment results using enrichR databases for each group
#' in a DE result set.
#'
#' @param DEGs Data frame of DE genes with columns: genes, group, avg_log2FC, p_val
#' @param order.by Character column to order genes by (default "log2FC_log10p")
#'        Options: "log2FC_log10p", "scores", "avg_log2FC", "p_val", "p_val_adj"
#' @param databases Character vector of enrichR database names
#'        (default "MSigDB_Hallmark_2020")
#'
#' @return List of data frames containing enrichR results per group
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr bind_rows
#' @importFrom tibble deframe
#' @importFrom pbapply pblapply
#' @importFrom enrichR enrichr
#'
#' @keywords internal
#' @noRd
enrichrRes <- function(DEGs, order.by = "log2FC_log10p", databases = "MSigDB_Hallmark_2020") {
  shiny::validate(need(length(databases) >= 1, message = "Need at least one database"))

  if (order.by == "log2FC_log10p") {
    DEGs[DEGs$p_val == 0, "p_val"] = min(DEGs[DEGs$p_val > 0, "p_val"], .Machine$double.xmin)
    DEGs$log2FC_log10p = DEGs$avg_log2FC * (-log10(DEGs$p_val))
  }

  DEGs_list <- split(DEGs, f = DEGs$group)

  enrichr_Res <- pbapply::pblapply(DEGs_list, function(deg) {
    suppressWarnings(geneRank <- deg[order(deg[order.by]), c("genes", "avg_log2FC")] %>%
                       tibble::deframe())
    tmp_list <- tryCatch({
      enrichR::enrichr(names(geneRank), databases = databases)
    }, error = function(cond) {
      message("enrichr fail")
      message(cond)
      return(NA)
    }, warning = function(cond) {
      message(cond)
      return(NULL)
    })

    # record and remove empty element in tmp
    emp <- c()
    for (k in seq_along(tmp_list)) {
      if (nrow(tmp_list[[k]]) > 0) {
        tmp_list[[k]][, "database"] = names(tmp_list[k])
      } else {
        emp = c(emp, k)
      }
    }
    if (!is.null(emp)) tmp_list[emp] = NULL
    tmp = dplyr::bind_rows(tmp_list)
    tmp = tmp[tmp$Adjusted.P.value < 0.05, ]
    if (nrow(tmp) > 0) {
      tmp$group = unique(deg$group)
    } else {
      tmp = NULL
    }
    return(tmp)
  })

  return(enrichr_Res)
}

#' Perform FGSEA Pathway Enrichment Analysis
#'
#' Generates fgsea results for pathway enrichment analysis on DE genes,
#' supporting multiple databases and custom pathways.
#'
#' @param DEGs Data frame of DE genes with group column
#' @param order.by Character gene ranking column (default "log2FC_log10p")
#' @param databases Character vector of pathway database names
#'        (default c("MSigDB_Hallmark_2020"))
#' @param inpdatabases Data frame of custom pathways (default NULL)
#' @param inpdatapath Character path for documentation (default NULL)
#' @param inpGrp Character grouping column name for documentation (default NULL)
#' @param inpsub1_2 Character subset values for documentation
#' @param inpsub2_2 Character subset values for documentation
#' @param inpsub3_2 Character subset values for documentation
#' @param verbose Logical print progress messages (default TRUE)
#'
#' @return Data frame of FGSEA results with pathway, NES, pval, padj columns
#'         and additional processed -log10 p-value columns
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr bind_rows filter
#' @importFrom tibble deframe
#' @importFrom fgsea fgseaMultilevel
#' @importFrom pbapply pblapply
#'
#' @keywords internal
#' @noRd
fgseaRes <- function(DEGs, order.by = "log2FC_log10p", databases = c("MSigDB_Hallmark_2020"), inpdatabases = NULL,
                     inpdatapath = NULL, inpGrp = NULL, inpsub1_2, inpsub2_2, inpsub3_2, verbose = TRUE) {

  shiny::validate(need(length(databases) >= 1, message = "Need at least one database"))

  if (order.by == "log2FC_log10p") {
    DEGs[DEGs$p_val == 0, "p_val"] = min(DEGs[DEGs$p_val > 0, "p_val"], .Machine$double.xmin)
    DEGs$log2FC_log10p = DEGs$avg_log2FC * (-log10(DEGs$p_val))
  }

  DEGs$genes %<>% toupper()
  DEGs_list <- split(DEGs, f = DEGs$group)

  pathways_list <- readRDS("tempData/Enrichr.rds")
  pathways_list <- pathways_list[databases] %>% unlist(recursive = FALSE)

  if (!is.null(inpdatabases)) {
    inp_pathways_list <- df2list(inpdatabases)
    pathways_list <- c(pathways_list, inp_pathways_list)
  }

  fgsea_Res <- pbapply::pblapply(DEGs_list, function(deg) {
    suppressWarnings(geneRank <- deg[order(deg[, order.by]), c("genes", order.by)] %>% tibble::deframe())

    tmp <- fgsea::fgseaMultilevel(pathways = pathways_list, stats = geneRank, eps = 1e-50) %>% dplyr::bind_rows()

    if (nrow(tmp) > 0) {
      leadingEdges = sapply(tmp[, "leadingEdge"]$leadingEdge, function(x) paste(x, collapse = ","))
      tmp[, "leadingEdge"] = leadingEdges
      colnames(tmp) %<>% sub("leadingEdge", "Genes", .)
      tmp$group = unique(deg$group)
    } else {
      tmp = NULL
    }
    return(tmp)
  }) %>% .[!is.null(.)] %>% dplyr::bind_rows() %>% as.data.frame()

  if (nrow(fgsea_Res) == 0) stop("No significant pathway! Try to increase DEGs's p-value!")

  fgsea_Res[, " -log10(pval)"] = -log10(fgsea_Res$pval)
  fgsea_Res[, " -log10(padj)"] = -log10(fgsea_Res$padj)
  fgsea_Res %<>% dplyr::relocate(Genes, .after = " -log10(padj)")

  if (verbose) message(round(dim(fgsea_Res)[1] / length(unique(fgsea_Res$group))))
  saveRDS(fgsea_Res, file = "tempData/fgsea_Res.rds")

  info = paste(c(inpGrp, "enriched_in", paste(databases, collapse = "_"),
                 inpdatapath,
                 paste(inpsub1_2, collapse = "_"),
                 paste(inpsub2_2, collapse = "_"),
                 paste(inpsub3_2, collapse = "_")), collapse = "_")

  write.table(info, file = "tempData/fgsea_Res.csv", quote = FALSE, row.names = F, col.names = F)
  return(fgsea_Res)
}

#' Save Pathway Data to Files
#'
#' Saves a pathway list to RDS format and database names to text file.
#'
#' @param pathways_list List of pathways to save
#'
#' @details Saves to tempData/Enrichr.rds and tempData/Enrichr_databases.txt
#'
#' @keywords internal
#' @noRd
save_pathway_data <- function(pathways_list) {

  # Save the list to RDS
  saveRDS(pathways_list, file = "tempData/Enrichr.rds")

  # Extract database names
  database_names <- names(pathways_list)

  # Write the database names to a note (txt file)
  write.table(database_names,
              file = "tempData/Enrichr_databases.txt",
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE)

  cat("Data saved successfully!\n")
}

#' Convert Data Frame to Gene Set List
#'
#' Converts a data frame (or matrix) with unequal length columns into a
#' named list of character vectors, removing NA and empty values.
#'
#' @param df Data frame or matrix to convert
#'
#' @return Named list where each column becomes a list element containing
#'         non-NA non-empty values
#'
#' @keywords internal
#' @noRd
df2list <- function(df) {
  if (is.matrix(df)) df <- as.data.frame(df)
  list <- lapply(df, as.vector)
  list <- lapply(list, function(x) x[!is.na(x)])
  list <- lapply(list, function(x) x[!(x == "")])
  list <- lapply(list, function(x) gsub("\\s", "", x)) # remove space
  names(list) <- names(df)
  return(list)
}

#' Dendrogram-based FGSEA Result Ordering
#'
#' Filters and orders fgsea results using hierarchical clustering dendrograms
#' for both pathways and groups. Supports custom group ordering.
#'
#' @param fgsea_Res Data frame of fgsea results with columns:
#'                  pathway, NES, pval, padj, group
#' @param inpgscutpadj Numeric adjusted p-value cutoff (default 0.25)
#' @param inpgscutpval Numeric p-value cutoff (default 0.05)
#' @param Rowv Logical cluster pathways (rows) (default TRUE)
#' @param Colv Logical cluster groups (columns) (default TRUE)
#' @param order_row Character group to order by NES (default "None" for dendrogram)
#' @param inpX Character vector of groups to include (default NULL for all)
#'
#' @return Filtered and ordered fgsea data frame with factors set for plotting
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#'
#' @keywords internal
#' @noRd
fgseaDendrogram <- function(fgsea_Res, inpgscutpadj = 0.25, inpgscutpval = 0.05,
                            Rowv = TRUE, Colv = TRUE,
                            order_row = "None",
                            inpX = NULL) {
  if (!is.null(inpX)) {
    fgsea_Res %<>% dplyr::filter(group %in% inpX)
    fgsea_Res[, "group"] %<>% factor(levels = inpX)
  }

  shiny::validate(need(length(unique(fgsea_Res$group)) > 1, "Dotplot need at least two groups. Do you want to try Barplot or use FindMarker?"))

  fgsea_Res %<>% dplyr::filter(padj < inpgscutpadj & pval < inpgscutpval)
  shiny::validate(need(nrow(fgsea_Res) > 1, "No pathways passed qc. Try more pathways. To debug, increase the gene sets p_val_adj and p_val"))

  # prepare dendrogram ==================
  mtx_fgseaRes <- tidyr::pivot_wider(fgsea_Res[, c("pathway", "NES", "group")],
                                     names_from = "group", values_from = "NES",
                                     values_fill = 0) %>%
    tibble::column_to_rownames("pathway")

  order.yaxis = rownames(mtx_fgseaRes)
  order.yaxis = order.yaxis[order.yaxis %in% fgsea_Res[, "pathway"]]

  if (order_row != "None") {
    order_yaxis <- fgsea_Res[, c("pathway", "NES", "group")] %>%
      dplyr::filter(group == order_row) %>%
      dplyr::arrange(desc(NES)) %>%
      .[, "pathway"]
    pty_dont_exist <- order.yaxis[!order.yaxis %in% order_yaxis]
    order.yaxis <- c(order_yaxis, pty_dont_exist)
  } else {
    if (Rowv & nrow(mtx_fgseaRes) > 2) {
      hcr <- hclust(as.dist(1 - cor(t(mtx_fgseaRes), method = "spearman")),
                    method = "ward.D2")
      ddr <- as.dendrogram(hcr)
      rowInd <- order.dendrogram(ddr)
      order.yaxis = order.yaxis[rowInd]
    }
  }

  fgsea_Res %<>% dplyr::filter(pathway %in% order.yaxis)
  fgsea_Res[, "pathway"] %<>% factor(levels = rev(order.yaxis))

  if (Colv & (ncol(mtx_fgseaRes) > 2)) {
    hcc <- hclust(as.dist(1 - cor(mtx_fgseaRes, method = "spearman")),
                  method = "ward.D2")
    ddc <- as.dendrogram(hcc)
    colInd <- order.dendrogram(ddc)
    inpX = colnames(mtx_fgseaRes)[colInd]
    fgsea_Res[, "group"] %<>% factor(levels = inpX)
  }

  return(fgsea_Res)
}

#' Sort FGSEA Results for Single Group
#'
#' Filters and sorts fgsea results for a single group by NES value.
#'
#' @param fgsea_Res Data frame of fgsea results
#' @param inpgscutpadj Numeric adjusted p-value cutoff (default 0.25)
#' @param inpgscutpval Numeric p-value cutoff (default 0.05)
#' @param order_row Character group to sort by (unused, kept for compatibility)
#' @param inpX Character vector of groups to filter (default NULL for all)
#'
#' @return Filtered fgsea data frame sorted by NES (ascending)
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter arrange
#'
#' @keywords internal
#' @noRd
fgseaSort <- function(fgsea_Res, inpgscutpadj = 0.25, inpgscutpval = 0.05,
                      order_row = "None",
                      inpX = NULL) {
  if (!is.null(inpX)) {
    fgsea_Res %<>% dplyr::filter(group %in% inpX)
  }

  fgsea_Res %<>% dplyr::filter(padj < inpgscutpadj & pval < inpgscutpval)

  shiny::validate(need(length(unique(fgsea_Res$group)) == 1, "Barplot need specify one group. Do you want to try Dotplot?"))
  fgsea_Res %<>% dplyr::arrange(NES)

  return(fgsea_Res)
}

#' Create FGSEA Dot Plot
#'
#' Generates a dot plot visualization of FGSEA pathway enrichment results
#' with pathway names on y-axis and groups on x-axis.
#'
#' @param fgsea_Res Data frame of fgsea results
#' @param inpvalToPlot Character metric to plot as dot size
#'        (default " -log10(pval)")
#' @param scale.by Character "size" or "radius" scaling function (default "size")
#' @param fill Character column for dot color (default "NES")
#' @param cols Character vector of color palette
#' @param inpfullrange Logical use full NES range for colors (default FALSE)
#' @param inpcircle Logical add circle outline to dots (default TRUE)
#' @param inppsz Numeric point size parameter (default 5)
#' @param inpfsz Character font size ("Extra Small", "Small", "Medium", "Large", "Extra Large")
#' @param inpflpxy Logical flip x and y axes (default FALSE)
#' @param verbose Logical print messages (default TRUE)
#' @param inpfrt Numeric axis text rotation angle (default 45)
#' @param ... Additional ggplot theme parameters
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 ggplot aes_string geom_point scale_fill_gradientn
#'             theme_bw element_blank element_text theme
#' @importFrom magrittr %>%
#'
#' @keywords internal
#' @noRd
FgseaDotPlot <- function(fgsea_Res, inpvalToPlot = " -log10(pval)",
                         scale.by = c('size', 'radius')[1],
                         fill = "NES",
                         cols = pal_gsea()(12), inpfullrange = FALSE, inpcircle = TRUE, inppsz = 5,
                         inpfsz = "Small", inpflpxy = FALSE,
                         verbose = T, inpfrt = 45, ...) {

  # generate color pal_gsea scale based on NES range.
  rescale_colors <- function(cols = cols, Range = range(fgsea_Res$NES, na.rm = T)) {
    n = round(length(cols) / 2)
    if (Range[1] > 0) return(cols[(n + 1):(2 * n)])
    if (Range[2] < 0) return(cols[1:n])
    if (Range[1] < 0 & Range[2] > 0) {
      remove <- (Range[2] + Range[1]) / (Range[2] - Range[1])
      if (remove > 0) return(cols[max(1, 2 * n * remove):(2 * n)])
      if (remove < 0) return(cols[1:(2 * n + min(-1, 2 * n * remove) + 1)])
    }
  }

  scale.func <- switch(
    EXPR = scale.by,
    'size' = ggplot2::scale_size,
    'radius' = ggplot2::scale_radius,
    stop("'scale.by' must be either 'size' or 'radius'")
  )

  plot <- ggplot2::ggplot(data = fgsea_Res, mapping = ggplot2::aes_string(x = "group", y = "pathway")) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(size = inpvalToPlot, fill = fill),
                        color = ifelse(inpcircle, "black", "white"),
                        pch = 21) +
    scale.func(range = c(1, as.numeric(inppsz))) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_bw(base_size = sList[inpfsz], base_family = "Avenir") +
    ggplot2::scale_fill_gradientn(colors = if (inpfullrange) cols else {
      rescale_colors(cols = cols, Range = range(fgsea_Res[, fill], na.rm = T))
    }) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = as.numeric(inpfrt),
                                                       hjust = switch(as.character(inpfrt),
                                                                      "0" = 0.5,
                                                                      "30" = 1,
                                                                      "45" = 1,
                                                                      "90" = 0),
                                                       vjust = switch(as.character(inpfrt),
                                                                      "0" = 0,
                                                                      "30" = 1,
                                                                      "45" = 1,
                                                                      "90" = 0.5)))

  if (inpflpxy) plot = plot + ggplot2::coord_flip()
  if (inpvalToPlot %in% c("padj", "pval")) {
    plot = plot + scale.func(breaks = c(0, 0.05, 0.10, 0.15, 0.2, 0.25),
                              labels = rev(c(0, 0.05, 0.10, 0.15, 0.2, 0.25)))
  }
  if (inpvalToPlot %in% c(" -log10(padj)", " -log10(pval)")) {
    plot = plot + scale.func(breaks = c(1, 2, 5, 10, 20, 30, 40))
  }

  return(plot)
}

#' Create FGSEA Bar Plot
#'
#' Generates a horizontal or vertical bar plot of FGSEA pathway enrichment
#' results, colored by regulation direction (upregulated vs downregulated).
#'
#' @param fgsea_Res Data frame of fgsea results
#' @param fill Character column to display as bar height (default "NES")
#' @param inpcols Character vector of colors for up/down regulation
#' @param inpcolinv Logical invert color order (default FALSE)
#' @param inpfsz Character font size (default "Small")
#' @param inpflpxy Logical flip to horizontal bars (default FALSE)
#' @param inpfrt Numeric axis label rotation (default 0)
#' @param verbose Logical print messages (default TRUE)
#'
#' @return ggplot bar plot object
#'
#' @importFrom ggplot2 ggplot aes_string geom_bar theme_bw element_blank
#'             element_text theme
#' @importFrom ggpubr ggbarplot
#' @importFrom magrittr %>% %<>%
#'
#' @keywords internal
#' @noRd
FgseaBarplot <- function(fgsea_Res, fill = "NES",
                         inpcols = pal_gsea()(12), inpcolinv = FALSE,
                         inpfsz = "Small", inpflpxy = FALSE, inpfrt = 0,
                         verbose = T) {

  fgsea_Res$sign <- ifelse(fgsea_Res$NES > 0, "Upregulated", "Downregulated")
  fgsea_Res$sign %<>% factor(levels = c("Upregulated", "Downregulated"))

  if (inpcolinv) inpcols = rev(inpcols)

  g <- ggpubr::ggbarplot(fgsea_Res,
                         x = "pathway",
                         y = fill,
                         fill = "sign",
                         color = "white",
                         rotate = inpflpxy,
                         palette = inpcols,
                         sort.val = "asc",
                         sort.by.groups = FALSE,
                         ylab = 'Normalized Enrichment Score',
                         legend.title = "") +
    ggplot2::theme_bw(base_size = sList[inpfsz], base_family = "Avenir") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = as.numeric(inpfrt),
                                                       hjust = switch(as.character(inpfrt),
                                                                      "0" = 0.5,
                                                                      "30" = 1,
                                                                      "45" = 1,
                                                                      "90" = 0),
                                                       vjust = switch(as.character(inpfrt),
                                                                      "0" = 0,
                                                                      "30" = 1,
                                                                      "45" = 1,
                                                                      "90" = 0.5)))

  return(g)
}
