#' Gene Correlation Analysis Functions
#'
#' This module contains functions for computing and visualizing gene-to-gene
#' correlations from single-cell data, including correlation matrices, volcano
#' plots, and interactive network visualizations.
#'
#' @keywords internal
#' @name correlation_analysis
NULL

#' Load Correlation Data for a Single Gene
#'
#' Reads correlation and p-value data for a single query gene from HDF5 files.
#' Returns genes sorted by correlation strength.
#'
#' @param inpH5 Character path to HDF5 file (default "tempData/scFindCor.h5")
#' @param featureFile Character path to RDS file with feature names
#'        (default "tempData/CorFeatures.rds")
#' @param gene Character gene name to query
#'
#' @return List with elements:
#'   - data: Data frame with correlation, p_val, and genes columns
#'   - gene: Input gene name
#'
#' @importFrom hdf5r H5File
#' @importFrom data.table data.table
#' @importFrom magrittr %>% %<>%
#'
#' @keywords internal
#' @noRd
loadCorDataGene <- function(inpH5 = "tempData/scFindCor.h5", featureFile = "tempData/CorFeatures.rds", gene = "") {
  h5file <- hdf5r::H5File$new(inpH5, mode = "r")

  features <- readRDS(featureFile) %>% .[["features"]]
  shiny::validate(need(gene %in% features, message = paste0("No gene is significantly correlated with ", gene,
                                                             ". If this happens first time, try to re-run the correlation.
                                                            Or lower the threshold and try other gene!")))

  geneIdx = which(features %in% gene[1])
  ggData <- data.table::data.table("correlation" = h5file[["cor"]]$read(args = list(geneIdx, quote(expr = ))),
                                   "p_val" = h5file[["pval"]]$read(args = list(geneIdx, quote(expr = ))),
                                   stringsAsFactors = F)
  h5file$close_all()

  ggData[["genes"]] = features
  ggData = ggData[-geneIdx, ]
  ggData = ggData[!is.nan(ggData$p_val), ]
  ggData = ggData[order(ggData$correlation, decreasing = T), ]

  return(list("data" = ggData,
              "gene" = gene))
}

#' Load Correlation Data for Multiple Genes
#'
#' Reads correlation and p-value matrices for multiple query genes from HDF5.
#' Returns top N correlated genes per query gene.
#'
#' @param inpH5 Character path to HDF5 file (default "tempData/scFindCor.h5")
#' @param featureFile Character path to RDS file with feature names
#'        (default "tempData/CorFeatures.rds")
#' @param gene Character vector of gene names to query
#' @param topN Integer number of top correlations per gene (default 10)
#' @param inpcutp Character p-value adjustment method
#'        "p_val_adj" for FDR or "p_val" for raw (default "p_val_adj")
#' @param corPosNeg Character filter correlations: "positive only", "negative only", "both"
#'        (default "both")
#'
#' @return List with elements:
#'   - cor: Correlation matrix for selected genes
#'   - pval: P-value matrix for selected genes
#'   - genes: Input gene names
#'
#' @importFrom hdf5r H5File
#' @importFrom magrittr %>% %<>%
#'
#' @keywords internal
#' @noRd
loadCorPvalGenes <- function(inpH5 = "tempData/scFindCor.h5",
                             featureFile = "tempData/CorFeatures.rds",
                             gene = "", topN = 10, inpcutp = c("p_val_adj", "p_val")[1],
                             corPosNeg = "both") {
  topN %<>% as.integer()
  h5file <- hdf5r::H5File$new(inpH5, mode = "r")

  features <- readRDS(featureFile) %>% .[["features"]]
  shiny::validate(need(gene %in% features, message = paste0("No gene is significantly correlated with ", gene,
                                                             ". Need lower the threshold or try other gene!")))

  geneIdxs = which(features %in% gene)

  h5file <- hdf5r::H5File$new(inpH5, mode = "r")
  topNCorGenes = c()
  for (geneIdx in geneIdxs) {
    topNCorGenes = switch(corPosNeg,
                          "positive only" = unique(c(topNCorGenes, tail(order(h5file[["cor"]][geneIdx, ]), topN))),
                          "negative only" = unique(c(topNCorGenes, head(order(h5file[["cor"]][geneIdx, ]), topN))),
                          "both" = unique(c(topNCorGenes,
                                            head(order(h5file[["cor"]][geneIdx, ]), n = ceiling(topN / 2)),
                                            tail(order(h5file[["cor"]][geneIdx, ]), n = ceiling(topN / 2)))))
  }

  topNCorGenes = unique(c(geneIdxs, topNCorGenes))
  corMat = h5file[["cor"]][topNCorGenes, topNCorGenes]
  pvalMat = h5file[["pval"]][, topNCorGenes]

  if (inpcutp == "p_val_adj") pvalMat = apply(pvalMat, 2, function(x) p.adjust(x, method = "fdr"))
  pvalMat = pvalMat[topNCorGenes, ]
  h5file$close_all()

  colnames(corMat) = rownames(corMat) =
    colnames(pvalMat) = rownames(pvalMat) = features[topNCorGenes]

  return(list("cor" = corMat, "pval" = pvalMat, "genes" = features[geneIdxs]))
}

#' Compute Gene-to-Gene Correlations
#'
#' Calculates Spearman correlation and p-values between genes across
#' selected cells using Python via reticulate. Saves results to HDF5.
#'
#' @param inpConf Data table configuration with UI and ID columns
#' @param inpMeta Data frame of cell metadata
#' @param inpsub1_1 Character column name for first subset dimension
#' @param inpsub1_2 Character vector of values to subset first dimension
#' @param inpsub2_1 Character column name for second subset dimension
#' @param inpsub2_2 Character vector of values to subset second dimension
#' @param inpsub3_1 Character column name for third subset dimension
#' @param inpsub3_2 Character vector of values to subset third dimension
#' @param min_expr Integer minimum expression threshold as percent (default 10)
#'
#' @details Calls Python script python/scFindCor_ad.py which performs
#'          correlation calculations on the H5AD gene expression matrix.
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom reticulate py_run_file
#'
#' @keywords internal
#' @noRd
scFindCor <- function(inpConf, inpMeta,
                      inpsub1_1, inpsub1_2, inpsub2_1, inpsub2_2, inpsub3_1, inpsub3_2,
                      min_expr = 10) {

  # Prepare ggData
  ggData = inpMeta[, c("sampleID",
                       inpConf[UI %in% inpsub1_1]$ID,
                       inpConf[UI %in% inpsub2_1]$ID,
                       inpConf[UI %in% inpsub3_1]$ID),
                   with = FALSE]
  colnames(ggData) = c("sampleID", "sub1", "sub2", "sub3")
  ggData = Subset(ggData, inpsub1_2, inpsub2_2, inpsub3_2)

  cells = which(inpMeta$sampleID %in% ggData$sampleID) - 1 %>% as.integer()

  # declared variable to the global environment
  assign("Cells", cells, envir = globalenv())
  assign("Min_expr", min_expr / 100, envir = globalenv())

  print("scFindCor start!!")
  if (!dir.exists("tempData")) dir.create("tempData")
  reticulate::py_run_file("python/scFindCor_ad.py")
  print("scFindCor end!!")

  # prepare a info  to store different analysis conditions information
  info = paste(c(
    paste(inpsub1_2, collapse = "_"),
    paste(inpsub2_2, collapse = "_"),
    paste(inpsub3_2, collapse = "_"),
    paste(min_expr, "%")), collapse = "_")
  write.table(info, file = "tempData/cor_info.csv", quote = FALSE, row.names = F, col.names = F)
}

#' Subset Data Frame by Metadata Values
#'
#' Filters a data frame by keeping only rows matching specified values
#' in three categorical columns.
#'
#' @param ggData Data frame (typically data.table) with sub1, sub2, sub3 columns
#' @param inpsub1_2 Character vector of values to keep in sub1 (default NULL)
#' @param inpsub2_2 Character vector of values to keep in sub2 (default NULL)
#' @param inpsub3_2 Character vector of values to keep in sub3 (default NULL)
#'
#' @return Subsetted data frame (same object type as input)
#'
#' @keywords internal
#' @noRd
Subset <- function(ggData, inpsub1_2 = NULL, inpsub2_2 = NULL, inpsub3_2 = NULL) {

  if (length(inpsub1_2) != 0 & length(inpsub1_2) != nlevels(ggData$sub1)) {
    ggData = ggData[sub1 %in% inpsub1_2]
  }
  if (length(inpsub2_2) != 0 & length(inpsub2_2) != nlevels(ggData$sub2)) {
    ggData = ggData[sub2 %in% inpsub2_2]
  }
  if (length(inpsub3_2) != 0 & length(inpsub3_2) != nlevels(ggData$sub3)) {
    ggData = ggData[sub3 %in% inpsub3_2]
  }

  return(ggData)
}

#' Create Volcano Plot of Correlations
#'
#' Generates a volcano plot showing correlated genes with x-axis as correlation
#' coefficient and y-axis as -log10(p-value). Highlights genes by correlation
#' strength and allows selective gene labeling.
#'
#' @param ggData Data frame with correlation, p_val, pts, genes columns
#' @param inp Character placeholder for legacy parameter
#' @param inpGene Character vector of manually selected genes to highlight
#' @param inpsub1_2 Character subset values for subtitle
#' @param inpsub2_2 Character subset values for subtitle
#' @param inpsub3_2 Character subset values for subtitle
#' @param inpcutp Character p-value column ("p_val_adj", "p_val")
#' @param inpcutpval Numeric p-value threshold
#' @param inpcutfc Numeric correlation coefficient threshold
#' @param inpcutptc Numeric percent threshold
#' @param inptop Integer number of top genes to label
#' @param inpsort Character column to sort top genes ("p_val", "p_val_adj", "avg_log2FC")
#' @param inpcols Character color palette name
#' @param inpcolinv Logical invert color palette
#' @param inpalpha Numeric alpha transparency (0-1)
#' @param inppsz Numeric point size
#' @param inpfsz Character font size
#' @param inpasp Character aspect ratio ("Square", "Fixed", "Free")
#' @param inplab1 Character label style for top genes ("No labels", "black text", "black labels", etc.)
#' @param inplab2 Character label style for manual genes
#' @param inpleg Logical show legend
#' @param inplegpos Character legend position
#'
#' @return ggplot volcano plot object
#'
#' @importFrom ggplot2 ggplot aes_string geom_point geom_vline geom_hline
#'             labs theme element_text coord_fixed scale_fill_manual
#' @importFrom ggrepel geom_text_repel geom_label_repel
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter
#'
#' @keywords internal
#' @noRd
VolcanoPlots <- function(ggData, inp, inpGene, inpsub1_2, inpsub2_2, inpsub3_2,
                         inpcutp, inpcutpval, inpcutfc, inpcutptc, inptop,
                         inpsort, inpcols, inpcolinv, inpalpha, inppsz, inpfsz,
                         inpasp, inplab1, inplab2, inpleg, inplegpos = "bottom") {

  VIRTUALENV_NAME = 'pyShinyCell'
  reticulate::use_virtualenv(VIRTUALENV_NAME, required = T)

  geneList = scGeneList(inp, inpGene)
  geneList = geneList[present == TRUE]

  cols = color_generator(inpcols, n = 10, alpha = inpalpha)[seq(1, 10, 2)]
  if (inpcolinv) cols = rev(cols)

  cols.order = c("Most Down", "Down", "Stable", "Up", "Most Up")
  names(cols) = cols.order

  ggData[, paste0("log10_", inpcutp)] = -log10(ggData[, inpcutp])
  inf = is.infinite(ggData[, paste0("log10_", inpcutp)])
  ggData[inf, paste0("log10_", inpcutp)] = 337

  ggData[, "change"] = "Stable"
  inpcutpval %<>% as.numeric()
  inpcutfc %<>% as.numeric()
  inpcutptc %<>% as.numeric()

  ggData %<>% dplyr::filter(pts > inpcutptc / 100)
  ggData[ggData[, inpcutp] <= inpcutpval & ggData$correlation > 0, "change"] = "Up"
  ggData[ggData[, inpcutp] <= inpcutpval & ggData$correlation < 0, "change"] = "Down"
  ggData[ggData$correlation > inpcutfc & ggData[, inpcutp] < inpcutpval, "change"] = "Most Up"
  ggData[ggData$correlation < -inpcutfc & ggData[, inpcutp] < inpcutpval, "change"] = "Most Down"

  ggData$change %<>% factor(levels = cols.order)

  Up <- ggData[ggData$change %in% "Most Up", ]
  Down <- ggData[ggData$change %in% "Most Down", ]

  inptop %<>% as.integer()
  if (inpsort %in% c("p_val_adj", "p_val")) {
    Up_gene_index <- rownames(Up)[Up[, inpsort] <= tail(head(sort(Up[, inpsort], decreasing = F), inptop), 1)]
    Down_gene_index <- rownames(Down)[Down[, inpsort] <= tail(head(sort(Down[, inpsort], decreasing = F), inptop), 1)]
  }

  # If too many gene with adj_p = 0
  if (length(Up_gene_index) > inptop) Up_gene_index = head(Up_gene_index, inptop)
  if (length(Down_gene_index) > inptop) Down_gene_index = tail(Down_gene_index, inptop)

  # prepare title
  subtitle = paste(c(paste(inpsub1_2, collapse = "."),
                     paste(inpsub2_2, collapse = "."),
                     paste(inpsub3_2, collapse = ".")), collapse = " ")
  if (subtitle == "  ") subtitle = NULL

  ggOut <- ggplot2::ggplot(
    ggData,
    mapping = ggplot2::aes_string(x = "correlation",
                                  y = paste0("log10_", inpcutp),
                                  fill = "change")) +
    ggplot2::geom_point(alpha = inpalpha, size = inppsz, color = "black", pch = 21) +
    ggplot2::ggtitle(label = paste(c("Gene Correlation", subtitle), collapse = " \n in "))

  # Add reference lines
  ggOut = ggOut + ggplot2::geom_vline(xintercept = c(-inpcutfc, inpcutfc), lty = 4, col = "black", lwd = 0.8)
  ggOut = ggOut + ggplot2::geom_hline(yintercept = -log10(inpcutpval), lty = 4, col = "black", lwd = 0.8)

  ggOut = ggOut +
    sctheme(base_size = sList[inpfsz]) +
    ggplot2::labs(x = "Spearman Correlation",
                  y = paste("-log10 (",
                            switch(inpcutp,
                                   "p_val_adj" = "adjusted p-value",
                                   "p-value" = "p value"), ")")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title = ggplot2::element_text(size = sList[inpfsz]),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = sList[inpfsz]),
                   legend.position = switch(as.character(inpleg),
                                            "FALSE" = "none",
                                            "TRUE" = inplegpos))

  ggOut = ggOut + ggplot2::scale_fill_manual("", values = cols[sort(unique(ggData$change), decreasing = F)])

  # Label top genes
  if (inplab1 != "No labels" & length(c(Down_gene_index, Up_gene_index)) > 0) {
    options(ggrepel.max.overlaps = Inf)
    lab_data = ggData[c(Down_gene_index, Up_gene_index), ]

    ggOut = ggOut + switch(inplab1,
                           "black text" = ggrepel::geom_text_repel(data = lab_data,
                                                                   ggplot2::aes_string(x = "correlation",
                                                                                       y = paste0("log10_", inpcutp),
                                                                                       label = "genes"),
                                                                   colour = "grey10",
                                                                   alpha = inpalpha,
                                                                   bg.color = "grey95", bg.r = 0.15,
                                                                   box.padding = unit(0.8, "lines"),
                                                                   point.padding = unit(1, "lines"),
                                                                   size = lList[inpfsz], seed = 42, force = 5),
                           "black labels" = ggrepel::geom_label_repel(data = lab_data,
                                                                      ggplot2::aes_string(x = "correlation",
                                                                                          y = paste0("log10_", inpcutp),
                                                                                          label = "genes"),
                                                                      colour = "grey10",
                                                                      fill = "white",
                                                                      alpha = inpalpha,
                                                                      box.padding = unit(0.8, "lines"),
                                                                      point.padding = unit(1, "lines"),
                                                                      size = lList[inpfsz], seed = 42, force = 5))
  }

  # Label manual genes
  if (inplab2 != "No labels" & any(geneList$gene %in% ggData$genes)) {
    ggOut = ggOut + switch(inplab2,
                           "red text" = ggrepel::geom_text_repel(data = ggData[ggData$genes %in% geneList$gene, ],
                                                                 ggplot2::aes_string(x = "correlation",
                                                                                     y = paste0("log10_", inpcutp),
                                                                                     label = "genes"),
                                                                 colour = "red",
                                                                 force = 5,
                                                                 box.padding = unit(0.5, "lines"),
                                                                 point.padding = unit(0.8, "lines"),
                                                                 size = lList[inpfsz], seed = 42),
                           "red labels" = ggrepel::geom_label_repel(data = ggData[ggData$genes %in% geneList$gene, ],
                                                                    ggplot2::aes_string(x = "correlation",
                                                                                        y = paste0("log10_", inpcutp),
                                                                                        label = "genes"),
                                                                    colour = "red",
                                                                    fill = "white",
                                                                    force = 5,
                                                                    alpha = inpalpha,
                                                                    box.padding = unit(0.5, "lines"),
                                                                    point.padding = unit(0.8, "lines"),
                                                                    size = lList[inpfsz], seed = 42))
  }

  rat = (max(ggData$correlation) - min(ggData$correlation)) /
    (max(ggData[, paste0("log10_", inpcutp)]) - min(ggData[, paste0("log10_", inpcutp)]))

  if (inpasp == "Square" & rat != Inf & rat != -Inf) {
    ggOut = ggOut + ggplot2::coord_fixed(ratio = rat)
  } else if (inpasp == "Fixed" & rat != Inf & rat != -Inf) {
    ggOut = ggOut + ggplot2::coord_fixed()
  }

  return(ggOut)
}

#' Create Correlation Scatter Plot
#'
#' Generates a scatter plot of gene pair correlations with labeled genes
#' of interest. Shows correlation on x-axis and -log10(p-value) on y-axis.
#'
#' @param ggData Data frame with correlation, p_val, genes columns
#' @param inp1 Character primary gene of interest
#' @param inp Character vector of additional genes to label
#' @param inpGene Character vector of all available genes
#' @param cor_info Character info string for subtitle
#' @param inpcutp Character p-value column ("p_val_adj", "p_val")
#' @param inpcutpval Numeric p-value threshold for secondary labels
#' @param inpcutfc Numeric correlation threshold
#' @param inptop Integer number of top genes to label
#' @param inpcol1 Character color for threshold-based labels
#' @param inpcol2 Character color for manual gene labels
#' @param inpalpha Numeric alpha transparency
#' @param inpsiz Numeric point size
#' @param inpfsz Character font size
#' @param inpasp Character aspect ratio
#'
#' @return ggplot scatter plot object
#'
#' @importFrom ggplot2 ggplot aes_string geom_point geom_vline geom_hline
#'             theme coord_fixed
#' @importFrom ggpubr ggline
#' @importFrom ggrepel geom_text_repel
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter slice_max slice_min
#'
#' @keywords internal
#' @noRd
CorPlots <- function(ggData, inp1, inp = "", inpGene, cor_info,
                     inpcutp = c("p_val_adj", "p_val")[1],
                     inpcutpval, inpcutfc, inptop,
                     inpcol1, inpcol2,
                     inpalpha, inpsiz, inpfsz, inpasp) {

  geneList = inp[inp %in% inpGene]

  inpcutpval %<>% as.numeric()
  inpcutfc %<>% as.numeric()

  if (inpcutp == "p_val_adj") ggData$p_val_adj = p.adjust(ggData$p_val, method = "fdr")

  log10_inpcutp <- -log10(ggData[[inpcutp]])
  log10_inpcutp[is.infinite(log10_inpcutp)] = log10(.Machine$double.xmax)

  ggData[[paste0("log10_", inpcutp)]] = log10_inpcutp
  ggData$idx = 1:nrow(ggData)

  inptop %<>% as.integer()
  Up_gene_index <- ggData %>% dplyr::filter(correlation > inpcutfc) %>%
    dplyr::slice_max(correlation, n = inptop) %>% .[["idx"]]
  Down_gene_index <- ggData %>% dplyr::filter(correlation < -inpcutfc) %>%
    dplyr::slice_min(correlation, n = inptop) %>% .[["idx"]]

  if (length(Up_gene_index) > inptop) Up_gene_index = head(Up_gene_index, inptop)
  if (length(Down_gene_index) > inptop) Down_gene_index = tail(Down_gene_index, inptop)

  # prepare selected label
  label.select = unique(c(ggData$genes[c(Up_gene_index, Down_gene_index)], geneList))
  label.select = label.select[label.select %in% ggData$genes]
  shiny::validate(need(length(label.select) > 0, "No gene pass filter. Lower the threshold!"))

  label_ggData = ggData[ggData$genes %in% label.select, ]
  label_ggData[["mark"]] = inpcol1
  if (length(geneList) > 0) label_ggData[label_ggData$genes %in% geneList, "mark"] = inpcol2

  options(ggrepel.max.overlaps = (inptop * 2 + length(geneList)))

  ggOut <- ggpubr::ggline(ggData,
                          x = "correlation",
                          y = paste0("log10_", inpcutp),
                          numeric.x.axis = TRUE,
                          ylab = paste("-log10 (", ifelse(inpcutp == "p_val_adj", "adjusted p-value", "p-value"), ")"),
                          xlab = "Spearman Correlation",
                          font.label = list(size = sList[inpfsz],
                                            face = "plain",
                                            color = label_ggData$mark),
                          label = "genes",
                          label.select = label.select,
                          repel = TRUE,
                          max.overlaps = Inf,
                          title = paste(c(paste("gene correlated with", inp1), cor_info), collapse = " \n in ")) +
    ggplot2::geom_point(data = label_ggData,
                        color = label_ggData$mark,
                        alpha = inpalpha, size = inpsiz, pch = 21) +
    ggplot2::ggtitle(label = paste(c(paste("gene correlated with", inp1), cor_info), collapse = " \n in "))

  # Add reference lines
  ggOut = ggOut + ggplot2::geom_vline(xintercept = c(-inpcutfc, inpcutfc), lty = 4, col = "black", lwd = 0.8)
  ggOut = ggOut + ggplot2::geom_hline(yintercept = -log10(inpcutpval), lty = 4, col = "black", lwd = 0.8)

  ggOut = ggOut + ggplot2::theme_bw() +
    sctheme(base_size = sList[inpfsz]) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title = ggplot2::element_text(size = sList[inpfsz]))

  rat = (max(ggData$correlation) - min(ggData$correlation)) /
    (max(ggData[[paste0("log10_", inpcutp)]]) - min(ggData[[paste0("log10_", inpcutp)]]))

  if (inpasp == "Square" & rat != Inf & rat != -Inf) {
    ggOut = ggOut + ggplot2::coord_fixed(ratio = rat)
  } else if (inpasp == "Fixed" & rat != Inf & rat != -Inf) {
    ggOut = ggOut + ggplot2::coord_fixed()
  }

  return(ggOut)
}

#' Create Interactive Gene Correlation Network
#'
#' Generates an interactive force-directed network graph showing gene
#' correlations using networkD3 visualization.
#'
#' @param corMat Numeric correlation matrix
#' @param pvalMat Numeric p-value matrix
#' @param cor_info Character info string for documentation
#' @param inpcutpval Numeric p-value threshold
#' @param inpcutfc Numeric correlation coefficient threshold
#' @param corPosNeg Character filter: "positive only", "negative only", "both"
#' @param inpcols Character color palette name
#' @param inpcolinv Logical invert colors
#' @param linkDistance Numeric link distance in network
#' @param linkWidth Numeric link stroke width
#' @param inpfsz Character font size (unused in networkD3)
#' @param showName Logical show node names on hover
#' @param inpleg Logical show legend
#' @param inplegpos Character legend position
#'
#' @return forceNetwork object (interactive HTMLwidget)
#'
#' @importFrom igraph graph.adjacency simplify cluster_walktrap membership
#' @importFrom networkD3 forceNetwork igraph_to_networkD3
#' @importFrom magrittr %>% %<>%
#' @importFrom plyr mapvalues
#'
#' @keywords internal
#' @noRd
CorNetwork <- function(corMat, pvalMat, cor_info,
                       inpcutpval, inpcutfc,
                       corPosNeg = "positive only",
                       inpcols, inpcolinv,
                       linkDistance, linkWidth, inpfsz, showName,
                       inpleg = FALSE, inplegpos = "bottom") {

  inpcutpval %<>% as.numeric()
  inpcutfc %<>% as.numeric()

  # Filter correlation matrix by thresholds
  if (corPosNeg == "positive only") corMat[which(corMat <= inpcutfc)] = 0
  if (corPosNeg == "negative only") corMat[which(corMat >= -inpcutfc)] = 0
  if (corPosNeg == "both") corMat[which(abs(corMat) <= inpcutfc)] = 0

  corMat[which(pvalMat > inpcutpval)] = 0

  # Remove genes with no connections (except self-loop)
  keep = which(rowSums(abs(corMat)) > 1)
  corMat <- corMat[names(keep), names(keep)]

  shiny::validate(need(!any(dim(corMat) == c(0, 0)), "No gene pass filter. Either lower the correlation cut-off,
                                        or increase the p-value cut-off"))

  # Remove duplicate edges and create igraph
  g1 <- igraph::graph.adjacency(corMat, weight = T, mode = "undirected")
  g1 <- igraph::simplify(g1)

  # Find group membership via clustering
  wt <- igraph::cluster_walktrap(g1, steps = 6)
  members <- igraph::membership(wt)

  # Convert igraph to list for networkD3
  sj_list <- networkD3::igraph_to_networkD3(g1, group = members)

  # Prepare link colors based on correlation values
  Range = range(sj_list$links$value, na.rm = T)
  n = ceiling(max(Range) * 10) * 4

  ggCol = switch(inpcols,
                 "default" = rep("#333", n * 2),
                 color_generator(inpcols, n = n * 2))
  if (inpcolinv) ggCol = rev(ggCol)

  linkcols = cut(sj_list$links$value, breaks = c(-n:n) / 40)

  sj_list$links$color = plyr::mapvalues(linkcols,
                                         from = levels(linkcols),
                                         to = ggCol)
  sj_list$links$color %<>% droplevels()

  # Set node groups
  sj_list$nodes$group = 1

  p <- networkD3::forceNetwork(Links = sj_list$links, Nodes = sj_list$nodes,
                               Source = 'source', Value = "value",
                               Target = 'target', NodeID = 'name', Group = 'group',
                               linkColour = sj_list$links$color,
                               fontFamily = "Avenir",
                               fontSize = 20,
                               opacity = 1,
                               opacityNoHover = ifelse(showName, 5, 0),
                               legend = inpleg, bounded = FALSE,
                               linkDistance = as.numeric(linkDistance),
                               linkWidth = as.numeric(linkWidth),
                               charge = -30,
                               zoom = FALSE)

  return(p)
}
