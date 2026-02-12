#' TCR/BCR Immune Repertoire Analysis Functions
#'
#' Functions for analyzing T cell receptor (TCR) and B cell receptor (BCR)
#' clonotype data including abundance, diversity, and clonal expansion tracking.
#' Wrapper around scRepertoire package functionality adapted for pyShinyCell.
#'
#' @keywords internal
#' @name tcr_repertoire
NULL

#' Main TCR/BCR Repertoire Analysis Wrapper
#'
#' Orchestrates TCR/BCR analysis including clonotype quantification, diversity,
#' and clonal expansion tracking. Integrates multiple analysis types into a
#' single visualization.
#'
#' @param inpConf data.table. Configuration object (scConf).
#' @param inpMeta data.frame. Cell metadata.
#' @param inp1 Character. Primary grouping variable (e.g., "cell.type").
#' @param inp2 Character. Secondary grouping variable (e.g., "condition").
#' @param inpsub1_1,inpsub1_2 Character. First subset dimension and values.
#' @param inpsub2_1,inpsub2_2 Character. Second subset dimension and values.
#' @param inpsub3_1,inpsub3_2 Character. Third subset dimension and values.
#' @param inptyp Character. Analysis type.
#' @param inpscale Logical. Convert to percentages?
#' @param inpindex Character. Diversity index.
#' @param inpcloneCall Character. Clonotype definition.
#' @param inplvls Numeric. Expansion/contraction threshold.
#' @param inpcols Character. Color palette name.
#' @param inpflpxy Logical. Flip coordinates?
#' @param inppts Logical. Show points?
#' @param inpsiz Numeric. Point/bar size.
#' @param inpfsz Character. Font size.
#' @param inplsz Character. Line size.
#' @param inpfrt Numeric. X-axis rotation.
#' @param inpleg Logical. Show legend?
#' @param inplegpos Character. Legend position.
#'
#' @return ggplot object with TCR/BCR visualization.
#' @keywords internal
#' @noRd
scRepertoire <- function(inpConf, inpMeta, inp1, inp2,
                         inpsub1_1, inpsub1_2, inpsub2_1, inpsub2_2, inpsub3_1, inpsub3_2,
                         inptyp, inpscale, inpindex, inpcloneCall,
                         inplvls, inpcols,
                         inpflpxy, inppts, inpsiz, inpfsz, inplsz,
                         inpfrt, inpleg, inplegpos = "bottom") {
  cloneCall <- switch(inpcloneCall,
    "TCR/Ig genes" = "CTgene",
    "CDR3 nucleotide" = "CTnt",
    "CDR3 amino acid" = "CTaa",
    "TCR/Ig + CDR3 nucleotide" = "CTstrict"
  )

  ggData = inpMeta[, c(
    inpConf[UI == inp1]$ID,
    inpConf[UI == inp2]$ID,
    inpConf[UI == "patient"]$ID,
    inpConf[UI == cloneCall]$ID,
    inpConf[UI == "tcr.frequency"]$ID,
    inpConf[UI %in% inpsub1_1]$ID,
    inpConf[UI %in% inpsub2_1]$ID,
    inpConf[UI %in% inpsub3_1]$ID
  ),
  with = FALSE
  ]
  colnames(ggData) = c("X", inp2, "paired", cloneCall, "tcr.frequency", "sub1", "sub2", "sub3")
  ggData = Subset(ggData, inpsub1_2, inpsub2_2, inpsub3_2)

  temp1 <- doFactoring(ggData, inpConf, col = "X", inp1, inpcols)
  temp2 <- doFactoring(temp1$ggData, inpConf, col = inp2, inp2, inpcols)

  gglvl1 = temp1$gglvl; ggCol1 = temp1$ggCol
  gglvl2 = temp2$gglvl; ggCol2 = temp2$ggCol
  ggData = temp2$ggData
  rm(temp1); rm(temp2)

  ggOut = switch(inptyp,
    "Unique Barplot" = scquantContig(ggData, cloneCall = cloneCall, scale = inpscale, gglvl1, cols = ggCol1[gglvl1]),
    "Clone sizes distribution" = scabundanceContig(ggData, inptyp, cloneCall = cloneCall, chain = "both", group = "X", cols = ggCol1[gglvl1], inppts, inpsiz, inpfsz, inplsz),
    "Cumulative clone sizes distribution" = scabundanceContig(ggData, inptyp, cloneCall = cloneCall, chain = "both", group = "X", cols = ggCol1[gglvl1], inppts, inpsiz, inpfsz, inplsz),
    "Proportion Barplot" = scClonalProportion(ggData, Split = c(10, 100, 1000, 10000, 30000, 100000), cloneCall = cloneCall, chain = "both", inpcols),
    "Paired Diversity" = scClonalDiversity(ggData, inptyp, cloneCall = cloneCall, chain = "both", scale = inpscale, inpindex = inpindex, inp1, groupBy = inp2, x.axis = "X", cols = ggCol1[gglvl1], gglvl1, inppts, inpsiz, inpfsz, inplsz = inplsz, n.boots = 100),
    "unpaired Diversity" = scClonalDiversity(ggData, inptyp, cloneCall = cloneCall, chain = "both", scale = inpscale, inpindex = inpindex, inp1, groupBy = inp2, x.axis = "X", cols = ggCol1[gglvl1], gglvl = gglvl1, inppts, inpsiz, inpfsz, inplsz, n.boots = 100),
    "Paired scatter Clonotype" = ScatterClonotype(ggData, inptyp, inpConf, cloneCall = cloneCall, chain = "both", scale = inpscale, inp2, x.axis = gglvl1[1], y.axis = gglvl1[2], inplvls, inpcols, inpsiz, inplsz),
    "unpaired scatter Clonotype" = ScatterClonotype(ggData, inptyp, inpConf, cloneCall = cloneCall, chain = "both", scale = inpscale, inp2, x.axis = gglvl1[1], y.axis = gglvl1[2], inplvls, inpcols, inpsiz, inplsz),
    "Novel & Expand & Contract Barplot_1" = ScClonotypeBar(ggData, inptyp, inpConf, cloneCall = cloneCall, chain = "both", x.axis = gglvl1[1], y.axis = gglvl1[2], inp2, gglvl2, ggCol2, inpscale, inpcols, inplvls, inppts, inpsiz, inplsz),
    "Novel & Expand & Contract Barplot_2" = ScClonotypeBar(ggData, inptyp, inpConf, cloneCall = cloneCall, chain = "both", x.axis = gglvl1[1], y.axis = gglvl1[2], inp2, gglvl2, ggCol2, inpscale, inpcols, inplvls, inppts, inpsiz, inplsz)
  )

  if(inpflpxy) ggOut = ggOut + ggplot2::coord_flip()
  ggOut = ggOut + sctheme(base_size = sList[inpfsz]) +
    ggplot2::theme(legend.position = switch(as.character(inpleg), "FALSE" = "none", "TRUE" = inplegpos),
                   legend.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = as.numeric(inpfrt), hjust = switch(as.character(inpfrt), "0" = 0.5, "30" = 1, "45" = 1, "90" = 0),
                                                       vjust = switch(as.character(inpfrt), "0" = 0, "30" = 1, "45" = 1, "90" = 0.5)))
  return(ggOut)
}

#' Quantify Unique Clonotypes
#' @param ggData data.frame. Input data.
#' @param cloneCall Character. Clonotype column name.
#' @param scale Logical. Convert to percentages?
#' @param gglvl Character vector. Factor levels.
#' @param chain Character. TCR/BCR chain.
#' @param cols Character vector. Colors.
#' @return ggplot object.
#' @keywords internal
#' @noRd
scquantContig <- function(ggData, cloneCall = "CTstrict", scale = FALSE, gglvl, chain = "both", cols) {
  df <- split(ggData, by = "X")
  df = df[gglvl]
  inpcloneCall <- switch(cloneCall, "CTgene" = "TCR/Ig genes", "CTnt" = "CDR3 nucleotide", "CTaa" = "CDR3 amino acid", "CTstrict" = "TCR/Ig + CDR3 nucleotide")
  x <- "values"; labs <- "patient"
  Con.df <- data.frame(matrix(NA, length(df), 3))
  colnames(Con.df) <- c("contigs", "values", "total")
  for (i in seq_along(df)) {
    Con.df[i, 1] <- length(unique(df[[i]][[cloneCall]]))
    Con.df[i, 2] <- names(df)[i]
    Con.df[i, 3] <- length(df[[i]][[cloneCall]])
  }
  if (scale == TRUE) {
    y <- "scaled"; Con.df$scaled <- Con.df$contigs/Con.df$total * 100
    ylab <- paste("Percent of Unique", inpcloneCall, "Clonotype")
  } else {
    y <- "contigs"; ylab <- paste("Unique", inpcloneCall, "Clonotype")
  }
  Con.df[, x] %<>% factor(levels = gglvl)
  plot <- ggplot2::ggplot(ggplot2::aes(x = Con.df[, x], y = Con.df[, y], fill = as.factor(Con.df[, x])), data = Con.df) +
    ggplot2::stat_summary(geom = "errorbar", fun.data = ggplot2::mean_se, position = "dodge", width = 0.5) +
    ggplot2::labs(fill = labs) + ggplot2::stat_summary(fun = mean, geom = "bar", color = "black", lwd = 0.25) +
    ggplot2::theme_classic() + ggplot2::xlab("patient") + ggplot2::ylab(ylab) + ggplot2::scale_fill_manual(values = cols)
  return(plot)
}

#' Plot Clonotype Size Distribution
#' @param ggData data.frame. Input data.
#' @param inptyp Character. Plot type.
#' @param cloneCall Character. Clonotype column.
#' @param chain Character. TCR/BCR chain.
#' @param group Character. Grouping column.
#' @param cols Character. Colors.
#' @param inppts,inpsiz,inpfsz,inplsz Numeric/Character. Plot parameters.
#' @return ggplot object.
#' @keywords internal
#' @noRd
scabundanceContig <- function(ggData, inptyp, cloneCall = "CTstrict", chain = "both", group = "X", cols, inppts, inpsiz, inpfsz = "Medium", inplsz = "Medium") {
  df <- split(ggData, f = ggData$X)
  Con.df <- NULL; xlab <- "Clone size"; names <- names(df)
  for (i in seq_along(df)) {
    if (chain != "both") df[[i]] <- off.the.chain(df[[i]], chain, cloneCall)
    data1 <- df[[i]] %>% dplyr::group_by(df[[i]][[cloneCall]]) %>% dplyr::summarise(Abundance = n())
    colnames(data1)[1] <- cloneCall; data1$values <- names[i]
    label <- df[[i]][1, ..group]; data1[, paste(group)] <- label
    Con.df <- rbind.data.frame(Con.df, data1)
  }
  Con.df <- data.frame(Con.df); fill <- group
  ylab <- switch(inptyp, "Clone sizes distribution" = "Clone sizes distribution", "Cumulative clone sizes distribution" = "Cumulative clone sizes distribution")
  Con.df_group = Con.df %>% dplyr::group_by(X, Abundance) %>% dplyr::summarise(Count = n())
  Con.df_group %<>% dplyr::group_by(X) %>% dplyr::mutate(Pk = Count/sum(Count))
  if(inptyp == "Cumulative clone sizes distribution") Con.df_group %<>% dplyr::arrange(dplyr::desc(Abundance)) %>% dplyr::group_by(X) %>% dplyr::mutate(cumsum_Pk = cumsum(Pk))
  plot = ggplot2::ggplot(data = Con.df_group, ggplot2::aes_string(x = "Abundance", y = switch(inptyp, "Clone sizes distribution" = "Pk", "Cumulative clone sizes distribution" = "cumsum_Pk"), color = "X")) +
    ggplot2::geom_point(shape = ifelse(inppts, 19, NA), size = inpsiz) + ggplot2::scale_color_manual(values = cols) + ggplot2::geom_smooth(method = "lm", se = FALSE, size = lList[inplsz]/5) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggpubr::yscale("log10", .format = TRUE) + ggpubr::xscale("log10", .format = TRUE) +
    ggpmisc::stat_poly_eq(ggplot2::aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = sList[inpfsz]/3)
  return(plot)
}

#' Clonotype Proportion by Size Bins
#' @param ggData data.frame. Input data.
#' @param Split Numeric. Size bin boundaries.
#' @param cloneCall,chain Character. Clonotype and chain.
#' @param inpcols Character. Color palette.
#' @return ggplot object.
#' @keywords internal
#' @noRd
scClonalProportion <- function(ggData, Split = c(10, 30, 100, 300, 1000, 3000, 10000, 3000, 100000), cloneCall = "CTstrict", chain = "both", inpcols) {
  df <- split(ggData, f = ggData$X)
  lvl = switch(class(ggData$X), "factor" = levels(ggData$X), "character" = unique(ggData$X))
  Con.df <- NULL
  mat <- matrix(0, length(df), length(Split), dimnames = list(names(df), paste0('[', c(1, Split[-length(Split)] + 1), ':', Split, ']')))
  if (chain != "both") { for (x in seq_along(df)) { df[[x]] <- off.the.chain(df[[x]], chain, cloneCall) } }
  df <- lapply(df, '[[', cloneCall)
  df <- lapply(df, na.omit)
  df <- lapply(df, function(x) as.data.frame(table(x)))
  for (i in seq_along(df)) { df[[i]] <- rev(sort(as.numeric(df[[i]][,2]))) }
  Cut <- c(1, Split[-length(Split)] + 1)
  for (i in seq_along(Split)) { mat[,i] <- vapply(df, function (x) sum(na.omit(x[Cut[i]:Split[i]])), FUN.VALUE = numeric(1)) }
  mat = mat[,colSums(mat) > 0]; gglvl = colnames(mat)
  mat %<>% as.data.frame(); mat %<>% tibble::rownames_to_column(var = "X")
  mat_melt <- tidyr::pivot_longer(as.data.frame(mat), cols = tidyselect::starts_with("["))
  mat_melt$X %<>% factor(levels = lvl); mat_melt$name %<>% factor(levels = gglvl)
  if(inpcols == "default") inpcols = "colorBlind"
  plot <- ggplot2::ggplot(mat_melt, ggplot2::aes(x = as.factor(X), y = value, fill = name)) +
    ggplot2::geom_bar(stat = "identity", position = "fill", color = "black", lwd = 0.25) +
    ggplot2::scale_fill_manual(name = "", values = color_generator(inpcols, length(gglvl))) +
    ggplot2::xlab("patient") + ggplot2::ylab("TCR Repertoire %") + ggplot2::theme_classic()
  return(plot)
}

#' Extract Specific TCR/BCR Chain
#' @param dat data.frame. Input data.
#' @param chain Character. Chain to extract.
#' @param cloneCall Character. Clonotype column.
#' @return data.frame with chain-filtered clonotype.
#' @keywords internal
#' @noRd
off.the.chain <- function(dat, chain, cloneCall) {
  chain1 <- toupper(chain)
  if (chain1 %in% c("TRA", "TRD", "IGH")) { x <- 1 } else if (chain1 %in% c("TRB", "TRG", "IGL")) { x <- 2 } else { warning("It looks like ", chain, " does not match the available options") }
  dat[[cloneCall]] <- stringr::str_split(dat[[cloneCall]], "_", simplify = TRUE)[,x]
  return(dat)
}

#' Calculate Clonal Diversity with Bootstrap
#' @param ggData data.frame. Input data.
#' @param inptyp,cloneCall,chain Character. Analysis parameters.
#' @param scale,inpindex Logical/Character. Scale and diversity index.
#' @param inp1,groupBy,x.axis Character. Variable names.
#' @param gglvl Character. Factor levels.
#' @param cols Character. Colors.
#' @param inppts,inpsiz,inpfsz,inplsz Numeric/Character. Plot parameters.
#' @param n.boots Integer. Bootstrap samples.
#' @return ggplot object.
#' @keywords internal
#' @noRd
scClonalDiversity <- function(ggData, inptyp, cloneCall = "CTstrict", chain = "both", scale = FALSE, inpindex = "Shannon", inp1 = inp1, groupBy = inp2, x.axis = "X", gglvl, cols, inppts, inpsiz, inpfsz, inplsz = "Medium", n.boots = 100) {
  set.seed(101)
  ggData %<>% as.data.frame()
  df <- split(ggData, f = ggData$X)
  Min <- c(); mat <- NULL; mat_a <- NULL; sample <- c()
  df <- dplyr::bind_rows(df, .id = "element.names")
  df$group.element <- paste0(df[,groupBy], ".", df[,x.axis], ".", df[,"paired"])
  group.element.uniq <- unique(df$group.element)
  df[,"group.element"] %<>% factor(levels = group.element.uniq)
  df <- split(df, f = df[,"group.element"])
  Min <- sapply(df, function(x) length(which(!is.na(unique(x[,cloneCall])))))
  keep = Min >= 10
  Min <- min(Min[keep], 100)
  scdiversityCall <- function(data, index) {
    output <- switch(index, "Shannon" = vegan::diversity(data[,"Freq"], index = "shannon"), "Simpson" = vegan::diversity(data[,"Freq"], index = "simpson"),
                     "Inv.Simpson" = vegan::diversity(data[,"Freq"], index = "invsimpson"), "Chao" = vegan::estimateR(data[,"Freq"])[2], "ACE" = vegan::estimateR(data[,"Freq"])[4])
    return(output)
  }
  df = df[keep]
  for (i in seq_along(df)) {
    data <- as.data.frame(table(df[[i]][,cloneCall]))
    mat_a <- NULL; sample <- c()
    for (j in seq(seq_len(n.boots))) {
      x <- dplyr::sample_n(data, Min)
      sample <- scdiversityCall(x, index = inpindex)
      mat_a <- rbind(mat_a, sample)
    }
    mat_a[is.na(mat_a)] <- 0
    mat_a <- colMeans(mat_a)
    mat_a <- as.data.frame(t(mat_a))
    mat <- rbind(mat, mat_a)
  }
  colnames(mat) <- inpindex
  mat[,"grpBy"] <- stringr::str_split(group.element.uniq[keep], "[.]", simplify = TRUE)[,1]
  mat[,x.axis] <- stringr::str_split(group.element.uniq[keep], "[.]", simplify = TRUE)[,2]
  mat[,"paired"] <- stringr::str_split(group.element.uniq[keep], "[.]", simplify = TRUE)[,3]
  inppair = ifelse(inptyp == "Paired Diversity", TRUE, FALSE)
  if (inppair) {
    Count = mat %>% dplyr::group_by(paired) %>% dplyr::summarise(n = n())
    if (any(Count$n %% length(gglvl) != 0)) {
      keep = Count[Count$n %% length(gglvl) == 0, "paired"]
      mat %<>% dplyr::filter(paired %in% keep$paired)
      mat %<>% split(f = mat$X) %>% lapply(function(obj) obj[order(obj$grpBy),])
      mat = mat[gglvl]; mat %<>% dplyr::bind_rows()
    }
  }
  mat_melt <- tidyr::pivot_longer(as.data.frame(mat), cols = inpindex, names_to = "variable") %>% as.data.frame()
  mat_melt[,"X"] %<>% factor(levels = gglvl)
  ggOut = switch(inptyp,
    "Paired Diversity" = ggpubr::ggpaired(data = mat_melt, x = "X", y = "value", color = "X", shape = ifelse(inppts,19,NA), point.size = inpsiz, size = inpsiz/5, palette = cols,
                                           ylab = paste(inpindex, "Index Score"), xlab = inp1, add = ifelse(inppts,"point","none"), line.color = "gray", line.size = ifelse(inppair, sList[inplsz]/50, 0)),
    "unpaired Diversity" = ggpubr::ggboxplot(data = mat_melt, x = "X", y = "value", fill = "X", shape = ifelse(inppts,19,NA), palette = cols,
                                             ylab = paste(inpindex, "Index Score"), xlab = inp1, outlier.shape = NA, line.color = "gray", line.size = ifelse(inppair, sList[inplsz]/50, 0)) +
      ggplot2::geom_jitter(size = ifelse(inppts, inpsiz, NA), shape = 19)
  )
  if (length(gglvl) == 3) {
    comparisons <- list(gglvl[1:2], gglvl[2:3], gglvl[c(1,3)])
    ggOut = ggOut + ggpubr::stat_compare_means(method = "wilcox.test", paired = inppair, comparisons = comparisons, size = sList[inpfsz]/4)
  }
  if (length(gglvl) == 2) {
    comparisons <- list(gglvl[1:2])
    ggOut = ggOut + ggpubr::stat_compare_means(method = "wilcox.test", paired = inppair, comparisons = comparisons, size = sList[inpfsz]/4)
  }
  return(ggOut)
}

#' Scatter Plot of Clonotype Changes
#' @param ggData data.frame. Input data.
#' @param inptyp,inpConf Character/data.table. Plot type and config.
#' @param cloneCall,chain,inp2 Character. Clonotype, chain, grouping.
#' @param scale Logical. Scale by size?
#' @param x.axis,y.axis Character. Axis categories.
#' @param inplvls,inpcols Numeric/Character. Thresholds and colors.
#' @param inpsiz,inplsz Numeric/Character. Sizes.
#' @return ggplot object.
#' @keywords internal
#' @noRd
ScatterClonotype <- function(ggData, inptyp, inpConf, cloneCall = "CTstrict", chain = "both", scale = inpscale, inp2 = inp2, x.axis = NULL, y.axis = NULL, inplvls, inpcols, inpsiz, inplsz) {
  set.seed(101)
  colnames(ggData) %<>% sub(inp2, "grpBy", .)
  ggData = ggData[, c(cloneCall, "X", "grpBy", "tcr.frequency"), with = FALSE]
  ggData = tidyr::pivot_wider(ggData, names_from = "X", values_from = "tcr.frequency", values_fn = sum)
  ggData[is.na(ggData)] = 0
  ggData %<>% dplyr::mutate(logFC = log1p(ggData[[y.axis]]) - log1p(ggData[[x.axis]]), class = cut(logFC, breaks = c(-Inf, -inplvls, inplvls, Inf), labels = c("Contracted", "Persistent", "Expanded")))
  ggData$class %<>% as.character()
  ggData[(ggData[[x.axis]] == 0 & ggData[["class"]] == "Expanded"), "class", with = FALSE] = "Novel"
  ggData$class %<>% factor(levels = c("Novel", "Persistent", "Expanded", "Contracted"))
  if (inptyp == "Paired scatter Clonotype") {
    gglvl = levels(ggData$class); ggCol = color_generator(inpcols, length(gglvl)); names(ggCol) = gglvl; ggCol["Persistent"] = "#CCC9C9"
  }
  if (inptyp == "unpaired scatter Clonotype") {
    gglvl = levels(ggData$grpBy); ggData$grpBy %<>% as.character()
    ggData[ggData[["class"]] %in% "Persistent", "grpBy"] = "Persistent"; ggData$grpBy %<>% factor(c("Persistent", gglvl))
    temp2 <- doFactoring(ggData, inpConf, col = "grpBy", inp2, inpcols); gglvl = temp2$gglvl; ggCol = c("#CCC9C9", temp2$ggCol); names(ggCol) = gglvl; ggData = temp2$ggData
  }
  plot <- ggpubr::ggscatter(ggData, x = x.axis, y = y.axis, color = switch(inptyp, "Paired scatter Clonotype" = "class", "unpaired scatter Clonotype" = "grpBy"),
                            palette = ggCol[gglvl], size = inpsiz, xlab = paste("\nClone percentage in", x.axis, "(%)"), ylab = paste("Clone percentage in", y.axis, "(%)\n")) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) + ggplot2::geom_abline(slope = 1, intercept = 0, alpha = 0.4, lty = 2, size = lList[inplsz]/10) +
    ggplot2::scale_x_continuous(trans = 'log10', breaks = 10^c(-4:1), labels = c(0.0001, 0.001, 0.01, 0.1, 1, 10), oob = scales::squish_infinite) +
    ggplot2::scale_y_continuous(trans = 'log10', breaks = 10^c(-4:1), labels = c(0.0001, 0.001, 0.01, 0.1, 1, 10), oob = scales::squish_infinite)
  return(plot)
}

#' Bar Plot of Clonotype Classifications
#' @param ggData data.frame. Input data.
#' @param inptyp,inpConf Character/data.table. Plot type and config.
#' @param cloneCall,chain,x.axis,y.axis,inp2 Character. Parameters.
#' @param gglvl2,ggCol2 Character. Factor levels and colors.
#' @param inpscale,inpcols Logical/Character. Scale and colors.
#' @param inplvls,inppts,inpsiz,inplsz Numeric/Character. Parameters.
#' @return ggplot object.
#' @keywords internal
#' @noRd
ScClonotypeBar <- function(ggData, inptyp, inpConf, cloneCall = cloneCall, chain = "both", x.axis, y.axis, inp2, gglvl2, ggCol2, inpscale, inpcols, inplvls, inppts, inpsiz, inplsz) {
  set.seed(101)
  colnames(ggData) %<>% sub(inp2, "grpBy", .)
  ggData = ggData[, c(cloneCall, "X", "grpBy", "tcr.frequency"), with = FALSE]
  ggData = tidyr::pivot_wider(ggData, names_from = "X", values_from = "tcr.frequency", values_fn = sum)
  ggData[is.na(ggData)] = 0
  ggData %<>% dplyr::mutate(logFC = log1p(ggData[[y.axis]]) - log1p(ggData[[x.axis]]), class = cut(logFC, breaks = c(-Inf, -inplvls, inplvls, Inf), labels = c("Contracted", "Persistent", "Expanded")))
  ggData$class %<>% as.character()
  ggData[(ggData[[x.axis]] == 0 & ggData[["class"]] == "Expanded"), "class", with = FALSE] = "Novel"
  ggData$class %<>% factor(levels = c("Novel", "Persistent", "Expanded", "Contracted"))
  ggCol1 = color_generator(inpcols, length(levels(ggData$class))); names(ggCol1) = levels(ggData$class); ggCol1 = ggCol1[names(ggCol1) != "Persistent"]
  ggData %<>% data.table::as.data.table()
  ggData = ggData[, .(nCells = .N), by = c("class", "grpBy")]
  ggData %<>% dplyr::filter(class != "Persistent")
  ggData = switch(inptyp,
    "Novel & Expand & Contract Barplot_1" = ggData[, {tot = sum(nCells); .SD[, .(tot = tot, nCells = nCells, pctCells = 100 * sum(nCells) / tot), by = "grpBy"]}, by = "class"],
    "Novel & Expand & Contract Barplot_2" = ggData[, {tot = sum(nCells); .SD[, .(tot = tot, nCells = nCells, pctCells = 100 * sum(nCells) / tot), by = "class"]}, by = "grpBy"])
  ggData$pctCells %<>% round(digits = 0)
  plot <- switch(inptyp,
    "Novel & Expand & Contract Barplot_1" = ggpubr::ggbarplot(data = ggData, x = "class", y = ifelse(inpscale, "pctCells", "nCells"), fill = "grpBy", lab.size = inpsiz*2, lab.vjust = inpsiz/4,
                                                               label = inppts, lab.pos = "in", lab.col = "white", width = 0.95, position = ggplot2::position_stack(), palette = ggCol2[gglvl2],
                                                               xlab = "", ylab = ifelse(inpscale, "Clone type percentage (%)\n", "Number of Clone type\n")),
    "Novel & Expand & Contract Barplot_2" = ggpubr::ggbarplot(data = ggData, x = "grpBy", y = ifelse(inpscale, "pctCells", "nCells"), fill = "class", lab.size = inpsiz*2, lab.vjust = inpsiz/4,
                                                               label = inppts, lab.pos = "in", lab.col = "white", width = 0.95, position = ggplot2::position_stack(), palette = ggCol1,
                                                               xlab = paste0("\n", inp2), ylab = ifelse(inpscale, "Clone type percentage (%)\n", "Number of Clone type\n")))
  plot <- plot + ggplot2::theme(legend.title = ggplot2::element_blank())
  return(plot)
}

#' Clean String Input
#' @param text Character. Input string.
#' @return Character vector.
#' @keywords internal
#' @noRd
strSplitClean <- function(text) gsub(" ", "", unlist(stringr::str_split(text, pattern = ",")))
