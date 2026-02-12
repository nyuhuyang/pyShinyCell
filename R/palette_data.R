#' Initialize and Manage Color Palettes for pyShinyCell
#'
#' Internal functions to manage color palettes used in pyShinyCell Shiny apps.
#' These functions integrate custom, ggsci, and RColorBrewer palettes into a
#' unified palette information system.
#'
#' @keywords internal
#' @name palette_data
NULL

#' Palette Information Data Frame
#'
#' A data frame containing metadata about available color palettes for use in
#' pyShinyCell applications. Combines custom palettes, ggsci (scientific journal)
#' palettes, and RColorBrewer palettes.
#'
#' Columns:
#' - maxcolors: Integer. Maximum number of colors in the palette.
#' - category: Character. Type of palette ("qual", "seq", "div" for qualitative,
#'   sequential, or diverging).
#' - palette: Character. Palette name or NA for internal references.
#' - package: Character. Package providing the palette ("custom", "ggsci", or
#'   "RColorBrewer").
#' - pal.setup: List. Color vector for each palette.
#'
#' @format A data frame with palette metadata.
#'
#' @details
#' This data frame is used internally by generated Shiny apps to provide
#' users with palette selection options. It includes:
#' - 6 custom palettes (sequential and diverging)
#' - 40 ggsci palettes (scientific journal color schemes)
#' - 35 RColorBrewer palettes (sequential, diverging, qualitative)
#'
#' @examples
#' # View available palettes
#' head(pal.info)
#' unique(pal.info$package)
#'
#' @docType data
#' @name pal.info
#' @keywords data
NULL

#' Custom Color Palette List
#'
#' A list of manually curated color palettes used as defaults in pyShinyCell.
#'
#' @keywords internal
.createCustomPalettes <- function() {
  list(
    default = c("#FFF7EC","#FEE8C8","#FDD49E","#FDBB84","#FC8D59",
                "#EF6548","#D7301F","#B30000","#7F0000"),
    "Blue-White-Red" = c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF",
                         "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)],
    "Blue-Yellow-Red" = c("blue","cyan3","green","greenyellow","yellow","orange","chocolate1","red","darkred")[c(1,1:9,9)],
    "Yellow-Green-Purple" = c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C",
                              "#2C728E","#3B528B","#472D7B","#440154"),
    "colorBlind" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
    "Singler" = c("#7FC97F","#BEAED4","#FDC086","#386CB0","#F0027F",
                  "#BF5B17","#666666","#1B9E77","#7570B3","#66A61E",
                  "#E6AB02","#A6761D","#A6CEE3","#B2DF8A","#FB9A99",
                  "#E31A1C","#FF7F00","#6A3D9A","#8DA0CB",
                  "#4DAF4A","#984EA3","#c6c386","#999999","#66C2A5",
                  "#FC8D62","#A6D854","#FFD92F","#BEBADA",
                  "#FB8072","#80B1D3","#FDB462","#BC80BD","#B3B3B3",
                  "#33A02C","#B3DE69","#4038b0","#ee7576","#e94749","#E78AC3","#ff0000",
                  "#A65628","#d80172","#F781BF","#D95F02","#E7298A",
                  "#1F78B4","#FDBF6F","#CAB2D6","#B15928","#FBB4AE",
                  "#B3CDE3",
                  '#0173b2','#de8f05','#029e73','#d55e00','#cc78bc','#ca9161','#fbafe4','#949494','#ece133','#56b4e9',
                  "#00AFBB", "#E7B800", "#FC4E07",
                  "#FFDB6D", "#C4961A", "#F4EDCA", "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")
  )
}

#' Create Palette Information Data Frame
#'
#' Internal function to create the combined palette information data frame
#' from custom, ggsci, and RColorBrewer sources.
#'
#' @keywords internal
.createPaletteInfo <- function() {
  # Custom palettes
  custom_list <- .createCustomPalettes()
  custom_info <- data.frame(
    maxcolors = sapply(custom_list, length),
    category = c("seq","div","div","div","qual","qual"),
    palette = NA_character_,
    package = "custom",
    row.names = names(custom_list),
    stringsAsFactors = FALSE
  )

  # ggsci palettes
  ggsci_data <- list(
    c("NPG", 10, "qual", "nrc"),
    c("AAAS", 10, "qual", "default"),
    c("NEJM", 8, "qual", "default"),
    c("Lancet", 9, "qual", "lanonc"),
    c("JAMA", 7, "qual", "default"),
    c("JCO", 10, "qual", "default"),
    c("UCSCGB", 26, "qual", "default"),
    c("D3.10", 10, "qual", "category10"),
    c("D3.20", 20, "qual", "category20"),
    c("D3.20b", 20, "qual", "category20b"),
    c("D3.20c", 20, "qual", "category20c"),
    c("LocusZoom", 7, "qual", "default"),
    c("IGV", 51, "qual", "default"),
    c("IGV.alternating", 2, "div", "alternating"),
    c("UChicago", 9, "qual", "default"),
    c("Uchicago.light", 9, "qual", "light"),
    c("Uchicago.dark", 9, "qual", "dark"),
    c("StarTrek", 7, "qual", "uniform"),
    c("Tron", 7, "qual", "legacy"),
    c("Futurama", 12, "qual", "planetexpress"),
    c("RickandMorty", 12, "qual", "schwifty"),
    c("Simpsons", 16, "qual", "springfield"),
    c("GSEA", 12, "div", "default"),
    c("Material.red", 10, "seq", "red"),
    c("Material.pink", 10, "seq", "pink"),
    c("Material.purple", 10, "seq", "purple"),
    c("Material.indigo", 10, "seq", "indigo"),
    c("Material.blue", 10, "seq", "blue"),
    c("Material.light-blue", 10, "seq", "light-blue"),
    c("Material.cyan", 10, "seq", "cyan"),
    c("Material.teal", 10, "seq", "teal"),
    c("Material.green", 10, "seq", "green"),
    c("Material.light-green", 10, "seq", "light-green"),
    c("Material.lime", 10, "seq", "lime"),
    c("Material.yellow", 10, "seq", "yellow"),
    c("Material.amber", 10, "seq", "amber"),
    c("Material.orange", 10, "seq", "orange"),
    c("Material.deep-orange", 10, "seq", "deep-orange"),
    c("Material.brown", 10, "seq", "brown"),
    c("Material.grey", 10, "seq", "grey"),
    c("Material.blue-grey", 10, "seq", "blue-grey")
  )

  ggsci_info <- do.call(rbind, lapply(ggsci_data, function(x) {
    data.frame(Name = x[1], maxcolors = as.integer(x[2]),
               category = x[3], palette = x[4],
               stringsAsFactors = FALSE)
  }))
  rownames(ggsci_info) <- ggsci_info$Name
  ggsci_info <- ggsci_info[, -1]
  ggsci_info$package <- "ggsci"

  # RColorBrewer palettes
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    brewer_info <- RColorBrewer::brewer.pal.info
    brewer_info$package <- "RColorBrewer"
    brewer_info$palette <- NA
    brewer_info$colorblind <- NULL
    # Keep only columns matching ggsci_info
    brewer_info <- brewer_info[, intersect(colnames(brewer_info), colnames(ggsci_info))]
  } else {
    brewer_info <- data.frame()
  }

  # Combine all
  if (nrow(brewer_info) > 0) {
    pal_info <- rbind(custom_info, ggsci_info, brewer_info)
  } else {
    pal_info <- rbind(custom_info, ggsci_info)
  }

  pal_info
}

#' Create Palette Data File
#'
#' Internal function to create and save the pal.info data object to
#' data/pal.info.rda. This is called during package build.
#'
#' @keywords internal
.createPaletteDataFile <- function() {
  pal.info <- .createPaletteInfo()
  usethis::use_data(pal.info, overwrite = TRUE)
}
