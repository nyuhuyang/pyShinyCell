test_that("prepare files from readySeu_rset.rds using pyShinyCell workflow", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("ShinyCell")

  data_candidates <- c(
    file.path("data", "readySeu_rset.rds"),
    file.path("..", "..", "data", "readySeu_rset.rds")
  )
  data_path <- data_candidates[file.exists(data_candidates)][1]
  if (is.na(data_path) || !nzchar(data_path)) {
    skip("data/readySeu_rset.rds not found")
  }

  seu <- readRDS(data_path)
  meta_data <- seu@meta.data

  sc_conf <- createPyConfig(
    seu,
    meta.to.include = colnames(meta_data),
    maxLevels = 300
  )

  if ("cell.types" %in% colnames(meta_data) &&
      "cell.types.colors" %in% colnames(meta_data) &&
      "cell.types" %in% sc_conf$ID) {
    colors_df <- meta_data[!duplicated(meta_data$cell.types),
                           c("cell.types", "cell.types.colors"),
                           drop = FALSE]
    colors_df <- colors_df[order(colors_df$cell.types), , drop = FALSE]
    sc_conf$fCL[sc_conf$ID == "cell.types"] <- paste(
      colors_df$cell.types.colors,
      collapse = "|"
    )
  }

  out_dir <- file.path(tempdir(), "pyshinycell-test-files")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  gene_ids <- rownames(seu)
  if (is.null(gene_ids) || length(gene_ids) < 2) {
    skip("Need at least two genes in readySeu_rset.rds")
  }
  default_gene1 <- gene_ids[[1]]
  default_gene2 <- gene_ids[[2]]

  default_dimred <- NA
  if (methods::is(seu, "Seurat") && length(names(seu@reductions)) > 0) {
    red_name <- names(seu@reductions)[[1]]
    red_emb <- Seurat::Embeddings(seu, reduction = red_name)
    if (!is.null(red_emb) && ncol(red_emb) >= 2) {
      default_dimred <- colnames(red_emb)[1:2]
    }
  }

  can_make_h5ad <- FALSE
  if (methods::is(seu, "Seurat")) {
    seurat_ns <- asNamespace("Seurat")
    can_make_h5ad <- exists("SaveH5Seurat", where = seurat_ns, mode = "function") &&
      exists("Convert", where = seurat_ns, mode = "function")
  }

  makePyShinyFiles(
    obj = seu,
    scConf = sc_conf,
    gex.assay = "RNA",
    gex.slot = "data",
    gene.mapping = TRUE,
    shiny.prefix = "sc1",
    shiny.dir = out_dir,
    default.gene1 = default_gene1,
    default.gene2 = default_gene2,
    default.dimred = default_dimred,
    generate_h5ad = can_make_h5ad,
    compress_h5ad = FALSE,
    calculate_maxlvl = FALSE,
    verbose = FALSE
  )

  expected_base <- c(
    "sc1conf.rds",
    "sc1meta.rds",
    "sc1gene.rds",
    "sc1gexpr.h5",
    "sc1def.rds"
  )
  expected_paths <- file.path(out_dir, expected_base)
  expect_true(all(file.exists(expected_paths)))
  expect_true(all(file.info(expected_paths)$size > 0))

  sc_conf_out <- readRDS(file.path(out_dir, "sc1conf.rds"))
  sc_meta_out <- readRDS(file.path(out_dir, "sc1meta.rds"))
  sc_gene_out <- readRDS(file.path(out_dir, "sc1gene.rds"))
  sc_def_out <- readRDS(file.path(out_dir, "sc1def.rds"))

  expect_true(nrow(sc_conf_out) > 0)
  expect_true(nrow(sc_meta_out) > 0)
  expect_true(length(sc_gene_out) > 0)
  expect_true(length(sc_def_out) > 0)

  if (requireNamespace("hdf5r", quietly = TRUE)) {
    h5 <- hdf5r::H5File$new(file.path(out_dir, "sc1gexpr.h5"), mode = "r")
    on.exit(h5$close_all(), add = TRUE)
    expect_true(length(h5$ls()$name) > 0)
  }

  if (can_make_h5ad) {
    h5ad_path <- file.path(out_dir, "sc1gexpr.h5ad")
    expect_true(file.exists(h5ad_path))
    expect_true(file.info(h5ad_path)$size > 0)
  }
})
