test_that("palette data loads correctly", {
  # Check that palette data is properly loaded
  data("pal.info", package = "pyShinyCell")
  expect_s3_class(pal.info, "data.frame")
  expect_true(nrow(pal.info) > 0)
})

test_that("package attributes set correctly in createPyConfig", {
  skip_if_not_installed("Seurat")

  # Create test data with UMAP and metadata
  set.seed(42)
  counts <- matrix(rnbinom(n = 100, size = 1, prob = 0.1), ncol = 20)
  rownames(counts) <- paste0("Gene", seq_len(nrow(counts)))
  colnames(counts) <- paste0("Cell", seq_len(ncol(counts)))

  seu <- Seurat::CreateSeuratObject(counts = counts)
  seu$cluster <- factor(rep(c("C1", "C2"), 10))  # Add metadata for ShinyCell

  # Add UMAP
  set.seed(42)
  umap_embeddings <- matrix(rnorm(40), ncol = 2)
  colnames(umap_embeddings) <- c("UMAP_1", "UMAP_2")
  rownames(umap_embeddings) <- colnames(seu)

  seu[["umap"]] <- Seurat::CreateDimReducObject(
    embeddings = umap_embeddings,
    key = "UMAP_"
  )

  # Create config
  scConf <- createPyConfig(seu, enable_py_analysis = TRUE)

  # Check that attributes are set
  expect_equal(attr(scConf, "package"), "pyShinyCell")
  expect_true(!is.null(attr(scConf, "version")))
  expect_true(!is.null(attr(scConf, "py_analysis")))
})

test_that("py_analysis attribute correctly identifies suitable variables", {
  skip_if_not_installed("Seurat")

  # Create test data with categorical variables
  set.seed(42)
  counts <- matrix(rnbinom(n = 100, size = 1, prob = 0.1), ncol = 60)
  rownames(counts) <- paste0("Gene", seq_len(nrow(counts)))
  colnames(counts) <- paste0("Cell", seq_len(ncol(counts)))

  seu <- Seurat::CreateSeuratObject(counts = counts)
  seu$celltype <- factor(rep(c("TypeA", "TypeB", "TypeC"), 20))  # 3 levels
  seu$batch <- factor(rep(c("B1", "B2"), 30))                    # 2 levels
  seu$too_many_levels <- factor(paste0("L", 1:60))               # 60 levels (exceeds maxLevels=50)

  # Add UMAP
  set.seed(42)
  umap_embeddings <- matrix(rnorm(120), ncol = 2)
  colnames(umap_embeddings) <- c("UMAP_1", "UMAP_2")
  rownames(umap_embeddings) <- colnames(seu)

  seu[["umap"]] <- Seurat::CreateDimReducObject(
    embeddings = umap_embeddings,
    key = "UMAP_"
  )

  # Create config
  scConf <- createPyConfig(seu, maxLevels = 50)

  # Get py_analysis mapping
  py_analysis <- attr(scConf, "py_analysis")

  # Check expectations
  expect_true(py_analysis["celltype"])  # Should be marked (3 levels)
  expect_true(py_analysis["batch"])     # Should be marked (2 levels)
  # too_many_levels would be excluded from config due to maxLevels, so check if it exists
  if ("too_many_levels" %in% names(py_analysis)) {
    expect_false(py_analysis["too_many_levels"])  # Should NOT be marked (too many levels)
  }
})
