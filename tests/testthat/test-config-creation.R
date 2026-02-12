test_that("createPyConfig creates valid config object with UMAP", {
  skip_if_not_installed("Seurat")

  # Create test data with UMAP dimension reduction
  set.seed(42)
  counts <- matrix(rnbinom(n = 200, size = 1, prob = 0.1), ncol = 20)
  rownames(counts) <- paste0("Gene", seq_len(nrow(counts)))
  colnames(counts) <- paste0("Cell", seq_len(ncol(counts)))

  seu <- Seurat::CreateSeuratObject(counts = counts)
  seu$group <- rep(c("A", "B"), each = 10)

  # Add UMAP reduction (required by ShinyCell)
  set.seed(42)
  umap_embeddings <- matrix(rnorm(40), ncol = 2)
  colnames(umap_embeddings) <- c("UMAP_1", "UMAP_2")
  rownames(umap_embeddings) <- colnames(seu)

  seu[["umap"]] <- Seurat::CreateDimReducObject(
    embeddings = umap_embeddings,
    key = "UMAP_"
  )

  # Create config
  scConf <- createPyConfig(seu, maxLevels = 50)

  # Check structure
  expect_s3_class(scConf, "data.table")
  expect_true(nrow(scConf) > 0)
  expect_true(all(c("UI", "ID", "default") %in% colnames(scConf)))

  # Check attributes
  expect_true("py_analysis" %in% names(attributes(scConf)))
  expect_true("package" %in% names(attributes(scConf)))

  # Check py_analysis attribute structure
  py_analysis_attr <- attr(scConf, "py_analysis")
  expect_type(py_analysis_attr, "logical")
  expect_equal(length(py_analysis_attr), nrow(scConf))
})

test_that("createPyConfig marks suitable variables for analysis", {
  skip_if_not_installed("Seurat")

  # Create test data with UMAP and known grouping variables
  set.seed(42)
  counts <- matrix(rnbinom(n = 200, size = 1, prob = 0.1), ncol = 30)
  rownames(counts) <- paste0("Gene", seq_len(nrow(counts)))
  colnames(counts) <- paste0("Cell", seq_len(ncol(counts)))

  seu <- Seurat::CreateSeuratObject(counts = counts)
  seu$celltype <- factor(rep(c("A", "B", "C"), 10))  # 3 levels - should be marked
  seu$batch <- factor(rep(c("X", "Y"), 15))           # 2 levels - should be marked

  # Add UMAP
  set.seed(42)
  umap_embeddings <- matrix(rnorm(60), ncol = 2)
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

  # Check that appropriate variables are marked
  expect_true(py_analysis["celltype"])
  expect_true(py_analysis["batch"])
})

test_that("createPyConfig requires dimension reduction", {
  skip_if_not_installed("Seurat")

  # Create test data WITHOUT reduction
  set.seed(42)
  counts <- matrix(rnbinom(n = 100, size = 1, prob = 0.1), ncol = 20)
  rownames(counts) <- paste0("Gene", seq_len(nrow(counts)))
  colnames(counts) <- paste0("Cell", seq_len(ncol(counts)))

  seu <- Seurat::CreateSeuratObject(counts = counts)

  # Should error without UMAP/TSNE
  expect_error({
    scConf <- createPyConfig(seu)
  }, "dimension reduction")
})

test_that("createPyConfig with enable_py_analysis=FALSE works", {
  skip_if_not_installed("Seurat")

  # Create test data with UMAP
  set.seed(42)
  counts <- matrix(rnbinom(n = 200, size = 1, prob = 0.1), ncol = 20)
  rownames(counts) <- paste0("Gene", seq_len(nrow(counts)))
  colnames(counts) <- paste0("Cell", seq_len(ncol(counts)))

  seu <- Seurat::CreateSeuratObject(counts = counts)
  seu$group <- rep(c("A", "B"), each = 10)

  # Add UMAP
  set.seed(42)
  umap_embeddings <- matrix(rnorm(40), ncol = 2)
  colnames(umap_embeddings) <- c("UMAP_1", "UMAP_2")
  rownames(umap_embeddings) <- colnames(seu)

  seu[["umap"]] <- Seurat::CreateDimReducObject(
    embeddings = umap_embeddings,
    key = "UMAP_"
  )

  # Create config without py_analysis flag
  scConf <- createPyConfig(seu, enable_py_analysis = FALSE)

  # Should still create valid config
  expect_s3_class(scConf, "data.table")
  expect_true(nrow(scConf) > 0)
})
