#' Make a shiny app
#'
#' Make a shiny app based on the shinycell config data.table and single-cell 
#' data object.
#'
#' @param obj input single-cell object for Seurat (v3+) / SingleCellExperiment 
#'   data or input file path for h5ad / loom files
#' @param scConf shinycell config data.table
#' @param gex.assay assay in single-cell data object to use for plotting 
#'   gene expression, which must match one of the following:
#'   \itemize{
#'     \item Seurat objects: "RNA" or "integrated" assay,
#'       default is "RNA"
#'     \item SCE objects: "logcounts" or "normcounts" or "counts",
#'       default is "logcounts"
#'     \item h5ad files: "X" or any assay in "layers",
#'       default is "X"
#'     \item loom files: "matrix" or any assay in "layers",
#'       default is "matrix"
#'   }
#' @param gex.slot slot in single-cell assay to plot. This is only used 
#'   for Seurat objects (v3+). Default is to use the "data" slot
#' @param gene.mapping specifies whether to convert human / mouse Ensembl gene 
#'   IDs (e.g. ENSG000xxx / ENSMUSG000xxx) into "user-friendly" gene symbols. 
#'   Set this to \code{TRUE} if you are using Ensembl gene IDs. Default is 
#'   \code{FALSE} which is not to perform any conversion. Alternatively, users 
#'   can supply a named vector where \code{names(gene.mapping)} correspond 
#'   to the actual gene identifiers in the gene expression matrix and 
#'   \code{gene.mapping} correspond to new identifiers to map to
#' @param shiny.title title for shiny app
#' @param shiny.footnotes text for shiny app footnote. When given as a list, 
#'   citation can be inserted by specifying author, title, journal, volume, 
#'   page, year, doi, link. See example below. 
#' @param shiny.dir specify directory to create the shiny app in. Default is 
#'   to create a new directory named "shinyApp"
#' @param ganalytics Google analytics tracking ID (e.g. "UA-123456789-0")
#' @param default.gene1 specify primary default gene to show
#' @param default.gene2 specify secondary default gene to show
#' @param default.multigene character vector specifying the default genes to 
#'   show in bubbleplot / heatmap
#' @param default.dimred character vector specifying the two default dimension 
#'   reductions. Default is to use UMAP if not TSNE embeddings
#'
#' @return directory containing shiny app
#'
#' @author John F. Ouyang
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' # Create example Seurat object
#' if (requireNamespace("Seurat", quietly = TRUE)) {
#'   counts <- matrix(rnbinom(1000, size = 1, prob = 0.1), ncol = 50)
#'   rownames(counts) <- paste0("Gene", 1:nrow(counts))
#'   colnames(counts) <- paste0("Cell", 1:ncol(counts))
#'   seu <- Seurat::CreateSeuratObject(counts = counts)
#'   seu$orig.ident <- factor(rep(c("sample1", "sample2"), each = 25))
#'
#'   # Create configuration from Seurat object
#'   scConf <- createConfig(seu, meta.to.include = c("orig.ident"))
#'
#'   # Generate Shiny app (requires specific directory setup)
#'   makeShinyApp(seu, scConf, shiny.dir = "myapp/",
#'                shiny.title = "My Single Cell Analysis")
#' }
#' }
#'
#' @export
makeShinyApp <- function(
  obj, scConf, gex.assay = NA, gex.slot = c("data", "scale.data", "counts"), 
  gene.mapping = FALSE, 
  shiny.title = "scRNA-seq shiny app", shiny.footnotes = "",
  shiny.dir = "shinyApp/", ganalytics = NA,
  default.gene1 = NA, default.gene2 = NA, default.multigene = NA, 
  default.dimred = NA){
  
  # Checks are performed in respective functions
  # Wrapper for two main functions
  makeShinyFiles(obj = obj, scConf = scConf, 
                 gex.assay = gex.assay[1], gex.slot = gex.slot[1], 
                 gene.mapping = gene.mapping, 
                 shiny.prefix = "sc1", shiny.dir = shiny.dir, 
                 default.gene1, default.gene2, default.multigene, default.dimred)
  makeShinyCodes(shiny.title = shiny.title, shiny.footnotes = shiny.footnotes,
                 shiny.prefix = "sc1", shiny.dir = shiny.dir, 
                 ganalytics = ganalytics)

}
