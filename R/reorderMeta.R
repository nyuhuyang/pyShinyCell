#' Reorder the order in which metadata appear in the shiny app
#'
#' Reorder the order in which metadata appear in the dropdown menu in the 
#' shiny app.
#'
#' @param scConf shinycell config data.table
#' @param new.meta.order character vector containing new order. All metadata 
#'   names must be included, which can be found at \code{scConf$ID}
#' 
#' @return updated shinycell config data.table
#'
#' @author John F. Ouyang
#'
#' @import data.table
#'
#' @examples
#' # Create minimal example config
#' scConf <- data.table::data.table(
#'   ID = c("cell_type", "batch", "condition"),
#'   UI = c("Cell Type", "Batch", "Condition"),
#'   fID = c("TypeA|TypeB", "B1|B2", NA),
#'   default = c(1, 0, 0)
#' )
#' # Reorder: batch first, then cell_type, then condition
#' scConf <- reorderMeta(scConf, c("batch", "cell_type", "condition"))
#'
#' @export
reorderMeta <- function(scConf, new.meta.order){
  # Check if new.meta.order matches scConf$ID
  if(!all.equal(sort(new.meta.order), sort(as.character(scConf$ID)))){
    stop("new.meta.order does not match scConf$ID!")
  }
  
  # Start reordering
  scConf$ID = factor(scConf$ID, levels = new.meta.order)
  scConf = scConf[order(ID)]

  return(scConf)
}


