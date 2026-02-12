#' Modify the legend labels for categorical metadata
#'
#' Modify the legend labels for categorical metadata.
#'
#' @param scConf shinycell config data.table
#' @param meta.to.mod metadata for which to modify the legend labels. Users 
#'   can either use the actual metadata column names or display names. Please 
#'   specify only one metadata
#' @param new.labels character vector of new legend labels
#' 
#' @return updated shinycell config data.table
#'
#' @author John F. Ouyang
#'
#' @import data.table
#'
#' @examples
#' # Create minimal example config with categorical metadata
#' scConf <- data.table::data.table(
#'   ID = c("library"),
#'   UI = c("Library"),
#'   fID = c("S1|S2|S3|S4"),
#'   fUI = c("Sample1|Sample2|Sample3|Sample4"),
#'   default = c(0)
#' )
#' # Modify legend labels
#' scConf <- modLabels(scConf, meta.to.mod = "library",
#'                     new.labels = c("Fib", "Primed", "Naive", "RSeT"))
#'
#' @export
modLabels <- function(scConf, meta.to.mod, new.labels){
  # Check that only one metadata is provided
  if(length(meta.to.mod) != 1){
    stop("Please specify only one metadata to modify legend labels!")
  }
  
  # Check if meta.to.mod exist
  if(meta.to.mod %in% scConf$ID){
    useID = TRUE   # Use IDs
  } else if(meta.to.mod %in% scConf$UI){
    useID = FALSE  # Use UIs
  } else {
    stop("meta.to.mod not found in shinycell config!")
  }
  
  # Check if meta.to.mod is categorical and if length(new.labels) matches 
  if(useID){
    res = strsplit(scConf[ID == meta.to.mod]$fUI, "\\|")[[1]]
  } else {
    res = strsplit(scConf[UI == meta.to.mod]$fUI, "\\|")[[1]]
  }
  if(is.na(res[1])){
    stop("meta.to.mod is not a categorical metadata!")
  }
  if(length(res) != length(new.labels)){
    stop("Length of new.labels does not match!")
  }
  
  # Start changing the colours
  if(useID){
    scConf[ID == meta.to.mod]$fUI = paste0(new.labels, collapse = "|")
  } else {
    scConf[UI == meta.to.mod]$fUI = paste0(new.labels, collapse = "|")
  }
  return(scConf)
}


