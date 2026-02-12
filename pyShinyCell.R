
object <- readRDS("data/readySeu_rset.rds")
meta.data <- object@meta.data



scConf = createConfig(object, meta.to.include =meta.data,maxLevels = 300)
meta.data = object@meta.data
meta.data = meta.data[!duplicated(meta.data$cell.types),]
meta.data = meta.data[order(meta.data$cell.types),]
scConf$fCL[which(scConf$ID == "cell.types")] = paste(meta.data$cell.types.colors,collapse = "|")

GeneSets = read.delim("data/seurat_resources/gene_set_Bladder_Cancer.txt",row.names =1,header = F,
                      stringsAsFactors = F)
#GeneSets %>% kable() %>% kable_styling()
GeneSets.df <- as.data.frame(t(GeneSets))
(markers <- sapply(GeneSets.df,function(x) head(x,2)) %>% as.vector())


markers <- Hmisc::capitalize(tolower(markers))
(markers <- markers[markers %in% rownames(object)])
makeShinyApp(object, scConf, gex.assay = "RNA",gene.mapping = TRUE,
             gex.slot = "data",default.gene1 = "Krt14",default.gene2 = "Krt8",
             default.multigene = markers,
             default.dimred = c("tSNE_1","tSNE_2"),shiny.dir = "shinyApp/shinyApp_stable",
             shiny.title = "Mouse BladderCancer scRNA-seq")

sc1def  = readRDS("shinyApp/shinyApp_stable/sc1def.rds")
sc1def$grp1 = "cell.types"
sc1def$grp2 = "orig.ident"
sc1def$dimred = paste0("CCAUMAP",1:2)
saveRDS(sc1def,"shinyApp/shinyApp_stable/sc1def.rds")


format(object.size(object@assays),unit = "GB")
#object@assays$RNA <- NULL
object@assays$integrated <- NULL

#object[["SCT"]]@counts = matrix(0,0,0)
#object[["SCT"]]@scale.data = matrix(0,0,0)

Class = sapply(object@meta.data,class)
for(col in  names(Class)[Class %in% "factor"]){
        object@meta.data[,col] %<>% as.character()
}
for(col in grep("RNA_snn_res",names(Class),value =T)){
        object@meta.data[,col] %<>% as.integer()
}

format(object.size(object),unit = "GB")

file.remove("shinyApp/shinyApp_stable/sc1csr_gexpr.h5ad")
SaveH5Seurat(object, filename = "shinyApp/shinyApp_stable/sc1csr_gexpr.h5Seurat")
Convert("shinyApp/shinyApp_stable/sc1csr_gexpr.h5Seurat", dest = "h5ad")
file.remove("shinyApp/shinyApp_stable/sc1csr_gexpr.h5Seurat")

library(reticulate)
reticulate::use_virtualenv('pyShinyCell', required = T)
sc <- reticulate::import("scanpy")
scipy <- reticulate::import("scipy")
adata = sc$read_h5ad("shinyApp/shinyApp_stable/sc1csr_gexpr.h5ad")
adata$write("shinyApp/shinyApp_stable/sc1csr_gexpr_gzip9.h5ad",compression='gzip',
            compression_opts=9)
max_exp <- scipy$sparse$csr_matrix$max(adata$X,axis=0L)
max_exp_df = data.frame("val"= as.vector(max_exp),row.names = rownames(object))
saveRDS(max_exp_df,"shinyApp/shinyApp_stable/sc1maxlvl.rds")
