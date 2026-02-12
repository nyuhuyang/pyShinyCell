#import numpy as np
#from scipy import stats
#from scipy import sparse
#from statsmodels.stats.multitest import multipletests
import pandas as pd
import anndata as ad

def LoadAllDGEs():
    adata_degs = ad.read_h5ad('tempData/rank_genes_groups.h5ad')
    p_val =  pd.melt(pd.DataFrame(adata_degs.uns['rank_genes_groups']['pvals']),
                    var_name='cluster',value_name='p_val')
    avg_log2FC =  pd.melt(pd.DataFrame(adata_degs.uns['rank_genes_groups']['logfoldchanges']),
                    var_name='cluster',value_name='avg_log2FC')
    p_val_adj =  pd.melt(pd.DataFrame(adata_degs.uns['rank_genes_groups']['pvals_adj']),
                    var_name='cluster',value_name='p_val_adj')
    genes  =  pd.melt(pd.DataFrame(adata_degs.uns['rank_genes_groups']['names']),
                    var_name='cluster',value_name='genes')
    scores  =  pd.melt(pd.DataFrame(adata_degs.uns['rank_genes_groups']['scores']),
                    var_name='cluster',value_name='scores')
    adata_degs.uns['rank_genes_groups']['pts'].index.name = 'genes'
    adata_degs.uns['rank_genes_groups']['pts'].reset_index(inplace=True)
    adata_degs.uns['rank_genes_groups']['pts_rest'].index.name = 'genes'
    adata_degs.uns['rank_genes_groups']['pts_rest'].reset_index(inplace=True)
    pts  =  pd.melt(adata_degs.uns['rank_genes_groups']['pts'],id_vars ='genes', var_name='cluster',value_name='pts')
    pts_rest =  pd.melt(adata_degs.uns['rank_genes_groups']['pts_rest'],id_vars ='genes', var_name='cluster',value_name='pts_rest')
    pts['genes_cluster'] = pts['genes'] + "_" + pts['cluster']
    pts_pts_rest = pd.concat([pts, pts_rest['pts_rest']], axis=1)
    markers = pd.DataFrame({'p_val':p_val['p_val'] ,
                            'avg_log2FC':avg_log2FC['avg_log2FC'],
                            'p_val_adj':p_val_adj['p_val_adj'],
                            #'avg_log2_UMI.1':avg_UMI_1,
                            #'avg_log2_UMI.2':avg_UMI_2,
                            'scores':scores['scores'],
                            'cluster':genes['cluster'],
                            'gene':genes['genes']
                            })
    markers['genes_cluster'] = markers['gene'] + "_" + markers['cluster']
    markers = markers.merge(pts_pts_rest[['pts','pts_rest','genes_cluster']],left_on='genes_cluster', right_on='genes_cluster')
    markers = markers.dropna()
    markers = markers[['p_val', 'avg_log2FC', 'pts', 'pts_rest','p_val_adj', 'scores', 'cluster','gene']]
    return markers
    #store = pd.HDFStore('tempData/scFindAllMarkers.h5', 'w', complib=str('zlib'))
    #store.put('de', markers, data_columns=markers.columns)
    #store.close()
if __name__ == "__main__":
    markers = LoadAllDGEs()
