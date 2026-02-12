import numpy as np
from scipy import stats
from scipy import sparse
from statsmodels.stats.multitest import multipletests
import pandas as pd
import anndata as ad
import gc
import h5py
import pyreadr

def scFindCor_py():
    adata = ad.read_h5ad('sc1csr_gexpr.h5ad')
    print(adata.X.shape)
    exp = adata[r.Cells].X.toarray()
    if adata.var_names[0]== "0":
        adata.var_names = adata.var["features"]
    features = adata.var_names

    del adata
    gc.collect()

    exp_pos = exp > 0
    pct = np.ravel(exp_pos.mean(axis=0,dtype=np.float32))
    exp =  exp[:,pct > r.Min_expr]
    print(exp.shape)

    rho, pval = stats.spearmanr(exp,axis=0)
    print("start corrlation")

    store = h5py.File('tempData/scFindCor.h5', 'w')
    store.create_dataset('cor', data=rho)
    store.create_dataset('pval', data=pval)
    #store.create_dataset('features', data=list(features[pct > r.Min_expr]))
    store.close()

    df = pd.DataFrame(features[pct > r.Min_expr], columns=['features'])
    pyreadr.write_rds("tempData/CorFeatures.rds", df)
    print("saved corrlation")

if __name__ == "__main__":
    scFindCor_py()
