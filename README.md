<<<<<<< HEAD
# pyShinyCell: Python-Enhanced Interactive Single-Cell Analysis

**pyShinyCell** extends [ShinyCell](https://github.com/SGDDNB/ShinyCell) with Python-powered statistical analysis tools, enabling interactive single-cell genomics applications with integrated differential expression, enrichment analysis, and correlation networks.

[![R package](https://img.shields.io/badge/R%20package-v0.1.0-blue.svg)](https://github.com/Olivier-Delaneau/pyShinyCell)
[![Python integration](https://img.shields.io/badge/Python%20integration-scanpy%20%2B%20gseapy-green.svg)](#python-analysis)
=======
# pyShinyCell package
`pyShinyCell` is a R + python package that allows users to create interactive Shiny-based 
web applications to visualise single-cell data via (i) visualising cell 
information and/or gene expression on reduced dimensions e.g. UMAP, (ii) 
visualising the coexpression of two genes on reduced dimensions, (iii) 
visualising the distribution of continuous cell information e.g. nUMI / module 
scores using violin plots / box plots, (iv) visualising the composition of 
different clusters / groups of cells using proportion plots and (v) 
visualising the expression of multiple genes using bubbleplots / heatmap. 
Examples of ShinyCell-generated shiny apps for single and multi datasets can 
be found at http://shinycell1.ddnetbio.com and http://shinycell2.ddnetbio.com 
respectively.

If you are using `pyShinyCell`, please cite [Ouyang et al. ShinyCell: Simple and 
sharable visualisation of single-cell gene expression data. Bioinformatics, 
doi:10.1093/bioinformatics/btab209](
http://dx.doi.org/10.1093/bioinformatics/btab209). The manuscript 
is recently accepted and we will update the full citation when it is available.

Key features of `pyShinyCell` include: 
>>>>>>> 0afd0be1ed7d8d7be420e8b04d8f5d63b4336976

## Features

### 🎯 Core Capabilities

- **Interactive web apps**: Built on Shiny for reproducible, shareable analysis interfaces
- **Python integration**: Differential expression, enrichment, correlation analysis via Python (scanpy, gseapy)
- **5 Custom analysis tabs**:
  - Pairwise differential expression (DE1)
  - All-vs-rest marker discovery (DE2)
  - Gene set enrichment analysis (GSEA/Enrichr)
  - Gene correlation networks
  - TCR/BCR repertoire analysis

- **H5AD-first**: Native [AnnData](https://anndata.readthedocs.io/) support for seamless Python workflows
- **One-command generation**: Single `makePyShinyApp()` call generates a complete app

<<<<<<< HEAD
### 📊 Visualization Features (Inherited from ShinyCell)

- Gene expression on 2D/3D embeddings (UMAP, t-SNE, etc.)
- Cell metadata visualization with customizable colors
- Gene co-expression plots
- Violin plots, proportion plots, heatmaps
- Interactive cell brushing and table display
- Export plots as PDF/PNG
=======
7. It is easy to use and customise aethetsics e.g. label names and colour 
   palettes. In the simplest form, pyShinyCell can convert an input single-cell 
   data into a Shiny app with five lines of code 
   (see [Quick Start Guide](#quick-start-guide))

We also compared pyShinyCell with nine other popular scRNA-seq visualisation 
tools, which further highlights the key features of `pyShinyCell`. For a more 
detailed description, see the 
[Supplementary Information](docs/OuyangEtAl_Shinycell_SuppInfo.pdf).
>>>>>>> 0afd0be1ed7d8d7be420e8b04d8f5d63b4336976

### ⚡ Performance & Scalability

- Memory-efficient: Gene expression stored in HDF5 (not RAM)
- Handles datasets up to 100K+ cells
- Low-latency web interface
- Deployed on shinyapps.io, AWS, Shiny Server, or local networks

### 🎨 Customization

- 30+ built-in color palettes (RColorBrewer, ggsci)
- Custom color assignment for metadata
- Enable/disable analysis tabs per app
- Default genes and plot settings
- Flexible metadata selection

<<<<<<< HEAD
## Quick Start
=======
- [Installation](#installation) on how to install `pyShinyCell`
>>>>>>> 0afd0be1ed7d8d7be420e8b04d8f5d63b4336976

### Installation

```r
# Install Bioconductor dependencies (required)
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("SummarizedExperiment", "qvalue", "fgsea"))

# Install from GitHub
devtools::install_github("Olivier-Delaneau/pyShinyCell")

# Python dependencies auto-setup on first use
library(pyShinyCell)
setupPythonEnv()  # One-time setup
```

### Generate Your First App (3 Lines!)

```r
library(pyShinyCell)

seu <- readRDS("path/to/your/seurat_object.rds")

makePyShinyApp(seu, shiny.dir = "myapp/", shiny.title = "My Analysis")
```

<<<<<<< HEAD
### Run the App

```r
shiny::runApp("myapp")
=======
`ShinyCell` can then be installed from GitHub as follows:
``` r
devtools::install_github("nyuhuyang/ShinyCell")
>>>>>>> 0afd0be1ed7d8d7be420e8b04d8f5d63b4336976
```

Open `http://localhost:3838` in your browser. Done! 🎉

## pyShinyCell vs ShinyCell

| Feature | ShinyCell | pyShinyCell |
|---------|-----------|-------------|
| Interactive visualization | ✅ | ✅ |
| Metadata + gene expression plots | ✅ | ✅ |
| HDF5-backed data | ✅ | ✅ |
| Pairwise differential expression | ❌ | ✅ (DE1) |
| All-vs-rest markers | ❌ | ✅ (DE2) |
| Gene set enrichment (fgsea/enrichR) | ❌ | ✅ (GSEA) |
| Correlation networks (igraph) | ❌ | ✅ |
| TCR/repertoire analysis | ❌ | ✅ |
| Python analysis integration | ❌ | ✅ |
| H5AD support | Partial | ✅ (native) |
| Single-command app generation | `makeShinyApp()` | ✅ `makePyShinyApp()` |

## Full Example

```r
library(pyShinyCell)
library(RColorBrewer)

# Load your Seurat object
seu <- readRDS("mydata.rds")

# Create configuration (optional, auto-created if omitted)
sc_conf <- createPyConfig(seu, meta.to.include = c("cell_type", "batch"))

# Customize colors
colors <- brewer.pal(n_cell_types, "Set2")
sc_conf$fCL[sc_conf$ID == "cell_type"] <- paste(colors, collapse = "|")

# Generate customized app
makePyShinyApp(
  obj = seu,
  scConf = sc_conf,
  shiny.dir = "myapp/",
  shiny.title = "My Customized App",

  # Select analysis tabs
  enable_tabs = c("de1", "de2", "gsea", "correlation"),

  # Set defaults
  default.gene1 = "NANOG",
  default.gene2 = "POU5F1",

  # Optimize files
  remove_assays = c("integrated", "SCT"),
  compress_h5ad = TRUE
)

# Run the app
shiny::runApp("myapp")
```

## Documentation

### 📖 Getting Started

See `vignette("getting-started")` for:
- Installation and setup
- Your first app in 3 steps
- Understanding generated files
- App interface overview

### 🎨 Customization Guide

See `vignette("customization")` for:
- Changing colors and metadata
- Selecting analysis tabs
- Setting default genes
- Optimizing file size
- Python environment configuration

### 🔧 Function Reference

```r
?makePyShinyApp       # Main API (orchestrates entire pipeline)
?createPyConfig       # Create/modify configuration
?makePyShinyFiles     # Generate data files
?setupPythonEnv       # Setup Python environment
?color_generator      # Generate custom colors
?pal.info             # Available color palettes
```

### 📚 Additional Resources

- **Reference implementation**: `system.file("extdata/shinyApp_stable", package = "pyShinyCell")`
- **Parent framework**: [ShinyCell GitHub](https://github.com/SGDDNB/ShinyCell)
- **Python analysis**: [scanpy documentation](https://scanpy.readthedocs.io/)

## Python Analysis Details

### What Python Does

When you run analysis in the app (DE, GSEA, correlation):

1. **Data**: Gene expression loaded from H5AD file
2. **Analysis**: Computed using Python (scanpy, gseapy, scipy)
3. **Results**: Cached in `tempData/` folder
4. **Visualization**: Plotted in R/Shiny UI

### Supported Analyses

| Tab | Python Library | What It Does |
|-----|----------------|--------------|
| **DE1** | scanpy.tl.rank_genes_groups | Wilcoxon rank-sum test |
| **DE2** | scanpy.tl.rank_genes_groups | Find markers per cluster |
| **GSEA** | gseapy + fgsea | Pathway enrichment |
| **Correlation** | scipy.stats | Spearman correlation |
| **TCR** | scRepertoire (R) | Clonotype analysis |

### Python Environment

<<<<<<< HEAD
pyShinyCell auto-manages a Python virtual environment (`~/.virtualenvs/pyShinyCell`) containing:
- numpy, pandas, scipy, h5py
- scanpy (single-cell analysis)
- gseapy (enrichment analysis)

To use a custom Python environment:

```r
makePyShinyApp(seu, python_env = "my_env", ...)
```

## System Requirements

### For App Generation
- R ≥ 3.5
- RAM: 8-16 GB (for building app with typical datasets)
- Python 3.6+ (auto-installed with reticulate)

### For Running Generated App
- Minimal: Works on laptops, servers, cloud platforms
- Memory: <1 GB (data loaded via HDF5)
- Network: Can be accessed remotely

## Use Cases

✅ **Research labs**: Share analysis with collaborators
✅ **Clinical settings**: Enable non-programmers to explore data
✅ **Teaching**: Interactive single-cell analysis demonstrations
✅ **Publications**: Supplementary interactive figures
✅ **Companies**: Internal data exploration tools

## Citation

If you use **pyShinyCell**, please cite both:

**pyShinyCell** (this package):
```bibtex
@software{pyShinyCell2024,
  title={pyShinyCell: Python-Enhanced Interactive Single-Cell Analysis},
  author={Hu, Yang},
  year={2024},
  url={https://github.com/Olivier-Delaneau/pyShinyCell}
}
```

**ShinyCell** (parent framework):
```bibtex
@article{Ouyang2021,
  title={ShinyCell: Simple and sharable visualisation of single-cell gene expression data},
  author={Ouyang, JF and others},
  journal={Bioinformatics},
  year={2021},
  doi={10.1093/bioinformatics/btab209}
}
```

## Troubleshooting

### Common Issues

**Q: Python module not found**
```r
# Reinstall Python environment
setupPythonEnv(venv_name = "pyShinyCell", force = TRUE)
```

**Q: App generation is slow**
- First run: Normal (creates HDF5 files)
- Subsequent runs: Caches data
- Tip: Remove large assays to speed up: `remove_assays = c("integrated")`

**Q: Gene names don't match**
- Check available genes: `head(rownames(seu), 20)`
- Enable gene ID mapping: `gene.mapping = TRUE`

**Q: File size is too large**
- Enable compression: `compress_h5ad = TRUE`
- Remove unneeded assays: `remove_assays = c("integrated", "SCT")`
- Skip H5AD if Python analysis not needed: `generate_h5ad = FALSE`

## Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request with description

## License

GPL-3 (same as parent ShinyCell project)

## Acknowledgments

pyShinyCell builds on the excellent [ShinyCell](https://github.com/SGDDNB/ShinyCell) framework by Ouyang et al., extending it with Python analysis capabilities for comprehensive single-cell analysis workflows.

---

**Get started**: `vignette("getting-started")`
**Customize**: `vignette("customization")`
**Report issues**: [GitHub Issues](https://github.com/Olivier-Delaneau/pyShinyCell/issues)
=======
<br/><br/>
<br/><br/>
<br/><br/>
<br/><br/>
<br/><br/>
>>>>>>> 0afd0be1ed7d8d7be420e8b04d8f5d63b4336976
