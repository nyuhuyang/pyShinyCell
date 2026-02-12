# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**pyShinyCell** is an R package extending [ShinyCell](https://github.com/SGDDNB/ShinyCell) with **Python analysis tools** (scanpy, gseapy) for single-cell genomics Shiny web apps.

Key capabilities beyond ShinyCell:
- **Local Python Analysis**: DE, correlation, GSEA via Python scripts called from Shiny (reticulate)
- **Additional Visualizations**: FGSEA dot/bar plots, volcano plots, correlation networks (igraph)
- **H5AD Integration**: Full AnnData (h5ad) support as primary data structure
- **5 Custom Analysis Tabs**: Pairwise DE, All-vs-rest DE, GSEA, Correlation, TCR/Repertoire

## Project Status (as of Feb 11, 2025)

**Refactoring: Phases 1-5 COMPLETE ✅ | Phase 6 IN PROGRESS (Testing)**
Converting from manual workflow to proper R package. Full API stack functional with 50+ analysis functions extracted and documented. User-facing documentation complete. Testing framework established with 32 passing tests.

### Completed Implementation:

**Phase 1: Infrastructure (✅ Complete)**
- `R/setup_python.R` - Python virtual environment management
- `R/palette_data.R` - Color palette utilities (custom, ggsci, RColorBrewer)
- `inst/python/` - 3 Python analysis scripts packaged

**Phase 2: Data Pipeline (✅ Complete)**
- `R/convertToH5AD.R` - Seurat to H5AD conversion with gzip compression
- `R/createPyConfig.R` - Extended config with Python analysis flags
- `R/makePyShinyFiles.R` - Data file generation (HDF5, H5AD, maxlvl)

**Phase 3: Code Generation (✅ Complete)**
- `R/makePyShinyCode.R` - Code generation orchestrator (server.R, ui.R, global.R)
- `R/makePyShinyApp.R` - **Main user API** (orchestrates Phases 1-3)
- Framework for tab-specific code generation (DE1, DE2, GSEA, Correlation, TCR)

**Phase 4: Analysis Functions (✅ COMPLETE)**
Extracted 50+ functions from `shinyApp_stable/util.R` into organized, documented modules:
- `R/utils_colors.R` (3) - Color generation: color_generator, colorRamp2D, bilinear
- `R/utils_plotting.R` (7) - Plot utilities: doFactoring, g_legend, sctheme, rectangle, tableBrush, geom_split_violin, GeomSplitViolin
- `R/de_analysis.R` (8) - DE analysis: scFindMarkers, scFindAllMarkers, loadDEGs, LoadAnndata, subsetAnndata, etc.
- `R/gsea_analysis.R` (8) - GSEA/Enrichr: fgseaRes, FgseaDotPlot, FgseaBarplot, enrichrRes, etc.
- `R/correlation_analysis.R` (7) - Correlation: scFindCor, CorPlots, CorNetwork, VolcanoPlots, etc.
- `R/tcr_repertoire.R` (8+4) - TCR/BCR: scRepertoire, scClonalDiversity, clonotype analysis functions

**Phase 5: Documentation & Vignettes (✅ COMPLETE)**
- `vignettes/getting-started.Rmd` - Installation, 3-step quickstart, app interface tour, troubleshooting (built to HTML)
- `vignettes/customization.Rmd` - Config customization, color palettes, tabs, defaults, optimization (built to HTML)
- `README.md` - Complete rewrite for pyShinyCell (not ShinyCell): features, quick start, comparison table, Python details
- `NEWS.md` - Changelog, breaking changes, infrastructure changes, migration guide
- `DESCRIPTION` - Updated VignetteBuilder, added all missing Imports (dplyr, enrichR, fgsea, ggpubr, ggrepel, magrittr, networkD3, pbapply, plyr, qvalue, tibble, tidyr)
- Fixed roxygen2 data export for `pal.info` (use @docType data instead of @export)
- Fixed `makePyShinyApp.R` line 131: removed invalid `verbose` parameter in createPyConfig() call

**All Supporting Infrastructure:**
- `inst/python/` - 3 Python analysis scripts (scFindMarkers_ad.py, scFindCor_ad.py, loadAllDEGs_ad.py)
- `data/pal.info.rda` - Palette metadata (lazy-loaded data)
- `man/` - Auto-generated roxygen2 documentation (50+ .Rd files)
- `NAMESPACE` - Exports with roxygen2 declarations

### Phase 6: Testing & Validation (IN PROGRESS ⏳)

**Completed:**
- ✅ Fixed critical bug: `py_analysis` column→attribute in createPyConfig (line 45-50 of R/createPyConfig.R) to avoid ShinyCell column mismatch
- ✅ Comprehensive test suite with 32 passing tests across 4 test files:
  - `tests/testthat/test-prepare-files-readyseu.R` (7 tests) - Full `makePyShinyFiles()` integration with real reference data
  - `tests/testthat/test-config-creation.R` (5 tests) - Configuration creation, variable identification, dimension reduction requirement
  - `tests/testthat/test-color-utils.R` (3 tests) - Palette data, color utilities, ggplot themes
  - `tests/testthat/test-pipeline-core.R` (3 tests) - Package attributes, py_analysis metadata, variable level filtering

**Remaining (Phase 6):**
- Need: Unit tests for 50+ analysis functions (DE, GSEA, correlation, TCR - complex Python integration required)
- Need: Error handling and edge case tests (invalid inputs, missing dependencies)
- Verify: R CMD check clean with no errors/warnings
- Verify: All vignette examples run without errors
- Goal: 100% pass rate on `devtools::test()` and `devtools::check()`

## Architecture

### Main User API (Phases 1-4 Complete)
```r
makePyShinyApp(seu, shiny.dir = "myapp/", shiny.title = "My App")
```
Single function orchestrates entire pipeline:
1. Config creation/validation (`createPyConfig()`)
2. Data file generation (`makePyShinyFiles()` → HDF5, H5AD, RDS)
3. Shiny code generation (`makePyShinyCode()` → server.R, ui.R, global.R)
4. Python environment setup (`setupPythonEnv()`)

### Data Files Generated
- `{prefix}conf.rds` - Configuration (scConf data.table)
- `{prefix}meta.rds` - Cell metadata
- `{prefix}gene.rds` - Gene name mapping
- `{prefix}gexpr.h5` - Gene expression matrix (HDF5)
- `{prefix}def.rds` - Default settings (genes/groups/dimred defaults)
- `{prefix}gexpr.h5ad` - Gene expression (H5AD for Python)
- `{prefix}maxlvl.rds` - Max expression per gene

### Code Files Generated
- `server.R` - Shiny server logic (generated from template + tab code)
- `ui.R` - Shiny UI definitions (generated from template + tab UI)
- `global.R` - Data loading + Python environment setup
- `util.R` - Plotting functions (copied from package)
- `util_palette.R` - Color palettes (copied from package)

### Configuration Data.Table (`scConf`)
Central object with columns: UI, ID, uiType, default, fCL, fRow, show, legend, order (from ShinyCell).
- `fCL`: Pipe-separated factor color levels for categorical variables
- `py_analysis` (attribute, not column): Logical vector indicating if each variable (2-50 categorical levels) is suitable for Python-based analysis. Stored as `attr(scConf, "py_analysis")` to avoid conflicts with ShinyCell's column structure.
  - Key fix (Feb 2025): Changed from column to attribute to prevent data.table rbindlist column mismatch when calling ShinyCell::makeShinyFiles()

### Data Flow (Python Integration)
```
Seurat/SCE → H5AD (with gzip) → Shiny UI → reticulate → Python scripts → tempData/ → R plots → Shiny
```

## Development Commands

### Package Development
```r
# Install Bioconductor dependencies (one-time per environment)
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("SummarizedExperiment", "qvalue", "fgsea"))

devtools::load_all()     # Reload package (required to test new code)
devtools::document()     # Build roxygen2 documentation
devtools::test()         # Run testthat test suite (Phase 6 active)
devtools::check()        # R CMD check (Phase 6 requirement)
```

### Running Tests
```r
devtools::test()                    # Run all tests (32 passing as of Feb 11)
testthat::test_file("tests/testthat/test-config-creation.R")  # Run specific test
testthat::expect_true(value)        # Assert within tests
```

### Using the API
```r
# Create an app with your data
library(pyShinyCell)
seu <- readRDS("mydata.rds")
makePyShinyApp(seu, shiny.dir = "myapp/", shiny.title = "My Analysis")

# Run the generated app
shiny::runApp("myapp")
```

### Reference Implementation
```r
# Test with reference data (already has generated app)
shiny::runApp("shinyApp_stable")
```

## Reference Implementation Files

The `shinyApp_stable/` directory contains a fully functional pyShinyCell app generated with all 5 custom analysis tabs. Use for:
- Testing the full workflow: `shiny::runApp("shinyApp_stable")`
- Reference for expected output structure and behavior
- Template for custom app generation

| File | Lines | Purpose |
|------|-------|---------|
| `shinyApp_stable/server.R` | ~1543 | Generated server logic (custom tabs) |
| `shinyApp_stable/ui.R` | ~2319 | Generated UI definitions (custom tabs) |
| `shinyApp_stable/util.R` | ~3656 | Plotting/analysis functions (now extracted to R/*.R) |
| `shinyApp_stable/util_palette.R` | ~110 | Color utilities (reference for palette_data.R) |
| `shinyApp_stable/global.R` | ~86 | Data loading (reference for setup) |

## Important Notes

- **Memory**: Building apps loads entire single-cell object in RAM (~8-16GB). Generated apps use HDF5 for gene expression (off-memory).
- **File prefix convention**: Generated files use prefix (default "sc1"): `sc1conf.rds`, `sc1meta.rds`, `sc1gene.rds`, `sc1gexpr.h5`
- **Multi-format input**: Supports Seurat (v3+), SingleCellExperiment, h5ad, loom
- **Dependencies**: Core includes Bioconductor packages `SummarizedExperiment`, `qvalue`, `fgsea` (install via `BiocManager::install(c("SummarizedExperiment", "qvalue", "fgsea"))`) plus data.table, Matrix, hdf5r, reticulate, ggplot2, gridExtra, glue, readr, RColorBrewer, igraph. Generated apps also need: shiny, shinyhelper, DT

## Implemented API ✅

```r
library(pyShinyCell)

# Basic usage
makePyShinyApp(seu, shiny.dir = "myapp/")

# Full customization
makePyShinyApp(
  obj = seu,
  scConf = NULL,                                    # Auto-created if NULL
  shiny.dir = "my_app/",
  shiny.title = "My Analysis",
  enable_tabs = c("de1", "de2", "gsea", "correlation", "tcr"),
  gene.mapping = TRUE,
  default.gene1 = "NANOG",
  default.gene2 = "OCT4",
  python_env = "pyShinyCell"                        # Auto-setup
)

# Result: Ready-to-run Shiny app in my_app/
shiny::runApp("my_app")
```

## Phase 5: Documentation & Vignettes (✅ COMPLETE)

**Scope**: Essential documentation to make package functional (not comprehensive). Focus on core workflow and customization.

**Completed Deliverables:**

1. **Two Comprehensive Vignettes** (built to HTML, ~80 KB total)
   - `vignettes/getting-started.Rmd` (28 KB HTML)
     - Installation & Python setup
     - 3-step quickstart
     - Understanding generated files
     - App interface tour
     - Troubleshooting
   - `vignettes/customization.Rmd` (52 KB HTML)
     - Configuration customization with examples
     - Analysis tab selection
     - Color palette management
     - Default genes & plot settings
     - File optimization strategies
     - Advanced config structure

2. **README.md Rewrite** (~290 lines)
   - Project overview (pyShinyCell vs ShinyCell comparison table)
   - Quick start (3-line example)
   - Full customization example with real code
   - Python analysis details
   - System requirements
   - Use cases & deployment
   - Citation instructions
   - Troubleshooting

3. **NEWS.md** (~250 lines)
   - Complete changelog with breaking changes
   - 50+ functions organized by category
   - Infrastructure improvements
   - Migration guide for ShinyCell users

4. **Package Infrastructure**
   - Updated `DESCRIPTION`: VignetteBuilder, Suggests (knitr, rmarkdown), Imports (12 new packages)
   - Fixed roxygen2 data export (pal.info: @docType data instead of @export)
   - Fixed `makePyShinyApp.R` line 131 (removed invalid verbose parameter)

**Quality Assurance:**
- ✅ Both vignettes build successfully: `devtools::build_vignettes()`
- ✅ Package check passes: `devtools::check()` → 0 errors, 0 warnings, 1 note (expected: 28 imports)
- ✅ All vignette examples are valid R code

**Key Design Decisions:**
- Used shinyApp_stable/ as reference (users can explore working app)
- Kept vignettes focused on workflow, not implementation details
- Python integration details documented separately from basic workflow
- Both vignettes wrapped in HTML vignette template for easy browsing

<!-- Next update trigger: After Phase 6 completion (R CMD check passes with no errors), or when adding tests for analysis functions (DE, GSEA, correlation, TCR) -->
