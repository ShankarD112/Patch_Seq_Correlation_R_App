# PatchSeq QC Explorer

## Overview
PatchSeq QC Explorer is a Shiny application for exploring Patch-seq metadata alongside gene expression counts. It provides an interactive table for filtering metadata, distribution plots for numeric variables or genes, and correlation analysis across filtered subsets to help you identify quality control trends and relationships between metadata and expression values.

## Key features
- **Interactive metadata table** with dynamic filters for categorical and numeric columns.
- **Column visibility controls** so you can focus on the fields that matter.
- **Distribution plots** (box/violin style) for either gene expression values or numeric metadata.
- **Correlation analysis** between any two numeric variables (metadata or gene expression), including summary stats and a fitted trend line.
- **Downloadable filtered table** exported to Excel.

## Data inputs
The app reads three files by default (under the `table/` directory):

| File | Purpose | Format |
| --- | --- | --- |
| `table/patchseqQC_master_table.xlsx` | Metadata table for all samples | Excel file with a required `Sample` column |
| `table/gene_counts_TPM.csv` | TPM expression matrix | CSV, either genes-by-sample or sample-by-genes |
| `table/gene_counts.csv` | Raw count matrix | CSV, either genes-by-sample or sample-by-genes |

### Expression matrix orientation
The loader auto-detects orientation based on the first column:
- **Genes-by-sample**: first column contains gene IDs (e.g., `Reln`, `Gad1`).
- **Sample-by-genes**: first column contains sample IDs.

## Running locally
1. **Install R packages** (example):
   ```r
   install.packages(c(
     "shiny", "DT", "openxlsx", "dplyr", "purrr",
     "ggplot2", "tidyr", "plotly"
   ))
   ```
2. **Launch the app** from the project root:
   ```r
   shiny::runApp()
   ```

## Deployment
A starter deployment script is provided in `run_app.R`. Update the `setwd()`, `appDir`, and `rsconnect::setAccountInfo()` fields with your own settings before running the script.

## Repository structure
```
.
├── global.R   # Data loading, column mapping, and helpers
├── server.R   # Server-side logic (filters, plots, correlations)
├── ui.R       # UI layout and inputs
├── run_app.R  # Example rsconnect deployment script
└── table/     # Input data files (not committed in this repo)
```

## Tips
- Use the **Filters** panel to define a filtered subset and then explore distributions or correlations for that subset.
- If you need to see all metadata fields, use the **Columns** selector to show additional columns.
- If the app shows `No valid Sample IDs left after filtering`, check the `Sample` column for blank or numeric-only IDs.
