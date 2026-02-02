############################
## global.R
############################

library(shiny)
library(DT)
library(openxlsx)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(plotly)

## ---------- 0. Paths ----------
master_table_path <- "table/patchseqQC_master_table.xlsx"

tpm_counts_path <- "table/gene_counts_TPM.csv"
raw_counts_path <- "table/gene_counts.csv"

## ---------- 1. Load metadata ----------
metadata_with_markers <- read.xlsx(master_table_path)

# Save original column names (pretty names)
orig_colnames <- colnames(metadata_with_markers)

# Internally use syntactically valid column names
colnames(metadata_with_markers) <- make.names(orig_colnames, unique = TRUE)

# Lookup: internal -> pretty (names = internal, values = pretty)
pretty_col_lookup <- setNames(orig_colnames, colnames(metadata_with_markers))

# helper: pretty -> internal
pretty_to_internal <- function(pretty_vec) {
  idx <- match(pretty_vec, pretty_col_lookup)
  names(pretty_col_lookup)[idx]
}

# internal name of Sample column
sample_col_internal <- pretty_to_internal("Sample")
if (length(sample_col_internal) != 1 || is.na(sample_col_internal)) {
  stop("Could not find a 'Sample' column in the metadata table.")
}

# IMPORTANT: force Sample to character (prevents Excel numeric coercion like 55.3)
metadata_with_markers[[sample_col_internal]] <- as.character(metadata_with_markers[[sample_col_internal]])

## ---------- 2. Core / visible columns ----------
base_cols_pretty <- c(
  "col1","col2","col3"
)

base_cols <- pretty_to_internal(base_cols_pretty)
base_cols <- intersect(base_cols, colnames(metadata_with_markers))

other_cols <- setdiff(colnames(metadata_with_markers), base_cols)

all_columns_internal <- c(base_cols, other_cols)
all_columns_pretty   <- pretty_col_lookup[all_columns_internal]

## ---------- 3. Filterable variables ----------
# numeric detector based on column classes (after Excel import)
col_is_numeric <- sapply(metadata_with_markers, function(x) {
  is.numeric(x) || is.integer(x)
})

filterable_numeric <- names(which(col_is_numeric))
categorical_cols   <- setdiff(colnames(metadata_with_markers), filterable_numeric)

# drop Sample from default categorical filter choices
filterable_categorical <- setdiff(categorical_cols, sample_col_internal)

filterable_all        <- c(filterable_categorical, filterable_numeric)
filterable_all_pretty <- pretty_col_lookup[filterable_all]

is_numeric_col <- function(colname) colname %in% filterable_numeric

## ---------- 4. Groupable & metadata numeric plot choices ----------
# grouping (x-axis) from categorical columns (pretty labels)
groupable_pretty <- pretty_col_lookup[filterable_categorical]
groupable_pretty <- c("None", groupable_pretty)

# numeric metadata choices (for metadata distributions + correlation metadata)
marker_choices_pretty <- pretty_col_lookup[filterable_numeric]

## ---------- 5. Expression matrix loading ----------
# Robust loader:
# Accepts:
#   A) gene x sample with first column = gene IDs
#   B) sample x gene with first column = sample IDs
# Returns list(mat = numeric matrix with rownames=genes, colnames=samples, genes = gene vector)
load_expr_matrix <- function(path) {
  df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  
  if (nrow(df) < 2 || ncol(df) < 2) {
    stop("Expression file looks empty or malformed: ", path)
  }
  
  first_col <- df[[1]]
  first_col_name <- colnames(df)[1]
  
  # Heuristic: if first column looks like gene IDs, treat as gene x sample
  looks_like_gene_ids <- is.character(first_col) &&
    length(unique(first_col)) > 0.9 * length(first_col)
  
  # Also allow hint by column name
  if (!looks_like_gene_ids && grepl("gene", tolower(first_col_name))) {
    looks_like_gene_ids <- TRUE
  }
  
  if (looks_like_gene_ids) {
    genes <- as.character(first_col)
    df2 <- df[, -1, drop = FALSE]
    
    for (j in seq_len(ncol(df2))) suppressWarnings(df2[[j]] <- as.numeric(df2[[j]]))
    mat <- as.matrix(df2)
    
    rownames(mat) <- genes
    colnames(mat) <- as.character(colnames(df2))
    
    # enforce character dimnames
    rownames(mat) <- as.character(rownames(mat))
    colnames(mat) <- as.character(colnames(mat))
    
    return(list(mat = mat, genes = genes))
  }
  
  # Otherwise assume sample x gene (first column is sample IDs)
  looks_like_sample_ids <- is.character(first_col) &&
    length(unique(first_col)) > 0.9 * length(first_col)
  
  if (!looks_like_sample_ids) {
    stop(
      "Could not confidently detect orientation for expression file: ", path,
      "\nExpected first column to be gene IDs (gene x sample) or sample IDs (sample x gene)."
    )
  }
  
  samples <- as.character(first_col)
  df2 <- df[, -1, drop = FALSE]
  
  for (j in seq_len(ncol(df2))) suppressWarnings(df2[[j]] <- as.numeric(df2[[j]]))
  mat <- t(as.matrix(df2))
  
  rownames(mat) <- as.character(colnames(df2))  # genes
  colnames(mat) <- samples                      # samples
  
  # enforce character dimnames
  rownames(mat) <- as.character(rownames(mat))
  colnames(mat) <- as.character(colnames(mat))
  
  list(mat = mat, genes = rownames(mat))
}

tpm_obj <- load_expr_matrix(tpm_counts_path)
raw_obj <- load_expr_matrix(raw_counts_path)

expr_mats <- list(
  TPM = tpm_obj$mat,
  Raw = raw_obj$mat
)

# Gene universe (intersection preferred; fallback to TPM genes)
all_genes <- sort(intersect(rownames(expr_mats$TPM), rownames(expr_mats$Raw)))
if (length(all_genes) == 0) {
  all_genes <- sort(unique(rownames(expr_mats$TPM)))
}

## ---------- 6. Helper: get expression vector for a gene across samples ----------
get_expr_vec <- function(mat, gene, samples) {
  samples <- as.character(samples)
  if (is.null(gene) || is.na(gene) || gene == "") return(rep(NA_real_, length(samples)))
  if (!(gene %in% rownames(mat))) return(rep(NA_real_, length(samples)))
  
  smp <- intersect(samples, as.character(colnames(mat)))
  out <- rep(NA_real_, length(samples))
  names(out) <- samples
  
  out[smp] <- as.numeric(mat[gene, smp])
  out
}
