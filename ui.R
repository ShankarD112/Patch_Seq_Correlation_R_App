############################
## ui.R
############################

ui <- fluidPage(
  titlePanel("PatchSeq QC explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      #### FILTER BUILDER ####
      # Dynamic filters by metadata column (categorical or numeric)
      h4("Filters"),
      helpText("Add filters. 0 filters = show all cells."),
      
      fluidRow(
        column(8, actionButton("add_filter", "➕ Add filter", width = "100%")),
        column(4, actionButton("clear_filters", "Clear", width = "100%"))
      ),
      br(),
      
      uiOutput("filters_ui"),
      
      hr(),
      
      #### COLUMN VISIBILITY ####
      # Table columns shown in the metadata view
      h4("Columns"),
      helpText("Select which metadata columns to display in the table."),
      
      selectizeInput(
        "visible_cols_pretty",
        "Visible columns:",
        choices = all_columns_pretty,
        selected = pretty_col_lookup[base_cols],
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      
      hr(),
      
      #### TABLE OPTIONS ####
      # Table UX helpers + export
      h4("Table options"),
      checkboxInput("show_col_search", "Per-column search boxes", value = FALSE),
      
      downloadButton("download_filtered", "Download view (.xlsx)"),
      br(), br(),
      helpText("Download respects filters + visible columns.")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        tabPanel(
          "Table",
          DTOutput("qc_table")
        ),
        
        tabPanel(
          "Distributions (box/violin)",
          # Switch between gene expression and numeric metadata distributions
          fluidRow(
            column(
              4,
              selectInput(
                "dist_source",
                "Plot source:",
                choices = c("Gene expression", "Metadata numeric"),
                selected = "Gene expression"
              )
            ),
            column(
              4,
              conditionalPanel(
                condition = "input.dist_source == 'Gene expression'",
                selectInput(
                  "expr_type",
                  "Expression type:",
                  choices = c("TPM", "Raw"),
                  selected = "TPM"
                )
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.dist_source == 'Gene expression'",
            selectizeInput(
              "violin_genes",
              "Gene(s):",
              choices = NULL,      # populated server-side
              selected = NULL,
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                placeholder = "Type a gene (e.g., Reln) ..."
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.dist_source == 'Metadata numeric'",
            selectizeInput(
              "violin_meta_pretty",
              "Metadata variable(s):",
              choices = marker_choices_pretty,
              selected = NULL,
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                placeholder = "Select 1+ numeric metadata variables"
              )
            )
          ),
          
          selectInput(
            "violin_group_pretty",
            "Group by (x-axis):",
            choices = groupable_pretty,
            selected = "None"
          ),
          
          br(),
          uiOutput("violin_wrapper")
        ),
        
        tabPanel(
          "Correlation",
          # Compare any two numeric variables (metadata or gene expression)
          h4("Correlation between two variables (filtered subset)"),
          helpText("X and Y can be metadata numeric columns or gene expression (TPM/Raw)."),
          
          fluidRow(
            column(
              6,
              h5("X axis"),
              radioButtons(
                "corr_x_source",
                "X source:",
                choices = c("Metadata", "Gene"),
                selected = "Metadata",
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.corr_x_source == 'Metadata'",
                selectInput(
                  "corr_x_meta_pretty",
                  "X metadata variable:",
                  choices = marker_choices_pretty,
                  selected = if (length(marker_choices_pretty) >= 1) marker_choices_pretty[1] else NULL
                )
              ),
              conditionalPanel(
                condition = "input.corr_x_source == 'Gene'",
                selectInput(
                  "corr_x_expr_type",
                  "X expression type:",
                  choices = c("TPM", "Raw"),
                  selected = "TPM"
                ),
                selectizeInput(
                  "corr_x_gene",
                  "X gene:",
                  choices = NULL,   # populated server-side
                  selected = NULL,
                  multiple = FALSE,
                  options = list(placeholder = "Type a gene ...")
                )
              )
            ),
            
            column(
              6,
              h5("Y axis"),
              radioButtons(
                "corr_y_source",
                "Y source:",
                choices = c("Metadata", "Gene"),
                selected = "Metadata",
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.corr_y_source == 'Metadata'",
                selectInput(
                  "corr_y_meta_pretty",
                  "Y metadata variable:",
                  choices = marker_choices_pretty,
                  selected = if (length(marker_choices_pretty) >= 2) {
                    marker_choices_pretty[2]
                  } else if (length(marker_choices_pretty) == 1) {
                    marker_choices_pretty[1]
                  } else {
                    NULL
                  }
                )
              ),
              conditionalPanel(
                condition = "input.corr_y_source == 'Gene'",
                selectInput(
                  "corr_y_expr_type",
                  "Y expression type:",
                  choices = c("TPM", "Raw"),
                  selected = "TPM"
                ),
                selectizeInput(
                  "corr_y_gene",
                  "Y gene:",
                  choices = NULL,   # populated server-side
                  selected = NULL,
                  multiple = FALSE,
                  options = list(placeholder = "Type a gene ...")
                )
              )
            )
          ),
          
          hr(),
          selectInput(
            "corr_method",
            "Correlation method:",
            choices = c("pearson", "spearman"),
            selected = "pearson"
          ),
          
          verbatimTextOutput("corr_stats"),
          br(),
          plotlyOutput("corr_plot", height = "850px")
        )
      )
    )
  )
)
