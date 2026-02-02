############################
## server.R  (revamped)
############################

server <- function(input, output, session) {
  
  #############################################
  # 0) Populate gene selectize inputs
  #############################################
  observe({
    # Use server-side selectize for performance with large gene lists
    updateSelectizeInput(session, "violin_genes", choices = all_genes, server = TRUE)
    updateSelectizeInput(session, "corr_x_gene",  choices = all_genes, server = TRUE)
    updateSelectizeInput(session, "corr_y_gene",  choices = all_genes, server = TRUE)
  })
  
  #############################################
  # 1) Helpers for Sample hygiene (PLOT ONLY)
  #############################################
  clean_id <- function(x) trimws(as.character(x))
  
  # "valid sample id" for plotting/correlation:
  # - not NA
  # - not empty
  # - contains at least one letter (drops "30.3", "55", etc.)
  is_valid_sample_id <- function(x) {
    x <- clean_id(x)
    !is.na(x) & x != "" & grepl("[A-Za-z]", x)
  }
  
  # deterministic jitter in [-w, w] based on sample string
  # (so jitter is stable across rerenders)
  jitter_from_id <- function(ids, w = 0.22) {
    ids <- clean_id(ids)
    # simple deterministic hash -> [0,1)
    h <- vapply(ids, function(s) {
      if (is.na(s) || s == "") return(0.5)
      bytes <- utf8ToInt(s)
      # polynomial rolling hash
      val <- 0
      for (b in bytes) val <- (val * 131 + b) %% 1000003
      val / 1000003
    }, numeric(1))
    (h - 0.5) * 2 * w
  }
  
  #############################################
  # A) Track active filters by ID
  #############################################
  # Filter rows are dynamic; store stable IDs to keep inputs synced.
  filter_ids <- reactiveVal(character(0))
  
  new_filter_id <- function() paste0("filt_", as.integer(runif(1, 1e6, 9e6)))
  
  observeEvent(input$add_filter, {
    fid <- new_filter_id()
    filter_ids(c(filter_ids(), fid))
  })
  
  observeEvent(input$clear_filters, {
    filter_ids(character(0))
  })
  
  #############################################
  # B) Render filter rows container
  #############################################
  output$filters_ui <- renderUI({
    ids <- filter_ids()
    if (length(ids) == 0) return(tags$em("No active filters"))
    
    tagList(lapply(ids, function(fid) {
      existing_choice <- isolate(input[[paste0("filter_col_", fid)]])
      if (is.null(existing_choice)) existing_choice <- filterable_all_pretty[1]
      
      wellPanel(
        fluidRow(
          column(
            width = 8,
            selectInput(
              inputId  = paste0("filter_col_", fid),
              label    = "Column",
              choices  = filterable_all_pretty,
              selected = existing_choice
            )
          ),
          column(
            width = 4,
            actionButton(
              inputId = paste0("rm_", fid),
              label = "🗑 Remove",
              class = "btn-danger",
              style = "margin-top:25px; width:100%;"
            )
          )
        ),
        uiOutput(paste0("filter_value_ui_", fid))
      )
    }))
  })
  
  observe({
    lapply(filter_ids(), function(fid) {
      observeEvent(input[[paste0("rm_", fid)]], {
        filter_ids(setdiff(filter_ids(), fid))
      }, ignoreInit = TRUE)
    })
  })
  
  #############################################
  # C) Per-filter UI (numeric vs categorical)
  #############################################
  observe({
    lapply(filter_ids(), function(fid) {
      
      output[[paste0("filter_value_ui_", fid)]] <- renderUI({
        col_pretty <- input[[paste0("filter_col_", fid)]]
        if (is.null(col_pretty)) return(tags$em("Select a column"))
        
        internal_col <- names(pretty_col_lookup)[pretty_col_lookup == col_pretty]
        if (length(internal_col) == 0 || is.na(internal_col[1])) return(tags$em("Invalid column"))
        internal_col <- internal_col[1]
        
        if (internal_col %in% filterable_numeric) {
          vals <- metadata_with_markers[[internal_col]]
          suppressWarnings({ if (!is.numeric(vals)) vals <- as.numeric(vals) })
          
          rng <- range(vals, na.rm = TRUE)
          if (!is.finite(rng[1]) || !is.finite(rng[2])) return(tags$em("Column has no finite numeric values."))
          
          min_default <- floor(rng[1])
          max_default <- ceiling(rng[2])
          
          cur_min <- isolate(input[[paste0("num_min_", fid)]])
          cur_max <- isolate(input[[paste0("num_max_", fid)]])
          cur_rng <- isolate(input[[paste0("num_slider_", fid)]])
          
          if (is.null(cur_min)) cur_min <- min_default
          if (is.null(cur_max)) cur_max <- max_default
          if (is.null(cur_rng) || length(cur_rng) != 2) cur_rng <- c(cur_min, cur_max)
          
          tagList(
            fluidRow(
              column(6, numericInput(paste0("num_min_", fid), "Min", value = cur_min, step = 0.01)),
              column(6, numericInput(paste0("num_max_", fid), "Max", value = cur_max, step = 0.01))
            ),
            sliderInput(
              inputId = paste0("num_slider_", fid),
              label   = NULL,
              min     = floor(rng[1]),
              max     = ceiling(rng[2]),
              value   = cur_rng,
              step    = max(0.01, (ceiling(rng[2]) - floor(rng[1])) / 100)
            )
          )
        } else {
          # Categorical filter: multiselect list of allowed values
          vals <- sort(unique(metadata_with_markers[[internal_col]]))
          cur_cats <- isolate(input[[paste0("cat_vals_", fid)]])
          if (is.null(cur_cats) && length(vals) > 0) cur_cats <- vals[1]
          
          selectInput(
            inputId   = paste0("cat_vals_", fid),
            label     = "Allowed values",
            choices   = vals,
            multiple  = TRUE,
            selected  = cur_cats
          )
        }
      })
    })
  })
  
  #############################################
  # D) Keep numeric slider and boxes in sync
  #############################################
  # Maintain a single source of truth for numeric filters.
  observe({
    lapply(filter_ids(), function(fid) {
      observeEvent(input[[paste0("num_slider_", fid)]], {
        rng <- input[[paste0("num_slider_", fid)]]
        if (!is.null(rng) && length(rng) == 2) {
          updateNumericInput(session, paste0("num_min_", fid), value = rng[1])
          updateNumericInput(session, paste0("num_max_", fid), value = rng[2])
        }
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    lapply(filter_ids(), function(fid) {
      observeEvent(
        list(input[[paste0("num_min_", fid)]], input[[paste0("num_max_", fid)]]),
        {
          vmin <- input[[paste0("num_min_", fid)]]
          vmax <- input[[paste0("num_max_", fid)]]
          if (!is.null(vmin) && !is.null(vmax)) {
            updateSliderInput(session, paste0("num_slider_", fid), value = c(vmin, vmax))
          }
        },
        ignoreInit = TRUE
      )
    })
  })
  
  #############################################
  # E) Column visibility (metadata table)
  #############################################
  columns_to_display <- reactive({
    # Preserve selected order from the UI (using pretty -> internal map).
    chosen_pretty <- input$visible_cols_pretty
    internal <- names(pretty_col_lookup)[pretty_col_lookup %in% chosen_pretty]
    internal <- internal[match(chosen_pretty, pretty_col_lookup[internal])]
    intersect(internal, colnames(metadata_with_markers))
  })
  
  #############################################
  # F) Apply filters to metadata (TABLE)
  #    (we DO NOT drop bad Sample IDs here; table stays diagnostic)
  #############################################
  filtered_df <- reactive({
    df <- metadata_with_markers
    df[[sample_col_internal]] <- as.character(df[[sample_col_internal]])
    
    ids <- filter_ids()
    if (length(ids) == 0) return(df)
    
    for (fid in ids) {
      col_pretty <- input[[paste0("filter_col_", fid)]]
      if (is.null(col_pretty)) next
      
      internal_col <- names(pretty_col_lookup)[pretty_col_lookup == col_pretty]
      if (length(internal_col) == 0 || is.na(internal_col[1])) next
      internal_col <- internal_col[1]
      
      if (internal_col %in% filterable_numeric) {
        vmin <- input[[paste0("num_min_", fid)]]
        vmax <- input[[paste0("num_max_", fid)]]
        if (!is.null(vmin) && !is.null(vmax)) {
          suppressWarnings(df[[internal_col]] <- as.numeric(df[[internal_col]]))
          df <- df %>%
            filter(is.na(.data[[internal_col]]) |
                     (.data[[internal_col]] >= vmin & .data[[internal_col]] <= vmax))
        }
      } else {
        cats <- input[[paste0("cat_vals_", fid)]]
        if (!is.null(cats) && length(cats) > 0) {
          df <- df %>% filter(.data[[internal_col]] %in% cats)
        }
      }
    }
    df
  })
  
  #############################################
  # G) Plot-ready metadata (PLOTS + CORR)
  #    (this is the key: all hygiene happens once, here)
  #############################################
  plot_meta <- reactive({
    df <- filtered_df()
    df$Sample_internal <- clean_id(df[[sample_col_internal]])
    
    df <- df %>%
      filter(is_valid_sample_id(Sample_internal))  # drops blanks + numeric-only + NA
    
    validate(need(nrow(df) > 0, "No valid Sample IDs left after filtering."))
    
    df
  })
  
  #############################################
  # H) Table output (diagnostic; includes everything after filters)
  #############################################
  output$qc_table <- renderDT({
    df <- filtered_df()[, columns_to_display(), drop = FALSE]
    display_df <- df
    colnames(display_df) <- pretty_col_lookup[colnames(df)]
    
    filter_opt <- if (isTRUE(input$show_col_search)) "top" else "none"
    
    datatable(
      display_df,
      rownames = FALSE,
      filter   = filter_opt,
      extensions = c("Buttons","FixedHeader"),
      options = list(
        scrollX = TRUE,
        pageLength = 20,
        fixedHeader = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      class = "stripe hover compact"
    )
  })
  
  #############################################
  # I) Distributions data (genes OR metadata numeric)
  #############################################
  dist_plot_df <- reactive({
    req(input$dist_source)
    
    df_meta <- plot_meta()
    
    # grouping variable (categorical x-axis)
    group_col_internal <- NULL
    if (!is.null(input$violin_group_pretty) && input$violin_group_pretty != "None") {
      tmp <- names(pretty_col_lookup)[pretty_col_lookup == input$violin_group_pretty]
      tmp <- tmp[1]
      if (!is.null(tmp) && !is.na(tmp) && tmp %in% colnames(df_meta)) group_col_internal <- tmp
    }
    df_meta$group_var <- if (!is.null(group_col_internal)) as.character(df_meta[[group_col_internal]]) else "all_cells"
    df_meta$group_var[is.na(df_meta$group_var) | trimws(df_meta$group_var) == ""] <- "NA_group"
    df_meta$Sample <- df_meta$Sample_internal
    
    if (input$dist_source == "Gene expression") {
      req(input$expr_type, input$violin_genes)
      genes <- input$violin_genes
      validate(need(length(genes) > 0, "Select at least one gene."))
      
      mat <- expr_mats[[input$expr_type]]
      validate(need(!is.null(mat), "Expression matrix not available."))
      
      smp_in_mat <- intersect(df_meta$Sample_internal, as.character(colnames(mat)))
      validate(need(length(smp_in_mat) > 0, "No filtered samples found in expression matrix."))
      
      df_meta2 <- df_meta %>% filter(Sample_internal %in% smp_in_mat)
      
      long_list <- lapply(genes, function(g) {
        v <- get_expr_vec(mat, g, df_meta2$Sample_internal)
        data.frame(
          Sample = df_meta2$Sample_internal,
          group_var = df_meta2$group_var,
          feature = g,
          value = as.numeric(v),
          stringsAsFactors = FALSE
        )
      })
      
      long_df <- bind_rows(long_list) %>% filter(!is.na(value))
      validate(need(nrow(long_df) > 0, "No non-NA expression values to plot (after filtering)."))
      
      long_df$feature <- factor(long_df$feature, levels = genes)
      long_df$group_var <- factor(long_df$group_var)
      
      attr(long_df, "ylab")  <- paste0(input$expr_type, " expression")
      attr(long_df, "title") <- paste0(input$expr_type, " gene expression across filtered cells")
      return(long_df)
    }
    
    # Metadata numeric
    req(input$violin_meta_pretty)
    feats_pretty <- input$violin_meta_pretty
    validate(need(length(feats_pretty) > 0, "Select 1+ numeric metadata variables."))
    
    feats_internal <- pretty_to_internal(feats_pretty)
    feats_internal <- feats_internal[!is.na(feats_internal)]
    feats_internal <- intersect(feats_internal, colnames(df_meta))
    validate(need(length(feats_internal) > 0, "None of the selected metadata columns were found."))
    
    for (f in feats_internal) suppressWarnings(df_meta[[f]] <- as.numeric(df_meta[[f]]))
    
    long_df <- df_meta %>%
      select(Sample_internal, group_var, all_of(feats_internal)) %>%
      pivot_longer(cols = all_of(feats_internal), names_to = "feature_internal", values_to = "value") %>%
      filter(!is.na(value))
    
    validate(need(nrow(long_df) > 0, "No non-NA values to plot after filtering."))
    
    long_df$Sample  <- long_df$Sample_internal
    long_df$feature <- factor(pretty_col_lookup[long_df$feature_internal], levels = feats_pretty)
    long_df$group_var <- factor(long_df$group_var)
    
    attr(long_df, "ylab")  <- "Value"
    attr(long_df, "title") <- "Metadata distributions across filtered cells"
    long_df
  })
  
  dist_height_px <- reactive({
    if (input$dist_source == "Gene expression") {
      n <- length(input$violin_genes)
    } else {
      n <- length(input$violin_meta_pretty)
    }
    if (is.null(n) || is.na(n) || n < 1) n <- 1
    max(700, n * 360)
  })
  
  output$violin_wrapper <- renderUI({
    plotlyOutput("violin_plot", height = paste0(dist_height_px(), "px"), width = "100%")
  })
  
  #############################################
  # J) Distributions plot (PURE PLOTLY, jitter stable, no mystery points)
  #############################################
  output$violin_plot <- renderPlotly({
    pdat <- dist_plot_df()
    
    ylab <- attr(pdat, "ylab")
    ttl  <- attr(pdat, "title")
    
    xlab <- if (is.null(input$violin_group_pretty) || input$violin_group_pretty == "None") {
      "Cells"
    } else {
      input$violin_group_pretty
    }
    
    # Keep tooltip-friendly sample label
    pdat$Sample2 <- as.character(pdat$Sample)
    pdat$Sample2[is.na(pdat$Sample2) | trimws(pdat$Sample2) == ""] <- "(missing)"
    
    p <- ggplot(
      pdat,
      aes(
        x = group_var,
        y = value,
        text = paste(
          "Sample:", Sample2,
          "<br>Group:", group_var,
          "<br>Feature:", feature,
          "<br>Value:", signif(value, 4)
        )
      )
    ) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      geom_jitter(width = 0.20, alpha = 0.45, size = 0.85) +
      facet_grid(feature ~ ., scales = "free_y") +
      labs(x = xlab, y = ylab, title = ttl) +
      theme_bw(base_size = 12) +
      theme(
        strip.background = element_rect(fill = "gray90", color = NA),
        strip.text.y     = element_text(angle = 0, hjust = 1),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.spacing.y  = unit(1, "lines"),
        plot.margin      = margin(10, 20, 10, 10)
      )
    
    gp <- ggplotly(p, tooltip = "text")
    gp <- plotly::layout(
      gp,
      autosize = TRUE,
      height = dist_height_px(),
      margin = list(l = 80, r = 220, t = 60, b = 80)
    )
    
    
    # --- CRITICAL FIX #1 ---
    # Disable ANY points/outliers that Plotly draws for box traces
    gp$x$data <- lapply(gp$x$data, function(tr) {
      if (!is.null(tr$type) && tr$type == "box") {
        tr$boxpoints <- "false"   # turn off outlier points
        tr$jitter    <- 0
        tr$pointpos  <- 0
        tr$hoverinfo <- "skip"
        # Extra safety: if a plotly version still tries to draw points, hide them
        tr$marker <- list(size = 0, opacity = 0)
      }
      tr
    })
    
    # --- CRITICAL FIX #2 ---
    # Remove any extra marker scatter traces with no tooltip text (rare, but happens)
    gp$x$data <- Filter(function(tr) {
      if (is.null(tr$type) || tr$type != "scatter") return(TRUE)
      if (is.null(tr$mode) || !grepl("markers", tr$mode)) return(TRUE)
      if (!is.null(tr$text) && length(tr$text) > 0) return(TRUE)
      FALSE
    }, gp$x$data)
    
    gp
  })
  
  
  
  #############################################
  # K) Correlation (X/Y = metadata or gene), uses plot_meta() hygiene
  #############################################
  corr_df <- reactive({
    df_meta <- plot_meta()
    df_meta$Sample <- df_meta$Sample_internal
    
    base_samples <- unique(df_meta$Sample_internal)
    
    # X axis mapping (metadata numeric or gene expression)
    if (input$corr_x_source == "Metadata") {
      req(input$corr_x_meta_pretty)
      x_col <- pretty_to_internal(input$corr_x_meta_pretty)
      validate(need(!is.na(x_col) && x_col %in% colnames(df_meta), "Invalid X metadata column."))
      x_map <- df_meta %>% transmute(Sample = Sample_internal, x = suppressWarnings(as.numeric(.data[[x_col]])))
      x_label <- input$corr_x_meta_pretty
    } else {
      req(input$corr_x_gene, input$corr_x_expr_type)
      matx <- expr_mats[[input$corr_x_expr_type]]
      validate(need(!is.null(matx), "X expression matrix not available."))
      smp_in_mat <- intersect(base_samples, as.character(colnames(matx)))
      validate(need(length(smp_in_mat) > 0, "No filtered samples found in X expression matrix."))
      xv <- get_expr_vec(matx, input$corr_x_gene, smp_in_mat)
      x_map <- data.frame(Sample = smp_in_mat, x = as.numeric(xv), stringsAsFactors = FALSE)
      x_label <- paste0(input$corr_x_gene, " (", input$corr_x_expr_type, ")")
      base_samples <- smp_in_mat
    }
    
    # Y axis mapping (metadata numeric or gene expression)
    if (input$corr_y_source == "Metadata") {
      req(input$corr_y_meta_pretty)
      y_col <- pretty_to_internal(input$corr_y_meta_pretty)
      validate(need(!is.na(y_col) && y_col %in% colnames(df_meta), "Invalid Y metadata column."))
      y_map <- df_meta %>% transmute(Sample = Sample_internal, y = suppressWarnings(as.numeric(.data[[y_col]])))
      y_label <- input$corr_y_meta_pretty
    } else {
      req(input$corr_y_gene, input$corr_y_expr_type)
      maty <- expr_mats[[input$corr_y_expr_type]]
      validate(need(!is.null(maty), "Y expression matrix not available."))
      smp_in_mat <- intersect(base_samples, as.character(colnames(maty)))
      validate(need(length(smp_in_mat) > 0, "No filtered samples found in Y expression matrix."))
      yv <- get_expr_vec(maty, input$corr_y_gene, smp_in_mat)
      y_map <- data.frame(Sample = smp_in_mat, y = as.numeric(yv), stringsAsFactors = FALSE)
      y_label <- paste0(input$corr_y_gene, " (", input$corr_y_expr_type, ")")
      base_samples <- smp_in_mat
    }
    
    out <- inner_join(x_map, y_map, by = "Sample") %>%
      filter(!is.na(x) & !is.na(y))
    
    validate(need(nrow(out) > 0, "No overlapping non-NA values for X and Y after filtering."))
    
    attr(out, "x_label") <- x_label
    attr(out, "y_label") <- y_label
    out
  })
  
  corr_fit <- reactive({
    df <- corr_df()
    if (nrow(df) < 2) return(NULL)
    # Linear fit + confidence interval for trend line
    fit <- lm(y ~ x, data = df)
    xs  <- seq(min(df$x, na.rm = TRUE), max(df$x, na.rm = TRUE), length.out = 100)
    preds <- predict(fit, newdata = data.frame(x = xs), interval = "confidence")
    data.frame(x = xs, y = preds[, "fit"], ymin = preds[, "lwr"], ymax = preds[, "upr"])
  })
  
  output$corr_stats <- renderPrint({
    df <- corr_df()
    method <- input$corr_method
    if (is.null(method) || !(method %in% c("pearson","spearman"))) method <- "pearson"
    
    if (nrow(df) < 3) {
      cat("Not enough points to compute a reliable correlation (need at least 3).\n")
      cat("N (points): ", nrow(df), "\n")
      return()
    }
    
    # Report correlation stats for filtered subset only
    r <- suppressWarnings(cor(df$x, df$y, use = "complete.obs", method = method))
    fit <- lm(y ~ x, data = df)
    s   <- summary(fit)
    
    r2    <- s$r.squared
    p_val <- coef(s)[2, 4]
    p_adj <- p.adjust(p_val, method = "BH")
    
    cat("Method:                ", method, "\n")
    cat("Correlation (r):       ", round(r, 3), "\n")
    cat("R-squared (R^2):       ", round(r2, 3), "\n")
    cat("p-value (slope):       ", signif(p_val, 4), "\n")
    cat("BH-adjusted p (padj):  ", signif(p_adj, 4), "\n")
    cat("N (points):            ", nrow(df), "\n")
  })
  
  output$corr_plot <- renderPlotly({
    df     <- corr_df()
    fit_df <- corr_fit()
    
    xlab <- attr(df, "x_label")
    ylab <- attr(df, "y_label")
    
    p <- ggplot(
      df,
      aes(
        x = x,
        y = y,
        text = paste("Sample:", Sample, "<br>X:", signif(x, 4), "<br>Y:", signif(y, 4))
      )
    ) + geom_point(alpha = 0.7, size = 1.8, color = "black")
    
    if (!is.null(fit_df)) {
      p <- p +
        geom_ribbon(data = fit_df, aes(x = x, ymin = ymin, ymax = ymax),
                    inherit.aes = FALSE, alpha = 0.2, fill = "coral") +
        geom_line(data = fit_df, aes(x = x, y = y),
                  inherit.aes = FALSE, color = "coral2", linewidth = 0.9, linetype = "dashed")
    }
    
    p <- p + labs(x = xlab, y = ylab, title = "Correlation scatter plot (filtered subset)") +
      theme_bw(base_size = 12)
    
    ggplotly(p, tooltip = "text")
  })
  
  #############################################
  # L) Download filtered table view
  #############################################
  output$download_filtered <- downloadHandler(
    filename = function() paste0("PatchSeq_QC_filtered_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- filtered_df()[, columns_to_display(), drop = FALSE]
      export_df <- df
      colnames(export_df) <- pretty_col_lookup[colnames(df)]
      write.xlsx(export_df, file = file, rowNames = FALSE)
    }
  )
}
