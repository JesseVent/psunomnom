# app.R
library(shiny)

anonymise_df <- function(df) {
  classes <- sapply(df, function(x) class(x)[1])
  
  # Column name prefixes by type
  col_prefix <- function(cls) {
    if (cls %in% c("character", "factor")) "CHT"
    else if (cls %in% c("numeric", "integer")) "CHN"
    else if (cls %in% c("Date", "POSIXct", "POSIXt")) "CHD"
    else if (cls %in% c("logical")) "CHL"
    else "CHU"
  }
  
  # Value prefixes by type
  val_prefix <- function(cls) {
    if (cls %in% c("character", "factor")) "T"
    else if (cls %in% c("numeric", "integer")) "N"
    else if (cls %in% c("Date", "POSIXct", "POSIXt")) "D"
    else if (cls %in% c("logical")) "L"
    else "V"
  }
  
  # Generate anonymised column names
  counts <- integer(0)
  get_idx <- function(pref) {
    if (is.na(counts[pref])) counts[pref] <<- 0L
    counts[pref] <<- counts[pref] + 1L
    counts[pref]
  }
  
  new_names <- character(ncol(df))
  for (j in seq_along(df)) {
    cls  <- classes[j]
    pref <- col_prefix(cls)
    idx  <- get_idx(pref)
    new_names[j] <- paste0(pref, idx)
  }
  
  name_map <- data.frame(
    original_name = names(df),
    pseudo_name   = new_names,
    class         = classes,
    stringsAsFactors = FALSE
  )
  
  # Anonymise values, per column, preserving equality (referential integrity)
  df_anon <- df
  value_map_rows <- list()
  
  for (j in seq_along(df)) {
    col_name_orig <- names(df)[j]
    col_name_new  <- new_names[j]
    cls           <- classes[j]
    pref          <- val_prefix(cls)
    
    x <- df[[j]]
    uniq_vals <- unique(x)
    non_na    <- uniq_vals[!is.na(uniq_vals)]
    
    # Codes like T1, T2, N1, ...
    codes <- paste0(pref, seq_along(non_na))
    map   <- setNames(codes, as.character(non_na))
    
    x_chr <- as.character(x)
    is_na <- is.na(x)
    x_new <- map[x_chr]
    x_new[is_na] <- NA
    
    # Store anonymised as characters (good for LLMs)
    df_anon[[j]] <- x_new
    
    if (length(non_na)) {
      value_map_rows[[length(value_map_rows) + 1L]] <-
        data.frame(
          original_column = col_name_orig,
          pseudo_column   = col_name_new,
          class           = cls,
          original_value  = as.character(non_na),
          pseudo_value    = codes,
          stringsAsFactors = FALSE
        )
    }
  }
  
  value_map_df <- if (length(value_map_rows)) {
    do.call(rbind, value_map_rows)
  } else {
    data.frame(
      original_column = character(0),
      pseudo_column   = character(0),
      class           = character(0),
      original_value  = character(0),
      pseudo_value    = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  names(df_anon) <- new_names
  
  list(
    anon_df     = df_anon,
    name_map    = name_map,
    value_map   = value_map_df
  )
}

ui <- fluidPage(
  titlePanel("CSV Anonymiser (Structure-Preserving)"),
  sidebarLayout(
    sidebarPanel(
      tags$p("Paste your CSV (including header row)."),
      tags$small("Default separator: comma. Encoding: UTF-8."),
      textareaInput(
        "csv_text", NULL,
        value = "",
        rows = 15,
        width = "100%",
        placeholder = "clientid,name,amount,category\n1,Jesse,100.00,developer\n2,Anton,200.00,developer\n3,Aaron,100.00,analyst"
      ),
      checkboxInput("header", "Header row present", TRUE),
      actionButton("do_anonymise", "Anonymise"),
      tags$hr(),
      downloadButton("download_anon", "Download anonymised CSV"),
      br(), br(),
      downloadButton("download_mapping", "Download reverse index")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Anonymised data",
          tags$br(),
          tableOutput("anon_table")
        ),
        tabPanel(
          "Column mapping",
          tags$br(),
          tableOutput("name_map_table")
        ),
        tabPanel(
          "Value mapping (reverse index)",
          tags$br(),
          tableOutput("value_map_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  results <- reactiveVal(NULL)
  
  observeEvent(input$do_anonymise, {
    txt <- input$csv_text
    if (!nzchar(txt)) {
      showNotification("Please paste CSV text first.", type = "error")
      return()
    }
    
    df <- tryCatch({
      con <- textConnection(txt)
      on.exit(close(con), add = TRUE)
      read.csv(
        con,
        header = isTRUE(input$header),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }, error = function(e) {
      showNotification(
        paste("Error parsing CSV:", e$message),
        type = "error",
        duration = 5
      )
      NULL
    })
    
    if (is.null(df)) return()
    
    res <- anonymise_df(df)
    results(res)
  })
  
  output$anon_table <- renderTable({
    res <- results()
    req(res)
    head(res$anon_df, 50)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$name_map_table <- renderTable({
    res <- results()
    req(res)
    res$name_map
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$value_map_table <- renderTable({
    res <- results()
    req(res)
    head(res$value_map, 100)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$download_anon <- downloadHandler(
    filename = function() {
      paste0("anonymised_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- results()
      req(res)
      write.csv(res$anon_df, file, row.names = FALSE, quote = TRUE)
    }
  )
  
  output$download_mapping <- downloadHandler(
    filename = function() {
      paste0("reverse_index_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- results()
      req(res)
      # Combine column name mapping and value mapping for convenience
      nm <- res$name_map
      vm <- res$value_map
      # Write two CSVs concatenated with markers (simple, text-only)
      con <- file(file, open = "wt")
      on.exit(close(con), add = TRUE)
      writeLines("# Column name mapping", con)
      utils::write.table(nm, con, sep = ",", row.names = FALSE, col.names = TRUE)
      writeLines("", con)
      writeLines("# Value mapping", con)
      utils::write.table(vm, con, sep = ",", row.names = FALSE, col.names = TRUE)
    }
  )
}

shinyApp(ui, server)
