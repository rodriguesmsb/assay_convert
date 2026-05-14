library(shiny)
library(shinyFiles)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(writexl)
source("R/functions.R")

server <- function(input, output, session) {
  
  # Define which folders the app is allowed to browse
  roots <- c(home = "//win.ad.jhu.edu/Users$/HOME/")

  # Allow Shiny to browse for Excel files directly
  shinyFileChoose(
    input,
    id = "xlsx_files",
    roots = roots,
    filetypes = c("xlsx")
    )

  # Return only the files explicitly selected by the user
  selected_files <- reactive({

    # Wait until at least one file is selected
    req(input$xlsx_files)

    # Convert shinyFiles output into a regular data frame
    file_info <- parseFilePaths(roots, input$xlsx_files)

    # Make sure at least one file was selected
    validate(
      need(nrow(file_info) > 0, "Please select at least one .xlsx file.")
      )
    
    # Keep only real .xlsx files and skip Excel lock files
    files <- file_info$datapath
    files <- files[grepl("\\.xlsx$", files, ignore.case = TRUE)]
    files <- files[!grepl("^~\\$", basename(files))]
    validate(
      need(length(files) > 0, "No valid .xlsx files were selected.")
      )
    files
    })

  
  
  # loop trough all files and process them
  long_data <- reactive({
    req(xlsx_files())
    
    failures <- character(0)
    results <- lapply(xlsx_files(), function(f) {
      tryCatch(
        process_elisa_file(f),
        error = function(e) {
          failures <<- c(failures, paste0(basename(f), ": ", conditionMessage(e)))
          NULL
        }
      )
    })
    
    if (length(failures) > 0) {
      showNotification(paste("Skipped:", paste(failures, collapse = "; ")),
                       type = "warning", duration = 10)
    }
    
    results <- results[!vapply(results, is.null, logical(1))]
    validate(need(length(results) > 0, "No files could be processed."))
    dplyr::bind_rows(results)
  })
  
  #compute AUC for each sample
  auc_data <- reactive({
    req(long_data())
    create_auc_df(long_data())
  })
  
  #add a reactive function for plot
  
  # Create a ggplot object that will be used for both Plotly and export
  curve_plot <- reactive({
    req(long_data())
    long_data() %>%
      mutate(
        dilution_num = readr::parse_number(as.character(dilution)),
        value = as.numeric(value)
      ) %>%
      arrange(sample, dilution_num) %>%
      ggplot(
        aes(
          x = log10(dilution_num), # plot dilution on log10 scale
          y = value,
          color = sample,
          group = sample,
          text = paste0(
            "Sample: ", sample,
            "<br>Dilution: ", dilution,
            "<br>Value: ", round(value, 3),
            "<br>Assay: ", assay,
            "<br>File: ", file_name
          )
        )
      ) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) +
      facet_wrap(~ file_name, scales = "free_y") +
      labs(
        title = "ELISA values by dilution",
        x = "Dilution",
        y = "Value",
        color = "Sample"
      ) +
      theme_minimal()
  })
  
  # Render plot
  output$value_plot <- renderPlotly({
    req(curve_plot())
    plotly::ggplotly(curve_plot())
  })
  
  # Show selected path
  output$selected_path <- renderText({
    req(selected_dir())
    paste0(selected_dir(), "  (", length(xlsx_files()), " file(s) found)")
  })
  
  # Preview the final long data
  output$preview_long <- renderTable({
    head(long_data(), 15)
  })
  
  #Preview AUC result
  output$preview_auc <- renderTable({
    head(auc_data(), 15)
  })
  
  
  # Export the processed data and figure as a zip file
  output$download_results <- downloadHandler(
    
    # Name of the downloaded file
    filename = function() {
      
      # Protect against missing or blank file names
      export_name <- input$export_name
      
      if (is.null(export_name)) {
        export_name <- "elisa_results"
      }
      
      export_name <- gsub("[^A-Za-z0-9_-]", "_", trimws(export_name))
      
      if (!nzchar(export_name)) {
        export_name <- "elisa_results"
      }
      
      paste0(export_name, ".zip")
    },
    
    # Tell the browser this is a zip file
    contentType = "application/zip",
    
    # Build the zip file
    content = function(file) {
      
      # Make sure required objects exist
      req(long_data(), auc_data(), curve_plot())
      
      # Protect against missing or blank file names
      export_name <- input$export_name
      
      if (is.null(export_name)) {
        export_name <- "elisa_results"
      }
      
      export_name <- gsub("[^A-Za-z0-9_-]", "_", trimws(export_name))
      
      if (!nzchar(export_name)) {
        export_name <- "elisa_results"
      }
      
      # Create a temporary folder to store files before zipping
      export_dir <- tempfile("export_")
      dir.create(export_dir, recursive = TRUE)
      
      # Define output file names
      xlsx_name <- paste0(export_name, ".xlsx")
      png_name  <- paste0(export_name, ".png")
      
      xlsx_path <- file.path(export_dir, xlsx_name)
      png_path  <- file.path(export_dir, png_name)
      
      # Write the Excel workbook with two sheets
      writexl::write_xlsx(
        x = list(
          long_data = long_data(),
          auc_data  = auc_data()
        ),
        path = xlsx_path
      )
      
      # Save the plot as PNG
      ggplot2::ggsave(
        filename = png_path,
        plot = curve_plot(),
        width = 10,
        height = 6,
        dpi = 300
      )
      
      # Create the zip archive
      zip::zipr(
        zipfile = file,
        files = c(xlsx_name, png_name),
        root = export_dir
      )
    }
  )
  
}

