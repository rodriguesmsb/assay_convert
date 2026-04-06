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
  roots <- c(home = "~")
  
  # Connect the shinyFiles button to file browsing
  shinyFileChoose(
    input,
    id = "xlsx_file",
    roots = roots,
    filetypes = c("xlsx")
  )
  
  # Return the selected Excel file path
  selected_file <- reactive({
    req(input$xlsx_file)
    
    # Convert shinyFiles output into a regular data frame
    fileinfo <- parseFilePaths(roots, input$xlsx_file)
    req(nrow(fileinfo) > 0)
    
    fileinfo$datapath[1]
  })
  
  # Read all sheet names from the workbook
  sheet_names <- reactive({
    excel_sheets(selected_file())
  })
  
  # Find the sheet ending with "_value"
  value_sheet <- reactive({
    matches <- grep("_value$", sheet_names(), value = TRUE)
    
    validate(
      need(length(matches) == 1,
           paste0("Expected exactly one sheet ending with '_value', but found ", length(matches), "."))
    )
    
    matches[1]
  })
  
  # Find the sheet ending with "_map"
  map_sheet <- reactive({
    matches <- grep("_map$", sheet_names(), value = TRUE)
    
    validate(
      need(length(matches) == 1,
           paste0("Expected exactly one sheet ending with '_map', but found ", length(matches), "."))
    )
    
    matches[1]
  })
  
  # Read the value sheet
  values_data <- reactive({
    read_excel(selected_file(), sheet = value_sheet())
  })
  
  # Read the map sheet
  map_data <- reactive({
    read_excel(selected_file(), sheet = map_sheet())
  })
  
  # Create the final long data
  long_data <- reactive({
    
    # Basic safety check to make sure both sheets have same shape
    validate(
      need(ncol(values_data()) == ncol(map_data()),
           "The value and map sheets do not have the same number of columns."),
      need(nrow(values_data()) == nrow(map_data()),
           "The value and map sheets do not have the same number of rows.")
    )
    
    create_elisa_long(
      values_df = values_data(),
      map_df = map_data(),
      file_path = selected_file(),
      assay_name = input$assay_name
    )
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
          x = dilution_num,
          y = value,
          color = sample,
          group = sample,
          text = paste0(
            "Sample: ", sample,
            "<br>Timepoint: ", timepoint,
            "<br>Dilution: ", dilution,
            "<br>Value: ", round(value, 3),
            "<br>Assay: ", assay,
            "<br>File: ", file_name
          )
        )
      ) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) +
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
    selected_file()
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

