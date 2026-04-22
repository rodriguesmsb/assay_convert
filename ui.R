# Load required packages
library(shiny)
library(bslib)
library(shinyFiles)
library(plotly)


# Define UI for the app
ui <- page_sidebar(
  
  # Apply a modern Bootstrap theme
  theme = bs_theme(
    version = 5,
    bootswatch = "slate"
  ),
  
  # Define the sidebar content
  sidebar = sidebar(
    
    # Let the user browse for the Excel workbook
    shinyFilesButton(
      id = "xlsx_file",
      label = "Select Excel file",
      title = "Choose an .xlsx file",
      multiple = FALSE
    ),
    
    
    # Let the user choose the export file name
    textInput(
      inputId = "export_name",
      label = "Export file name",
      value = "elisa_results"
    ),
    
    # Download button for the exported zip file
    downloadButton(
      outputId = "download_results",
      label = "Export results"
    )
  ),
  
  # App title shown at the top of the page
  title = "Lab Results Processor",
  

  #create a first row with a single card that spans the full width of the page
  layout_columns(
    
    # This card spans the full row and shows the plot
    card(
      full_screen = TRUE,
      card_header("Assay plot"),
      plotlyOutput("value_plot", height = "600px")
    ),
    
    # Full-width row
    col_widths = c(12)
    
    
  ),
  
  # create a second row with two cards side by side, each taking up half the width
  layout_columns(
    
    #display long dat on left card
    card(
      full_screen = TRUE,
      card_header("Preview of processed long data"),
      tableOutput("preview_long")
    ),
    
    
    #displaty AUC calculations on right card
    card(
      full_screen = TRUE,
      card_header("Preview AUC calculations"),
      tableOutput("preview_auc")
    ),
    
    # Split into two equal columns
    col_widths = c(6, 6)
  )
)