#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(readxl)

if (interactive()) {
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "ReTrac Data (as Excel file)",
                  accept = c(
                    ".xls",
                    ".xlsx"      )
        ),
        tags$hr(),
       checkboxInput("header", "Header", TRUE)
      ),
      mainPanel(
        tableOutput("contents")
      )
    )
  )
  
  server <- function(input, output) {
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read_excel(inFile$datapath)
    })
  }
  
  shinyApp(ui, server)
}



