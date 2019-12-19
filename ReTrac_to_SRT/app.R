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
  # sidebarLayout(
  #    sidebarPanel(
        actionButton("go", "Run Program"),
        fileInput("file1", "ReTrac Data (as Excel file)",
                  accept = c(
                    ".xls",
                    ".xlsx"      )
        ),
        tags$hr(),
       checkboxInput("header", "Header", TRUE)
      ,
      plotOutput("plot")   
  
  # mainPanel(
       # tableOutput("contents")
   #   )
#    )
#add button
##$head(tags$script(src = "message-handler.js")),
#actionButton("do", "Run Program")
    
    
    )
  
  server <- function(input, output) {
   # output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      
      inFile <- eventReactive(input$go,{ 
      read_excel(input$file1)
    
      
        })
 
      output$plot <- eventReactive(input$go, {
            renderPlot({
        hist(inFile$`Trash (lbs)`()) }) 
        })
        
#observeEvent(input$do, {
  #    session$sendCustomMessage(type = 'testmessage',
 #                               message = 'Thank you for clicking')
 #   })
    
    }

  
  shinyApp(ui, server)
}



