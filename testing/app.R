library(readxl)
library(shiny)
library(readxl)
library(lubridate)
library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)
library(safejoin)
library(stringdist)
library(fuzzyjoin)
#devtools::install_github("hansthompson/rusps")
library(rusps)
library(XML)

'%!in%' <- function(x,y)!('%in%'(x,y))  #I always like to create my own "opposite of %in%" for nice logicals.

# using the jaro-winkler string distance, must have an address vector called "Address" 
addJoin <- function(x,b3,threshold){
  joined <- 
    stringdist_left_join(x,
                         b3, 
                         by = "Address",
                         distance_col = "distance",
                         max_dist = threshold,
                         method = "jw"
    )
}

add_clean<-function(char){
  lng<-c("us","-","avenue","street","highway","lane","drive","alley","boulevard","circle","court","county","north","west","east","south","road","second","first","third",".")
  shrt<-c("hwy"," ","ave","st","hwy","ln","dr","aly","blvd","cir","ct","co","n","w","e","s","rd","2nd","1st","3rd","")
  char %<>% stri_trans_tolower()    
  for (i in 1:length(lng)){
    char<-stri_replace_all_fixed(char,lng[i],shrt[i]) }
  char
}



runApp(list(
  ui = shinyUI(pageWithSidebar(
    headerPanel('ReTrac to SRT App'),
    sidebarPanel( 
      fileInput('file1', 'Choose file to upload',
                accept = c('.xlsx',
                           '.xls')),
      uiOutput('buttonsUI'), br(),
      uiOutput('downloadUI')
    ),
    mainPanel(
      tableOutput('readytable')
    )
  )), 
  server = shinyServer(function(input, output) {
    # variables to control the sequence of processes 
    controlVar <- reactiveValues(fileReady = FALSE, tableReady = FALSE)
    # to keep the data upload
    dat <- NULL
    # handle the file reading
    observeEvent(input$file1, {
      controlVar$fileReady <- FALSE
      if (is.null(input$file1))
        return()
      inFile <- input$file1
 #Here we read the file 
      dat <<- read_excel(inFile$datapath)
      if(!is.data.frame(dat))
        return()
      controlVar$fileReady <- TRUE
      
      })
    # show buttons only when file is uploaded
    output$buttonsUI <- renderUI({
      if (controlVar$fileReady)
        div(
          dateInput('date','Select when the file was uploaded',
                    value = NULL,
                    format = 'yyyy-mm-dd'),
          textInput('text1','What is your email?'),
          textInput('text2','What is your name?'),
          actionButton('go','go')
          
        )
    })
    # show a download button only if data is ready
    output$downloadUI <- renderUI({
      if (controlVar$tableReady)
        downloadButton('downloadData', 'Download')
    })
    # add columns to dat and run some script on it
    observeEvent(input$go, {
      controlVar$tableReady <- FALSE
      if (!is.null(input$text1))
        dat$`Email:` <<- input$text1
      if (!is.null(input$text2))
        dat$`Contact name:`<<- input$text2
################################################## Script Goes here  #############################################
      
      
      
      
      
      
##################################################################################################################
      controlVar$tableReady <- TRUE  
    })
    # render table after uploading file or running the script
    output$readytable <- renderTable({
      input$go
      if (controlVar$fileReady || controlVar$tableReady)
        dat
    })
    # handle the download button
    output$downloadData <- downloadHandler(
      filename = function() { 'newData.xlsx' },
      content = function(file) {
        openxlsx::write.xlsx(dat,file)
      }
    )
  })
))