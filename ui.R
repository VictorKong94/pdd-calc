library(shiny)
library(shinythemes)

fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  titlePanel("PDD Calculations"),
  sidebarLayout(
    sidebarPanel(
      
      # Upload -> Prescription Data
      fileInput(inputId = "datafile",
                label = "Select Prescription Data File",
                accept = ".csv"),
      
      conditionalPanel(
        condition = "output.fileUploaded",
        
        # Print -> Please Wait For Large Files
        tags$div(
          HTML("Please Wait For Large Files</br></br>"),
          class = "center-text"
        ),
        
        # Select -> Select ID Column
        selectInput(inputId = "IDcolumn",
                    label = "Select ID Column",
                    choices = NULL),
        
        # Download Button -> Download Processed Data
        downloadButton(outputId = "downloadData",
                       label = "Download Processed Data",
                       class = "dl-button")
      )
      
    ),
    mainPanel(
      conditionalPanel(
        condition = "output.fileUploaded",
        tableOutput(outputId = "table")
      )
    )
  )
)
