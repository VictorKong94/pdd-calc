library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("PDD Calculations"),
  sidebarLayout(
    sidebarPanel(
      
      # Upload -> Prescription Data
      fileInput(inputId = "datafile",
                label = "Select Prescription Data File",
                accept = ".csv"),
      
      conditionalPanel(
        condition = "output.fileUploaded",
        
        # Download Button -> Download Processed Data
        downloadButton(outputId = "downloadData",
                       label = "Download")
      )
      
    ),
    mainPanel(
      conditionalPanel(
        condition = "output.fileUploaded",
        tableOutput(outputId = "table")
      )
    )
  )
))