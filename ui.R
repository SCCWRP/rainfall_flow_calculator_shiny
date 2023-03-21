library(shinyjs)
library(shiny)
library(shinycssloaders)
ui <- fluidPage(
  useShinyjs(),
  
  
  # Application title
  titlePanel("BMP Hydrology"),

  
  fluidRow(
    column(
      1,
      div()
    ),
    column(
      2,
      radioButtons(
        "analysistype", 
        "Choose an option:",
        choices = c("Rainfall Analysis", "Flow Analysis", "Both Rainfall and Flow Analysis")
      )
    ),
    column(
      7,
      uiOutput("user_input")
    )
  ),
    
  tabsetPanel(
    tabPanel(
      "Instructions",
      h3("Using this Application"),
      "This application allows user to choose between 3 different analysis types.",
      HTML("
      <ol>
        <li>
              Rainfall Analysis: When a user uploads rainfall data, the application provides a summary statistics of rainfall, and the plot of cumulative rainfall
        </li>
        
        <li>
              Flow Analysis: When a user uploads flow data, the application provides a summary statistics of flow
        </li>
        
        <li>
              Both Rainfall and Flow Analysis: When a user uploads both rainfall and flow data, the application provides the summary statistics of both rainfall and flow
        </li>
        
        </li>
      
          
      </ol>
         "),
      HTML("Download the demo data by clicking ",), downloadLink("download_demo_1min", label = "here for 1-min data"), HTML("or",), downloadLink("download_demo_tt", label = "here for time of tips data"), HTML(""),
      hr()
    ),
    id='full_page'
  )
)
  
