library(shinyjs)
library(shiny)
library(shinycssloaders)
ui <- fluidPage(
  useShinyjs(),
  
  
  # Application title
  titlePanel("Rainfall and Flow Analysis"),

  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(
      1,
      div()
    ),
    column(2,
    radioButtons(
      "analysistype", 
      "Choose an option:",
      choices = c("Rainfall Analysis", "Flow Analysis", "Both Rainfall and Flow Analysis")
    )),
    column(7,
           uiOutput("test")
           
           )
   
  ),
    
  tabsetPanel(
    tabPanel(
      "Instructions",
      h3("Using this Application"),
      "This application will produce a table of summarization of the rain events, and a plot of cumulative rainfall versus elapsed time from the first rain tip.",
      HTML("
      <ol>
         <li>
          Upload the rainfall data by clicking the 'Browse' button, and clicking the 'Submit' button. The application will generate the rain event table as well as the plot of cumulative rainfall versus elapsed hours.
        </li>
        <li>
          Either 1-min data or time-of-tips data can be used to analyze the rain events
        </li>
        <li>
          Download the demo data by clicking ",), downloadLink("download_demo_1min", label = "here for 1-min data"), HTML("or",), downloadLink("download_demo_tt", label = "here for time of tips data"), HTML("
        </li>
      </ol>
         "),
      hr()
    ),
    id='full_page'
  )
)
  
