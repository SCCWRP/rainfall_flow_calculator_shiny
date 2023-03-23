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
              Rainfall Analysis: When a user uploads rainfall data, the application provides a summary statistics of rainfall, and a plot of cumulative rainfall"),
                HTML("<ul>"),
                HTML("<li>"),
                downloadLink("download_demo_1min", label = "Download 1-min demo data"), 
                HTML("</li>"),
                HTML("<li>"),
                downloadLink("download_demo_tt", label = "Download time of tips demo data"), 
                HTML("</li>"),
                HTML("</ul>"),
                HTML("
        </li>
        <li>
              Flow Analysis: When a user uploads flow data, the application provides a summary statistics of flow"),
                HTML("<ul>"),
                HTML("<li>"),
                downloadLink("download_demo_flow", label = "Download flow demo data"), 
                HTML("</li>"),
                HTML("</ul>"),
                HTML("     
        </li>
        
        <li>
              Both Rainfall and Flow Analysis: When a user uploads both rainfall and flow data, the application provides the summary statistics of both rainfall and flow"),
                HTML("<ul>"),
                HTML("<li>"),
                downloadLink("download_demo_rainfall_flow", label = "Download both rainfall and flow data in one file"), 
                HTML("</li>"),
                HTML("</ul>"),
                HTML("         
        </li>
        </li>
      </ol>
         "),
      hr(),
      h3("Data Requirements"),
      HTML("The uploaded Excel spreadsheet must conform to the following requirements:
        <ul>
          <li>
            It must contain exactly two sheets, one for the flow rate measurement data, and one for the sample timestamps/pollutant measurement data.
          </li>
          <li>
            The first sheet must have exactly two columns, one for the timestamps and one for the flow rate measurements.
          </li>
          <li>
            The first column of each sheet must be timestamps with both date and time in the 'mm/dd/yy  hh:mm:ss' format. The 'Datetime' columns in the provided template file are already in the correct format.
          </li>
          <li>
            Any number of pollutant columns in the second sheet are supported. If you do not have pollutant data, delete the 'Pollutant' columns entirely before uploading the template.
          </li>
          <li>
            The column headers are required and can be renamed as needed, but cannot be exclusively numeric characters [0-9]. The flow rate and pollutant column headers will be used for axis titles and can contain the units of the measurements, for example.
          </li>
          <li>
            All flow rate and pollutant measurements must be greater than zero.
          </li>
          <li>
            There may not be any missing values in the spreadsheet.
          </li>
        </ul>")#,
      # hr(),
      # h3("Technical Details"),
      # HTML("The hydrograph volume is calculated by a trapezoidal approximation, with partitions set at every data point. The aliquot volume values are allocated to each sample timestamp by using the area under the curve between the midpoint of the previous sample and the current sample and the current sample and the next sample. For example if we have samples 1, 2, and 3 at times 10, 14, and 26, then the area under the curve between times 12 and 20 will be allocated to sample 2, since the midpoint between 10 and 14 is 12, and the midpoint between 14 and 26 is 20.")
    ),
    id='full_page'
  )
)
  
