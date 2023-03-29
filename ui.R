library(shinyjs)
library(shiny)
library(shinycssloaders)


markdown_text <- httr::GET("https://raw.githubusercontent.com/SCCWRP/rainfall_flow_calculator_api/master/README.md") |>
  httr::content()



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
        #choices = c("Rainfall Analysis", "Flow Analysis", "Both Rainfall and Flow Analysis")
        choices = c("Rainfall Analysis", "Flow Analysis")
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
      "Templates are provided below for each analysis type, and demo data are available below each template. 
      The datetime column represents the timestamps, and the rain/flow column represents the values that are associated with those timestamps.",
      HTML("<br>"),
      HTML("<b>Please do not make changes to the column names or the tab names.</b>"),
      HTML("
      <ol>
        <li>
              Rainfall Analysis: Rainfall data (either 1-min or time of tips data) should be copy-pasted from the user's datasheet to the downloaded template. Note that the rain gauge values should be placed in the rain column in the template"),
              
                HTML("<ul>"),
                
                HTML("<li>"),
                downloadLink("download_rainfall_template", label = "Download rainfall template"), 
                HTML("</li>"),
                
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
              Flow Analysis: Flow data should be copy-pasted from the user's datasheet to the 
                     downloaded template. There are four flow types: inflow, inflow2, outflow and bypass.
                     A user does not need to submit all four. 
                     Any combination of the flow type is acceptable."), 
                HTML("<br>"),
                HTML("<b> Please note that if the data are not available for any of the tabs, 
                     leave the sheet as it is.</b>"),
                HTML("<ul>"),
                
                HTML("<li>"),
                downloadLink("download_flow_template", label = "Download flow template"), 
                HTML("</li>"),
      
                HTML("<li>"),
                downloadLink("download_demo_flow", label = "Download flow demo data"), 
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
            Each tab must contain exactly two columns, one for the sample timestamps data, and one for the associated values. 
          </li>
          
          <li>
            The column's names and the tab's names must not be changed from the template.
          </li>
          
        </ul>")#,
      # hr(),
      # h3("Technical Details"),
      # HTML("The hydrograph volume is calculated by a trapezoidal approximation, with partitions set at every data point. The aliquot volume values are allocated to each sample timestamp by using the area under the curve between the midpoint of the previous sample and the current sample and the current sample and the next sample. For example if we have samples 1, 2, and 3 at times 10, 14, and 26, then the area under the curve between times 12 and 20 will be allocated to sample 2, since the midpoint between 10 and 14 is 12, and the midpoint between 14 and 26 is 20.")
    ),
    tabPanel(
      "Methods",
      markdown_text |>
        commonmark::markdown_html() |>
        HTML() |>
        withMathJax()
    ),
    id='full_page'
  )
)
