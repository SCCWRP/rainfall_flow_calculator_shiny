library(shinyjs)


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
    column(
      1,
      align = "left",
      fileInput(
        "file",
        "Choose Excel File",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      actionButton("submit", "Submit")
    ),
    column(
      2,
      align = "left",
      selectInput(inputId = "rainfall_resolution",
                  label = "Choose your rainfall resolution:",
                  choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
                  selected = "0.01 inch"),
      uiOutput("date_column"),
      uiOutput("rain_column")
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
        # h3("Data Requirements"),
        # HTML("The uploaded Excel spreadsheet must conform to the following requirements:
        # <ul>
        #   <li>
        #     It must contain exactly two sheets, one for the flow rate measurement data, and one for the sample timestamps/pollutant measurement data.
        #   </li>
        #   <li>
        #     The first sheet must have exactly two columns, one for the timestamps and one for the flow rate measurements.
        #   </li>
        #   <li>
        #     The first column of each sheet must be timestamps with both date and time in the 'mm/dd/yy  hh:mm:ss' format. The 'Datetime' columns in the provided template file are already in the correct format.
        #   </li>
        #   <li>
        #     Any number of pollutant columns in the second sheet are supported. If you do not have pollutant data, delete the 'Pollutant' columns entirely before uploading the template.
        #   </li>
        #   <li>
        #     The column headers are required and can be renamed as needed, but cannot be exclusively numeric characters [0-9]. The flow rate and pollutant column headers will be used for axis titles and can contain the units of the measurements, for example.
        #   </li>
        #   <li>
        #     All flow rate and pollutant measurements must be greater than zero.
        #   </li>
        #   <li>
        #     There may not be any missing values in the spreadsheet.
        #   </li>
        # </ul>")
        #,
        # hr(),
        # h3("Technical Details"),
        # HTML("The hydrograph volume is calculated by a trapezoidal approximation, with partitions set at every data point. The aliquot volume values are allocated to each sample timestamp by using the area under the curve between the midpoint of the previous sample and the current sample and the current sample and the next sample. For example if we have samples 1, 2, and 3 at times 10, 14, and 26, then the area under the curve between times 12 and 20 will be allocated to sample 2, since the midpoint between 10 and 14 is 12, and the midpoint between 14 and 26 is 20.")
      ),
      id='full_page'
    )
      # tabPanel("Rainfall Analysis",
      #   fluidRow(
      #     column(
      #       2,
      #       br(),
      #       HTML(
      #         "<div>
      #             <p><b>Instruction: </b></p>
      #               <ul>
      #                 <li>Upload your rainfall data (either 1-min data or the time of tips) <b>as an Excel file (.xlsx)</b></li>
      #                 <li>Choose the names of the columns containing timestamp and rainfall values</li>
      #                 <li>Click submit</li>
      #               </ul>
      #             <p> You can also download demo data below </p>
      #         </div>"
      #       ),
      #       downloadButton("download_demo_1min", "Download Demo 1-min Data"),
      #       downloadButton("download_demo_tt", "Download Demo Time-of-tips Data"),
      #     ),
      # 
      #   )
      # ),
      # tabPanel("Flow Analysis - Under Development",
      #    fluidRow(
      #     column(
      #        2,
      #        br(),
      #        HTML(
      #          "<div>
      #               <p><b>Instruction: </b></p>
      #                 <ul>
      #                   <li>Upload your flow data <b>as an Excel file (.xlsx)</b></li>
      #                   <li>Choose the names of the columns containing timestamp and flow rate values</li>
      #                   <li>Click submit</li>
      #                 </ul>
      #               <p> You can also download demo data below </p>
      #           </div>"
      #        ),
      #        downloadButton("download_demo_flow", "Download Demo FlowData")
      #     )
      #   )
      # )
    )
  
