library(shinyjs)


ui <- fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("Rainfall and Flow Analysis"),
  
  # Sidebar with a slider input for number of bins 
  
  mainPanel(
    tabsetPanel(
      tabPanel("Rainfall Analysis",
        fluidRow(
          column(
            2,
            br(),
            HTML(
              "<div>
                  <p><b>Instruction: </b></p>
                    <ul>
                      <li>Upload your rainfall data (either 1-min data or the time of tips)</li>
                      <li>Choose the names of the columns containing timestamp and rainfall values</li>
                      <li>Click submit</li>
                    </ul>
                  <p> You can also download demo data below </p>
              </div>"
            ),
            downloadButton("download_demo_1min", "Download Demo 1-min Data"),
            downloadButton("download_demo_tt", "Download Demo Time-of-tips Data"),
            fileInput(
              "file",
              "Choose Excel File",
              multiple = FALSE,
              accept = ".xlsx"
            ),
            selectInput(inputId = "rainfall_resolution",
                        label = "Choose your rainfall resolution:",
                        choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
                        selected = "0.01 inch"),
            uiOutput("date_column"),
            uiOutput("rain_column"),
            actionButton("submit", "Submit"),
      
            
          ),
          column(
            10,
            shinycssloaders::withSpinner(
              tagList(
                plotOutput("cumulative_rain"),
                downloadButton("download_plot", "Download Plot"),
                uiOutput("choose_graph"),
                DT::dataTableOutput("stats"),
                downloadButton("download_summary", "Download Table")
              )
            )
          )
        )
      ),
      tabPanel("Flow Analysis",
               # Content for the second tab goes here
               h1("Coming soon")
      )
    )
  )
)