ui <- fluidPage(
  
  # Application title
  titlePanel("Bioretention Rain App"),
  
  # Sidebar with a slider input for number of bins 
  
  fluidRow(
    column(
      2,
      fileInput(
        "file",
        "Choose Excel File",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      actionButton("submit", "Submit"),
      uiOutput("date_column"),
      uiOutput("rain_column")
    ),
    column(
      10,
      plotOutput("cumulative_rain"),
      uiOutput("choose_graph"),
     #actionButton("gen_graph", "Generate Graph"),
      DT::dataTableOutput("stats")
    )
  )
)