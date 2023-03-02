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
      actionButton("submit", "Submit")
    ),
    column(
      10,
      plotOutput("cumulative_rain"),
      DT::dataTableOutput("stats")
    )
  )
)