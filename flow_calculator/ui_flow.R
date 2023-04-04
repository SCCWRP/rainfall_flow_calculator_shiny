library(shiny)


flowUI <- function(id) {
  
  tagList(
    column(
      4,
      align = "left",
      br(),
      fileInput(
        "file",
        "Step 1: Upload an Excel File",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      selectInput(
        inputId = "flow_unit",
        label = "Step 2: Choose the flow units",
        choices = c("L/s","g/m","ft3/s"),
        selected = "L/s"
      ),
    ),
    column(
      4,
      align = "left",
      
      dateInput(
        inputId = "start_date_flow", 
        label = "Step 2: Input the start of the hydrograph", 
        value = NULL, 
        min = NULL, 
        max = NULL
      ),
      timeInput(
        inputId = "start_time_flow", 
        label = 'Input the start time'
      ),
      dateInput(
        inputId = "end_date_flow", 
        label = "Step 3: Input the end of the hydrograph", 
        value = NULL, min = NULL, max = NULL
      ),
      timeInput(
        inputId = "end_time_flow", 
        label = 'Input the end time'
      )
    ),
    column(
      4,
      br(),
      br(),
      br(),
      br(),
      align = "left",
      shinyWidgets::actionBttn("submit", "Submit", width = '200px')
    )
  )
}