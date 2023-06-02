library(shiny)
library(shinyTime)

flowUI <- function(id) {
  
  tagList(
    column(
      6,
      align = "left",
      style = "vertical-align: top",
      fileInput(
        "file",
        "Step 1: Upload an Excel File",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      selectInput(
        inputId = "flow_unit",
        label = "Step 2: Choose the flow units of the submitted data",
        choices = c(
          "L/s" = "L/s",
          "g/min (gpm)" = "g/m",
          "ft3/s (cfs)" = "ft3/s"
        ),
        selected = "L/s"
      ),
    ),
    column(
      6,
      align = "left",
      dateInput(
        inputId = "start_date_flow", 
        label = "Step 3: Input the start of the hydrograph", 
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
        label = "Step 4: Input the end of the hydrograph", 
        value = NULL, min = NULL, max = NULL
      ),
      timeInput(
        inputId = "end_time_flow", 
        label = 'Input the end time'
      ),
      shinyjs::disabled(shinyWidgets::actionBttn("submit", "Submit"))
    )
  )
}