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
        "Upload an Excel File",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      selectInput(
        inputId = "flow_unit",
        label = "Choose the flow units of the submitted data",
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
        label = "Start Date", 
        value = NULL, 
        min = NULL, 
        max = NULL
      ),
      timeInput(
        inputId = "start_time_flow", 
        label = 'Start Time'
      ),
      dateInput(
        inputId = "end_date_flow", 
        label = "End Date", 
        value = NULL, min = NULL, max = NULL
      ),
      timeInput(
        inputId = "end_time_flow", 
        label = 'End Time'
      ),
      textInput(
        inputId = "title",
        label = "Graph Title",
        placeholder = "Enter an optional title for the graph(s)",
        value = "",
        width = "100%"
      ),
      shinyjs::disabled(shinyWidgets::actionBttn("submit", "Submit"))
    )
  )
}