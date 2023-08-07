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
        "Step 1: Upload flow data (.xlsx file). The template is provided in the Instruction section.",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      selectInput(
        inputId = "flow_unit",
        label = "Step 2: Choose a flow unit of the submitted data",
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
      shiny::textOutput("startdatetime-notice"),
      tagAppendAttributes(
        dateInput(
          inputId = "start_date_flow", 
          label = "Step 3: Choose a start date", 
          value = NULL, 
          min = NULL, 
          max = NULL
        ),
        onkeydown ='return false'
      ),
      timeInput(
        inputId = "start_time_flow", 
        label = 'Step 4: Choose a start time'
      ),
      tagAppendAttributes(
        dateInput(
          inputId = "end_date_flow", 
          label = "Step 5: Choose an end date", 
          value = NULL, min = NULL, max = NULL
        ),
        onkeydown ='return false'
      ),
      timeInput(
        inputId = "end_time_flow", 
        label = 'Step 6: Choose an end time'
      ),
      textInput(
        inputId = "title",
        label = "Step 7: Input a title for the graph (optional)",
        placeholder = "Enter an optional title for the graph(s)",
        value = "",
        width = "100%"
      ),
      shinyjs::disabled(shinyWidgets::actionBttn("submit", "Submit"))
    )
  )
}