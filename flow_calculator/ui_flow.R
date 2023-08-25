library(shiny)
library(shinyTime)

flowUI <- function(id) {
  
  tagList(
    column(
      6,
      align = "left",
      style = "vertical-align: top",
      HTML("<strong>Step 1: Upload flow data</strong>"),
      column(
        12,
        "The template is provided in the Instructions section below.",
        fileInput(
          "file",
          "Choose Excel File",
          multiple = FALSE,
          accept = ".xlsx"
        )
      ),
      br(),
      HTML("<strong>Step 2: Indicate units of flow measurement</strong>"),
      column(
        12,
        selectInput(
          inputId = "flow_unit",
          label = "",
          choices = c(
            "L/s" = "L/s",
            "gal/min" = "gal/min",
            "ft3/s (cfs)" = "ft3/s"
          ),
          selected = "L/s"
        )
      ),
    ),
    column(
      6,
      align = "left",
      HTML("<strong>Step 3 (Optional): Select Input Filter Parameters</strong>"),
      column(
        12,
        tagAppendAttributes(
          dateInput(
            inputId = "start_date_flow", 
            label = "Start Date", 
            value = NULL, 
            min = NULL, 
            max = NULL
          ),
          onkeydown ='return false'
        ),
        timeInput(
          inputId = "start_time_flow", 
          label = 'Start Time'
        ),
        tagAppendAttributes(
          dateInput(
            inputId = "end_date_flow", 
            label = "End Date", 
            value = NULL, min = NULL, max = NULL
          ),
          onkeydown ='return false'
        ),
        timeInput(
          inputId = "end_time_flow", 
          label = 'End Time'
        ),
        textInput(
          inputId = "title",
          label = "Graph Title",
          placeholder = "Enter an optional title for the graph",
          value = "",
          width = "100%"
        ),
        shinyjs::disabled(shinyWidgets::actionBttn("submit", "Submit"))
      )
    )
  )
}