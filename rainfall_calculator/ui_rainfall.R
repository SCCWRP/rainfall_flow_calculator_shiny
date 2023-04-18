library(shiny)


rainfallUI <- function(id) {

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
        inputId = "rainfall_resolution",
        label = "Step 2: Choose the rainfall resolution",
        choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
        selected = "0.01 inch"
      ),
    ),
    column(
      4,
      br(),
      br(),
      br(),
      br(),
      align = "left",
      shinyjs::disabled(shinyWidgets::actionBttn("submit", "Submit", width = '200px'))
    )
  )
}