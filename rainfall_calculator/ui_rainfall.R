library(shiny)


rainfallUI <- function(id) {

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
        inputId = "rainfall_resolution",
        label = "Step 2: Choose the rainfall resolution",
        choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
        selected = "0.01 inch"
      ),
      shinyjs::disabled(shinyWidgets::actionBttn("submit", "Submit"))
    ),
    column(
      6
    )
  )
}