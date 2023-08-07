library(shiny)


rainfallUI <- function(id) {
  
  tagList(
    column(
      6,
      align = "left",
      style = "vertical-align: top",
      fileInput(
        "file",
        "Step 1: Upload rainfall data (.xlsx file). The template is provided in the Instruction section.",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      selectInput(
        inputId = "rainfall_resolution",
        label = "Step 2: Choose a rainfall resolution",
        choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
        selected = "0.01 inch"
      ),
      textInput(
        inputId = "title",
        label = "Step 3: Input a title for the graph (optional)",
        placeholder = "Enter an optional title for the graph(s)",
        value = "",
        width = "100%"
      ),
      shiny::textOutput("resubmit_notice"),
      shinyjs::disabled(shinyWidgets::actionBttn("submit", "Submit"))
    ),
    column(
      6
    )
  )
}