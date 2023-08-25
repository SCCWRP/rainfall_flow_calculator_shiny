library(shiny)


rainfallUI <- function(id) {
  shinyjs::useShinyjs()
  tagList(
    column(
      6,
      align = "left",
      style = "vertical-align: top",
      HTML("<strong>Step 1: Upload rainfall data</strong>"),
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
      HTML("<strong>Step 2: Indicate units of rainfall measurement</strong>"),
      column(
        12,
        selectInput(
          inputId = "rainfall_unit",
          label = "",
          choices = c("in", "mm"),
          selected = "in"
        )
      )
    ),
    column(
      6,
      HTML("<strong>Step 3 (Optional): Input a title for the graph</strong>"),
      column(
        12,
        htmltools::tagQuery(
          textInput(
            inputId = "title",
            label = "Graph Title",
            placeholder = "Enter an optional title for the graph",
            value = "",
            width = "100%"
          )
        )$children("input")$addAttrs(maxlength=40)$allTags(),
        shiny::textOutput("resubmit_notice"),
        shinyjs::disabled(shinyWidgets::actionBttn("submit", "Submit"))
      )
    )
  )
}