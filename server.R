library(ggplot2)

server <- function(input, output) {
  
  data_input <- reactive({
    rain <- readxl::read_excel(input$file$datapath)
    rain <- rain |>
      dplyr::select(c(input$date_column, input$rain_column))
    names(rain) <- c("datetime", "rain")
    rain <- rain |>
      dplyr::arrange(datetime)
    rain_json <- jsonlite::toJSON(rain, dataframe = "columns", POSIXt = "ISO8601")
    rain_json
  })
  
  output$date_column <- renderUI({
    selectInput(inputId = "date_column", label = "Date Column", choices = "")
  })
  
  output$rain_column <- renderUI({
    selectInput(inputId = "rain_column", label = "Rain Depth Column", choices = "")
  })
  
  output$choose_graph <- renderUI({
    selectInput(inputId = "choose_graph", label = "Choose Rain Event", choices = "")
  }) |>
    bindEvent(statistics())
  
  observe({
    updateSelectInput(inputId = "date_column", choices = readxl::read_excel(input$file$datapath) |> names())
  }) |>
    bindEvent(input$file$datapath)
  
  observe({
    updateSelectInput(inputId = "rain_column", choices = readxl::read_excel(input$file$datapath) |> names())
  }) |>
    bindEvent(input$file$datapath)
  
  observe({
    updateSelectInput(inputId = "choose_graph", choices = 1:nrow(statistics()))
  }) |>
    bindEvent(statistics())
  
  
  
  response <- reactive({
    body = data_input()
    res <- httr::PUT("https://nexus.sccwrp.org/bioretentionapi/rain", body = body, encode = "json", httr::content_type_json())
    content <- httr::content(res)
    content
  })
  
  statistics <- reactive({
    response()$statistics |>
      tibble::as_tibble() |>
      tidyr::unnest(
        cols = c(
          first_rain, 
          total_rainfall, 
          avg_rainfall_intensity,
          peak_5_min_rainfall_intensity,
          peak_10_min_rainfall_intensity
        )
      ) |>
      dplyr::mutate(first_rain = lubridate::as_datetime(first_rain)) |>
      dplyr::select(
        first_rain,
        total_rainfall,
        avg_rainfall_intensity,
        peak_5_min_rainfall_intensity,
        peak_10_min_rainfall_intensity
      ) |>
      dplyr::arrange(first_rain)
  }) |>
    bindEvent(input$submit)
  
  output$stats <- DT::renderDataTable({
    statistics()
  }, selection = 'none')
  
  output$cumulative_rain <- renderPlot({
    cumulative_rain <- response()$cumulative_rain |>
      tibble::as_tibble() |>
      tidyr::unnest(cols = c(datetime, rain)) |>
      dplyr::mutate(
        datetime = lubridate::as_datetime(datetime),
        hours = lubridate::time_length(datetime - datetime[1], unit = 'hours')
      )

    ggplot(cumulative_rain, aes(x = hours, y = rain)) +
      geom_line() + 
      labs(x = "Hours Elapsed", y = "Cumulative Rainfall")
  }) |>
    bindEvent(input$submit)
}