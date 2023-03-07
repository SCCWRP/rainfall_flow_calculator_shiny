library(ggplot2)

server <- function(input, output) {
  
  data_input <- reactive({
    rain <- readxl::read_excel(input$file$datapath)
    rain <- rain |>
      dplyr::select(c(input$date_column, input$rain_column))
    names(rain) <- c("datetime", "rain")
    rain
  })
  
  payload <- reactive({
    rain <- data_input() |>
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
    updateSelectInput(inputId = "choose_graph", choices = 1:nrow(statistics()), selected = 1)
  }) |>
    bindEvent(statistics())
  
  
  
  response <- reactive({
    body = payload()
    res <- httr::POST("https://nexus.sccwrp.org/bioretentionapi/rain", body = body, encode = "json", httr::content_type_json())
    content <- httr::content(res)
    content
  })
  
  statistics <- reactive({
    response()$statistics |>
      tibble::as_tibble() |>
      tidyr::unnest(
        cols = c(
          first_rain, 
          last_rain,
          total_rainfall, 
          avg_rainfall_intensity,
          peak_5_min_rainfall_intensity,
          peak_10_min_rainfall_intensity
        )
      ) |>
      dplyr::arrange(first_rain) |>
      dplyr::mutate(
        first_rain = lubridate::as_datetime(first_rain),
        last_rain = lubridate::as_datetime(last_rain),
        event = dplyr::row_number()
      ) |>
      dplyr::select(
        event,
        first_rain,
        last_rain,
        total_rainfall,
        avg_rainfall_intensity,
        peak_5_min_rainfall_intensity,
        peak_10_min_rainfall_intensity
      ) 
  }) |>
    bindEvent(input$submit)
  
  output$stats <- DT::renderDataTable({
    statistics() |>
      dplyr::select(-event, -last_rain)
  }, selection = 'none')
  
  output$cumulative_rain <- renderPlot({
    cumulative_rain <- data_input() |> 
      dplyr::mutate(dummy = TRUE) |> 
      dplyr::left_join(
        statistics() |> 
          dplyr::mutate(dummy = TRUE) |> 
          dplyr::select(dummy, event, first_rain, last_rain), 
        multiple = 'all'
      ) |> 
      dplyr::group_by(event) |>
      dplyr::filter(
        datetime >= first_rain, 
        datetime <= last_rain,
        event == as.numeric(input$choose_graph)
      ) |> 
      dplyr::select(-dummy) |> 
      dplyr::arrange(datetime) |>
      dplyr::mutate(
        cumsum = cumsum(rain),
        hours = lubridate::time_length(datetime - datetime[1], unit = "hour")
      )

    ggplot(cumulative_rain, aes(x = hours, y = cumsum)) +
      geom_line() + 
      labs(x = "Hours Elapsed", y = "Cumulative Rainfall")
  }) |>
    bindEvent(input$choose_graph)
}