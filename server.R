library(ggplot2)
library(zip)
library(writexl)
library(glue)

server <- function(input, output) {
  
  observe({

    if (input$analysistype == 'Rainfall Analysis') {
    
      output$user_input <- renderUI({
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
            selectInput(inputId = "rainfall_resolution",
                        label = "Step 2: Choose the rainfall resolution",
                        choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
                        selected = "0.01 inch"),
          ),
          column(
            4,
            align = "left",
            uiOutput("date_column"),
            uiOutput("rain_column")
          ),
          
          column(
            4,
            br(),
            br(),
            br(),
            br(),
            align = "left",
            actionButton("submit", "Submit", width = '200px')
          )
        )
      })
    } else if (input$analysistype == 'Flow Analysis') {
      output$user_input <- renderUI({
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
            selectInput(inputId = "rainfall_resolution",
                        label = "Step 2: Choose the rainfall resolution",
                        choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
                        selected = "0.01 inch"),
          ),
          column(
            4,
            align = "left",
            uiOutput("date_column"),
            uiOutput("flow_column")
          ),
          column(
            4,
            br(),
            br(),
            br(),
            br(),
            align = "left",
            actionButton("submit", "Submit", width = '200px')
          )
        )
      })
    } else if (input$analysistype == "Both Rainfall and Flow Analysis") {

      output$user_input <- renderUI({tagList(
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
          selectInput(inputId = "rainfall_resolution",
                      label = "Step 2: Choose the rainfall resolution",
                      choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
                      selected = "0.01 inch"),
        ),
        column(
          4,
          align = "left",
          uiOutput("date_column"),
          uiOutput("rain_column")
        ),
        
        column(
          4,
          align = "left",
          uiOutput("flow_column"),
          br(),
          br(),
          actionButton("submit", "Submit", width = '200px')
        )
        
      )})     
    }
  })
  
  observe({
    if(input$analysistype == 'Flow Analysis'){
      print("true")
      updateSelectInput(inputId = "flow_column", label = "Step 4: Choose a column containing flow values")
    }
  })
  
  unit <- reactive({
    if (input$rainfall_resolution == 0.01){
      out <- "inch"
    } else {
      out <- "mm"
    }
  })
  
  
  ######## tab control based on whether pollutant data is included, updates dynamically
  tabs_list <- reactiveValues(data = list("Result" = NULL))
  
  observeEvent(
    input$submit,{
      showModal(modalDialog("Loading", footer=NULL))
    }
  )

  observeEvent(statistics(), {
    if (is.null(tabs_list[['Result']])) {
        insertTab(
          "full_page",
          tab = tabPanel(
            "Result",
            fluidRow(
              column(
                6,
                align = "left",
                br(),
                DT::dataTableOutput("stats"),
                br(),
                downloadButton("download_summary", "Download Table")
              ),
              column(
                5,
                align = "left",
                br(),
                br(),
                plotOutput("cumulative_rain"),
                selectInput("choose_graph", "Choose An Event", choices = 1:nrow(statistics()), selected = 1),
                downloadButton("download_plot", "Download Plot"),
              )
            )
          ),
          select = TRUE
        )
        tabs_list[["Result"]] = "Result"
    }
    removeModal()
  }) 
  
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
    selectInput(inputId = "date_column", label = "Step 3: Choose a column containing datetime values", choices = "")
  })
  
  output$rain_column <- renderUI({
    selectInput(inputId = "rain_column", label = "Step 4: Choose a column containing rainfall values", choices = "")
  })
  
  output$flow_column <- renderUI({
    selectInput(inputId = "flow_column", label = "Step 5: Choose a column containing flow values", choices = "")
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
    updateSelectInput(inputId = "flow_column", choices = readxl::read_excel(input$file$datapath) |> names())
  }) |>
    bindEvent(input$file$datapath)
  
  
  observeEvent(input$submit,{
    updateSelectInput(inputId = "choose_graph", choices = append(as.character(1:nrow(statistics())), "All events"), selected = "All events")
  }
  )

  
  response <- reactive({
    body = payload()
    res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/rain", body = body, encode = "json", httr::content_type_json())
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
    
    data <- statistics() |>
      dplyr::select(-last_rain) |>
      dplyr::mutate(
        first_rain = format(as.POSIXct(first_rain), format = "%Y-%m-%d %H:%M:%S"),
        avg_rainfall_intensity = round(avg_rainfall_intensity, 2),
        peak_5_min_rainfall_intensity = round(peak_5_min_rainfall_intensity, 2),
        peak_10_min_rainfall_intensity = round(peak_10_min_rainfall_intensity, 2),
        
      ) 
    if (input$rainfall_resolution == 0.1){
      data <- data |> dplyr::rename(
        `Event ID`= event,
        `Storm Date`= first_rain,
        `Total Rainfall (P) (mm)` = total_rainfall,
        `Average Rainfall Intensity (mm/hour)` = avg_rainfall_intensity,
        `Peak 5-min Rainfall Intensity (mm/5-min)` = peak_5_min_rainfall_intensity,
        `Peak 10-min Rainfall Intensity (2*mm/5-min)` = peak_10_min_rainfall_intensity
        )
    } else {
      data <- data |> dplyr::rename(
        `Event ID`= event,
        `Storm Date`= first_rain,
        `Total Rainfall (P) (inches)` = total_rainfall,
        `Average Rainfall Intensity (inch/hour)` = avg_rainfall_intensity,
        `Peak 5-min Rainfall Intensity (inches/5-min)` = peak_5_min_rainfall_intensity,
        `Peak 10-min Rainfall Intensity (2*inches/5-min)` = peak_10_min_rainfall_intensity
      )  
    }
      
    
    
  }, selection = 'none', rownames = FALSE)
  
  plotInput <- reactive({
    if (input$choose_graph == "All events"){
      cumulative_rain <- data_input() |>
        dplyr::mutate(
          cumsum = cumsum(rain),
          hours = lubridate::time_length(datetime - datetime[1], unit = "hour")
        )
      ggplot(cumulative_rain, aes(x = hours, y = cumsum)) +
        geom_line() + 
        labs(x = "Elapsed hours from the first rain tip", y = glue("Cumulative rainfall ({unit()})"), title = "Total cumulative rainfall")
      
    } else {
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
        labs(x = "Elapsed hours from the first rain tip", y = glue("Cumulative rainfall ({unit()})"), title = "Cumulative rainfall per a rain event")
      
    }

    
    })
  
  output$cumulative_rain <- renderPlot({
    plotInput()
  }) |> bindEvent(input$choose_graph)
  
  output$download_demo_1min <- downloadHandler(
    filename = function() {
      paste("demo_data_1min", ".xlsx", sep = "")
    },
    content = function(file) {
      data <- readxl::read_excel("demo_data/1min.xlsx")
      print(data)
      writexl::write_xlsx(data, file)
    }
  )
  
  output$download_demo_tt <- downloadHandler(
    filename = function() {
      paste("demo_data_timeoftips", ".xlsx", sep = "")
    },
    content = function(file) {
      data <- readxl::read_excel("demo_data/timeoftips.xlsx")
      print(data)
      writexl::write_xlsx(data, file)
    }
  )
  
  output$download_demo_flow <- downloadHandler(
    filename = function() {
      paste("demo_data_flow", ".xlsx", sep = "")
    },
    content = function(file) {
      data <- readxl::read_excel("demo_data/flowrate.xlsx")
      print(data)
      writexl::write_xlsx(data, file)
    }
  )
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("rain_summary", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(statistics(), file, row.names = FALSE)
    }
  )
  
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("cumulative_rain", ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    }
  )
  
  


  
  
}