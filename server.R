library(ggplot2)
library(zip)
library(writexl)
library(glue)
library(dplyr)
library(shinyTime)
library(readxl)
library(lubridate)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


server <- function(input, output, session) {
  
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
          ),
          column(
            4,
            align = "left",
            uiOutput("start_date_flow"),
            timeInput(inputId = "start_time_flow", label = 'Input the start time'),
            uiOutput("end_date_flow"),
            timeInput(inputId = "end_time_flow", label = 'Input the end time')
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
          selectInput(
            inputId = "rainfall_resolution",
            label = "Step 2: Choose the rainfall resolution",
            choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
            selected = "0.01 inch"
          ),
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
                DT::dataTableOutput("stats") ,
                br(),
                downloadButton("download_summary", "Download Table") ,
                textOutput("percent_change")
              ),
              column(
                5,
                align = "left",
                br(),
                br(),
                plotOutput("rain_flow_plot") ,
                uiOutput("choose_graph_ui") ,
                downloadButton("download_plot", "Download Plot") ,

              )
            )
          ),
          select = TRUE
        )
        tabs_list[["Result"]] = "Result"
    }
    removeModal()
  }) 

  
  output$choose_graph_ui <- renderUI({
    print(input$analysistype)
    if (input$analysistype == 'Rainfall Analysis'){

      selectInput(
        inputId = "choose_graph", 
        label = "Choose a rain event:", 
        choices = append(as.character(1:nrow(statistics())), "All events"), 
        selected = "All events"
      )
    } else {
      selectInput(
        inputId = "choose_graph", 
        label = "Choose a flow type:", 
        choices = as.character(statistics()$flow_type),
        selected = as.character(statistics()$flow_type)[1],
        multiple = TRUE
      )

    }
  }) |> bindEvent(input$submit)
    
  output$start_date_flow <- renderUI({
    dateInput(inputId = "start_date_flow", label = "Step 2: Input the start of the hydrograph", value = NULL, min = NULL, max = NULL)
  })
  
  output$end_date_flow <- renderUI({
    dateInput(inputId = "end_date_flow", label = "Step 3: Input the end of the hydrograph", value = NULL, min = NULL, max = NULL)
  })
  
  output$rain_column <- renderUI({
    selectInput(inputId = "rain_column", label = "Step 4: Choose a column containing rainfall values", choices = "")
  })
  
  output$flow_column <- renderUI({
    selectInput(inputId = "flow_column", label = "Step 5: Choose a column containing flow values", choices = "")
  })
  

  
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
  
  
  observe({
    if (input$analysistype == 'Rainfall Analysis') {
      updateSelectInput(inputId = "choose_graph", label = "Choose a rain event:", choices = append(as.character(1:nrow(statistics())), "All events"), selected = "All events")
    } else {
      updateSelectInput(inputId = "choose_graph", label = "Choose a flow type:", choices = as.character(statistics()$flow_type)) 
    }
  }) |>
    bindEvent(input$submit)
  
  observe({
    updateSelectInput(inputId = "flow_column", choices = readxl::read_excel(input$file$datapath) |> names())
  }) |>
    bindEvent(input$file$datapath)
  
  observe({
    if (input$analysistype == 'Flow Analysis'){
      updateDateInput(
        inputId = "start_date_flow",
        value = min(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'inflow')$datetime)),
        min = min(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'inflow')$datetime)),
        max = max(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'outflow')$datetime))
      )
    }
    
  }) |>
    bindEvent(input$file$datapath)
  
  observe({
    if (input$analysistype == 'Flow Analysis'){
      updateDateInput(
        inputId = "end_date_flow", 
        value = max(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'outflow')$datetime)),
        min = min(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'inflow')$datetime)),
        max = max(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'outflow')$datetime))
      )   
      }

  }) |>
    bindEvent(input$file$datapath)
  
  observe({
    if (input$analysistype == 'Flow Analysis'){
      updateTimeInput(
        session,
        inputId = "start_time_flow", 
        value = hms::as_hms(
          format(
            min(as.POSIXct(readxl::read_excel(input$file$datapath, sheet = 'inflow')$datetime)), format='%H:%M:%S'
          )
        )
      )     
    }

  }) |>
    bindEvent(input$file$datapath)
  
  observe({
    if (input$analysistype == 'Flow Analysis'){
      updateTimeInput(
        session,
        inputId = "end_time_flow", 
        value = hms::as_hms(
          format(
            max(as.POSIXct(readxl::read_excel(input$file$datapath, sheet = 'outflow')$datetime)), format='%H:%M:%S'
          )
        )
      ) 
    }

  }) |>
    bindEvent(input$file$datapath)


  data_input <- reactive({
    
    if(input$analysistype == 'Rainfall Analysis'){
      user_data <- readxl::read_excel(input$file$datapath)
      user_data <-user_data |>
        dplyr::select(c("datetime","rain"))
        user_data
    
    } else if (input$analysistype == 'Flow Analysis'){

      # start <- as.POSIXct(paste(input$start_date_flow, format(as.POSIXct(input$start_time_flow),format="%H:%M:%S"), sep = " ")))
      # end <- as.POSIXct(paste(input$end_date_flow, format(as.POSIXct(input$end_time_flow),format="%H:%M:%S"), sep = " ")))
      # 
      user_data <- read_excel_allsheets(input$file$datapath)
      user_data
    
    } else {
      
      user_data <- user_data |>
        dplyr::select(c(input$date_column, input$rain_column, input$flow_column))
        names(user_data) <- c("datetime", "rain", "flow")
        user_data
    }
  })
  
  payload <- reactive({
    if (input$analysistype == 'Rainfall Analysis'){
      
      user_data <- data_input() %>% arrange(datetime)
      user_data <- list(rain=user_data)
      
    } else {
      user_data <- data_input() |>
        lapply(function (df) df %>% arrange(datetime))
    }

    user_data <- jsonlite::toJSON(user_data, dataframe = "columns", POSIXt = "ISO8601")
    user_data
  })

  
  response <- reactive({
    body = payload()

    if (input$analysistype == 'Rainfall Analysis'){

      res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/rain", body = body, encode = "json", httr::content_type_json())
    } else if (input$analysistype == 'Flow Analysis'){

      res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/flow", body = body, encode = "json", httr::content_type_json())

    } else {
      res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/rainflow", body = body, encode = "json", httr::content_type_json())
    }
    
    content <- httr::content(res)
    content
  
  })
  
  
  statistics <- reactive({
    if (input$analysistype == 'Rainfall Analysis'){
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
    } else if (input$analysistype == 'Flow Analysis'){
      my_content <- response()$statistics
      my_content <- lapply(
        seq_along(my_content), function(i) if(names(my_content)[i] %in% c('inflow','inflow2','bypass','outflow') ){
          my_content[[i]] %>% as_tibble() %>% tidyr::unnest()    %>%    
            mutate(flow_type = names(my_content)[i]) %>% 
            select(flow_type, start_time, peak_flow_rate, runoff_duration, runoff_volume) %>%
            mutate(start_time = format(as.POSIXct(start_time), format = "%Y-%m-%d %H:%M:%S")) 
        }
        ) %>% 
        dplyr::bind_rows() 
        
    } else {
      response()$statistics |>
        tibble::as_tibble() 
    }
  }) |>
    bindEvent(input$submit)
  
  
  
  output$percent_change = renderText({
    if (input$analysistype == 'Flow Analysis'){
      result <- ""
      print(as.character(response()$statistics['percent_change_volume']))
        if (as.character(response()$statistics['percent_change_volume']) != 'NULL'){
          pct_change_volume <- as.character(response()$statistics['percent_change_volume'][[1]])
          result <- paste(result, glue("The percentage of change in volume is: {pct_change_volume}."), sep = " ")
        }
        if (as.character(response()$statistics['percent_change_flow_rate']) != 'NULL'){
          pct_change_flow_rate <- as.character(response()$statistics['pct_change_flow_rate'][[1]])
          result <- paste(result, glue("The percentage of change in flow rate is: {pct_change_flow_rate}"), sep = " ")
        }
      print(result)
      result
    }
  })
  
  output$stats <- DT::renderDataTable({
    if (input$analysistype == 'Rainfall Analysis'){
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
          `Average Rainfall Intensity (mm/hr)` = avg_rainfall_intensity,
          `Peak 5-min Rainfall Intensity (mm/hr)` = peak_5_min_rainfall_intensity,
          `Peak 10-min Rainfall Intensity (mm/hr)` = peak_10_min_rainfall_intensity
          )
      } else {
        data <- data |> dplyr::rename(
          `Event ID`= event,
          `Storm Date`= first_rain,
          `Total Rainfall (P) (inches)` = total_rainfall,
          `Average Rainfall Intensity (inch/hr)` = avg_rainfall_intensity,
          `Peak 5-min Rainfall Intensity (inch/hr)` = peak_5_min_rainfall_intensity,
          `Peak 10-min Rainfall Intensity (inch/hr)` = peak_10_min_rainfall_intensity
        )  
      }
      DT::datatable(
        data,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',
          'Statistics of the rainfall data'
        )
      )
    } else if (input$analysistype == 'Flow Analysis'){
      data <- statistics()
      DT::datatable(
        data |> dplyr::rename(
          `Type of flow`= flow_type,
          `Storm Date`= start_time,
          `Peak flow rate (volume/time)` = peak_flow_rate,
          `Duration of runoff (minute)` = runoff_duration,
          `Runoff volume` = runoff_volume
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',
          'Statistics of the flow data'
        )
      ) 
    } else {
      data <- statistics()
    }
    
  }, selection = 'none', rownames = FALSE)
  
  
  
  
  plotInput <- reactive({
    if (input$analysistype == 'Rainfall Analysis'){
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
    } else if (input$analysistype == 'Flow Analysis'){
      
      flow_df <- data_input()
      
      flow_df <- bind_rows(
        lapply(
          seq_along(flow_df), 
          function(i) {
            flow_df[[i]] %>% mutate(flow_type=names(flow_df)[i]  )
          }
        )
      ) %>% filter(flow_type %in% input$choose_graph)
      
      ggplot(flow_df, aes(x = datetime, y = flow, colour = flow_type)) + geom_line() 

    } else {
      flow <- data_input()
      ggplot(flow, aes(x = datetime, y = flow)) + geom_line() 
    }
    
    }) |> bindEvent(input$choose_graph)
  
  output$rain_flow_plot <- renderPlot({
    if (input$analysistype == 'Rainfall Analysis'){
      plotInput() 
    } else if (input$analysistype == 'Flow Analysis'){
      plotInput()
    } else {
      plotInput()
    }
    
  }) 
  
  # output$flow <- renderPlot({
  #   plotInput_flow()
  # }) |> bindEvent(input$choose_graph)
  # 
  
  output$download_rainfall_template <- downloadHandler(
    filename = function() {
      paste("rainfall_template", ".xlsx", sep = "")
    },
    content = function(file) {
      template_path <- "templates/rainfall_template.xlsx"
      file.copy(template_path, file, overwrite = TRUE)
    }
  )
  
  
  output$download_demo_1min <- downloadHandler(
    filename = function() {
      paste("demo_data_1min", ".xlsx", sep = "")
    },
    content = function(file) {
      data <- readxl::read_excel("demo_data/1min.xlsx")
      writexl::write_xlsx(data, file)
    }
  )
  
  
  output$download_demo_tt <- downloadHandler(
    filename = function() {
      paste("demo_data_timeoftips", ".xlsx", sep = "")
    },
    content = function(file) {
      data <- readxl::read_excel("demo_data/timeoftips.xlsx")
      writexl::write_xlsx(data, file)
    }
  )
  
  output$download_flow_template <- downloadHandler(
    filename = function() {
      paste("flow_template", ".xlsx", sep = "")
    },
    content = function(file) {
      template_path <- "templates/flow_template.xlsx"
      file.copy(template_path, file, overwrite = TRUE)
    }
  )
  
  output$download_demo_flow <- downloadHandler(
    filename = function() {
      paste("demo_data_flow", ".xlsx", sep = "")
    },
    content = function(file) {
      template_path <- "demo_data/flowrate.xlsx"
      file.copy(template_path, file, overwrite = TRUE)
    }
  )
  
  output$download_demo_rainfall_flow <- downloadHandler(
    filename = function() {
      paste("demo_data_rainfall_flow", ".xlsx", sep = "")
    },
    content = function(file) {
      data <- readxl::read_excel("demo_data/1min-and-flow.xlsx")
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