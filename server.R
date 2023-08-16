source("rainfall_calculator/ui_rainfall.R")
source("flow_calculator/ui_flow.R")
source("file_validation/file_validator.R")
library(shinyjs)
library(shiny)
library(shinyvalidate)
library(shinycssloaders)
library(DT)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(zip)
library(writexl)
library(glue)
library(shinyTime)
library(readxl)
library(stringr)
library(shinyWidgets)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  sheets <- sheets[sheets != "Instructions"]
  x <- lapply(sheets, function(X) {
    current_sheet <- readxl::read_excel(filename, sheet = X) |>
      mutate(
        datetime = lubridate::as_datetime(datetime) |>
          lubridate::force_tz("UTC")
      )
    current_sheet
  })
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


server <- function(input, output, session) {
  
  rainfall_file_validator <- shinyvalidate::InputValidator$new()
  rainfall_file_validator$add_rule("file", function(file) is_correct_filetype(file))
  
  observe({
    rainfall_file_validator$add_rule("file", function(file, analysis_type) has_two_columns(file, analysis_type), analysis_type=input$analysistype)
  })
  
  rainfall_file_validator$add_rule("file", function(file) has_headers(file))
  
  rainfall_file_validator$add_rule("file", function(file) has_correct_date_format(file))
  
  rainfall_file_validator$add_rule("file", function(file) has_correct_measurement_format(file))
  
  rainfall_file_validator$add_rule("file", function(file) has_no_missing_values(file))
  
  rainfall_file_validator$add_rule("file", function(file) has_no_negative_values(file))
  
  
  flow_file_validator <- shinyvalidate::InputValidator$new()
  
  flow_file_validator$add_rule("file", function(file) is_correct_filetype(file))
  
  flow_file_validator$add_rule("file", function(file) has_five_sheets(file))
  
  observe({
    flow_file_validator$add_rule("file", function(file, analysis_type) has_two_columns(file, analysis_type), analysis_type=input$analysistype)
  })
  
  flow_file_validator$add_rule("file", function(file) has_headers(file))
  
  flow_file_validator$add_rule("file", function(file) has_correct_date_format(file))
  
  flow_file_validator$add_rule("file", function(file) has_correct_measurement_format(file))
  
  flow_file_validator$add_rule("file", function(file) has_no_missing_values(file))
  
  flow_file_validator$add_rule("file", function(file) has_no_negative_values(file))
  
  flow_file_validator$add_rule("file", function(file) has_at_least_inflow_outflow(file))
  
  flow_file_validator$add_rule("file", function(file) has_valid_outflow_time(file))
  
  
  observe({
    shinyjs::disable("analysistype")
  }) |>
    bindEvent(input$submit)
  
  
  observe({
    if(input$analysistype == "rainfall") {
      rainfall_file_validator$enable()
      flow_file_validator$disable()
      
      shinyjs::toggleState("submit", rainfall_file_validator$is_valid())
    }
    else if (input$analysistype == "flow") {
      showModal(
        modalDialog(
          id="my_modal",
          title="Validating file...", 
          footer=NULL
        )
      )
      rainfall_file_validator$disable()
      flow_file_validator$enable()
      if (flow_file_validator$is_valid()) {
        removeModal()
        showModal(
          modalDialog(
            id="my_modal",
            title="File is cleaned. Populating data for the inputs. This window will close automatically when the file is loaded",
            footer=NULL
          )
        )
      } else {
        removeModal()
        showModal(
          modalDialog(
            id="my_modal",
            title="Errors are found in the uploaded file"
          )
        )
      }
      
      shinyjs::toggleState("submit", flow_file_validator$is_valid())
    }
  }) |>
    bindEvent(input$file)
  
  # Define reactiveValues
  tabs_list <- reactiveValues(data = list("Result" = NULL))
  
  rain_unit <- reactive({
    if (input$analysistype == 'rainfall'){
      if (input$rainfall_unit == "inch"){
        out <- "inch"
      } else {
        out <- "mm"
      }
    }
  })
  
  flow_volume_unit <- reactive({
    if (input$analysistype == 'flow'){
      out <- strsplit(input$flow_unit,"/")[[1]][1]
    }
  })
  
  flow_time_unit <- reactive({
    if (input$analysistype == 'flow'){
      
      out <- strsplit(input$flow_unit,"/")[[1]][2]
      
    }
  }) |> bindEvent(input$flow_unit)
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  ##
  
  observe({
    shinyjs::js$refresh_page()
  }) |> bindEvent(input$refresh)
  
  observe({
    if (input$analysistype == 'rainfall'){
      output$user_input <- renderUI({ rainfallUI("rainfall") })
      
    } else if (input$analysistype == 'flow'){
      output$user_input <- renderUI({ flowUI("flow") })
    } 
  }) |> bindEvent(input$analysistype)
  
  observe({
    showModal(modalDialog("Calculating...", footer=NULL))
  }) |> bindEvent(input$submit)
  
  observe({
    if (is.null(tabs_list[['Result']])) {
      insertTab(
        "full_page",
        tab = tabPanel(
          "Result",
          fluidRow(
            column(
              7,
              align = "left",
              br(),
              DT::dataTableOutput("stats") ,
              br(),
              downloadButton("download_summary", "Download Table") ,
              downloadButton("download_summary_smc", "Download Table in SMC Format") ,
              
              textOutput("percent_change")
            ),
            column(
              5,
              align = "left",
              br(),
              br(),
              h4("Brush and double-click to zoom in . Double-click again to zoom out"),
              plotOutput(
                "rain_flow_plot",
                dblclick = "plot1_dblclick",
                brush = brushOpts(
                  id = "plot1_brush",
                  resetOnNew = TRUE
                )
              ) ,
              uiOutput("choose_graph_ui") ,
              downloadButton("download_plot", "Download Plot") ,
              
            )
          )
        ),
        target = "Methods",
        position = "after",
        select = TRUE
      )
      tabs_list[["Result"]] = "Result"
    }
    removeModal()
  }) |> bindEvent(statistics())
  
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(as.POSIXct(brush$xmin, origin = "1970-01-01"), as.POSIXct(brush$xmax, origin = "1970-01-01"))
      ranges$y <- c(as.POSIXct(brush$ymin, origin = "1970-01-01"), as.POSIXct(brush$ymax, origin = "1970-01-01"))
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$choose_graph_ui <- renderUI({
    
    if (input$analysistype == 'rainfall'){
      
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
        selected = as.character(statistics()$flow_type),
        multiple = TRUE
      )
      
    }
  }) |> bindEvent(input$submit)

  
  flow_dates_data <- reactive({
    if (input$analysistype == 'flow') {
      read_excel_allsheets(input$file$datapath)
    }
  }) |>
    bindEvent(req(flow_file_validator$is_valid()))
  
  min_datetime <- reactive({
    if (input$analysistype == 'flow') {
      do.call(min, lapply(flow_dates_data(), function(x) min(x$datetime)))
    }
  }) 
  
  max_datetime <- reactive({
    if (input$analysistype == 'flow') {
      do.call(max, lapply(flow_dates_data(), function(x) max(x$datetime)))
    }
  }) 
    
  observe({
    if (input$analysistype == 'flow'){
      
      max_date <- date(max_datetime())
      min_date <- date(min_datetime())
      max_time <- max_datetime()
      min_time <- min_datetime()
      
      
      updateDateInput(
        inputId = "start_date_flow",
        value = min_date,
        min = min_date,
        max = max_date
      )

      updateDateInput(
        inputId="end_date_flow",
        value=max_date,
        min = min_date,
        max = max_date
      )

      updateTimeInput(
        session,
        inputId="start_time_flow",
        value= min_time
      )
      
      updateTimeInput(
        session=session,
        inputId = "end_time_flow",
        value = max_time
      )
      
    }
    removeModal()
  }) |> bindEvent(req(flow_file_validator$is_valid()))
  
  
  input_start_time_d <- debounce(
    reactive({
      req(input$end_time_flow, input$end_date_flow)
      datetime_out <- input$start_time_flow
      date(datetime_out) <- input$start_date_flow
      lubridate::force_tz(datetime_out, "UTC")
    }), 
    500
  )
  input_end_time_d <- debounce(
    reactive({
      req(input$end_time_flow, input$end_date_flow)
      datetime_out <- input$end_time_flow
      date(datetime_out) <- input$end_date_flow
      lubridate::force_tz(datetime_out, "UTC")
    }), 
    500
  )
  
  observe({
    if (input$analysistype == 'flow') {
      if (input_start_time_d() < min_datetime()) {
        shinyTime::updateTimeInput(session = session, inputId = "start_time_flow", value = min_datetime())
        updateDateInput(session = session, inputId = "start_date_flow", value = lubridate::date(min_datetime()))
      }
      if (input_end_time_d() > max_datetime()) {
        shinyTime::updateTimeInput(session = session, inputId = "end_time_flow", value = max_datetime())
        updateDateInput(session = session, inputId = "end_date_flow", value = lubridate::date(max_datetime()))
      }
      if (input_start_time_d() > max_datetime()) {
        shinyTime::updateTimeInput(session = session, inputId = "start_time_flow", value = min_datetime())
        updateDateInput(session = session, inputId = "start_date_flow", value = lubridate::date(min_datetime()))
        
      }
      if (input_start_time_d() > input_end_time_d()) {
        shinyTime::updateTimeInput(session = session, inputId = "start_time_flow", value = min_datetime())
        updateDateInput(session = session, inputId = "start_date_flow", value = lubridate::date(min_datetime()))
        
        shinyTime::updateTimeInput(session = session, inputId = "end_time_flow", value = max_datetime())
        updateDateInput(session = session, inputId = "end_date_flow", value = lubridate::date(max_datetime()))
      }
    }
  })
  
  
  reactive_text <- reactiveVal("")
  observe({
    reactive_text("If you would like to submit another file, repeat the steps and click Re-Submit")
    updateActionButton(session, inputId = "submit", label = "Re-submit")
  }) |> bindEvent(input$submit)
  output$resubmit_notice <- renderText({
    reactive_text()
  })
  
  data_input <- reactive({
    
    if(input$analysistype == 'rainfall'){
      user_data <- read_excel_allsheets(input$file$datapath)
      user_data <- user_data[[2]] |> dplyr::select(c("datetime","rain"))
      user_data
      
    } else if (input$analysistype == 'flow'){
      
      start <- ymd_hms(paste(input$start_date_flow, format(as.POSIXct(input$start_time_flow),format="%H:%M:%S"), sep = " "))
      end <- ymd_hms(paste(input$end_date_flow, format(as.POSIXct(input$end_time_flow),format="%H:%M:%S"), sep = " "))
      
      
      user_data <- read_excel_allsheets(input$file$datapath)
      user_data <- Filter(function(df) nrow(df) > 0, user_data)
      user_data <- lapply(user_data, function (df) df %>% filter(between(datetime, start, end)))
      
      
    } else {
      
      user_data <- user_data |>
        dplyr::select(c(input$date_column, input$rain_column, input$flow_column))
      names(user_data) <- c("datetime", "rain", "flow")
      user_data
    }
  })
  
  payload <- reactive({
    if (input$analysistype == 'rainfall'){
      user_data <- data_input() %>% arrange(datetime)
      user_data <- list(rain=user_data)
    } else {
      user_data <- data_input() |>
        lapply( function (df) {
          tmp <- df %>% arrange(datetime)
          tmp
        })
      user_data <- lapply(user_data, function(x) append(x, list(time_unit = flow_time_unit())))
    }
    
    user_data <- jsonlite::toJSON(user_data, dataframe = "columns", POSIXt = "ISO8601", auto_unbox = TRUE)
    user_data
  })
  
  
  response <- reactive({
    body = payload()
    if (input$analysistype == 'rainfall'){
      res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/rain", body = body, encode = "json", httr::content_type_json())
    } else if (input$analysistype == 'flow'){
      res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/flow", body = body, encode = "json", httr::content_type_json())
    } else {
      res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/rainflow", body = body, encode = "json", httr::content_type_json())
    }
    content <- httr::content(res)
    content
  })
  
  
  statistics <- reactive({
    
    if (input$analysistype == 'rainfall'){
      response()$statistics |>
        tibble::as_tibble() |>
        tidyr::unnest(
          cols = c(
            first_rain,
            last_rain,
            total_rainfall,
            avg_rainfall_intensity,
            peak_5_min_rainfall_intensity,
            peak_10_min_rainfall_intensity,
            peak_60_min_rainfall_intensity,
            antecedent_dry_period
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
          peak_10_min_rainfall_intensity,
          antecedent_dry_period,
          peak_60_min_rainfall_intensity
        )
    } else if (input$analysistype == 'flow'){
      my_content <- response()$statistics
      my_content <- lapply(
        seq_along(my_content), function(i) if(names(my_content)[i] %in% c('inflow1','inflow2','bypass','outflow') ){
          my_content[[i]] %>% as_tibble() %>% tidyr::unnest(cols = c(end_time, peak_flow_rate, runoff_duration, runoff_volume, start_time))    %>%
            mutate(
              flow_type = names(my_content)[i],
              runoff_duration = round(runoff_duration, 1),
              peak_flow_rate = round(peak_flow_rate, 1),
              runoff_volume = round(runoff_volume, 1),
              
              
            ) %>%
            select(flow_type, start_time, peak_flow_rate, runoff_duration, runoff_volume, end_time) %>%
            mutate(
              start_time = stringr::str_replace(start_time,"T"," "),
              end_time = stringr::str_replace(end_time,"T"," "),
              
            )
          
        }
      ) %>%
        dplyr::bind_rows() %>%
        mutate(flow_type = factor(flow_type, levels = c("inflow1", "inflow2", "bypass", "outflow"))) %>%
        arrange(flow_type)
      
      
    }
  }) |> bindEvent(input$submit)
  
  
  output$stats <- DT::renderDataTable({
    if (input$analysistype == 'rainfall'){
      data <- statistics() |>
        dplyr::select(
          event, 
          first_rain, 
          total_rainfall, 
          avg_rainfall_intensity,
          peak_5_min_rainfall_intensity,
          peak_10_min_rainfall_intensity,
          peak_60_min_rainfall_intensity,
          antecedent_dry_period
        ) |>
        dplyr::mutate(
          first_rain = format(as.POSIXct(first_rain), format = "%Y-%m-%d %H:%M:%S"),
          total_rainfall = round(total_rainfall, 2),
          avg_rainfall_intensity = round(avg_rainfall_intensity, 2),
          peak_5_min_rainfall_intensity = round(peak_5_min_rainfall_intensity, 2),
          peak_10_min_rainfall_intensity = round(peak_10_min_rainfall_intensity, 2),
          peak_60_min_rainfall_intensity = round(peak_60_min_rainfall_intensity, 2),
          antecedent_dry_period = round(antecedent_dry_period, 2)
        )
      if (input$rainfall_unit == "mm"){
        data <- data |> dplyr::rename(
          `Event ID`= event,
          `Storm Date`= first_rain,
          `Total Rainfall (mm)` = total_rainfall,
          `Average Rainfall Intensity (mm/hr)` = avg_rainfall_intensity,
          `Peak 5-min Rainfall Intensity (mm/hr)` = peak_5_min_rainfall_intensity,
          `Peak 10-min Rainfall Intensity (mm/hr)` = peak_10_min_rainfall_intensity,
          `Peak 60-min Rainfall Intensity (mm/hr)` = peak_60_min_rainfall_intensity,
          `Antecedent Dry Period (days)` = antecedent_dry_period
        )
      } else {
        data <- data |> dplyr::rename(
          `Event ID`= event,
          `Storm Date`= first_rain,
          `Total Rainfall (inches)` = total_rainfall,
          `Average Rainfall Intensity (inch/hr)` = avg_rainfall_intensity,
          `Peak 5-min Rainfall Intensity (inch/hr)` = peak_5_min_rainfall_intensity,
          `Peak 10-min Rainfall Intensity (inch/hr)` = peak_10_min_rainfall_intensity,
          `Peak 60-min Rainfall Intensity (inch/hr)` = peak_60_min_rainfall_intensity,
          `Antecedent Dry Period (days)` = antecedent_dry_period
        )
      }
      DT::datatable(
        data,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',
          'Statistics of the rainfall data'
        ),
        options = list(searching = FALSE, lengthChange = FALSE),
        rownames = FALSE,
        selection = 'none'
      )
    } else if (input$analysistype == 'flow'){
      data <- statistics()
      
      DT::datatable(
        data |>
          mutate(start_time = as.POSIXct(start_time)) |>
          #select(flow_type,start_time,peak_flow_rate,runoff_duration) |>
          select(flow_type, peak_flow_rate, runoff_duration, runoff_volume) |>
          
          setNames(
            c(
              "Type of flow",
              #"Storm Date",
              glue("Peak flow rate ({flow_volume_unit()}/{flow_time_unit()})"),
              glue("Duration of runoff (h)"),
              glue("Runoff volume ({flow_volume_unit()})")
            )
          )
        ,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',
          'Statistics of the flow data'
        ),
        options = list(searching = FALSE, lengthChange = FALSE),
        rownames = FALSE,
        selection = 'none'
      )
    } else {
      data <- statistics()
    }
    
  }) |> bindEvent(input$submit)
  
  
  output$percent_change = renderText({
    if (input$analysistype == 'flow'){
      result <- ""
      if (as.character(response()$statistics['percent_change_volume']) != 'NULL'){
        pct_change_volume <- as.character(
          round(
            as.numeric(
              as.character(
                response()$statistics['percent_change_volume'][[1]]
              )
            ),
            1
          )
        )
        result <- paste(result, glue("The percentage of change in volume is: {pct_change_volume}%."), sep = " ")
      }
      if (as.character(response()$statistics['percent_change_flow_rate']) != 'NULL'){
        pct_change_flow_rate <- as.character(
          round(
            as.numeric(
              as.character(
                response()$statistics['percent_change_flow_rate'][[1]]
              )
            ),
            1
          )
        )
        result <- paste(result, glue("The percentage of change in flow rate is: {pct_change_flow_rate}%"), sep = " ")
      }
      result
    }
  }) |> bindEvent(input$submit)
  
  plotInput <- reactive({
    if (input$analysistype == 'rainfall'){
      if (input$choose_graph == "All events"){
        cumulative_rain <- data_input() |>
          dplyr::mutate(
            cumsum = cumsum(rain),
            hours = lubridate::time_length(datetime - datetime[1], unit = "hour")
          )
        
        ggplot(cumulative_rain, aes(x = hours, y = cumsum)) +
          geom_line() +
          labs(
            x="Elapsed hours from the first rain tip", 
            y=glue("Cumulative rainfall ({rain_unit()})"), 
            title = input$title
          ) +
          theme(
            axis.text=element_text(size=18),
            axis.title=element_text(size=18,face="bold"),
            legend.title=element_text(size=18),
            legend.text=element_text(size=18),
            plot.title = element_text(size = 18, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
        
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
          labs(
            x="Elapsed hours from the first rain tip",
            y=glue("Cumulative rainfall ({rain_unit()})"),
            title=input$title
          ) +
          theme(
            axis.text=element_text(size=18),
            axis.title=element_text(size=18,face="bold"),
            legend.title=element_text(size=18),
            legend.text=element_text(size=18),
            plot.title=element_text(size = 18, face = "bold"),
            axis.text.x=element_text(angle = 45, hjust = 1)
          )
        
      }
    } else if (input$analysistype == 'flow'){
      
      flow_df <- data_input()
      
      flow_df <- bind_rows(
        lapply(
          seq_along(flow_df),
          function(i) {
            flow_df[[i]] %>% mutate(flow_type=names(flow_df)[i]  )
          }
        )
      ) %>% filter(flow_type %in% input$choose_graph) %>%
        mutate(hours = lubridate::time_length(datetime - datetime[1], unit = "minute"))
      
      ggplot(flow_df, aes(x = datetime, y = flow, colour = flow_type)) +
        geom_line() +
        ggtitle("Hydrograph") +
        scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M") + 
        theme(
          axis.text=element_text(size=18),
          axis.title=element_text(size=18,face="bold"),
          legend.title=element_text(size=18),
          legend.text=element_text(size=18),
          plot.title = element_text(size = 18, face = "bold"),
          axis.text.x = element_text(angle = 90, hjust = 1)
        ) +
        labs(
          x=glue("Datetime"),
          y=glue("Flow rate ({flow_volume_unit()}/{flow_time_unit()})"),
          title=input$title
        )
      
      
    } else {
      flow <- data_input()
      ggplot(flow, aes(x = datetime, y = flow)) + geom_line()
    }
    
  })
  
  
  output$rain_flow_plot <- renderPlot({
    if (input$analysistype == 'rainfall'){
      
      plotInput() +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    } else if (input$analysistype == 'flow'){
      
      plotInput() +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      
    } else {
      plotInput()
    }
    
  }) |> bindEvent(input$choose_graph, input$submit, input$plot1_dblclick)
  
  
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
      template_path <- "demo_data/1min.xlsx"
      file.copy(template_path, file, overwrite = TRUE)
    }
  )
  
  
  output$download_demo_tt <- downloadHandler(
    filename = function() {
      paste("demo_data_timeoftips", ".xlsx", sep = "")
    },
    content = function(file) {
      template_path <- "demo_data/timeoftips.xlsx"
      file.copy(template_path, file, overwrite = TRUE)
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
      paste("summary_table", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(statistics(), file, row.names = FALSE)
    }
  )
  
  output$download_summary_smc <- downloadHandler(
    filename = function() {
      paste(glue("summary_table_smc_format_{input$analysistype}"), ".csv", sep = "")
    },
    content = function(file) {
      df <- statistics()
      
      if (input$analysistype == 'rainfall'){
        
        if (input$rainfall_unit == 'mm'){
          totaldepthunits <- 'mm'
          onehourpeakrateunit <- 'mm/hr'
        } else {
          totaldepthunits <- 'inch'
          onehourpeakrateunit <- 'inch/hr'
        }
        df <- df %>% mutate(
          eventid = event,
          startdate = as.Date(first_rain),
          starttime = format(first_rain, "%H:%M:%S"),
          enddate = as.Date(last_rain),
          endtime = format(df$last_rain, "%H:%M:%S"),
          totaldepth = total_rainfall, 
          totaldepthunits = totaldepthunits,
          onehourpeakrate = peak_60_min_rainfall_intensity,
          onehourpeakrateunit = onehourpeakrateunit,
          antecedentdryperiod = antecedent_dry_period,
        ) %>% select(
          eventid, 
          startdate, 
          starttime, 
          enddate, 
          endtime, 
          totaldepth, 
          totaldepthunits, 
          onehourpeakrate, 
          onehourpeakrateunit, 
          antecedentdryperiod
        ) %>% rename(
          antecedentdryperiod_days = antecedentdryperiod
        )
      } else if (input$analysistype == 'flow'){
        
        df <- df %>% mutate(
          start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
          end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S")
        ) %>% mutate(
          monitoringstation = flow_type,
          datestart = as.Date(start_time),
          timestart = format(start_time, "%H:%M:%S"),
          dateend = as.Date(end_time),
          timeend = format(end_time, "%H:%M:%S"),
          volumetotal = runoff_volume , 
          volumeunits = glue("{flow_volume_unit()}"),
          peakflowrate = peak_flow_rate,
          peakflowunits = glue("{flow_volume_unit()}/{flow_time_unit()}")
        ) %>% select(
          monitoringstation, 
          datestart, 
          timestart, 
          dateend, 
          timeend, 
          volumetotal, 
          volumeunits, 
          peakflowrate, 
          peakflowunits
        )  %>% mutate(
          peakflowunits = gsub("L/s", "lps", peakflowunits)
        )
        
      }
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("downloaded_plot", ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plotInput() + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE), device = "png")
    }
  )
  
  
}


#

#
#
# server <- function(input, output, session) {
#
#   observe({
#     if (input$analysistype == 'Rainfall Analysis') {
#       output$user_input <- renderUI({
#         tagList(
#           column(
#             4,
#             align = "left",
#             br(),
#             fileInput(
#               "file",
#               "Step 1: Upload an Excel File",
#               multiple = FALSE,
#               accept = ".xlsx"
#             ),
#             selectInput(
#               inputId = "rainfall_resolution",
#               label = "Step 2: Choose the rainfall resolution",
#               choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
#               selected = "0.01 inch"
#             ),
#           ),
#           column(
#             4,
#             br(),
#             br(),
#             br(),
#             br(),
#             align = "left",
#             actionButton("submit", "Submit", width = '200px'),
#
#           )
#         )
#       })
#     } else if (input$analysistype == 'Flow Analysis') {
#       output$user_input <- renderUI({
#         tagList(
#           column(
#             4,
#             align = "left",
#             br(),
#             fileInput(
#               "file",
#               "Step 1: Upload an Excel File",
#               multiple = FALSE,
#               accept = ".xlsx"
#             ),
#             selectInput(
#               inputId = "flow_unit",
#               label = "Step 2: Choose the flow units",
#               choices = c("L/s","g/m","ft3/s"),
#               selected = "L/s"
#             ),
#           ),
#           column(
#             4,
#             align = "left",
#             uiOutput("start_date_flow"),
#             timeInput(inputId = "start_time_flow", label = 'Input the start time'),
#             uiOutput("end_date_flow"),
#             timeInput(inputId = "end_time_flow", label = 'Input the end time')
#           ),
#           column(
#             4,
#             br(),
#             br(),
#             br(),
#             br(),
#             align = "left",
#             actionButton("submit", "Submit", width = '200px')
#           )
#         )
#       })
#     } else if (input$analysistype == "Both Rainfall and Flow Analysis") {
#
#       output$user_input <- renderUI({tagList(
#         column(
#           4,
#           align = "left",
#           br(),
#           fileInput(
#             "file",
#             "Step 1: Upload an Excel File",
#             multiple = FALSE,
#             accept = ".xlsx"
#           ),
#           selectInput(
#             inputId = "rainfall_resolution",
#             label = "Step 2: Choose the rainfall resolution",
#             choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
#             selected = "0.01 inch"
#           ),
#         ),
#         column(
#           4,
#           align = "left",
#           uiOutput("date_column"),
#           uiOutput("rain_column")
#         ),
#         column(
#           4,
#           align = "left",
#           uiOutput("flow_column"),
#           br(),
#           br(),
#           actionButton("submit", "Submit", width = '200px'),
#           actionButton("refresh", "Clear All")
#         )
#       )})
#     }
#   })
#
#   observeEvent(input$refresh, {
#     shinyjs::js$refresh_page()
#   })
#
#   rain_unit <- reactive({
#     if (input$rainfall_resolution == 0.01){
#       out <- "inch"
#     } else {
#       out <- "mm"
#     }
#   }) |> bindEvent(input$submit)
#
#   flow_volume_unit <- reactive({
#     if (input$analysistype == 'Flow Analysis'){
#       out <- strsplit(input$flow_unit,"/")[[1]][1]
#     }
#   })
#
#   flow_time_unit <- reactive({
#     if (input$analysistype == 'Flow Analysis'){
#       out <- strsplit(input$flow_unit,"/")[[1]][2]
#     }
#   })
#
#   ######## tab control based on whether pollutant data is included, updates dynamically
#   tabs_list <- reactiveValues(data = list("Result" = NULL))
#
#   observeEvent(
#     input$submit,{
#       showModal(modalDialog("Loading", footer=NULL))
#     }
#   )
#
#   observeEvent(statistics(), {
#     if (is.null(tabs_list[['Result']])) {
#         insertTab(
#           "full_page",
#           tab = tabPanel(
#             "Result",
#             fluidRow(
#               column(
#                 7,
#                 align = "left",
#                 br(),
#                 DT::dataTableOutput("stats") ,
#                 br(),
#                 downloadButton("download_summary", "Download Table") ,
#                 textOutput("percent_change")
#               ),
#               column(
#                 5,
#                 align = "left",
#                 br(),
#                 br(),
#                 h4("Brush and double-click to zoom in . Double-click again to zoom out"),
#                 plotOutput(
#                   "rain_flow_plot",
#                   dblclick = "plot1_dblclick",
#                   brush = brushOpts(
#                     id = "plot1_brush",
#                     resetOnNew = TRUE
#                   )
#                 ) ,
#                 uiOutput("choose_graph_ui") ,
#                 downloadButton("download_plot", "Download Plot") ,
#
#               )
#             )
#           ),
#           select = TRUE
#         )
#         tabs_list[["Result"]] = "Result"
#     }
#     removeModal()
#   })
#
#   ranges <- reactiveValues(x = NULL, y = NULL)
#
#   observeEvent(input$plot1_dblclick, {
#     print("dblclick")
#     print(ranges)
#     brush <- input$plot1_brush
#     if (!is.null(brush)) {
#       print(brush$xmin)
#       print(brush$xmax)
#
#       ranges$x <- c(brush$xmin, brush$xmax)
#       ranges$y <- c(brush$ymin, brush$ymax)
#
#     } else {
#       ranges$x <- NULL
#       ranges$y <- NULL
#     }
#   })
#
#   output$choose_graph_ui <- renderUI({
#
#     if (input$analysistype == 'Rainfall Analysis'){
#
#       selectInput(
#         inputId = "choose_graph",
#         label = "Choose a rain event:",
#         choices = append(as.character(1:nrow(statistics())), "All events"),
#         selected = "All events"
#       )
#     } else {
#       selectInput(
#         inputId = "choose_graph",
#         label = "Choose a flow type:",
#         choices = as.character(statistics()$flow_type),
#         selected = as.character(statistics()$flow_type)[1],
#         multiple = TRUE
#       )
#
#     }
#   }) |> bindEvent(input$submit)
#
#   output$start_date_flow <- renderUI({
#     dateInput(inputId = "start_date_flow", label = "Step 2: Input the start of the hydrograph", value = NULL, min = NULL, max = NULL)
#   })
#
#   output$end_date_flow <- renderUI({
#     dateInput(inputId = "end_date_flow", label = "Step 3: Input the end of the hydrograph", value = NULL, min = NULL, max = NULL)
#   })
#
#   output$rain_column <- renderUI({
#     selectInput(inputId = "rain_column", label = "Step 4: Choose a column containing rainfall values", choices = "")
#   })
#
#   output$flow_column <- renderUI({
#     selectInput(inputId = "flow_column", label = "Step 5: Choose a column containing flow values", choices = "")
#   })
#
#   observe({
#     updateSelectInput(inputId = "date_column", choices = readxl::read_excel(input$file$datapath) |> names())
#   }) |>
#     bindEvent(input$file$datapath)
#
#   observe({
#     updateSelectInput(inputId = "rain_column", choices = readxl::read_excel(input$file$datapath) |> names())
#   }) |>
#     bindEvent(input$file$datapath)
#
#   observe({
#     updateSelectInput(inputId = "flow_column", choices = readxl::read_excel(input$file$datapath) |> names())
#   }) |>
#     bindEvent(input$file$datapath)
#
#   observe({
#     if (input$analysistype == 'Flow Analysis'){
#       updateDateInput(
#         inputId = "start_date_flow",
#         value = min(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'inflow1')$datetime)),
#         min = min(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'inflow1')$datetime)),
#         max = max(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'inflow1')$datetime))
#       )
#     }
#
#   }) |>
#     bindEvent(input$file$datapath)
#
#   observe({
#     if (input$analysistype == 'Flow Analysis'){
#       updateDateInput(
#         inputId = "end_date_flow",
#         value = max(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'outflow')$datetime)),
#         min = min(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'outflow')$datetime)),
#         max = max(ymd_hms(readxl::read_excel(input$file$datapath, sheet = 'outflow')$datetime))
#       )
#     }
#   }) |>
#     bindEvent(input$file$datapath)
#
#   observe({
#     if (input$analysistype == 'Flow Analysis'){
#       updateTimeInput(
#         session,
#         inputId = "start_time_flow",
#         value = hms::as_hms(
#           format(
#             min(as.POSIXct(readxl::read_excel(input$file$datapath, sheet = 'inflow1')$datetime)), format='%H:%M:%S'
#           )
#         )
#       )
#     }
#   }) |>
#     bindEvent(input$file$datapath)
#
#   observe({
#     if (input$analysistype == 'Flow Analysis'){
#       updateTimeInput(
#         session = session,
#         inputId = "end_time_flow",
#         value = hms::as_hms(
#           format(
#             max(as.POSIXct(readxl::read_excel(input$file$datapath, sheet = 'outflow')$datetime)), format='%H:%M:%S'
#           )
#         )
#       )
#     }
#   }) |>
#     bindEvent(input$file$datapath)
#
#   observe({
#     updateActionButton(session, inputId = "submit", label = "Re-submit")
#   }) |> bindEvent(input$submit)
#
#
#   data_input <- reactive({
#
#     if(input$analysistype == 'Rainfall Analysis'){
#       user_data <- readxl::read_excel(input$file$datapath)
#       user_data <-user_data |>
#         dplyr::select(c("datetime","rain"))
#         user_data
#
#     } else if (input$analysistype == 'Flow Analysis'){
#
#       start <- as.POSIXct(paste(input$start_date_flow, format(as.POSIXct(input$start_time_flow),format="%H:%M:%S"), sep = " "))
#       end <- as.POSIXct(paste(input$end_date_flow, format(as.POSIXct(input$end_time_flow),format="%H:%M:%S"), sep = " "))
#
#       user_data <- read_excel_allsheets(input$file$datapath)
#       user_data <- lapply(user_data, function (df) df %>% filter(between(datetime, start, end)))
#
#
#     } else {
#
#       user_data <- user_data |>
#         dplyr::select(c(input$date_column, input$rain_column, input$flow_column))
#         names(user_data) <- c("datetime", "rain", "flow")
#         user_data
#     }
#   })
#
#   payload <- reactive({
#     if (input$analysistype == 'Rainfall Analysis'){
#
#       user_data <- data_input() %>% arrange(datetime)
#       user_data <- list(rain=user_data)
#
#     } else {
#       user_data <- data_input() |>
#         lapply( function (df) {
#           tmp <- df %>% arrange(datetime)
#           tmp
#         })
#       user_data <- lapply(user_data, function(x) append(x, list(time_unit = flow_time_unit())))
#
#     }
#
#     user_data <- jsonlite::toJSON(user_data, dataframe = "columns", POSIXt = "ISO8601", auto_unbox = TRUE)
#     user_data
#
#
#   })
#
#
#   response <- reactive({
#     body = payload()
#
#     if (input$analysistype == 'Rainfall Analysis'){
#
#       res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/rain", body = body, encode = "json", httr::content_type_json())
#     } else if (input$analysistype == 'Flow Analysis'){
#
#       res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/flow", body = body, encode = "json", httr::content_type_json())
#
#     } else {
#       res <- httr::POST("https://nexus.sccwrp.org/bmp_hydrology/api/rainflow", body = body, encode = "json", httr::content_type_json())
#     }
#
#     content <- httr::content(res)
#     content
#
#
#   })
#
#
#   statistics <- reactive({
#     print("statistics being run")
#     if (input$analysistype == 'Rainfall Analysis'){
#       response()$statistics |>
#         tibble::as_tibble() |>
#         tidyr::unnest(
#           cols = c(
#             first_rain,
#             last_rain,
#             total_rainfall,
#             avg_rainfall_intensity,
#             peak_5_min_rainfall_intensity,
#             peak_10_min_rainfall_intensity
#           )
#         ) |>
#         dplyr::arrange(first_rain) |>
#         dplyr::mutate(
#           first_rain = lubridate::as_datetime(first_rain),
#           last_rain = lubridate::as_datetime(last_rain),
#           event = dplyr::row_number()
#         ) |>
#         dplyr::select(
#           event,
#           first_rain,
#           last_rain,
#           total_rainfall,
#           avg_rainfall_intensity,
#           peak_5_min_rainfall_intensity,
#           peak_10_min_rainfall_intensity
#         )
#     } else if (input$analysistype == 'Flow Analysis'){
#       my_content <- response()$statistics
#       my_content <- lapply(
#         seq_along(my_content), function(i) if(names(my_content)[i] %in% c('inflow1','inflow2','bypass','outflow') ){
#           my_content[[i]] %>% as_tibble() %>% tidyr::unnest()    %>%
#             mutate(
#               flow_type = names(my_content)[i],
#               runoff_duration = round(runoff_duration, 1),
#               peak_flow_rate = round(peak_flow_rate, 1),
#               runoff_volume = round(runoff_volume, 1)
#             ) %>%
#             select(flow_type, start_time, peak_flow_rate, runoff_duration, runoff_volume) %>%
#             mutate(start_time = stringr::str_replace(start_time,"T"," "))
#
#         }
#         ) %>%
#         dplyr::bind_rows() %>%
#         mutate(flow_type = factor(flow_type, levels = c("inflow1", "inflow2", "bypass", "outflow"))) %>%
#         arrange(flow_type)
#
#
#     } else {
#       response()$statistics |>
#         tibble::as_tibble()
#     }
#   }) |>
#     bindEvent(input$submit)
#
#
#
#   output$percent_change = renderText({
#     if (input$analysistype == 'Flow Analysis'){
#       result <- ""
#         if (as.character(response()$statistics['percent_change_volume']) != 'NULL'){
#           pct_change_volume <- as.character(
#             round(
#               as.numeric(
#                 as.character(
#                   response()$statistics['percent_change_volume'][[1]]
#                 )
#               ),
#               1
#             )
#           )
#           result <- paste(result, glue("The percentage of change in volume is: {pct_change_volume}%."), sep = " ")
#         }
#         if (as.character(response()$statistics['percent_change_flow_rate']) != 'NULL'){
#           pct_change_flow_rate <- as.character(
#             round(
#               as.numeric(
#                 as.character(
#                   response()$statistics['percent_change_flow_rate'][[1]]
#                 )
#               ),
#               1
#             )
#           )
#           result <- paste(result, glue("The percentage of change in flow rate is: {pct_change_flow_rate}%"), sep = " ")
#         }
#       result
#     }
#   }) |> bindEvent(input$submit)
#
#   output$stats <- DT::renderDataTable({
#     if (input$analysistype == 'Rainfall Analysis'){
#       data <- statistics() |>
#         dplyr::select(-last_rain) |>
#         dplyr::mutate(
#           first_rain = format(as.POSIXct(first_rain), format = "%Y-%m-%d %H:%M:%S"),
#           avg_rainfall_intensity = round(avg_rainfall_intensity, 2),
#           peak_5_min_rainfall_intensity = round(peak_5_min_rainfall_intensity, 2),
#           peak_10_min_rainfall_intensity = round(peak_10_min_rainfall_intensity, 2),
#
#         )
#       if (input$rainfall_resolution == 0.1){
#         data <- data |> dplyr::rename(
#           `Event ID`= event,
#           `Storm Date`= first_rain,
#           `Total Rainfall (P) (mm)` = total_rainfall,
#           `Average Rainfall Intensity (mm/hr)` = avg_rainfall_intensity,
#           `Peak 5-min Rainfall Intensity (mm/hr)` = peak_5_min_rainfall_intensity,
#           `Peak 10-min Rainfall Intensity (mm/hr)` = peak_10_min_rainfall_intensity
#           )
#       } else {
#         data <- data |> dplyr::rename(
#           `Event ID`= event,
#           `Storm Date`= first_rain,
#           `Total Rainfall (P) (inches)` = total_rainfall,
#           `Average Rainfall Intensity (inch/hr)` = avg_rainfall_intensity,
#           `Peak 5-min Rainfall Intensity (inch/hr)` = peak_5_min_rainfall_intensity,
#           `Peak 10-min Rainfall Intensity (inch/hr)` = peak_10_min_rainfall_intensity
#         )
#       }
#       DT::datatable(
#         data,
#         caption = htmltools::tags$caption(
#           style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',
#           'Statistics of the rainfall data'
#         ),
#         rownames = FALSE
#       )
#     } else if (input$analysistype == 'Flow Analysis'){
#       data <- statistics()
#
#       DT::datatable(
#         data |>
#           mutate(start_time = format(as.POSIXct(start_time), tz = "America/Los_Angeles")) |>
#           #select(flow_type,start_time,peak_flow_rate,runoff_duration) |>
#           select(flow_type, peak_flow_rate, runoff_duration, runoff_volume) |>
#
#           setNames(
#             c(
#               "Type of flow",
#               #"Storm Date",
#               glue("Peak flow rate ({flow_volume_unit()}/{flow_time_unit()})"),
#               glue("Duration of runoff ({flow_time_unit()})"),
#               glue("Runoff volume ({flow_volume_unit()})")
#             )
#           )
#         ,
#         caption = htmltools::tags$caption(
#           style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',
#           'Statistics of the flow data'
#         ),
#         rownames = FALSE
#       )
#     } else {
#       data <- statistics()
#     }
#
#   }, selection = 'none') |> bindEvent(input$submit)
#
#
#
#
#   plotInput <- reactive({
#     if (input$analysistype == 'Rainfall Analysis'){
#       if (input$choose_graph == "All events"){
#         cumulative_rain <- data_input() |>
#           dplyr::mutate(
#             cumsum = cumsum(rain),
#             hours = lubridate::time_length(datetime - datetime[1], unit = "hour")
#           )
#
#         ggplot(cumulative_rain, aes(x = hours, y = cumsum)) +
#           geom_line() +
#           labs(x = "Elapsed hours from the first rain tip", y = glue("Cumulative rainfall ({rain_unit()})"), title = "Total cumulative rainfall")
#
#       } else {
#         cumulative_rain <- data_input() |>
#           dplyr::mutate(dummy = TRUE) |>
#           dplyr::left_join(
#             statistics() |>
#               dplyr::mutate(dummy = TRUE) |>
#               dplyr::select(dummy, event, first_rain, last_rain),
#             multiple = 'all'
#           ) |>
#           dplyr::group_by(event) |>
#           dplyr::filter(
#             datetime >= first_rain,
#             datetime <= last_rain,
#             event == as.numeric(input$choose_graph)
#           ) |>
#           dplyr::select(-dummy) |>
#           dplyr::arrange(datetime) |>
#           dplyr::mutate(
#             cumsum = cumsum(rain),
#             hours = lubridate::time_length(datetime - datetime[1], unit = "hour")
#           )
#         ggplot(cumulative_rain, aes(x = hours, y = cumsum)) +
#           geom_line() +
#           labs(
#             x = "Elapsed hours from the first rain tip",
#             y = glue("Cumulative rainfall ({rain_unit()})"),
#             title = "Cumulative rainfall per a rain event"
#           ) +
#           theme(
#             axis.text=element_text(size=18),
#             axis.title=element_text(size=18,face="bold"),
#             legend.title=element_text(size=18),
#             legend.text=element_text(size=18)
#           )
#
#       }
#     } else if (input$analysistype == 'Flow Analysis'){
#
#       flow_df <- data_input()
#
#       flow_df <- bind_rows(
#         lapply(
#           seq_along(flow_df),
#           function(i) {
#             flow_df[[i]] %>% mutate(flow_type=names(flow_df)[i]  )
#           }
#         )
#       ) %>% filter(flow_type %in% input$choose_graph) %>%
#         mutate(hours = lubridate::time_length(datetime - datetime[1], unit = "minute"))
#
#       ggplot(flow_df, aes(x = hours, y = flow, colour = flow_type)) +
#       geom_line() +
#       ggtitle("Hydrograph") +
#       theme(
#         axis.text=element_text(size=18),
#         axis.title=element_text(size=18,face="bold"),
#         legend.title=element_text(size=18),
#         legend.text=element_text(size=18),
#         plot.title = element_text(size = 18, face = "bold")
#       ) +
#       labs(
#         x=glue("Elapsed time from the start time ({flow_time_unit()})"),
#         y=glue("Flow rate ({flow_volume_unit()}/{flow_time_unit()})")
#       )
#
#
#     } else {
#       flow <- data_input()
#       ggplot(flow, aes(x = datetime, y = flow)) + geom_line()
#     }
#
#     })
#
#
#   output$rain_flow_plot <- renderPlot({
#     if (input$analysistype == 'Rainfall Analysis'){
#
#       plotInput() +
#         coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
#     } else if (input$analysistype == 'Flow Analysis'){
#
#       plotInput() +
#         coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
#
#     } else {
#       plotInput()
#     }
#
#   }) |> bindEvent(input$choose_graph, input$submit, input$plot1_dblclick)
#
#
#   output$download_rainfall_template <- downloadHandler(
#     filename = function() {
#       paste("rainfall_template", ".xlsx", sep = "")
#     },
#     content = function(file) {
#       template_path <- "templates/rainfall_template.xlsx"
#       file.copy(template_path, file, overwrite = TRUE)
#     }
#   )
#
#
#   output$download_demo_1min <- downloadHandler(
#     filename = function() {
#       paste("demo_data_1min", ".xlsx", sep = "")
#     },
#     content = function(file) {
#       data <- readxl::read_excel("demo_data/1min.xlsx")
#       writexl::write_xlsx(data, file)
#     }
#   )
#
#
#   output$download_demo_tt <- downloadHandler(
#     filename = function() {
#       paste("demo_data_timeoftips", ".xlsx", sep = "")
#     },
#     content = function(file) {
#       data <- readxl::read_excel("demo_data/timeoftips.xlsx")
#       writexl::write_xlsx(data, file)
#     }
#   )
#
#   output$download_flow_template <- downloadHandler(
#     filename = function() {
#       paste("flow_template", ".xlsx", sep = "")
#     },
#     content = function(file) {
#       template_path <- "templates/flow_template.xlsx"
#       file.copy(template_path, file, overwrite = TRUE)
#     }
#   )
#
#   output$download_demo_flow <- downloadHandler(
#     filename = function() {
#       paste("demo_data_flow", ".xlsx", sep = "")
#     },
#     content = function(file) {
#       template_path <- "demo_data/flowrate.xlsx"
#       file.copy(template_path, file, overwrite = TRUE)
#     }
#   )
#
#   output$download_demo_rainfall_flow <- downloadHandler(
#     filename = function() {
#       paste("demo_data_rainfall_flow", ".xlsx", sep = "")
#     },
#     content = function(file) {
#       data <- readxl::read_excel("demo_data/1min-and-flow.xlsx")
#       writexl::write_xlsx(data, file)
#     }
#   )
#
#   output$download_summary <- downloadHandler(
#     filename = function() {
#       paste("summary_table", ".csv", sep = "")
#     },
#     content = function(file) {
#       write.csv(statistics(), file, row.names = FALSE)
#     }
#   )
#
#
#   output$download_plot <- downloadHandler(
#     filename = function() {
#       paste("downloaded_plot", ".png", sep = "")
#     },
#     content = function(file) {
#       ggsave(file, plot = plotInput() + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE), device = "png")
#     }
#   )
#   
#   
# 
# 
#   
#   
# }