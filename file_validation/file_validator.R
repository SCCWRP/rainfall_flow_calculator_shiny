is_correct_filetype <- function(file) {
  if (tools::file_ext(file$datapath) == "xlsx") {
    return(NULL)
  }
  else {
    return("File must be a .xlsx file. Please use the template provided in the Instructions tab.")
  }
}

has_five_sheets <- function(file) {
  if (length(readxl::excel_sheets(file$datapath)) == 5) {
    return(NULL)
  }
  else {
    return("Incorrect number of sheets. Please use the provided template.")
  }
}

has_two_columns <- function(file, analysis_type) {
  sheets <- readxl::excel_sheets(file$datapath)[-1]
  tmp <- data.frame(sheet = sheets, rows = NA, cols = NA)
  
  for (sheet in sheets) {
    data <- readxl::read_excel(file$datapath, sheet = sheet)
    tmp[tmp$sheet == sheet, c("rows", "cols")] <- dim(data)
  }
  tmp$valid <- tmp$cols == 2

  if (all(tmp$valid)) {
    return(NULL)
  }
  else {
      return(glue::glue("Incorrect number of columns in sheet(s) {paste0(tmp$sheet[tmp$valid == FALSE], collapse = ', ')}. Sheet should have exactly 2 columns: datetime and {analysis_type} measurements."))
  }
}

has_no_missing_values <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[-1]
  tmp <- data.frame(sheet = sheets, rows = NA, not_missing = NA)
  
  for (sheet in sheets) {
    data <- readxl::read_excel(file$datapath, sheet = sheet)
    tmp[tmp$sheet == sheet, "rows"] <- nrow(data)
    tmp[tmp$sheet == sheet, "not_missing"] <- all(!is.na(data))
  }
  tmp$valid <- (tmp$rows > 0 & tmp$not_missing) | (tmp$rows == 0)

  if (all(tmp$valid)) {
    return(NULL)
  }
  else {
    return(glue::glue("Missing values present on sheet(s) {paste0(tmp$sheet[tmp$valid == FALSE], collapse = ', ')}."))
  }
}

has_no_negative_values <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[-1]
  tmp <- data.frame(sheet = sheets, rows = NA, positive = NA)
  
  for (sheet in sheets) {
    data <- readxl::read_excel(file$datapath, sheet = sheet)
    tmp[tmp$sheet == sheet, "rows"] <- nrow(data)
    tmp[tmp$sheet == sheet, "positive"] <- all(data >= 0)
  }
  tmp$valid <- (tmp$rows > 0 & tmp$positive) | (tmp$rows == 0)
  
  if (all(tmp$valid)) {
    return(NULL)
  }
  else {
    return(glue::glue("Negative values present on sheet(s) {paste0(tmp$sheet[tmp$valid == FALSE], collapse = ', ')}."))
  }
}

has_correct_date_format <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[-1]
  tmp <- data.frame(sheet = sheets, rows = NA_integer_, date_type = "")
  
  for (sheet in sheets) {
    data <- readxl::read_excel(file$datapath, sheet = sheet)
    tmp[tmp$sheet == sheet, "date_type"] <- paste(purrr::map(data, class)[[1]], collapse = " ")
    tmp[tmp$sheet == sheet, "rows"] <- nrow(data)
  }
  tmp$valid <- (tmp$date_type == "POSIXct POSIXt") | (tmp$rows == 0 & tmp$date_type == "logical")
  
  if (all(tmp$valid)) {
    return(NULL)
  }
  else {
    return(glue::glue("Incorrect date format on sheet(s) {paste0(tmp$sheet[tmp$valid == FALSE], collapse = ', ')}. Please use the template provided in the Instructions tab."))
  }
}

has_correct_measurement_format <- function(file, sheet) {
  sheets <- readxl::excel_sheets(file$datapath)[-1]
  tmp <- data.frame(sheet = sheets, rows = NA_integer_, measure_type = "")
  
  for (sheet in sheets) {
    data <- readxl::read_excel(file$datapath, sheet = sheet)
    tmp[tmp$sheet == sheet, "measure_type"] <- paste(purrr::map(data, class)[[2]], collapse = " ")
    tmp[tmp$sheet == sheet, "rows"] <- nrow(data)
  }
  tmp$valid <- (tmp$measure_type == "numeric") | (tmp$rows == 0 & tmp$measure_type == "logical")
  
  if (all(tmp$valid)) {
    return(NULL)
  }
  else {
    return(glue::glue("Incorrect measurement format on sheet(s) {paste0(tmp$sheet[tmp$valid == FALSE], collapse = ', ')}. Make sure all measurements have a numeric format."))
  }
}

has_headers <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[-1]
  tmp <- data.frame(sheet = readxl::excel_sheets(file$datapath), header1 = NA, header2 = NA)
  
  for (sheet in sheets) {
    data <- readxl::read_excel(file$datapath, sheet = sheet, range = "A1:B1") |> colnames()
    tmp[tmp$sheet == sheet, c("header1", "header2")] <- c(data[1], data[2])
  }
  # if any column headers can be coerced to numeric then assume input data is missing headers
  tmp$valid <- suppressWarnings(is.na(as.numeric(tmp$header1)) & is.na(as.numeric(tmp$header2)))

  if (all(tmp$valid)) {
    return(NULL)
  }
  else {
    return(glue::glue("Missing or incorrectly-formatted header(s) on sheet(s) {paste0(tmp$sheet[tmp$valid == FALSE, 'sheet'], collapse = ', ')}. Please add headers to the data."))
  }
}

has_at_least_inflow_outflow <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[-1]
  tmp <- data.frame(sheet = sheets, rows = NA)
  
  for (sheet in sheets) {
    data <- readxl::read_excel(file$datapath, sheet = sheet)
    tmp[tmp$sheet == sheet, "rows"] <- nrow(data)
  } 
  
  if (tmp[tmp$sheet == "inflow1", "rows"] > 0 & tmp[tmp$sheet == "outflow", "rows"] > 0) {
    return(NULL)
  }
  else {
    return(glue::glue("Data sheet must include at least inflow1 and outflow data."))
  }
}

has_valid_outflow_time <- function(file) {
  sheets <- readxl::excel_sheets(file$datapath)[-1]
  tmp <- data.frame(sheet = sheets, first_timestamp = NA, first_outflow_timestamp = NA)
  
  for (sheet in sheets) {
    data <- readxl::read_excel(file$datapath, sheet = sheet, range = "A1:A2")
    tmp[tmp$sheet == sheet, "first_timestamp"] <- ifelse(is.na(data[1, 1]), 0, data[1, 1])
  }
  tmp$valid <- tmp$first_timestamp <= tmp[tmp$sheet == "outflow", "first_timestamp"]
  
  if (all(tmp$valid)) {
    return(NULL)
  }
  else {
    return(glue::glue("Invalid outflow timestamps. The first outflow timestamp should be after all inflow and/or bypass timestamps."))
  }
}
