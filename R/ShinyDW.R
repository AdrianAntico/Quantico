#' @title Shiny.DW.DeleteColumns
#'
#' @description server.R observeEvent() for concatenating columns in a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList CodeList from app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.DeleteColumns <- function(input,output,session,DataList,CodeList,TabCount = 5L, CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Pull in values
  if(Debug) print('Shiny.DW.DeleteColumns')
  SelectData <- DataList[[DataMuse:::ReturnParam(xx = tryCatch({input$DeleteVariables_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)]][['data']]
  Cols <- DataMuse:::ReturnParam(xx = tryCatch({input$DeleteVariables}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  NewName <- DataMuse:::ReturnParam(xx = tryCatch({input$DeleteVariables_NewName}, error = function(x) NULL), Type = 'character', Default = "Overwrite", Debug = Debug)

  # Dispatch
  if(Debug) print(Cols)

  # Run code
  if(Debug) print('Shiny.DW.DeleteColumns 2')
  if(NewName == 'Overwrite') {
    if(all(Cols %in% names(SelectData))) data.table::set(SelectData, j = c(eval(Cols)), value = NULL)
    DataList[[SelectData]][['data']] <- SelectData
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
  } else {
    temp <- data.table::copy(SelectData)
    if(all(Cols %in% names(temp))) data.table::set(temp, j = c(eval(Cols)), value = NULL)
    DataList[[NewName]][['data']] <- temp
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, NewName)}, error = function(x) DataList)
  }

  # Code
  if(Debug) print('Shiny.DW.DeleteColumns 5')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Delete Columns\n",
    "SelectData <- ", DataMuse:::CEP(input$DeleteVariables_SelectData), "\n",
    "Cols <- c(", DataMuse:::ExpandText(Cols), ")\n",
    "data.table::set(DataList[[SelectData]], j = c(Cols), value = NULL)\n"))

  # Update meta
  DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)

  for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
  for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.ConcatenateColumns
#'
#' @description server.R observeEvent() for concatenating columns in a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.ConcatenateColumns <- function(input,output,session,DataList,CodeList,TabCount=5,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Concatenate columns has begun..', value = 0, {

    if(Debug) print('Shiny.DW.ConcatenateColumns')

    # Pull in values
    Cols <- DataMuse:::ReturnParam(xx = tryCatch({input$ConcatColumns}, error = function(x) NULL), VarName = 'ConcatColumns', Type = 'character', Default = NULL, Debug = Debug)
    SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$ConcatColumns_SelectData}, error = function(x) NULL), VarName = 'ConcatColumns_SelectData', Type = 'character', Default = NULL, Debug = Debug)

    # Run code
    if(Debug) print('Shiny.DW.ConcatenateColumns 2')
    DataList[[SelectData]][['data']][, paste0(Cols, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(Cols)]

    # Code
    if(Debug) print('Shiny.DW.ConcatenateColumns 5')
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Concatenate Columns\n",
      "Cols <- c(", DataMuse:::ExpandText(Cols), ")\n",
      "DataList[[", DataMuse:::CEP(SelectData), "]][, paste0(Cols, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(Cols)]\n"))

    # Update meta
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)

    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.DW.RenameColumns
#'
#' @description server.R observeEvent() for concatenating columns in a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.RenameColumns <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Concatenate columns has begun..', value = 0, {

    if(Debug) print('Shiny.DW.RenameColumns')

    # Pull in values
    RenameColumn <- DataMuse:::ReturnParam(xx = tryCatch({input$RenameColumns_RenameColumn}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$RenameColumns_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    NewName <- DataMuse:::ReturnParam(xx = tryCatch({input$RenameColumns_NewName}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

    # Run code
    if(Debug) print('Shiny.DW.RenameColumns 2')
    data.table::setnames(x = DataList[[SelectData]][['data']], old = c(RenameColumn), new = c(NewName), skip_absent = TRUE)

    # Code
    if(Debug) print('Shiny.DW.RenameColumns 5')
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Rename Columns\n",
      "data.table::setnames(\n  ",
      "x = DataList[[", DataMuse:::CEP(SelectData), "]],\n  ",
      "old = ", DataMuse:::ExpandText(RenameColumn), ",\n  ",
      "new = ", DataMuse:::ExpandText(NewName), ", skip_absent = TRUE)\n"))

    # Update meta
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.DW.TimeTrendColumn
#'
#' @description server.R observeEvent() for adding a time trend column to a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.TimeTrendColumn <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Concatenate columns has begun..', value = 0, {

    # Pull in values
    SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$TimeTrendColumn_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    TimeTrendOrder <- DataMuse:::ReturnParam(xx = tryCatch({input$TimeTrendColumn_TimeTrendOrder}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    DateColumn <- DataMuse:::ReturnParam(xx = tryCatch({input$TimeTrendColumn_DateColumn}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    NewName <- DataMuse:::ReturnParam(xx = tryCatch({input$TimeTrendColumn_NewName}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(NewName) == 0 || NewName == "" || is.na(NewName)) NewName <- "TimeTrend"
    GroupVars <- DataMuse:::ReturnParam(xx = tryCatch({input$TimeTrendColumn_GroupVars}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

    # Time Trend
    if(TimeTrendOrder == 'Ascending') {
      TimeTrendOrder <- 1L
    } else {
      TimeTrendOrder <- -1L
    }

    # Run code
    if(Debug) print('Shiny.DW.TimeTrendColumn 2')
    if(length(GroupVars) > 0L && GroupVars != "") {

      # Sort then add variable
      data.table::setorderv(x = DataList[[SelectData]][['data']], cols = c(GroupVars, DateColumn), order = rep(TimeTrendOrder, length(paste0(GroupVars, DateColumn, collapse = ","))))
      DataList[[SelectData]][['data']][, eval(NewName) := seq_len(.N), by = c(eval(GroupVars))]


      # Code
      if(Debug) print('Shiny.DW.TimeTrendColumn 5')
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Time Trend\n",
        "data.table::setorderv(x = DataList[[", DataMuse:::CEP(SelectData), "]], cols = ", DataMuse:::ExpandText(paste0(GroupVars, DateColumn, collapse = ",")), ", order = ", DataMuse:::ExpandText(rep(TimeTrendOrder, length(paste0(GroupVars, DateColumn, collapse = ",")))), "\n",
        "DataList[[", DataMuse:::CEP(SelectData), "]][, eval(", DataMuse:::CEP(NewName), ") := seq_len(.N), by = c(", DataMuse:::CEP(GroupVars), ")]\n"))

    } else {

      # Time Trend
      data.table::setorderv(x = DataList[[SelectData]][['data']], cols = c(DateColumn), order = rep(TimeTrendOrder, length(DateColumn)))
      DataList[[SelectData]][['data']][, eval(NewName) := seq_len(.N)]

      # Code
      if(Debug) print('Shiny.DW.TimeTrendColumn 5')
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Time Trend\n",
        "data.table::setorderv(x = DataList[[", DataMuse:::CEP(SelectData), "]], cols = ", DataMuse:::CEP(DateColumn), ", order = ", DataMuse:::CEP(TimeTrendOrder), "), \n",
        "DataList[[", DataMuse:::CEP(SelectData), "]][, eval(", DataMuse:::CEP(NewName), ") := seq_len(.N)]\n"))
    }

    # Update meta
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.DW.TypeCast
#'
#' @description server.R observeEvent() for adding a time trend column to a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.TypeCast <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Shiny.DW.TypeCast has begun..', value = 0, {

    if(Debug) print('Shiny.DW.TypeCast')

    # Pull in values
    SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$TypeCast_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    Numeric <- DataMuse:::ReturnParam(xx = tryCatch({input$TypeCast_Numeric}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    Integer <- DataMuse:::ReturnParam(xx = tryCatch({input$TypeCast_Integer}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    Character <- DataMuse:::ReturnParam(xx = tryCatch({input$TypeCast_Character}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    Factor <- DataMuse:::ReturnParam(xx = tryCatch({input$TypeCast_Factor}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    Logical <- DataMuse:::ReturnParam(xx = tryCatch({input$TypeCast_Logical}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    Date <- DataMuse:::ReturnParam(xx = tryCatch({input$TypeCast_Date}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    ExcelDate <- DataMuse:::ReturnParam(xx = tryCatch({input$TypeCast_ExcelDate}, error = function(x) NULL), Type = "character", Default = NULL)
    Posix <- DataMuse:::ReturnParam(xx = tryCatch({input$TypeCast_Posix}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

    # Numeric
    if(length(Numeric) > 0L) {
      DataList[[SelectData]][, paste0(Numeric) := lapply(.SD, as.numeric), .SDcols = c(Numeric)]
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Convert to numeric\n",
        "Numeric <- ", DataMuse:::ExpandText(Numeric), "\n",
        "DataList[[", DataMuse:::CEP(SelectData), "]] <- DataList[[", DataMuse:::CEP(SelectData), "]][, paste0(Numeric) := lapply(.SD, as.numeric), .SDcols = c(Numeric)]\n"))
    }

    # Excel Date
    if(length(ExcelDate) > 0L) {
      DataList[[SelectData]][["data"]][, eval(ExcelDate) := openxlsx::convertToDate(get(ExcelDate))]
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Convert to numeric\n",
        "Numeric <- ", DataMuse:::ExpandText(Numeric), "\n",
        "DataList[[", DataMuse:::CEP(SelectData), "]][, ", DataMuse::CEP(eval(ExcelData)), " := openxlsx::convertToDate(get(", DataMuse:::CEP(ExcelDate), "))]\n"))
    }

    # Integer
    if(length(Integer) > 0L) {
      DataList[[SelectData]][['data']][, paste0(Integer) := lapply(.SD, as.integer), .SDcols = c(Integer)]
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Convert to integer\n",
        "Integer <- ", DataMuse:::ExpandText(Integer), "\n",
        "DataList[[", DataMuse:::CEP(SelectData), "]][, paste0(Integer) := lapply(.SD, as.integer), .SDcols = c(Integer)]\n"))
    }

    # Character
    if(length(Character) > 0L) {
      DataList[[SelectData]][['data']][, paste0(Character) := lapply(.SD, as.character), .SDcols = c(Character)]
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Convert to character\n",
        "Character <- ", DataMuse:::ExpandText(Character), "\n",
        "DataList[[", DataMuse:::CEP(SelectData), "]][, paste0(Character) := lapply(.SD, as.character), .SDcols = c(Character)]\n"))
    }

    # Factor
    if(length(Factor) > 0L) {
      DataList[[SelectData]][['data']][, paste0(Factor) := lapply(.SD, as.factor), .SDcols = c(Factor)]
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Convert to factor\n",
        "Factor <- ", DataMuse:::ExpandText(Factor), "\n",
        "DataList[[", DataMuse:::CEP(SelectData), "]][, paste0(Factor) := lapply(.SD, as.factor), .SDcols = c(Factor)]\n"))
    }

    # Logical
    if(length(Logical) > 0L) {
      DataList[[SelectData]][['data']][, paste0(Logical) := lapply(.SD, as.logical), .SDcols = c(Logical)]
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Convert to logical\n",
        "Logical <- ", DataMuse:::ExpandText(Logical), "\n",
        "DataList[[", DataMuse:::CEP(SelectData), "]][, paste0(Logical) := lapply(.SD, as.logical), .SDcols = c(Logical)]\n"))
    }

    # Date
    if(length(Date) > 0L) {
      for(d in Date) {
        x <- DataList[[SelectData]][['data']][1L, get(d)]
        x1 <- lubridate::guess_formats(x, orders = c('mdY', 'BdY', 'Bdy', 'bdY', 'bdy', 'mdy', 'dby', 'Ymd', 'Ydm', 'dmy'))
        DataList[[SelectData]][['data']][, eval(d) := as.Date(get(d), tryFormats = x1)]
      }
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Convert to date\n",
        "for(d in Date) {\n  ",
        "x <- DataList[[", DataMuse:::CEP(SelectData), "]][1L, get(d)]\n  ",
        "x1 <- lubridate::guess_formats(x, orders = c('mdY', 'BdY', 'Bdy', 'bdY', 'bdy', 'mdy', 'dby', 'Ymd', 'Ydm', 'dmy'))\n  ",
        "DataList[[", DataMuse:::CEP(SelectData), "]][, eval(d) := as.Date(get(d), tryFormats = ", DataMuse:::CEP(x1), "]\n"))
    }

    # Posix
    if(length(Posix) > 0L) {
      DataList[[SelectData]][['data']][, paste0(Posix) := lapply(.SD, as.POSIXct), .SDcols = c(Posix)]
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Convert to posixct\n",
        "Posix <- ", DataMuse:::ExpandText(Posix), "\n",
        "DataList[[", DataMuse:::CEP(SelectData), "]][, paste0(Posix) := lapply(.SD, as.POSIXct), .SDcols = c(Posix)]\n"))
    }

    # Update meta
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.DW.SampleData
#'
#' @description Sample from a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.SampleData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  # Vars
  SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$SampleData_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  data <- DataList[[SelectData]][['data']]
  NewName <- DataMuse:::ReturnParam(xx = tryCatch({input$SampleData_NewName}, error = function(x) NULL), Type = 'character', Default = 'Overwrite', Debug = Debug)
  StratifyColumns <- DataMuse:::ReturnParam(xx = tryCatch({input$SampleData_StratifyColumns}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Rate <- DataMuse:::ReturnParam(xx = tryCatch({input$SampleData_Rate}, error = function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)

  # Procedure
  if(length(StratifyColumns) == 0L) {
    temp <- data[order(runif(.N))][seq_len(floor(.N * Rate))]
    if(NewName == 'Overwrite') {
      DataList[[SelectData]][['data']] <- temp
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
    } else {
      DataList[[NewName]][['data']] <- temp
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, NewName)}, error = function(x) DataList)
    }
    DataList <<- DataList
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Sample Data\n",
      "NewName <- ", DataMuse:::CEP(NewName), "\n",
      "StratifyColumns <- ", DataMuse:::CEP(StratifyColumns), "\n",
      "Rate <- ", DataMuse:::CEPP(Rate), "\n",
      "DataList[[", if(NewName == 'Overwrite') DataMuse:::CEP(SelectData) else DataMuse:::CEP(NewName), "]] <- DataList[[", DataMuse:::CEP(SelectData), "]][order(runif(.N))][seq_len(floor(.N * ", DataMuse:::CEPP(Rate), "]\n"
    ))
  } else {
    for(i in 1:20) print("ADRIAN YOOOOO")
    temp <- data[, ID__temp_col := runif(.N), by = c(StratifyColumns)][
      , Max_ID__temp_col := max(ID__temp_col), by = c(StratifyColumns)][
        order(ID__temp_col)][
          ID__temp_col <= eval(Rate) * Max_ID__temp_col][
            , ID__temp_col := NULL][
              , Max_ID__temp_col := NULL]
    if(NewName == 'Overwrite') {
      DataList[[SelectData]][['data']] <- temp
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
    } else {
      DataList[[NewName]][['data']] <- temp
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, NewName)}, error = function(x) DataList)
      for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
      for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    }
    DataList <<- DataList
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Sample Data\n",
      "SelectData <- ", DataMuse:::CEP(SelectData), "\n",
      "data <- DataList[[SelectData]]\n",
      "NewName <- ", DataMuse:::CEP(NewName), "\n",
      "StratifyColumns <- ", DataMuse:::CEP(StratifyColumns), "\n",
      "Rate <- ", DataMuse:::CEPP(Rate), "\n",
      "DataList[[", DataMuse:::CEP(NewName), "]] <- data[, ID__temp_col := runif(.N), by = c(StratifyColumns)][\n  ",
      ", Max_ID__temp_col := max(ID__temp_col), by = c(StratifyColumns)][\n    ",
      "order(ID__temp_col)][\n      ",
      "ID_temp_col <= eval(Rate) * Max_ID__temp_col][\n        ",
      ", ID__temp_col := NULL][\n          ",
      ", Max_ID__temp_col := NULL]\n"
    ))
  }

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.SubsetData
#'
#' @description Subset a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.SubsetData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {
  SubsetData_SelectData <- DataMuse:::ReturnParam(xx=tryCatch({input[['SubsetData_SelectData']]}, error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_NewName <- DataMuse:::ReturnParam(xx=tryCatch({input[['SubsetData_NewName']]}, error=function(x) NULL), Type='character', Default='Overwrite')
  SubsetData_FilterVariable1  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterVariable1')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterVariable2  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterVariable2')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterVariable3  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterVariable3')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterVariable4  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterVariable4')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterLogic1     <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterLogic1')]]},       error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterLogic2     <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterLogic2')]]},       error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterLogic3     <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterLogic3')]]},       error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterLogic4     <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterLogic4')]]},       error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterValue_1_1  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterValue_1_1')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterValue_1_2  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterValue_1_2')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterValue_2_1  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterValue_2_1')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterValue_2_2  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterValue_2_2')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterValue_3_1  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterValue_3_1')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterValue_3_2  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterValue_3_2')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterValue_4_1  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterValue_4_1')]]},    error=function(x) NULL), Type='character', Default=NULL)
  SubsetData_FilterValue_4_2  <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SubsetData_FilterValue_4_2')]]},    error=function(x) NULL), Type='character', Default=NULL)
  if(Debug) print(length(SubsetData_FilterVariable1) != 0L)
  Var1 <- length(SubsetData_FilterVariable1) > 0L
  Var2 <- length(SubsetData_FilterVariable2) > 0L
  Var3 <- length(SubsetData_FilterVariable3) > 0L
  Var4 <- length(SubsetData_FilterVariable4) > 0L
  if(Debug) {
    print(Var1)
    print(Var2)
    print(Var3)
    print(Var4)
  }
  if(any(Var1,Var2,Var3,Var4)) {
    for(i in seq_len(sum(Var1,Var2,Var3,Var4))) {
      DataList[[if(SubsetData_NewName == 'Overwrite') SubsetData_SelectData else SubsetData_NewName]][['data']] <- DataMuse:::FilterLogicData(
        DataList[[if(i == 1 || SubsetData_NewName == 'Overwrite') SubsetData_SelectData else SubsetData_NewName]][['data']],
        FilterLogic    = get(paste0('SubsetData_FilterLogic', i)),
        FilterVariable = get(paste0('SubsetData_FilterVariable', i)),
        FilterValue    = get(paste0('SubsetData_FilterValue_',i,'_1')),
        FilterValue2   = get(paste0('SubsetData_FilterValue_',i,'_2')),
        Debug          = Debug)
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Subset Data\n",
        "if(!exists('DataList')) DataList <- list()\n",
        "DataList[[", DataMuse:::CEP(if(SubsetData_NewName == 'Overwrite') SubsetData_SelectData else SubsetData_NewName),"]] <- DataMuse:::FilterLogicData(\n  ",
        "DataList[[", DataMuse:::CEP(SubsetData_SelectData),"]],\n  ",
        "FilterLogic = ",DataMuse:::CEP(get(paste0('SubsetData_FilterLogic',i))),",\n  ",
        "FilterVariable = ", DataMuse:::CEP(get(paste0('SubsetData_FilterVariable',i))),",\n  ",
        "FilterValue = ", DataMuse:::CEP(get(paste0('SubsetData_FilterValue_',i,'_1'))),",\n  ",
        "FilterValue2 = ", if(DataMuse:::CEP(get(paste0('SubsetData_FilterLogic',i))) %in% c('%in%','<','>','<=','>=')) "NULL" else DataMuse:::CEP(get(paste0('SubsetData_FilterValue_',i,'_2'))),")\n"))
    }
  }

  # Display data
  if(Debug) print('DW Subset Data')
  if(SubsetData_NewName == 'Overwrite') {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SubsetData_SelectData)}, error = function(x) DataList)
  } else {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SubsetData_NewName)}, error = function(x) DataList)

    # Add data to DataOutputSelection Page
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  }

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.AggregateData
#'
#' @description Aggregate a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.AggregateData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  # Params
  AggregateData_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$AggregateData_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  AggregateData_NewName <- DataMuse:::ReturnParam(xx = tryCatch({input$AggregateData_NewName}, error = function(x) NULL), Type = 'character', Default = 'Overwrite', Debug = Debug)
  Aggregate_Columns <- DataMuse:::ReturnParam(xx = tryCatch({input$Aggregate_Columns}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Aggregate_ByVariables <- DataMuse:::ReturnParam(xx = tryCatch({input$Aggregate_ByVariables}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Aggregate_Stat <- DataMuse:::ReturnParam(xx = tryCatch({input$Aggregate_Stat}, error = function(x) NULL), Type = 'character', Default = 'mean', Debug = Debug)
  Aggregate_DateVariable <- DataMuse:::ReturnParam(xx = tryCatch({input$Aggregate_DateVariable}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Aggregate_TimeAgg <- DataMuse:::ReturnParam(xx = tryCatch({input$Aggregate_TimeAgg}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # Time Agg Prep Work
  if(length(Aggregate_DateVariable) > 0L && length(Aggregate_TimeAgg) > 0L) {

    # Ensure DateVar is the correct class type
    date_class <- class(DataList[[AggregateData_SelectData]][['data']][[eval(Aggregate_DateVariable)]])[1L]
    if(!date_class %in% c("Date","POSIXct","POSIXt")) {
      if(Aggregate_TimeAgg %in% c("second", "minute", "hour")) {
        DataList[[AggregateData_SelectData]][['data']][, eval(Aggregate_DateVariable) := as.POSIXct(get(Aggregate_DateVariable))]
      } else {
        DataList[[AggregateData_SelectData]][['data']][, eval(Aggregate_DateVariable) := as.Date(get(Aggregate_DateVariable))]
      }
    }

    # Floor Date
    DataList[[AggregateData_SelectData]][['data']][, eval(Aggregate_DateVariable) := lubridate::floor_date(get(Aggregate_DateVariable), unit = eval(Aggregate_TimeAgg))]
  }

  # Update by-vars to include date
  Aggregate_ByVariables <- unique(c(Aggregate_ByVariables, Aggregate_DateVariable))

  # Agg
  if(Aggregate_Stat == 'mean') {
    temp <- DataList[[AggregateData_SelectData]][['data']][, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(Aggregate_Columns), keyby = c(Aggregate_ByVariables)]
    if(AggregateData_NewName == 'Overwrite') {
      DataList[[AggregateData_SelectData]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, mean, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    } else {
      DataList[[AggregateData_NewName]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_NewName), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, mean, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    }
  } else if(Aggregate_Stat == 'count') {
    temp <- DataList[[AggregateData_SelectData]][['data']][, list(Count = .N), keyby = c(Aggregate_ByVariables)]
    if(AggregateData_NewName == 'Overwrite') {
      DataList[[AggregateData_SelectData]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, list(Count = .N), keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    } else {
      DataList[[AggregateData_NewName]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_NewName), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, list(Count = .N), keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    }
  } else if(Aggregate_Stat == 'median') {
    temp <- DataList[[AggregateData_SelectData]][['data']][, lapply(.SD, median, na.rm = TRUE), .SDcols = c(Aggregate_Columns), keyby = c(Aggregate_ByVariables)]
    if(AggregateData_NewName == 'Overwrite') {
      DataList[[AggregateData_SelectData]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, median, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    } else {
      DataList[[AggregateData_NewName]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_NewName), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, median, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    }

  } else if(Aggregate_Stat == 'sd') {
    temp <- DataList[[AggregateData_SelectData]][['data']][, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(Aggregate_Columns), keyby = c(Aggregate_ByVariables)]
    if(AggregateData_NewName == 'Overwrite') {
      DataList[[AggregateData_SelectData]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, sd, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    } else {
      DataList[[AggregateData_NewName]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_NewName), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, sd, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    }
  } else if(Aggregate_Stat == 'min') {
    temp <- DataList[[AggregateData_SelectData]][['data']][, lapply(.SD, min, na.rm = TRUE), .SDcols = c(Aggregate_Columns), keyby = c(Aggregate_ByVariables)]
    if(AggregateData_NewName == 'Overwrite') {
      DataList[[AggregateData_SelectData]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, min, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    } else {
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_NewName), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, min, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    }
  } else if(Aggregate_Stat == 'max') {
    temp <- DataList[[AggregateData_SelectData]][['data']][, lapply(.SD, max, na.rm = TRUE), .SDcols = c(Aggregate_Columns), keyby = c(Aggregate_ByVariables)]
    if(AggregateData_NewName == 'Overwrite') {
      DataList[[AggregateData_SelectData]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, max, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    } else {
      DataList[[AggregateData_NewName]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_NewName), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, max, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    }
  } else if(Aggregate_Stat == 'first') {
    temp <- DataList[[AggregateData_SelectData]][['data']][, lapply(.SD, data.table::first, na.rm = TRUE), .SDcols = c(Aggregate_Columns), keyby = c(Aggregate_ByVariables)]
    if(AggregateData_NewName == 'Overwrite') {
      DataList[[AggregateData_SelectData]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, data.table::first, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    } else {
      DataList[[AggregateData_NewName]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_NewName), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, data.table::first, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    }
  } else if(Aggregate_Stat == 'last') {
    temp <- DataList[[AggregateData_SelectData]][['data']][, lapply(.SD, data.table::last, na.rm = TRUE), .SDcols = c(Aggregate_Columns), keyby = c(Aggregate_ByVariables)]
    if(AggregateData_NewName == 'Overwrite') {
      DataList[[AggregateData_SelectData]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, data.table::last, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    } else {
      DataList[[AggregateData_NewName]][['data']] <- temp
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
        "\n",
        "# Aggregate ", DataMuse:::CEP(AggregateData_SelectData), "\n",
        "DataList[[", DataMuse:::CEP(AggregateData_NewName), "]] <- DataList[[", DataMuse:::CEP(AggregateData_SelectData), "]][, lapply(.SD, data.table::last, na.rm = TRUE), .SDcols = ", DataMuse:::ExpandText(Aggregate_Columns), ", keyby = ", DataMuse:::ExpandText(Aggregate_ByVariables), "]\n"
      ))
    }
  }

  # Display data
  if(Debug) print('DW Aggregate Data')
  if(AggregateData_NewName == 'Overwrite') {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, AggregateData_SelectData)}, error = function(x) DataList)
  } else {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, AggregateData_NewName)}, error = function(x) DataList)

    # Add data to DataOutputSelection Page
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  }

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.JoinData
#'
#' @description Join two data.table's
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.JoinData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {
  JoinData_NewName         <- DataMuse:::ReturnParam(xx=tryCatch({input[['JoinData_NewName']]},         error=function(x) NULL), Type='character', Default='Overwrite')
  JoinData_SelectData1     <- DataMuse:::ReturnParam(xx=tryCatch({input[['JoinData_SelectData1']]},     error=function(x) NULL), Type='character', Default=NULL)
  JoinData_SelectData2     <- DataMuse:::ReturnParam(xx=tryCatch({input[['JoinData_SelectData2']]},     error=function(x) NULL), Type='character', Default=NULL)
  JoinData_JoinType        <- DataMuse:::ReturnParam(xx=tryCatch({input[['JoinData_JoinType']]},        error=function(x) NULL), Type='character', Default=NULL)
  JoinData_Cartesian       <- DataMuse:::ReturnParam(xx=tryCatch({input[['JoinData_Cartesian']]},       error=function(x) NULL), Type='logical',   Default=FALSE)
  JoinData_ByXVariables    <- DataMuse:::ReturnParam(xx=tryCatch({input[['JoinData_ByXVariables']]},    error=function(x) NULL), Type='character', Default=NULL)
  JoinData_ByYVariables    <- DataMuse:::ReturnParam(xx=tryCatch({input[['JoinData_ByYVariables']]},    error=function(x) NULL), Type='character', Default=NULL)

  # DataList <- list()
  # JoinData_SelectData1 <- "C:/Users/Bizon/Documents/GitHub/rappwd/FakeBevData.csv"
  # JoinData_SelectData2 <- "C:/Users/Bizon/Documents/GitHub/rappwd/FakeBevData.csv"
  # DataList[[JoinData_SelectData1]][["data"]] <- data.table::fread(JoinData_SelectData1, select = c("Customer","Date","Brand","Category","Beverage Flavor","Daily Liters","Daily Margin","Daily Revenue","Daily Units","MultiClass"))
  # DataList[["bla"]][["data"]] <- data.table::fread(JoinData_SelectData1, select = c("Customer","Date","Brand","Category","Beverage Flavor","ClassTarget","MultiClass"))
  # JoinData_ByXVariables <- c("Customer","Date","Brand","Category","Beverage Flavor","MultiClass")
  # JoinData_ByYVariables <- c("Customer","Date","Brand","Category","Beverage Flavor","MultiClass")
  # data.table::setkeyv(x = DataList[[JoinData_SelectData1]][["data"]], cols = c(JoinData_ByXVariables), physical = TRUE)
  # data.table::setkeyv(x = DataList[["bla"]][["data"]], cols = c(JoinData_ByYVariables), physical = TRUE)
  # left_names <- names(DataList[[JoinData_SelectData1]][["data"]])
  # right_names <- names(DataList[["bla"]][["data"]])
  # common_cols <- intersect(left_names,right_names)
  # if(identical(character(0),common_cols)) common_cols <- NULL
  # right_diff_cols <- DataMuse::CharNull(setdiff(left_names,right_names))
  # if(identical(character(0),right_names)) right_diff_cols <- NULL
  # left_diff_cols <- DataMuse::CharNull(setdiff(right_names,left_names))
  # if(identical(character(0),left_diff_cols)) left_diff_cols <- NULL

  # Define tables and create keys (index + sorting)
  left_table <- DataList[[JoinData_SelectData1]][['data']]
  right_table <- DataList[[JoinData_SelectData2]][['data']]
  data.table::setkeyv(x = left_table, cols = c(JoinData_ByXVariables), physical = TRUE)
  data.table::setkeyv(x = right_table, cols = c(JoinData_ByYVariables), physical = TRUE)
  left_names <- names(left_table)
  right_names <- names(right_table)
  left_diff_cols <- setdiff(right_names,left_names)
  if(identical(character(0),left_diff_cols)) left_diff_cols <- NULL

  # If duplicates in either (or both) datasets
  # allow.cartesian=TRUE: works even if not any duplications, so keep it added
  # left_table <- data.table::fread(file.choose()) # ModelData; keep Combined.. Date, and Regression Target
  # left_table <- left_table[, .SD, .SDcols = c("CombinedGroups","Date","Regression Target")]
  # right_table <- data.table::fread(file.choose()) # ModelData; keep Combined.. Date, and Binary Classification Target
  # right_table <- right_table[, .SD, .SDcols = c("CombinedGroups","Date","Binary Classification Target")]
  # left_table <- data.table::rbindlist(list(left_table, left_table))
  # JoinData_ByXVariables <- c("CombinedGroups","Date")
  # JoinData_ByYVariables <- c("CombinedGroups","Date")
  # data.table::setkeyv(x = left_table, cols = c(JoinData_ByXVariables), physical = TRUE)
  # data.table::setkeyv(x = right_table, cols = c(JoinData_ByYVariables), physical = TRUE)
  # left_names <- names(left_table)
  # right_names <- names(right_table)
  # left_diff_cols <- setdiff(right_names,left_names)

  # :: Tasks :: inner, left, full, anti, semi, roll back, roll forward
  # new_table <- left_table[right_table[, .SD, .SDcols = c(unique(c(JoinData_ByYVariables,left_diff_cols)))], nomatch = NULL, allow.cartesian = TRUE]
  # left_table[right_table, paste0(left_diff_cols) := mget(paste0('i.', left_diff_cols)), allow.cartesian = TRUE]
  # new_table <- merge(left_table, right_table, by.x = JoinData_ByXVariables, by.y = JoinData_ByYVariables, all = TRUE, allow.cartesian = TRUE)
  # new_table <- left_table[!right_table, allow.cartesian = TRUE]
  # new_table <- left_table[na.omit(left_table[right_table, which = TRUE], allow.cartesian = TRUE)]
  # new_table <- left_table[right_table, roll = -Inf, allow.cartesian = TRUE]
  # new_table <- left_table[right_table, roll = TRUE, allow.cartesian = TRUE]

  # Join
  if(JoinData_JoinType == 'inner') {
    print("Join 10")
    left_table <- left_table[right_table[, .SD, .SDcols = c(unique(c(JoinData_ByYVariables,left_diff_cols)))], nomatch = NULL, allow.cartesian = TRUE]
    print("Join 11")
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Join Data\n",
      "JoinData_ByXVariables <- ", DataMuse:::ExpandText(JoinData_ByXVariables), "\n",
      "JoinData_ByYVariables <- ", DataMuse:::ExpandText(JoinData_ByYVariables), "\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]], cols = c(JoinData_ByXVariables), physical = TRUE)\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], cols = c(JoinData_ByYVariables), physical = TRUE)\n",
      "left_names <- names(DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]])\n",
      "right_names <- names(DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]])\n",
      "left_diff_cols <- setdiff(right_names,left_names)\n",
      "if(identical(character(0),left_diff_cols)) left_diff_cols <- NULL\n",
      "DataList[[", if(JoinData_NewName == 'Overwrite') DataMuse:::CEP(JoinData_NewName) else DataMuse:::CEP(JoinData_SelectData1),"]] <- DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]][DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]][, .SD, .SDcols = c(unique(c(JoinData_ByXVariables,left_diff_cols)))], nomatch = NULL, allow.cartesian = TRUE]\n"))

  } else if(JoinData_JoinType == 'left') {
    left_table[right_table, paste0(left_diff_cols) := mget(paste0('i.', left_diff_cols)), allow.cartesian = TRUE]
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Join Data\n",
      "JoinData_ByXVariables <- ", DataMuse:::ExpandText(JoinData_ByXVariables), "\n",
      "JoinData_ByYVariables <- ", DataMuse:::ExpandText(JoinData_ByYVariables), "\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]], cols = c(JoinData_ByXVariables), physical = TRUE)\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], cols = c(JoinData_ByYVariables), physical = TRUE)\n",
      "left_names <- names(DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]])\n",
      "right_names <- names(DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]])\n",
      "left_diff_cols <- DataMuse::CharNull(setdiff(right_names,left_names))\n",
      "if(identical(character(0),left_diff_cols)) left_diff_cols <- NULL\n",
      "DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]][DataList[[",DataMuse:::CEP(JoinData_SelectData2),"]], paste0(left_diff_cols) := mget(paste0('i.', left_diff_cols)), allow.cartesian = TRUE]\n"))

  } else if(JoinData_JoinType == 'full') {
    left_table <- merge(left_table, right_table, by.x = JoinData_ByXVariables, by.y = JoinData_ByYVariables, all = TRUE, allow.cartesian = TRUE)
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Join Data\n",
      "JoinData_ByXVariables <- ", DataMuse:::ExpandText(JoinData_ByXVariables), "\n",
      "JoinData_ByYVariables <- ", DataMuse:::ExpandText(JoinData_ByYVariables), "\n",
      "DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]] <- merge(DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]], DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], by.x = JoinData_ByXVariables, by.y = JoinData_ByYVariables, all = TRUE, allow.cartesian = TRUE)\n"))

  } else if(JoinData_JoinType == 'anti') {
    left_table <- left_table[!right_table, allow.cartesian = TRUE]
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Join Data\n",
      "JoinData_ByXVariables <- ", DataMuse:::ExpandText(JoinData_ByXVariables), "\n",
      "JoinData_ByYVariables <- ", DataMuse:::ExpandText(JoinData_ByYVariables), "\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]], cols = c(JoinData_ByXVariables), physical = TRUE)\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], cols = c(JoinData_ByYVariables), physical = TRUE)\n",
      "DataList[[", if(JoinData_NewName == 'Overwrite') DataMuse:::CEP(JoinData_NewName) else DataMuse:::CEP(JoinData_SelectData1),"]] <- DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]][!DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], allow.cartesian = TRUE]\n"))

  } else if(JoinData_JoinType == 'semi') {
    left_table <- left_table[na.omit(left_table[right_table, which = TRUE], allow.cartesian = TRUE)]
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Join Data\n",
      "JoinData_ByXVariables <- ", DataMuse:::ExpandText(JoinData_ByXVariables), "\n",
      "JoinData_ByYVariables <- ", DataMuse:::ExpandText(JoinData_ByYVariables), "\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]], cols = c(JoinData_ByXVariables), physical = TRUE)\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], cols = c(JoinData_ByYVariables), physical = TRUE)\n",
      "DataList[[", if(JoinData_NewName == 'Overwrite') DataMuse:::CEP(JoinData_NewName) else DataMuse:::CEP(JoinData_SelectData1),"]] <- DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]][na.omit(DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]][DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], which = TRUE, allow.cartesian = TRUE])]\n"))

  } else if(JoinData_JoinType == 'cross') {
    left_table <- data.table::CJ(left_table, right_table)
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Join Data\n",
      "DataList[[", if(JoinData_NewName == 'Overwrite') DataMuse:::CEP(JoinData_NewName) else DataMuse:::CEP(JoinData_SelectData1),"]] <- data.table::CJ(DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]], DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]])\n"))

  } else if(JoinData_JoinType == 'RollBackward') {
    left_table <- left_table[right_table, roll = -Inf, allow.cartesian = TRUE]
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Join Data\n",
      "JoinData_ByXVariables <- ", DataMuse:::ExpandText(JoinData_ByXVariables), "\n",
      "JoinData_ByYVariables <- ", DataMuse:::ExpandText(JoinData_ByYVariables), "\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]], cols = c(JoinData_ByXVariables), physical = TRUE)\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], cols = c(JoinData_ByYVariables), physical = TRUE)\n",
      "DataList[[",if(JoinData_NewName == 'Overwrite') DataMuse:::CEP(JoinData_NewName) else DataMuse:::CEP(JoinData_SelectData1),"]] <- DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]][DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], roll = -Inf, allow.cartesian = TRUE]\n"))

  } else if(JoinData_JoinType == 'RollForward') {
    left_table <- left_table[right_table, roll = TRUE, allow.cartesian = TRUE]
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Join Data\n",
      "JoinData_ByXVariables <- ", DataMuse:::ExpandText(JoinData_ByXVariables), "\n",
      "JoinData_ByYVariables <- ", DataMuse:::ExpandText(JoinData_ByYVariables), "\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]], cols = c(JoinData_ByXVariables), physical = TRUE)\n",
      "data.table::setkeyv(x = DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], cols = c(JoinData_ByYVariables), physical = TRUE)\n",
      "DataList[[", if(JoinData_NewName == 'Overwrite') DataMuse:::CEP(JoinData_NewName) else DataMuse:::CEP(JoinData_SelectData1),"]] <- DataList[[", DataMuse:::CEP(JoinData_SelectData1),"]][DataList[[", DataMuse:::CEP(JoinData_SelectData2),"]], roll = TRUE, allow.cartesian = TRUE]\n"))

  }

  # Update DataList
  if(JoinData_NewName == 'Overwrite') {
    DataList[[JoinData_SelectData1]][['data']] <- left_table
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, JoinData_NewName)}, error = function(x) DataList)
  } else {
    DataList[[JoinData_NewName]][['data']] <- left_table
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, JoinData_SelectData1)}, error = function(x) DataList)

    # Add data to DataOutputSelection Page
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  }

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.UnionData
#'
#' @description Union two data.table's
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.UnionData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  # Params
  UnionData_NewName     <- DataMuse:::ReturnParam(xx=tryCatch({input[['UnionData_NewName']]},     error=function(x) NULL), Type='character', Default='Overwrite')
  UnionData_SelectData1 <- DataMuse:::ReturnParam(xx=tryCatch({input[['UnionData_SelectData1']]}, error=function(x) NULL), Type='character', Default=NULL)
  UnionData_SelectData2 <- DataMuse:::ReturnParam(xx=tryCatch({input[['UnionData_SelectData2']]}, error=function(x) NULL), Type='character', Default=NULL)
  UnionData_Fill        <- TRUE#DataMuse:::ReturnParam(xx=tryCatch({input[['UnionData_Fill']]},        error=function(x) NULL), Type='logical', Default=TRUE)
  UnionData_UseNames    <- TRUE#DataMuse:::ReturnParam(xx=tryCatch({input[['UnionData_UseNames']]},    error=function(x) NULL), Type='logical',   Default=TRUE)

  # Define tables and create keys (index + sorting)
  if(UnionData_NewName == 'Overwrite') {
    DataList[[UnionData_SelectData1]][['data']] <- data.table::rbindlist(list(DataList[[UnionData_SelectData1]][['data']],DataList[[UnionData_SelectData2]][['data']]), use.names = UnionData_UseNames, fill = UnionData_Fill)
  } else {
    DataList[[UnionData_NewName]][['data']] <- data.table::rbindlist(list(DataList[[UnionData_SelectData1]][['data']],DataList[[UnionData_SelectData2]][['data']]), use.names = UnionData_UseNames, fill = UnionData_Fill)
  }

  # Code collect
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Union Data\n",
    "DataList[[", if(UnionData_NewName == 'Overwrite') DataMuse:::CEP(UnionData_SelectData1) else DataMuse:::CEP(UnionData_NewName), "]] <- data.table::rbindlist(\n  ",
    "list(\n    DataList[[", DataMuse:::CEP(UnionData_SelectData1), "]],\n    ",
    "DataList[[", DataMuse:::CEP(UnionData_SelectData2), "]]),\n  ",
    "use.names = ", DataMuse:::CEPP(UnionData_UseNames), ",\n  ",
    "fill = ", DataMuse:::CEPP(UnionData_Fill), ")\n"))

  # Display data
  if(Debug) print('DW Join Data')
  if(UnionData_NewName == 'Overwrite') {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, UnionData_SelectData1)}, error = function(x) DataList)
  } else {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, UnionData_NewName)}, error = function(x) DataList)

    # Add data to DataOutputSelection Page
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  }

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.MeltData
#'
#' @description Melt a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.MeltData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  # Params
  MeltData_NewName <- DataMuse:::ReturnParam(xx=tryCatch({input[['MeltData_NewName']]}, error=function(x) NULL), Type='character', Default='Overwrite')
  MeltData_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$MeltData_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  MeltData_id.vars <- DataMuse:::ReturnParam(xx = tryCatch({input$MeltData_id.vars}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  MeltData_measure.vars <- DataMuse:::ReturnParam(xx = tryCatch({input$MeltData_measure.vars}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  MeltData_variable.name <- DataMuse:::ReturnParam(xx = tryCatch({input$MeltData_variable.name}, error = function(x) NULL), Type = 'character', Default = 'Variable', Debug = Debug)
  MeltData_value.name <- DataMuse:::ReturnParam(xx = tryCatch({input$MeltData_value.name}, error = function(x) NULL), Type = 'character', Default = 'Value', Debug = Debug)
  MeltData_na.rm <- DataMuse:::ReturnParam(xx = tryCatch({input$MeltData_na.rm}, error = function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
  MeltData_variable.factor <- DataMuse:::ReturnParam(xx = tryCatch({input$MeltData_variable.factor}, error = function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
  MeltData_value.factor <- DataMuse:::ReturnParam(xx = tryCatch({input$MeltData_value.factor}, error = function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)

  # # QA
  # data <- data.table::data.table(Z = letters[1:6], A = rep(c('A', 'B', 'C'), 2), B = 1:6, C = 7:12)
  # data.table::melt.data.table(data = data, id.vars = 'Z', measure.vars = c('B', 'C'), variable.name = 'Variable', value.name = 'Value', na.rm = TRUE, variable.factor = FALSE, value.factor = FALSE)

  # Define tables and create keys (index + sorting)
  if(MeltData_NewName == 'Overwrite') {
    DataList[[MeltData_SelectData]][['data']] <- data.table::melt.data.table(
      data = DataList[[MeltData_SelectData]][['data']],
      id.vars = MeltData_id.vars,
      measure.vars = MeltData_measure.vars,
      variable.name = MeltData_variable.name,
      value.name = MeltData_value.name,
      na.rm = MeltData_na.rm,
      variable.factor = MeltData_variable.factor,
      value.factor = MeltData_value.factor)
  } else {
    DataList[[MeltData_NewName]][['data']] <- data.table::melt.data.table(
      data = DataList[[MeltData_SelectData]][['data']],
      id.vars = MeltData_id.vars,
      measure.vars = MeltData_measure.vars,
      variable.name = MeltData_variable.name,
      value.name = MeltData_value.name,
      na.rm = MeltData_na.rm,
      variable.factor = MeltData_variable.factor,
      value.factor = MeltData_value.factor)
  }

  # Code collect
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Melt Data\n",
    "DataList[[", if(MeltData_NewName == 'Overwrite') DataMuse:::CEP(MeltData_SelectData) else DataMuse:::CEP(MeltData_NewName), "]] <- data.table::melt.data.table(\n  ",
    "data = DataList[[", DataMuse:::CEP(MeltData_SelectData),"]],\n  ",
    "id.vars = ", DataMuse:::CEP(MeltData_id.vars),",\n  ",
    "measure.vars = ", DataMuse:::CEP(MeltData_measure.vars),",\n  ",
    "variable.name = ", DataMuse:::CEP(MeltData_variable.name), ",\n  ",
    "value.name = ", DataMuse:::CEP(MeltData_value.name), ",\n  ",
    "na.rm = ", DataMuse:::CEPP(MeltData_na.rm), ",\n  ",
    "variable.factor = ", DataMuse:::CEPP(MeltData_variable.factor), ",\n  ",
    "value.factor = ", DataMuse:::CEPP(MeltData_value.factor), ")\n"))

  # Display data
  if(Debug) print('DW Join Data')
  if(MeltData_NewName == 'Overwrite') {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, MeltData_SelectData)}, error = function(x) DataList)
  } else {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, MeltData_NewName)}, error = function(x) DataList)

    # Add data to DataOutputSelection Page
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  }

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.CastData
#'
#' @description Cast a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.CastData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  # Params
  CastData_NewName       <- DataMuse:::ReturnParam(xx = tryCatch({input$CastData_NewName},    error = function(x) NULL), Type = 'character', Default = 'Overwrite', Debug = Debug)
  CastData_SelectData    <- DataMuse:::ReturnParam(xx = tryCatch({input$CastData_SelectData},    error = function(x) NULL), Type = 'character', Default = NULL,        Debug = Debug)

  CastData_id.vars       <- DataMuse:::ReturnParam(xx = tryCatch({input$CastData_id.vars},       error = function(x) NULL), Type = 'character', Default = NULL,        Debug = Debug)
  CastData_CastColumns   <- DataMuse:::ReturnParam(xx = tryCatch({input$CastData_CastColumns},   error = function(x) NULL), Type = 'character', Default = NULL,        Debug = Debug)
  CastData_value.var     <- DataMuse:::ReturnParam(xx = tryCatch({input$CastData_value.var},     error = function(x) NULL), Type = 'character', Default = NULL,        Debug = Debug)

  CastData_fun.aggregate <- DataMuse:::ReturnParam(xx = tryCatch({input$CastData_fun.aggregate}, error = function(x) NULL), Type = 'character', Default = 'sum',       Debug = Debug)
  CastData_fill          <- DataMuse:::ReturnParam(xx = tryCatch({input$CastData_fill},          error = function(x) NULL), Type = 'numeric',   Default = 0,           Debug = Debug)

  # Dcast
  if(Debug) {
    print(CastData_id.vars)
    print(CastData_CastColumns)
    print(as.formula(paste0(paste0(CastData_id.vars, collapse = "+"), " ~ ", paste0(CastData_CastColumns, collapse = "+"))))
  }
  DataList[[if('OVerwrite' == CastData_NewName) CastData_NewName else CastData_SelectData]][['data']] <- data.table::dcast.data.table(
    data = DataList[[CastData_SelectData]][['data']],
    formula = as.formula(paste0(paste0(CastData_id.vars, collapse = "+"), "~", paste0(CastData_CastColumns, collapse = "+"))),
    fun.aggregate = get(noquote(CastData_fun.aggregate)),
    fill = CastData_fill,
    value.var = CastData_value.var)

  # Code collect (dcastformula needs to be reassembled upon paste0())
  dcastformula <- as.formula(paste0(paste0(CastData_id.vars, collapse = "+"), " ~ ", paste0(CastData_CastColumns, collapse = "+")))
  if(Debug) print(paste0(dcastformula[2],' ',dcastformula[1],' ',dcastformula[3]))
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Cast Data\n",
    "DataList[[", if('OVerwrite' == CastData_NewName) DataMuse:::CEP(CastData_NewName) else DataMuse:::CEP(CastData_SelectData), "]] <- data.table::dcast.data.table(\n  ",
    "data = DataList[[", if('OVerwrite' == CastData_NewName) DataMuse:::CEP(CastData_NewName) else DataMuse:::CEP(CastData_SelectData), "]],\n  ",
    "formula = ", paste0(dcastformula[2],' ',dcastformula[1],' ',dcastformula[3]),",\n  ",
    "fun.aggregate = ", DataMuse:::CEPP(CastData_fun.aggregate),",\n  ",
    "fill = ", DataMuse:::CEP(CastData_fill), ",\n  ",
    "value.var = ", DataMuse:::CEP(CastData_value.var),")\n"))

  # Display data
  if(Debug) print('DW Join Data')
  if(CastData_NewName == 'Overwrite') {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, CastData_SelectData)}, error = function(x) DataList)
  } else {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, CastData_NewName)}, error = function(x) DataList)

    # Add data to DataOutputSelection Page
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  }

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.RemoveData
#'
#' @description Melt a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.RemoveData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  # Params
  RemoveData_Datasets <- DataMuse:::ReturnParam(xx = tryCatch({input$RemoveData_Datasets}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  if(Debug) {
    print('Shiny.DW.RemoveData')
    print(RemoveData_Datasets)
    print(names(DataList))
    print(length(RemoveData_Datasets))
  }

  # Remove data
  if(length(RemoveData_Datasets) > 0L) {
    for(i in RemoveData_Datasets) DataList[[i]] <- NULL
  }

  # Add data to DataOutputSelection Page
  for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
  for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

  # Code collect (dcastformula needs to be reassembled upon paste0())
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Remove Data\n",
    "RemoveData_Datasets <- ", DataMuse:::ExpandText(RemoveData_Datasets), "\n",
    "for(i in RemoveData_Datasets) DataList[[i]] <- NULL\n  "))

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.TimeSeriesFill
#'
#' @description Time series filling is for missing dates so that all series are complete. Missing dates are generally filled with zeros but other options are available.
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.TimeSeriesFill <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  if(Debug) print('Shiny.DW.TimeSeriesFill 1')

  # Params
  TSF_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$TSF_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TSF_NewName <- DataMuse:::ReturnParam(xx = tryCatch({input$TSF_NewName}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TSF_DateColumnName <- DataMuse:::ReturnParam(xx = tryCatch({input$TSF_DateColumnName}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TSF_GroupVariables <- DataMuse:::ReturnParam(xx = tryCatch({input$TSF_GroupVariables}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TSF_TimeUnit <- DataMuse:::ReturnParam(xx = tryCatch({input$TSF_TimeUnit}, error = function(x) NULL), Type = 'character', Default = 'day', Debug = Debug)
  TSF_MaxMissingPercent <- DataMuse:::ReturnParam(xx = tryCatch({input$TSF_MaxMissingPercent}, error = function(x) NULL), Type = 'numeric', Default = 0.50, Debug = Debug)
  TSF_SimpleImpute <- DataMuse:::ReturnParam(xx = tryCatch({input$TSF_SimpleImpute}, error = function(x) NULL), Type = 'numeric', Default = 0, Debug = Debug)

  if(Debug) print('Shiny.DW.TimeSeriesFill 2')
  if(Debug) print(TSF_NewName)

  # Fill
  DataList[[if(TSF_NewName == 'Overwrite') TSF_NewName else TSF_SelectData]][['data']] <- AutoQuant::TimeSeriesFill(
    DataList[[TSF_SelectData]][['data']],
    DateColumnName = TSF_DateColumnName,
    GroupVariables = TSF_GroupVariables,
    TimeUnit = TSF_TimeUnit,
    MaxMissingPercent = TSF_MaxMissingPercent)

  if(Debug) print('Shiny.DW.TimeSeriesFill 3')

  # Code Fill
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Time Series Fill\n",
    "DataList[[", if(TSF_NewName == 'Overwrite') DataMuse:::CEP(TSF_NewName) else DataMuse:::CEP(TSF_SelectData), "]] <- AutoQuant::TimeSeriesFill(\n  ",
    "DataList[[", DataMuse:::CEP(TSF_SelectData), "]],\n  ",
    "DateColumnName = ", DataMuse:::CEP(DateColumnName),",\n  ",
    "GroupVariables = ", DataMuse:::CEP(GroupVariables),",\n  ",
    "TimeUnit = ", DataMuse:::CEP(TimeUnit), ",\n  ",
    "MaxMissingPercent = ", DataMuse:::CEP(MaxMissingPercent), ")\n"))

  if(Debug) print('Shiny.DW.TimeSeriesFill 4')
  if(Debug) print(TSF_SimpleImpute)

  # Impute
  if(length(TSF_SimpleImpute) > 0L && !is.na(TSF_SimpleImpute)) {
    DataList[[if(TSF_NewName == 'Overwrite') TSF_NewName else TSF_SelectData]][['data']] <- AutoQuant::ModelDataPrep(
      DataList[[if(TSF_NewName == 'Overwrite') TSF_NewName else TSF_SelectData]][['data']],
      Impute = TRUE,
      CharToFactor = FALSE,
      IntToNumeric = FALSE,
      MissNum = TSF_SimpleImpute,
      MissFactor = "0")
  }

  if(Debug) print('Shiny.DW.TimeSeriesFill 5')

  # Code Model Data Prep
  if(length(TSF_SimpleImpute) > 0L && !is.na(TSF_SimpleImpute)) {
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Impute missing values\n",
      "DataList[[", if(TSF_NewName == 'Overwrite') DataMuse:::CEP(TSF_NewName) else DataMuse:::CEP(TSF_SelectData), "]] <- AutoQuant::ModelDataPrep(\n  ",
      "DataList[[", if(TSF_NewName == 'Overwrite') DataMuse:::CEP(TSF_NewName) else DataMuse:::CEP(TSF_SelectData), "]],\n  ",
      "Impute = TRUE,\n  ",
      "CharToFactor = FALSE,\n  ",
      "IntToNumeric = FALSE,\n  ",
      "MissFactor = 'missing',\n  ",
      "MissNum = ", DataMuse:::CEP(TSF_SimpleImpute), ")\n"))
  }

  # Display data
  if(Debug) print('DW Subset Data')
  if(TSF_NewName == 'Overwrite') {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, TSF_SelectData)}, error = function(x) DataList)
  } else {
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, TSF_NewName)}, error = function(x) DataList)

    # Add data to DataOutputSelection Page
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  }

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.TimeSeriesRoll
#'
#' @description Time series filling is for missing dates so that all series are complete. Missing dates are generally filled with zeros but other options are available.
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.TimeSeriesRoll <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  if(Debug) print('Shiny.DW.TimeSeriesRoll 1')

  # Params
  Roll_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$Roll_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Roll_NewName <- DataMuse:::ReturnParam(xx = tryCatch({input$Roll_NewName}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Roll_DateColumnName <- DataMuse:::ReturnParam(xx = tryCatch({input$Roll_DateColumnName}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Roll_RollVars <- DataMuse:::ReturnParam(xx = tryCatch({input$Roll_RollVars}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Roll_NonRollVars <- DataMuse:::ReturnParam(xx = tryCatch({input$Roll_NonRollVars}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Roll_RollDirection <- DataMuse:::ReturnParam(xx = tryCatch({input$Roll_RollDirection}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Roll_GroupVariables <- DataMuse:::ReturnParam(xx = tryCatch({input$Roll_GroupVariables}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  Roll_TimeUnit <- DataMuse:::ReturnParam(xx = tryCatch({input$Roll_TimeUnit}, error = function(x) NULL), Type = 'character', Default = 'day', Debug = Debug)
  Roll_SimpleImpute <- DataMuse:::ReturnParam(xx = tryCatch({input$Roll_SimpleImpute}, error = function(x) NULL), Type = 'numeric', Default = 0, Debug = Debug)

  if(Debug) print('Shiny.DW.TimeSeriesRoll 2')
  if(Debug) print(Roll_NewName)

  # TimeSeriesFill by CUSTOMER_COD and ARTICLE
  JoinBack <- names(DataList[[Roll_SelectData]][['data']])[!names(DataList[[Roll_SelectData]][['data']]) %in% c(Roll_NonRollVars,Roll_RollVars,Roll_DateColumnName)]
  temp_data <- unique(DataList[[Roll_SelectData]][['data']][,.SD,.SDcols=c(JoinBack)])

  # Code Fill
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Time Series Fill Prep\n",
    "JoinBack <- names(DataList[[", DataMuse:::CEP(Roll_SelectData), "]])[!names(DataList[[", DataMuse:::CEP(Roll_SelectData), "]]) %in% ", DataMuse:::ExpandText(c(Roll_NonRollVars,Roll_RollVars,Roll_DateColumnName)), "],\n",
    "temp_data <- unique(DataList[[", DataMuse:::CEP(Roll_SelectData), "]][, .SD, .SDcols = ", DataMuse:::ExpandText(JoinBack), "]),\n"))

  # Fill
  DataList[[if(Roll_NewName == 'Overwrite') Roll_NewName else Roll_SelectData]][['data']] <- AutoQuant::TimeSeriesFillRoll(
    DataList[[Roll_SelectData]][['data']],
    DateColumnName = Roll_DateColumnName,
    GroupVariables = Roll_GroupVariables,
    TimeUnit = Roll_TimeUnit,
    RollVars = Roll_RollVars,
    NonRollVars = Roll_NonRollVars,
    RollDirection = Roll_RollDirection)

  if(Debug) print('Shiny.DW.TimeSeriesRoll 3')

  # Code Fill
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Time Series Fill\n",
    "DataList[[", if(Roll_NewName == 'Overwrite') DataMuse:::CEP(Roll_NewName) else DataMuse:::CEP(Roll_SelectData), "]] <- AutoQuant::TimeSeriesFill(\n  ",
    "DataList[[", DataMuse:::CEP(Roll_SelectData), "]],\n  ",
    "DateColumnName = ", DataMuse:::CEP(Roll_DateColumnName),",\n  ",
    "GroupVariables = ", DataMuse:::CEP(Roll_GroupVariables),",\n  ",
    "TimeUnit = ", DataMuse:::CEP(Roll_TimeUnit), ",\n  ",
    "RollVars = ", DataMuse:::ExpandText(Roll_RollVars), ",\n  ",
    "NonRollVars = ", DataMuse:::ExpandText(Roll_NonRollVars), ",\n  ",
    "RollDirection = ", DataMuse:::CEP(Roll_RollDirection), ")\n"))

  if(Debug) print('Shiny.DW.TimeSeriesRoll 3.25')

  # Fill dates and spread metric over time
  DataList[[if(Roll_NewName == 'Overwrite') Roll_NewName else Roll_SelectData]][['data']] <- merge(DataList[[if(Roll_NewName == 'Overwrite') Roll_NewName else Roll_SelectData]][['data']], temp_data, by = c(Roll_GroupVariables), all = FALSE)

  if(Debug) print('Shiny.DW.TimeSeriesRoll 3.5')

  # Code Fill
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Merge temp_data\n",
    "DataList[[", if(Roll_NewName == 'Overwrite') Roll_NewName else Roll_SelectData, "]] <- merge(DataList[[", if(Roll_NewName == 'Overwrite') Roll_NewName else Roll_SelectData, "]], temp_data, by = ", DataMuse:::CEP(Roll_GroupVariables), ", all = FALSE),\n"))

  if(Debug) print('Shiny.DW.TimeSeriesRoll 4')

  # Impute
  if(length(Roll_SimpleImpute) > 0L && !is.na(Roll_SimpleImpute)) {
    DataList[[if(Roll_NewName == 'Overwrite') Roll_NewName else Roll_SelectData]][['data']] <- AutoQuant::ModelDataPrep(
      DataList[[if(Roll_NewName == 'Overwrite') Roll_NewName else Roll_SelectData]][['data']],
      Impute = TRUE,
      CharToFactor = FALSE,
      IntToNumeric = FALSE,
      MissNum = Roll_SimpleImpute,
      MissFactor = "0")
  }

  if(Debug) print('Shiny.DW.TimeSeriesFill 5')

  # Code Model Data Prep
  if(length(Roll_SimpleImpute) > 0L && !is.na(Roll_SimpleImpute)) {
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
      "\n",
      "# Impute missing values\n",
      "DataList[[", if(Roll_NewName == 'Overwrite') DataMuse:::CEP(Roll_NewName) else DataMuse:::CEP(Roll_SelectData), "]] <- AutoQuant::ModelDataPrep(\n  ",
      "DataList[[", if(Roll_NewName == 'Overwrite') DataMuse:::CEP(Roll_NewName) else DataMuse:::CEP(Roll_SelectData), "]],\n  ",
      "Impute = TRUE,\n  ",
      "CharToFactor = FALSE,\n  ",
      "IntToNumeric = FALSE,\n  ",
      "MissFactor = 'missing',\n  ",
      "MissNum = ", DataMuse:::CEP(Roll_SimpleImpute), ")\n"))
  }

  # Add data to DataOutputSelection Page
  if(Roll_NewName != 'Overwrite') {
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
    for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  }

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.MetaProgramming
#'
#' @description Code supplied by user to run
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.MetaProgramming <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  if(Debug) print('Shiny.DW.MetaProgramming 1')

  # Params
  MetaProgramming_TextCode <- DataMuse:::ReturnParam(xx = tryCatch({input$MetaProgramming_TextCode}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  if(Debug) print('Shiny.DW.MetaProgramming 2')
  if(Debug) print(MetaProgramming_TextCode)
  eval(parse(text = MetaProgramming_TextCode))

  # Code Model Data Prep
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Meta Programming User Code\n",
    MetaProgramming_TextCode,
    "\n"))

  # Update Data Panel
  for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
  for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.DW.SortData
#'
#' @description Sort data
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @export
Shiny.DW.SortData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug) {

  if(Debug) print('Shiny.DW.SortData 1')

  # Params
  SortData_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$SortData_SelectData}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  SortData_SortColumns <- DataMuse:::ReturnParam(xx = tryCatch({input$SortData_SortColumns}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  SortData_SortOrder <- DataMuse:::ReturnParam(xx = tryCatch({input$SortData_SortOrder}, error = function(x) NULL), Type = 'character', Default = 'Ascending', Debug = Debug)

  if(Debug) print('Shiny.DW.SortData 2')
  if(SortData_SortOrder == 'Ascending') {
    Order <- rep(1, length(SortData_SortColumns))
  } else {
    Order <- rep(-1, length(SortData_SortColumns))
  }
  data.table::setorderv(x = DataList[[SortData_SelectData]][['data']], cols = c(SortData_SortColumns), order = c(Order), na.last = TRUE)

  # Code Model Data Prep
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
    "\n",
    "# Sort Data\n",
    "data.table::setorderv(\n  ",
    "x = DataList[[", DataMuse:::CEP(SortData_SelectData), "]],\n  ",
    "cols = ", DataMuse:::ExpandText(SortData_SortColumns), ",\n  ",
    "order = ", DataMuse:::ExpandText(Order), ",\n  ",
    "na.last = TRUE)\n"))

  # Return
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}
