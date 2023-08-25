#' @title Shiny.FE.Date.Calendar
#'
#' @description server.R observeEvent() for executing CreateCalendarVariables() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.Date.Calendar <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Dispatch
  CalendarVar_DateVariables <- DataMuse:::ReturnParam(xx = tryCatch({input[['CalendarVariables_DateVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(Debug) print(CalendarVar_DateVariables)

  # Pull in values
  CalendarVar_TimeUnits <- DataMuse:::ReturnParam(xx = tryCatch({input[['CalendarVariables_TimeUnits']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$CalendarVariables_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # Caching
  if(Debug) print('FE Calendar Variables 1')

  # Run function
  if(Debug) print('FE Calendar Variables 2')
  DataList[[SelectData]][['data']] <- Rodeo::CreateCalendarVariables(
    data = DataList[[SelectData]][['data']],
    DateCols = CalendarVar_DateVariables,
    AsFactor = FALSE,
    TimeUnits = CalendarVar_TimeUnits,
    CachePath = CacheDir,
    Debug = Debug)

  # Code
  if(Debug) print('FE Calendar Variables 5')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Calendar Variables\n",
    "DataList[[", DataMuse:::CEP(SelectData), "]] <- Rodeo::CreateCalendarVariables(\n  ",
    "data = DataList[[", DataMuse:::CEP(SelectData), "]],\n  ",
    "DateCols = ", DataMuse:::ExpandText(CalendarVar_DateVariables), ",\n  ",
    "AsFactor = FALSE,\n  ",
    "TimeUnits = ", DataMuse:::ExpandText(CalendarVar_TimeUnits), ")\n"
  ))

  # Return
  if(Debug) {
    print('FE Calendar Variables 6')
    print(names(DataList))
    print(CodeList)
  }

  # Update meta
  DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)

  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.FE.Date.Holiday
#'
#' @description server.R observeEvent() for executing CreateHolidayVariables() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.Date.Holiday <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Holiday Variable creation has begun..', value = 0, {

    print('FE Holiday Variables')

    # Dispatch
    HolidayVar_DateVariables <- DataMuse:::ReturnParam(xx = tryCatch({input[['HolidayVariables_DateVariables']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)

    # Pull in values
    if(Debug) print('FE Holiday Variables 1')
    HolidayVar_HolidayGroups <- DataMuse:::ReturnParam(xx = tryCatch({input[['HolidayVariables_HolidayGroups']]}, error=function(x) NULL), VarName = 'HolidayVariables_HolidayGroups', Type = 'character', Default = NULL, Debug = Debug)
    HolidayVar_LookbackDays <- DataMuse:::ReturnParam(xx = tryCatch({input[['HolidayVariables_LookbackDays']]}, error=function(x) NULL), VarName = 'HolidayVariables_LookbackDays', Type = 'numeric', Default = 1, Debug = Debug)
    SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$HolidayVariables_SelectData}, error=function(x) NULL), VarName = 'HolidayVariables_SelectData', Type = 'character', Default = NULL, Debug = Debug)

    # if path is a character then data will be pulled inside the function
    #  otherwise you're passing data directly to function
    if(Debug) print('FE Holiday Variables 3')
    DataList[[SelectData]][['data']] <- Rodeo::CreateHolidayVariables(
      DataList[[SelectData]][['data']],
      DateCols = HolidayVar_DateVariables,
      LookbackDays = HolidayVar_LookbackDays,
      HolidayGroups = HolidayVar_HolidayGroups,
      CachePath = CacheDir,
      Debug = Debug)

    # Create code
    if(Debug) print('FE Holiday Variables 6')
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Holiday Variables\n",
      "DataList[[", DataMuse:::CEP(SelectData), "]] <- Rodeo::CreateHolidayVariables(\n  ",
      "DataList[[", DataMuse:::CEP(SelectData), "]],\n  ",
      "DateCols = ", DataMuse:::ExpandText(HolidayVar_DateVariables), ",\n  ",
      "LookbackDays = ", DataMuse:::CEP(HolidayVar_LookbackDays), ",\n  ",
      "HolidayGroups = ", DataMuse:::ExpandText(HolidayVar_HolidayGroups),
      ")\n"))

    # Return
    if(Debug) {print('FE Holiday Variables 6'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.Numeric.PercentRank
#'
#' @description server.R observeEvent() for executing PercRank() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.Numeric.PercentRank <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Numeric PercentRank has begun..', value = 0, {
    PercentRank_ColNames <- DataMuse:::ReturnParam(xx = tryCatch({input[['PercentRank_ColNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(PercentRank_ColNames) > 0L) {

      # Args
      PercentRank_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$PercentRank_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_SelectValidationData <- DataMuse:::ReturnParam(xx = tryCatch({input[['PercentRank_SelectValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_SelectTestData <- DataMuse:::ReturnParam(xx = tryCatch({input[['PercentRank_SelectTestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_GroupVars <- DataMuse:::ReturnParam(xx = tryCatch({input[['PercentRank_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_Granularity <- DataMuse:::ReturnParam(xx = tryCatch({input[['PercentRank_Granularity']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)

      # Function
      if(length(PercentRank_SelectValidationData) == 0L && length(PercentRank_SelectTestData) == 0L) {
        DataList[[PercentRank_SelectData]][['data']] <- Rodeo::PercRank(data = DataList[[PercentRank_SelectData]][['data']], ColNames = PercentRank_ColNames, GroupVars = PercentRank_GroupVars, Granularity = PercentRank_Granularity)

        # Create code
        if(Debug) print('FE PercRank 6')
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Percent Rank: value --> percentile\n",
          "DataList[[", DataMuse:::CEP(PercentRank_SelectData),"]] <- Rodeo::PercRank(\n  ",
          "data = DataList[[", DataMuse:::CEP(PercentRank_SelectData), "]],\n  ",
          "ColNames = ", DataMuse:::ExpandText(PercentRank_ColNames), ",\n  ",
          "GroupVars = ", DataMuse:::ExpandText(PercentRank_GroupVars), ",\n  ",
          "Granularity = ", DataMuse:::CEP(PercentRank_Granularity),")\n"))

      } else {

        Output <- Rodeo::PercRank(data = DataList[[PercentRank_SelectData]][['data']], ColNames = PercentRank_ColNames, GroupVars = PercentRank_GroupVars, Granularity = PercentRank_Granularity, ScoreTable = TRUE)
        DataList[[PercentRank_SelectData]][['data']] <- Output$data
        ScoreTable <- Output$ScoreTable

        # Create code
        if(Debug) print('FE PercRank 6')
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Percent Rank: value --> percentile\n",
          "Output <- Rodeo::PercRank(\n  ",
          "data = DataList[[", DataMuse:::CEP(PercentRank_SelectData), "]],\n  ",
          "ColNames = ", DataMuse:::ExpandText(PercentRank_ColNames), ",\n  ",
          "GroupVars = ", DataMuse:::ExpandText(PercentRank_GroupVars), ",\n  ",
          "Granularity = ", DataMuse:::CEP(PercentRank_Granularity), ",\n  ",
          "ScoreTable = TRUE)\n"))

        # Apply Standardization to Validation Data
        if(length(PercentRank_SelectValidationData) > 0L) {
          DataList[[PercentRank_SelectValidationData]][['data']] <- Rodeo::PercRankScoring(data = DataList[[PercentRank_SelectValidationData]][['data']], ScoreTable = ScoreTable, GroupVars = PercentRank_GroupVars, RollDirection = 'forward')
          CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
            "\n",
            "# Percent Rank: value --> percentile\n",
            "DataList[[",DataMuse:::CEP(PercentRank_SelectValidationData),"]] <- Rodeo::PercRankScoring(\n  ",
            "data = DataList[[", DataMuse:::CEP(PercentRank_SelectData), "]],\n  ",
            "ScoreTable = ScoreTable,\n  ",
            "RollDirection = 'forward',\n  ",
            "GroupVars = ", DataMuse:::ExpandText(PercentRank_GroupVars),")\n"))
        }

        # Apply Standardization to Test Data
        if(length(PercentRank_SelectTestData) > 0L) {
          DataList[[PercentRank_SelectTestData]][['data']] <- Rodeo::PercRankScoring(data = DataList[[PercentRank_SelectTestData]][['data']], ScoreTable = ScoreTable, GroupVars = PercentRank_GroupVars, RollDirection = 'forward')
          CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
            "\n",
            "# Percent Rank: value --> percentile\n",
            "DataList[[",DataMuse:::CEP(PercentRank_SelectTestData),"]] <- Rodeo::PercRankScoring(\n  ",
            "data = DataList[[", DataMuse:::CEP(PercentRank_SelectData), "]],\n  ",
            "ScoreTable = ScoreTable,\n  ",
            "RollDirection = 'forward',\n  ",
            "GroupVars = ", DataMuse:::ExpandText(PercentRank_GroupVars),")\n"))
        }
      }
    }

    # Return
    if(Debug) {print('FE PercRank 6'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, PercentRank_SelectData)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.Numeric.Standardize
#'
#' @description server.R observeEvent() for executing Standardize() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.Numeric.Standardize <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Numeric Standardize has begun..', value = 0, {
    Standardize_ColNames <- DataMuse:::ReturnParam(xx = tryCatch({input[['Standardize_ColNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(Standardize_ColNames) > 0L) {

      # Args
      Standardize_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$Standardize_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      Standardize_SelectValidationData <- DataMuse:::ReturnParam(xx = tryCatch({input[['Standardize_SelectValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      Standardize_SelectTestData <- DataMuse:::ReturnParam(xx = tryCatch({input[['Standardize_SelectTestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      Standardize_GroupVars <- DataMuse:::ReturnParam(xx = tryCatch({input[['Standardize_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      Standardize_Center <- DataMuse:::ReturnParam(xx = tryCatch({input[['Standardize_Center']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      Standardize_Scale <- DataMuse:::ReturnParam(xx = tryCatch({input[['Standardize_Scale']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)

      # Function
      if(length(Standardize_SelectValidationData) == 0L && length(Standardize_SelectTestData) == 0L) {
        DataList[[Standardize_SelectData]][['data']] <- Rodeo::Standardize(data = DataList[[Standardize_SelectData]][['data']], ColNames = Standardize_ColNames, GroupVars = Standardize_GroupVars, Center = Standardize_Center, Scale = Standardize_Scale)

        # Create code
        if(Debug) print('FE Standardize 6')
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Standardize\n",
          "DataList[[", DataMuse:::CEP(Standardize_SelectData),"]] <- Rodeo::Standardize(\n  ",
          "data = DataList[[", DataMuse:::CEP(Standardize_SelectData), "]],\n  ",
          "ColNames = ", DataMuse:::ExpandText(Standardize_ColNames), ",\n  ",
          "GroupVars = ", DataMuse:::ExpandText(Standardize_GroupVars), ",\n  ",
          "Center = ", DataMuse:::CEP(Standardize_Center),",\n  ",
          "Scale = ", DataMuse:::CEP(Standardize_Scale),")\n"))

      } else {

        # Run
        Output <- Rodeo::Standardize(data = DataList[[Standardize_SelectData]][['data']], ColNames = Standardize_ColNames, GroupVars = Standardize_GroupVars, Granularity = Standardize_Granularity, ScoreTable = TRUE)
        DataList[[Standardize_SelectData]][['data']] <- Output$data
        ScoreTable <- Output$ScoreTable

        # Create code
        if(Debug) print('FE Standardize 6')
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Standardize\n",
          "Output <- Rodeo::Standardize(\n  ",
          "data = DataList[[", DataMuse:::CEP(Standardize_SelectData), "]],\n  ",
          "ColNames = ", DataMuse:::ExpandText(Standardize_ColNames), ",\n  ",
          "GroupVars = ", DataMuse:::ExpandText(Standardize_GroupVars), ",\n  ",
          "Center = ", DataMuse:::CEP(Standardize_Center),",\n  ",
          "Scale = ", DataMuse:::CEP(Standardize_Scale),",\n  ",
          "ScoreTable = TRUE)\n"))

        # Apply Standardization to Validation Data
        if(length(Standardize_SelectValidationData) > 0L) {
          DataList[[Standardize_SelectValidationData]][['data']] <- Rodeo::StandardizeScoring(data = DataList[[Standardize_SelectValidationData]][['data']], ScoreTable = ScoreTable, Apply = 'apply', GroupVars = Standardize_GroupVars)
          CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
            "\n",
            "# Percent Rank: value --> percentile\n",
            "DataList[[",DataMuse:::CEP(Standardize_SelectValidationData),"]] <- Rodeo::StandardizeScoring(\n  ",
            "data = DataList[[", DataMuse:::CEP(Standardize_SelectData), "]],\n  ",
            "ScoreTable = ScoreTable,\n  ",
            "Apply = 'apply',\n  ",
            "GroupVars = ", DataMuse:::ExpandText(Standardize_GroupVars),")\n"))
        }

        # Apply Standardization to Test Data
        if(length(Standardize_SelectTestData) > 0L) {
          DataList[[Standardize_SelectTestData]][['data']] <- Rodeo::StandardizeScoring(data = DataList[[Standardize_SelectTestData]][['data']], ScoreTable = ScoreTable, Apply = 'apply', GroupVars = Standardize_GroupVars)
          CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
            "\n",
            "# Percent Rank: value --> percentile\n",
            "DataList[[",DataMuse:::CEP(Standardize_SelectTestData),"]] <- Rodeo::StandardizeScoring(\n  ",
            "data = DataList[[", DataMuse:::CEP(Standardize_SelectData), "]],\n  ",
            "ScoreTable = ScoreTable,\n  ",
            "Apply = 'apply',\n  ",
            "GroupVars = ", DataMuse:::ExpandText(Standardize_GroupVars),")\n"))
        }
      }
    }

    # Return
    if(Debug) {print('FE Standardize 6'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, Standardize_SelectData)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.Numeric.Interactions
#'
#' @description server.R observeEvent() for executing AutoInteraction() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.Numeric.Interactions <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Numeric Variable Interaction has begun..', value = 0, {
    AutoInteraction_NumericVars <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoInteraction_NumericVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoInteraction_NumericVars) != 0) {
      AutoInteraction_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoInteraction_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoInteraction_InteractionDepth <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoInteraction_InteractionDepth']]}, error=function(x) NULL), Type = 'numeric', Default = 2, Debug = Debug)
      AutoInteraction_Scale <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoInteraction_Scale']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      AutoInteraction_Center <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoInteraction_Center']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      DataList[[AutoInteraction_SelectData]][['data']] <- Rodeo::AutoInteraction(
        data = DataList[[AutoInteraction_SelectData]][['data']],
        NumericVars = AutoInteraction_NumericVars,
        InteractionDepth = AutoInteraction_InteractionDepth,
        Center = AutoInteraction_Center,
        Scale = AutoInteraction_Scale,
        SkipCols = NULL,
        Scoring = FALSE,
        File = NULL)
    }

    # Create code
    if(Debug) print('FE Numeric Interaction 1')
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Numeric Interaction: value --> percentile\n",
      "temp <- ", DataMuse:::CEP(AutoInteraction_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::AutoInteraction(\n  ",
      "data = DataList[[temp]],\n  ",
      "NumericVars = ", DataMuse:::ExpandText(AutoInteraction_NumericVars), ",\n  ",
      "InteractionDepth = ", DataMuse:::CEP(AutoInteraction_InteractionDepth), ",\n  ",
      "Center = ", DataMuse:::CEP(AutoInteraction_Center), ",\n  ",
      "Scale = ", DataMuse:::CEP(AutoInteraction_Scale), ",\n  ",
      "SkipCols = NULL,\n  ",
      "Scoring = FALSE,\n  ",
      "File = NULL)\n"))

    # Return
    if(Debug) {print('FE Numeric Interaction 2'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, AutoInteraction_SelectData)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.Numeric.Transformations
#'
#' @description server.R observeEvent() for executing AutoTransformationCreate() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.Numeric.Transformations <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Numeric Variable Transformations has begun..', value = 0, {
    AutoTransformationCreate_ColumnNames <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoTransformationCreate_ColumnNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoTransformationCreate_ColumnNames) != 0) {
      AutoTransformationCreate_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoTransformationCreate_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoTransformationCreate_Methods <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoTransformationCreate_Methods']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      for(col in AutoTransformationCreate_ColumnNames) {
        DataList[[AutoTransformationCreate_SelectData]][['data']][, paste0(col, "_Trans") := get(col)]
      }
      AutoTransformationCreate_ColumnNames <- paste0(AutoTransformationCreate_ColumnNames, "_Trans")
      DataList[[AutoTransformationCreate_SelectData]][['data']] <- Rodeo::AutoTransformationCreate(
        data = DataList[[AutoTransformationCreate_SelectData]][['data']],
        ColumnNames = AutoTransformationCreate_ColumnNames,
        Methods = AutoTransformationCreate_Methods,
        Path = NULL,
        TransID = "ModelID",
        SaveOutput = FALSE)$Data
    }

    # Create code
    if(Debug) print('FE Numeric Transformation 1')
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Numeric Transformation: value --> percentile\n",
      "temp <- ", DataMuse:::CEP(AutoTransformationCreate_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::AutoTransformationCreate(\n  ",
      "data = DataList[[temp]],\n  ",
      "ColumnNames = ", DataMuse:::ExpandText(AutoTransformationCreate_ColumnNames), ",\n  ",
      "Methods = ", DataMuse:::ExpandText(AutoTransformationCreate_Methods), ",\n  ",
      "Path = NULL,\n  ",
      "TransID = 'ModelID',\n  ",
      "SaveOutput = FALSE)\n"))

    # Return
    if(Debug) {print('FE Numeric Transformation 2'); print(names(DataList)); print(CodeList)}
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.Categorical.Dummify
#'
#' @description server.R observeEvent() for executing DummifyDT() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.Categorical.Dummify <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Dummy variables creation has begun..', value = 0, {
    DummifyDT_Cols <- DataMuse:::ReturnParam(xx = tryCatch({input[['DummifyDT_Cols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(DummifyDT_Cols) != 0) {
      DummifyDT_TopN <- DataMuse:::ReturnParam(xx = tryCatch({input[['DummifyDT_TopN']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      DummifyDT_KeepBaseCols <- DataMuse:::ReturnParam(xx = tryCatch({input[['DummifyDT_KeepBaseCols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      DummifyDT_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input[['DummifyDT_SelectData']]}, error = function(x) NULL), VarName = 'DummifyDT_SelectData', Type = 'character', Default = NULL, Debug = Debug)
      DataList[[DummifyDT_SelectData]][['data']] <- Rodeo::DummifyDT(
        data = DataList[[DummifyDT_SelectData]][['data']],
        cols = DummifyDT_Cols,
        TopN = DummifyDT_TopN,
        KeepFactorCols = as.logical(DummifyDT_KeepBaseCols),
        OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=NULL, ImportFactorLevels=FALSE, FactorLevelsList=NULL, ClustScore=FALSE, ReturnFactorLevels=FALSE, GroupVar=FALSE)
    }

    # Create code
    if(Debug) print('FE Partial Dummies 1')
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Partial Dummify Variables\n",
      "temp <- ", DataMuse:::CEP(DummifyDT_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::DummifyDT(\n  ",
      "data = DataList[[temp]],\n  ",
      "cols = ", DataMuse:::ExpandText(DummifyDT_Cols), ",\n  ",
      "TopN = ", DataMuse:::CEP(DummifyDT_TopN), ",\n  ",
      "KeepFactorCols = ", DataMuse:::CEP(as.logical(DummifyDT_KeepBaseCols)), ",\n  ",
      "OneHot = FALSE,\n  ",
      "SaveFactorLevels = FALSE,\n  ",
      "SavePath = NULL,\n  ",
      "ImportFactorLevels = FALSE,\n  ",
      "FactorLevelsList = NULL,\n  ",
      "ReturnFactorLevels = FALSE)\n"))

    # Return
    if(Debug) {print('FE Partial Dummies 2'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, DummifyDT_SelectData)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.CrossRow.CategoricalEncoding
#'
#' @description server.R observeEvent() for executing CategoricalEncoding() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.CrossRow.CategoricalEncoding <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Categorical Encoding has begun..', value = 0, {
    CategoricalEncoding_GroupVariables <- DataMuse:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_GroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(CategoricalEncoding_GroupVariables) != 0) {
      CategoricalEncoding_TargetVariable <- DataMuse:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_TargetVariable']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      CategoricalEncoding_Method <- DataMuse:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_Method']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      temp_train <- DataMuse:::ReturnParam(xx = tryCatch({input$CategoricalEncoding_TrainData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[temp_train]][['data']]
      temp_validate <- DataMuse:::ReturnParam(xx = tryCatch({input$CategoricalEncoding_ValidationData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(length(temp_validate) > 0L) y <- DataList[[temp_validate]][['data']] else y <- NULL
      temp_test <- DataMuse:::ReturnParam(xx = tryCatch({input$CategoricalEncoding_TestData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(length(temp_test) > 0L) z <- DataList[[temp_train]][['data']] else z <- NULL

      # Identify target type
      if(class(x[[eval(CategoricalEncoding_TargetVariable)]])[1L] %in% c('character','factor')) {
        MLType <- 'MultiClass'
      } else if(all(unique(x[[eval(CategoricalEncoding_TargetVariable)]]) %in% c(0,1))) {
        MLType <- 'Classification'
      } else {
        MLType <- 'Regression'
      }

      # Build Features
      Output <- DataMuse:::EncodeCharacterVariables(
        RunMode = 'train',
        ModelType = MLType,
        TrainData = x,
        ValidationData = y,
        TestData = z,
        TargetVariableName = CategoricalEncoding_TargetVariable,
        CategoricalVariableNames = CategoricalEncoding_GroupVariables,
        EncodeMethod = CategoricalEncoding_Method,
        KeepCategoricalVariables = TRUE,
        ReturnMetaData = TRUE,
        MetaDataPath = NULL,
        MetaDataList = NULL,
        ImputeMissingValue = 0)
      DataList[[temp_train]][['data']] <- x
      if(length(y) > 0L) DataList[[temp_validate]][['data']] <- y
      if(length(z) > 0L) DataList[[temp_test]][['data']] <- z
    }

    # Create code
    if(Debug) print('FE Categorical Encoding 1')
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Categorical Encoding\n",
      "temp_train <- ", DataMuse:::CEP(temp_train),"\n",
      "if(class(x[[eval(CategoricalEncoding_TargetVariable)]])[1L] %in% c('character','factor')) {\n  ",
      "temp_validate <- ", DataMuse:::CEP(temp_validate), "\n",
      "if(length(temp_validate) > 0L) y <- DataList[[temp_validate]] else y <- NULL\n",
      "temp_test <- ", DataMuse:::CEP(temp_test), "\n",
      "if(length(temp_test) > 0L) z <- DataList[[temp_train]] else z <- NULL\n  ",
      "MLType <- 'multiclass'\n",
      "} else if(all(unique(x[[eval(CategoricalEncoding_TargetVariable)]]) %in% c(0,1))) {\n  ",
      "MLType <- 'classification'\n",
      "} else {\n  ",
      "MLType <- 'regression'\n",
      "}\n",
      "Output <- DataMuse:::EncodeCharacterVariables(","\n  ",
      "RunMode = 'train',\n  ",
      "ModelType = MLType,\n  ",
      "TrainData = x,\n  ",
      "ValidationData = y,\n  ",
      "TestData = z,\n  ",
      "TargetVariableName = ", DataMuse:::CEP(CategoricalEncoding_TargetVariable), ",\n  ",
      "CategoricalVariableNames = ", DataMuse:::ExpandText(CategoricalEncoding_GroupVariables), ",\n  ",
      "EncodeMethod = ", DataMuse:::CEP(CategoricalEncoding_Method), ",\n  ",
      "KeepCategoricalVariables = TRUE,\n  ",
      "ReturnMetaData = TRUE,\n  ",
      "MetaDataPath = NULL,\n  ",
      "MetaDataList = NULL,\n  ",
      "ImputeMissingValue = 0)","\n",
      "TrainData <- Output$TrainData\n",
      "ValidationData <- Output$ValidationData\n",
      "TestData <- Output$TestData; rm(Output)\n"))

    # Return
    if(Debug) {print('FE Categorical Encoding 2'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_train)}, error = function(x) DataList)
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_validate)}, error = function(x) DataList)
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_test)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.CrossRow.RollingMode
#'
#' @description server.R observeEvent() for executing AutoLagRollMode() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.CrossRow.RollingMode <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Rolling Mode has begun..', value = 0, {
    if(Debug) print('FE Auto Lag Roll Mode')
    AutoLagRollMode_Targets <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_Targets']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print('FE Auto Lag Roll Mode 1')
    AutoLagRollMode_SortDateName <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_SortDateName']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print('FE Auto Lag Roll Mode 6')
    if(Debug) print(input[['AutoLagRollMode_WindowingLag']])
    if(length(AutoLagRollMode_Targets) > 0 && length(AutoLagRollMode_SortDateName) > 0) {
      if(Debug) print('FE Auto Lag Roll Mode 2')
      AutoLagRollMode_Lags <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_Lags', Type = 'numeric']]}, error=function(x) NULL), Default = 1, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 3')
      AutoLagRollMode_ModePeriods <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_ModePeriods']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 4')
      AutoLagRollMode_GroupingVars <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_GroupingVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 5')
      AutoLagRollMode_WindowingLag <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_WindowingLag']]}, error=function(x) NULL), Type = 'numeric', Default = 1, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 7')
      AutoLagRollMode_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoLagRollMode_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 8')

      # Checkpoint
      gggg <- TRUE; for(ggg in seq_along(AutoLagRollMode_Targets)) if(class(DataList[[AutoLagRollMode_SelectData]][['data']][[eval(ggg)]])[1L] %in% 'numeric') gggg <- FALSE
      if(!gggg) return(NULL)

      if(Debug) print('FE Auto Lag Roll Mode 9')

      if(Debug) print(DataList[[AutoLagRollMode_SelectData]][['data']])
      if(Debug) print(AutoLagRollMode_Targets)
      if(Debug) print(AutoLagRollMode_GroupingVars)
      if(Debug) print(AutoLagRollMode_SortDateName)
      if(Debug) print(AutoLagRollMode_WindowingLag)
      if(Debug) print(AutoLagRollMode_Lags)
      if(Debug) print(AutoLagRollMode_ModePeriods)


      DataList[[AutoLagRollMode_SelectData]][['data']] <- Rodeo::AutoLagRollMode(
        data = DataList[[AutoLagRollMode_SelectData]][['data']],
        Targets = AutoLagRollMode_Targets,
        GroupingVars = AutoLagRollMode_GroupingVars,
        SortDateName = AutoLagRollMode_SortDateName,
        WindowingLag = AutoLagRollMode_WindowingLag,
        Lags = AutoLagRollMode_Lags,
        ModePeriods = AutoLagRollMode_ModePeriods,
        Type = c("Lag"),
        SimpleImpute = TRUE,
        Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 10')

      # Create code
      if(Debug) print('FE Auto Lag Roll Mode 1')
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Auto Lag Roll Mode\n",
        "temp <- ", DataMuse:::CEP(AutoLagRollMode_SelectData),"\n",
        "DataList[[temp]] <- Rodeo::AutoLagRollMode(\n  ",
        "data = DataList[[temp]],\n  ",
        "Targets = ", DataMuse:::ExpandText(AutoLagRollMode_Targets), ",\n  ",
        "GroupingVars = ", DataMuse:::ExpandText(AutoLagRollMode_GroupingVars), ",\n  ",
        "SortDateName = ", DataMuse:::CEP(AutoLagRollMode_SortDateName), ",\n  ",
        "WindowingLag = ", DataMuse:::CEP(AutoLagRollMode_WindowingLag), ",\n  ",
        "Lags = ", DataMuse:::ExpandText(AutoLagRollMode_Lags), ",\n  ",
        "ModePeriods = ", DataMuse:::ExpandText(AutoLagRollMode_ModePeriods), ",\n  ",
        "Type = 'Lag',\n  ",
        "SimpleImpute = TRUE)\n"))

      # Return
      if(Debug) {print('FE Auto Lag Roll Mode 2'); print(names(DataList)); print(CodeList)}
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, AutoLagRollMode_SelectData)}, error = function(x) DataList)
      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))
    }
  })
}

#' @title Shiny.FE.CrossRow.RollingStats
#'
#' @description server.R observeEvent() for executing AutoLagRollStats() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.CrossRow.RollingStats <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Rolling Stats has begun..', value = 0, {
    AutoLagRollStats_Targets <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Targets']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoLagRollStats_DateColumn <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_DateColumn']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoLagRollStats_TimeUnits <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_TimeUnits']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoLagRollStats_Targets) != 0 && length(AutoLagRollStats_DateColumn) != 0 && length(AutoLagRollStats_TimeUnits) != 0) {
      AutoLagRollStats_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoLagRollStats_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoLagRollStats_GroupVars <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(length(AutoLagRollStats_GroupVars) > 1L) {
        DataList[[AutoLagRollStats_SelectData]][['data']][, paste0(AutoLagRollStats_GroupVars, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(AutoLagRollStats_GroupVars)]
        AutoLagRollStats_GroupVars <- paste0(AutoLagRollStats_GroupVars, collapse = '_')
      }
      AutoLagRollStats_Lags <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Lags']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_RollOnLag1 <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_RollOnLag1']]}, error=function(x) NULL), Type = 'logical', Default = NULL, Debug = Debug)
      if(AutoLagRollStats_RollOnLag1 == 0) {
        AutoLagRollStats_RollOnLag1 <- FALSE
      } else {
        AutoLagRollStats_RollOnLag1 <- TRUE
      }
      AutoLagRollStats_MA_RollWindows <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_MA_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_SD_RollWindows <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_SD_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Skew_RollWindows <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Skew_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Kurt_RollWindows <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Kurt_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Quantile_RollWindows <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Quantile_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Quantiles_Selected <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Quantiles_Selected']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      for(i in AutoLagRollStats_Targets) {
        DataList[[AutoLagRollStats_SelectData]][['data']] <- Rodeo::AutoLagRollStats(
          data                 = DataList[[AutoLagRollStats_SelectData]][['data']],
          Targets              = i,
          HierarchyGroups      = NULL,
          IndependentGroups    = AutoLagRollStats_GroupVars,
          DateColumn           = AutoLagRollStats_DateColumn,
          TimeUnit             = AutoLagRollStats_TimeUnits,
          TimeUnitAgg          = AutoLagRollStats_TimeUnits,
          TimeGroups           = AutoLagRollStats_TimeUnits,
          TimeBetween          = NULL,
          RollOnLag1           = AutoLagRollStats_RollOnLag1,
          Type                 = "Lag",
          SimpleImpute         = TRUE,
          Lags                 = AutoLagRollStats_Lags,
          MA_RollWindows       = AutoLagRollStats_MA_RollWindows,
          SD_RollWindows       = AutoLagRollStats_SD_RollWindows,
          Skew_RollWindows     = AutoLagRollStats_Skew_RollWindows,
          Kurt_RollWindows     = AutoLagRollStats_Kurt_RollWindows,
          Quantile_RollWindows = AutoLagRollStats_Quantile_RollWindows,
          Quantiles_Selected   = AutoLagRollStats_Quantiles_Selected,
          Debug = Debug)
      }

      if(length(AutoLagRollStats_GroupVars) > 1L) data.table::set(x, j = paste0(AutoLagRollStats_GroupVars, collapse = '_'), value = NULL)

      # Create code
      if(Debug) print('FE Auto Lag Roll Stats 1')
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Auto Lag Roll Stats\n",
        "temp <- ", DataMuse:::CEP(AutoLagRollStats_SelectData),"\n",
        "AutoLagRollStats_GroupVars <- ", DataMuse:::ExpandText(AutoLagRollStats_GroupVars), "\n",
        "if(length(AutoLagRollStats_GroupVars) > 1L) {\n  ",
        "DataList[[temp]][, paste0(AutoLagRollStats_GroupVars, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(AutoLagRollStats_GroupVars)]\n  ",
        "AutoLagRollStats_GroupVars <- paste0(AutoLagRollStats_GroupVars, collapse = '_')\n",
        "}\n",
        "AutoLagRollStats_Targets <- ", DataMuse:::ExpandText(AutoLagRollStats_Targets), "\n",
        "for(i in AutoLagRollStats_Targets) {",
        "DataList[[temp]] <- Rodeo::AutoLagRollStats(\n    ",
        "data <- DataList[[temp]],\n    ",
        "Targets = i,\n    ",
        "HierarchyGroups = NULL,\n    ",
        "IndependentGroups = AutoLagRollStats_GroupVars,\n    ",
        "DateColumn = ", DataMuse:::CEP(AutoLagRollStats_DateColumn), ",\n    ",
        "TimeUnit = ", DataMuse:::ExpandText(AutoLagRollStats_TimeUnits), ",\n    ",
        "TimeUnitAgg = ", DataMuse:::ExpandText(AutoLagRollStats_TimeUnits), ",\n    ",
        "TimeGroups = ", DataMuse:::ExpandText(AutoLagRollStats_TimeUnits), ",\n    ",
        "TimeBetween = NULL,\n    ",
        "RollOnLag1 = ", DataMuse:::CEP(AutoLagRollStats_RollOnLag1), ",\n    ",
        "Type = 'Lag',\n    ",
        "SimpleImpute = TRUE,\n    ",
        "Lags = ", DataMuse:::ExpandText(AutoLagRollStats_Lags), ",\n    ",
        "MA_RollWindows = ", DataMuse:::ExpandText(AutoLagRollStats_MA_RollWindows), ",\n    ",
        "SD_RollWindows = ", DataMuse:::ExpandText(AutoLagRollStats_SD_RollWindows), ",\n    ",
        "Skew_RollWindows = ", DataMuse:::ExpandText(AutoLagRollStats_Skew_RollWindows), ",\n    ",
        "Kurt_RollWindows = ", DataMuse:::ExpandText(AutoLagRollStats_Kurt_RollWindows),",\n    ",
        "Quantile_RollWindows = ", DataMuse:::ExpandText(AutoLagRollStats_Quantile_RollWindows), ",\n    ",
        "Quantiles_Selected = ", DataMuse:::ExpandText(AutoLagRollStats_Quantiles_Selected), ")\n",
        "}\n"))

      # Return
      if(Debug) {print('FE Auto Lag Roll Stats 2'); print(names(DataList)); print(CodeList)}
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, AutoLagRollStats_SelectData)}, error = function(x) DataList)
      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))
    }
  })
}


#' @title Shiny.FE.CrossRow.Differencing
#'
#' @description server.R observeEvent() for executing AutoDiffLagN() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.CrossRow.Differencing <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Differencing has begun..', value = 0, {
    AutoDiffLagN_DateVariable <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DateVariable']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoDiffLagN_NLag1 <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_NLag1']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
    AutoDiffLagN_NLag2 <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_NLag2']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
    if(length(AutoDiffLagN_DateVariable) != 0 &&
       length(AutoDiffLagN_NLag1) != 0 &&
       length(AutoDiffLagN_NLag2) != 0) {
      AutoDiffLagN_GroupVariables <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_GroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffVariables <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffDateVariables <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffDateVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffGroupVariables <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffGroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoDiffLagN_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      DataList[[AutoDiffLagN_SelectData]][['data']] <- Rodeo::AutoDiffLagN(
        data = DataList[[AutoDiffLagN_SelectData]][['data']],
        DateVariable = AutoDiffLagN_DateVariable,
        GroupVariables = AutoDiffLagN_GroupVariables,
        DiffVariables = AutoDiffLagN_DiffVariables,
        DiffDateVariables = AutoDiffLagN_DiffDateVariables,
        DiffGroupVariables = AutoDiffLagN_DiffGroupVariables,
        NLag1 = AutoDiffLagN_NLag1,
        NLag2 = AutoDiffLagN_NLag2,
        Sort = FALSE,
        RemoveNA = TRUE)
    }

    # Create code
    if(Debug) print('FE Auto Diff Lag N 1')
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Differencing Variables\n",
      "temp <- ", DataMuse:::CEP(AutoDiffLagN_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::AutoDiffLagN(\n  ",
      "data = DataList[[temp]],\n  ",
      "DateVariable = ", DataMuse:::ExpandText(AutoDiffLagN_DateVariable), ",\n  ",
      "GroupVariables = ", DataMuse:::ExpandText(AutoDiffLagN_GroupVariables), ",\n  ",
      "DiffVariables = ", DataMuse:::ExpandText(AutoDiffLagN_DiffVariables), ",\n  ",
      "DiffDateVariables = ", DataMuse:::ExpandText(AutoDiffLagN_DiffDateVariables), ",\n  ",
      "DiffGroupVariables = ", DataMuse:::ExpandText(AutoDiffLagN_DiffGroupVariables), ",\n  ",
      "NLag1 = ", DataMuse:::CEP(AutoDiffLagN_NLag1), ",\n  ",
      "NLag2 = ", DataMuse:::CEP(AutoDiffLagN_NLag2), ",\n  ",
      "Sort = TRUE,\n  ",
      "RemoveNA = TRUE)\n"))

    # Return
    if(Debug) {print('FE Auto Diff Lag N 2'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, AutoDiffLagN_SelectData)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.ModelDataPrep
#'
#' @description server.R observeEvent() for executing ModelDataPrep() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.ModelDataPrep <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Type Casting has begun..', value = 0, {
    ModelDataPrep_IgnoreCols <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IgnoreCols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    ModelDataPrep_CharToFactor <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_CharToFactor']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_FactorToChar <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_FactorToChar']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_DateToChar <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_DateToChar']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_IDateConversion <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IDateConversion']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_RemoveDates <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_RemoveDates']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_IntToNumeric <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IntToNumeric']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_LogicalToBinary <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_LogicalToBinary']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_MissFactor <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_MissFactor']]}, error=function(x) NULL), Type = 'character', Default = FALSE, Debug = Debug)
    ModelDataPrep_MissNum <- DataMuse:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_MissNum']]}, error=function(x) NULL), Type = 'numeric', Default = FALSE, Debug = Debug)
    ModelDataPrep_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$ModelDataPrep_SelectData}, error=function(x) NULL), Type = 'character', Default = FALSE, Debug = Debug)
    DataList[[ModelDataPrep_SelectData]][['data']] <- Rodeo::ModelDataPrep(
      DataList[[ModelDataPrep_SelectData]][['data']],
      Impute          = FALSE,
      CharToFactor    = ModelDataPrep_CharToFactor,
      FactorToChar    = ModelDataPrep_FactorToChar,
      IntToNumeric    = ModelDataPrep_IntToNumeric,
      LogicalToBinary = ModelDataPrep_LogicalToBinary,
      DateToChar      = ModelDataPrep_DateToChar,
      IDateConversion = ModelDataPrep_IDateConversion,
      RemoveDates     = ModelDataPrep_RemoveDates,
      MissFactor      = ModelDataPrep_MissFactor,
      MissNum         = ModelDataPrep_MissNum,
      IgnoreCols      = ModelDataPrep_IgnoreCols)

    # Create code
    if(Debug) print('FE Auto Diff Lag N 1')
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Model Data Prep\n",
      "temp <- ", DataMuse:::CEP(ModelDataPrep_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::ModelDataPrep(","\n  ",
      "DataList[[temp]],\n  ",
      "CharToFactor = ", DataMuse:::CEP(ModelDataPrep_CharToFactor), ",\n  ",
      "FactorToChar = ", DataMuse:::CEP(ModelDataPrep_FactorToChar), ",\n  ",
      "IntToNumeric = ", DataMuse:::CEP(ModelDataPrep_IntToNumeric), ",\n  ",
      "LogicalToBinary = ", DataMuse:::CEP(ModelDataPrep_LogicalToBinary), ",\n  ",
      "DateToChar = ", DataMuse:::CEP(ModelDataPrep_DateToChar), ",\n  ",
      "IDateConversion <- ", DataMuse:::CEP(ModelDataPrep_IDateConversion), ",\n  ",
      "RemoveDates <- ", DataMuse:::CEP(ModelDataPrep_RemoveDates), ",\n  ",
      "MissFactor <- ", DataMuse:::CEP(ModelDataPrep_MissFactor), ",\n  ",
      "MissNum <- ", DataMuse:::CEP(ModelDataPrep_MissNum), ",\n  ",
      "IgnoreCols <- ", DataMuse:::ExpandText(ModelDataPrep_IgnoreCols), ")\n"))

    # Return
    if(Debug) {print('FE Auto Diff Lag N 2'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, ModelDataPrep_SelectData)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.PartitionData
#'
#' @description server.R observeEvent() for executing AutoDataPartition() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.PartitionData <- function(input,output,session,DataList,CodeList,TabCount=5L,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Data Partitioning has begun..', value = 0, {
    AutoDataPartition_NumDataSets <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_NumDataSets']]}, error=function(x) NULL), Type = 'numeric', Default = 3L, Debug = Debug)
    AutoDataPartition_Ratios_Train <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Train']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.70), Debug = Debug)
    AutoDataPartition_Ratios_Validation <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Validation']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.20), Debug = Debug)
    AutoDataPartition_Ratios_Test <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Test']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.10), Debug = Debug)
    AutoDataPartition_PartitionType <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_PartitionType']]}, error=function(x) NULL), Type = 'character', Default = "random", Debug = Debug)
    AutoDataPartition_StratifyColumnNames <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_StratifyColumnNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoDataPartition_TimeColumnName <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_TimeColumnName']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoDataPartition_SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoDataPartition_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

    if(AutoDataPartition_NumDataSets == 2L) {
      xx <- AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation < 1 || AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation > 1
      AutoDataPartition_Ratios_Test <- NULL
    } else {
      xx <- AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation + AutoDataPartition_Ratios_Test < 1 || AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation + AutoDataPartition_Ratios_Test > 1
    }

    if(!xx) {
      DataSets <- Rodeo::AutoDataPartition(
        DataList[[AutoDataPartition_SelectData]][['data']],
        NumDataSets = AutoDataPartition_NumDataSets,
        Ratios = c(AutoDataPartition_Ratios_Train, AutoDataPartition_Ratios_Validation, AutoDataPartition_Ratios_Test),
        PartitionType = AutoDataPartition_PartitionType,
        StratifyColumnNames = AutoDataPartition_StratifyColumnNames,
        TimeColumnName = AutoDataPartition_TimeColumnName)
      DataList[[paste0(AutoDataPartition_SelectData, '_TrainData')]][['data']] <- DataSets[['TrainData']]
      DataList[[paste0(AutoDataPartition_SelectData, '_ValidationData')]][['data']] <- DataSets[['ValidationData']]
      DataList[[paste0(AutoDataPartition_SelectData, '_TestData')]][['data']] <- DataSets[['TestData']]
      rm(DataSets)

      # Create code
      if(Debug) print('FE Auto Diff Lag N 1')
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Data Partition\n",
        "temp <- ", DataMuse:::CEP(AutoDataPartition_SelectData),"\n",
        "DataSets <- Rodeo::AutoDataPartition(\n  ",
        "DataList[[temp]],\n  ",
        "NumDataSets = ", DataMuse:::CEP(AutoDataPartition_NumDataSets), ",\n  ",
        "Ratios = ", DataMuse:::ExpandText(c(AutoDataPartition_Ratios_Train, AutoDataPartition_Ratios_Validation, AutoDataPartition_Ratios_Test)), ",\n  ",
        "PartitionType = ", DataMuse:::CEP(AutoDataPartition_PartitionType), ",\n  ",
        "StratifyColumnNames = ", DataMuse:::ExpandText(AutoDataPartition_StratifyColumnNames), ",\n  ",
        "TimeColumnName = ", DataMuse:::CEP(AutoDataPartition_TimeColumnName), ")\n",
        "DataList[[paste0(temp, '_TrainData')]] <- DataSets[['TrainData']]\n",
        "DataList[[paste0(temp, '_ValidationData')]] <- DataSets[['ValidationData']]\n",
        "DataList[[paste0(temp, '_TestData')]] <- DataSets[['TestData']]\n",
        "rm(DataSets)\n"))

      # Return
      if(Debug) {print('FE Data Partition 2'); print(names(DataList)); print(CodeList)}
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, paste0(AutoDataPartition_SelectData, '_TrainData'))}, error = function(x) DataList)
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, paste0(AutoDataPartition_SelectData, '_ValidationData'))}, error = function(x) DataList)
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, paste0(AutoDataPartition_SelectData, '_TestData'))}, error = function(x) DataList)

      # Add data to DataOutputSelection Page
      for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      DataMuse::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
      for(i in seq_len(TabCount)) DataMuse::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Partition ratios do not sum to 1.0', type = NULL, btn_labels = 'warning', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })
}

#' @title Shiny.FE.Word2Vec.H2O
#'
#' @description server.R Word2Vec_H2O() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.Word2Vec.H2O <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  if(Debug) print('FE Word2Vec H2O 1')

  # Data
  stringCol <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_stringCol']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(Debug) print(input[['H2O_Word2Vec_TrainData']])
  temp_train <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TrainData <- DataList[[temp_train]][['data']]
  temp_validate <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]][['data']] else ValidationData <- NULL
  temp_test <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_test) > 0L) TestData <- DataList[[temp_test]][['data']] else TestData <- NULL
  if(Debug) print('FE Word2Vec H2O 2')

  # Create code
  if(Debug) print('FFE Word2Vec H2O 3')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Word2Vec H2O: Data\n",
    "stringCol <- ", DataMuse:::ExpandText(stringCol),"\n",
    "temp_train <- ", DataMuse:::CEP(temp_train),"\n",
    "TrainData <- DataList[[temp_train]]\n",
    "temp_validate <- ", DataMuse:::CEP(temp_validate),"\n",
    "if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]] else ValidationData <- NULL\n",
    "temp_test <- ", DataMuse:::CEP(temp_test),"\n",
    "if(length(temp_test) > 0L) TestData <- DataList[[temp_test]] else TestData <- NULL\n"))

  # Build
  if(length(stringCol) > 0L && length(TrainData) > 0L) {

    # AutoEncoder_H2O
    shiny::withProgress(message = 'Word2Vec H2O has begun..', value = 0, {

      # Args
      BuildType <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_BuildType']]}, error=function(x) NULL), Type = 'character', Default = 'combined', Debug = Debug)
      KeepStringCol <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_KeepStringCol']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      vects <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_vects']]}, error=function(x) NULL), Type = 'numeric', Default = 30, Debug = Debug)
      H2O_Word2Vec_MinWords <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_MinWords']]}, error=function(x) NULL), Type = 'numeric', Default = 1, Debug = Debug)
      WindowSize <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_WindowSize']]}, error=function(x) NULL), Type = 'numeric', Default = 5, Debug = Debug)
      Epochs <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_Epochs']]}, error=function(x) NULL), Type = 'numeric', Default = 10, Debug = Debug)
      MinWords <- DataMuse:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_MinWords']]}, error=function(x) NULL), Type = 'numeric', Default = 2, Debug = Debug)

      # Args tracking
      ModelID <- 'temp'
      model_path <- NULL
      Threads <- max(1L, parallel::detectCores()-2L)
      MaxMemory <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      # Create code
      if(Debug) print('WFE Word2Vec H2O 4')
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Word2Vec H2O Args\n",
        "BuildType <- ", DataMuse:::CEP(BuildType),"\n",
        "KeepStringCol <- ", DataMuse:::CEP(KeepStringCol),"\n",
        "vects <- ", DataMuse:::CEP(vects), "\n",
        "H2O_Word2Vec_MinWords <- ", DataMuse:::CEP(H2O_Word2Vec_MinWords),"\n",
        "WindowSize <- ", DataMuse:::CEP(WindowSize), "\n",
        "Epochs <- ", DataMuse:::CEP(Epochs), "\n",
        "MinWords <- ", DataMuse:::CEP(MinWords), "\n",
        "ModelID <- ", DataMuse:::CEP(ModelID), "\n",
        "model_path <- ", DataMuse:::CEP(model_path), "\n",
        "Threads <- ", DataMuse:::CEP(Threads), "\n",
        "gc()\n",
        "MaxMemory <- ", DataMuse:::CEP(MaxMem), "\n"))

      # Run function
      if(Debug) print('FE Word2Vec H2O 3')

      # MetaData
      Start <- Sys.time()
      tempnames <- names(TrainData)

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Word2Vec H2O MetaData\n",
        "Start <- Sys.time()\n",
        "tempnames <- names(TrainData)\n"))

      # Run AutoWord2VecModeler
      TrainData <- Rodeo::AutoWord2VecModeler(
        data = TrainData,
        BuildType = BuildType,
        stringCol = stringCol,
        KeepStringCol = TRUE,
        ModelID = ModelID,
        model_path = model_path,
        vects = vects,
        MinWords = MinWords,
        WindowSize = WindowSize,
        Epochs = Epochs,
        SaveModel = "standard",
        Threads = Threads,
        MaxMemory = MaxMemory)

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Word2Vec H2O Generate\n",
        "TrainData <- Rodeo::AutoWord2VecModeler(\n  ",
        "data = TrainData,\n  ",
        "BuildType = BuildType,\n  ",
        "stringCol = stringCol,\n  ",
        "KeepStringCol = TRUE,\n  ",
        "ModelID = ModelID,\n  ",
        "model_path = model_path,\n  ",
        "vects = vects,\n  ",
        "MinWords = MinWords,\n  ",
        "WindowSize = WindowSize,\n  ",
        "Epochs = Epoch,\n  ",
        "SaveModel = 'standard',\n  ",
        "Threads = Threads)\n"))

      # Updates
      DataList[[temp_train]][['data']] <- TrainData; rm(TrainData); gc()
      shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Update DataList\n",
        "DataList[[temp_train]] <- TrainData; rm(TrainData); gc()\n"))

      # Run time tracking
      End <- Sys.time()
      H2OWord2Vec_Training <- difftime(End, Start, units = "mins")

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Word2Vec H2O Generate\n",
        "End <- Sys.time()\n",
        "H2OWord2Vec_Training <- difftime(End, Start, units = 'mins')\n"))

      # Score validation data
      if(!is.null(ValidationData)) {
        ValidationData <- Rodeo::AutoWord2VecScoring(
          data = ValidationData,
          BuildType = BuildType,
          stringCol = stringCol,
          KeepStringCol = TRUE,
          ModelID = ModelID,
          ModelObject = NULL,
          model_path = model_path,
          H2OStartUp = TRUE,
          H2OShutdown = TRUE,
          Threads = Threads,
          MaxMemory = MaxMemory)
        DataList[[temp_validate]][['data']] <- ValidationData; rm(ValidationData); gc()
        shiny::incProgress(2/3, detail = 'Validation Data is complete. Test Data is next up')

        # Code
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score validation data\n",
          "if(!is.null(ValidationData)) {\n  ",
          "ValidationData <- Rodeo::AutoWord2VecScoring(\n    ",
          "data = ValidationData,\n    ",
          "BuildType = BuildType,\n    ",
          "stringCol = stringCol,\n    ",
          "KeepStringCol = TRUE,\n    ",
          "ModelID = ModelID,\n    ",
          "ModelObject = NULL,\n    ",
          "model_path = model_path,\n    ",
          "H2OStartUp = TRUE,\n    ",
          "H2OShutdown = TRUE,\n    ",
          "Threads = Threads,\n    ",
          "MaxMemory = MaxMemory)\n  ",
          "DataList[[temp_validate]] <- ValidationData; rm(ValidationData); gc()\n",
          "}\n"))

      } else {
        shiny::incProgress(2/3, detail = 'Validation Data is NULL')
      }

      # Score test data
      if(!is.null(TestData)) {
        TestData <- Rodeo::AutoWord2VecScoring(
          data = TestData,
          BuildType = BuildType,
          stringCol = stringCol,
          KeepStringCol = KeepStringCol,
          ModelID = ModelID,
          ModelObject = NULL,
          model_path = model_path,
          H2OStartUp = TRUE,
          H2OShutdown = TRUE,
          Threads = Threads,
          MaxMemory = MaxMemory)
        DataList[[temp_test]]['data'] <- TestData; rm(TestData); gc()
        shiny::incProgress(3/3, detail = 'Test Data is complete')

        # Code
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score test data\n",
          "if(!is.null(ValidationData)) {\n  ",
          "TestData <- Rodeo::AutoWord2VecScoring(\n    ",
          "data = TestData,\n    ",
          "BuildType = BuildType,\n    ",
          "stringCol = stringCol,\n    ",
          "KeepStringCol = TRUE,\n    ",
          "ModelID = ModelID,\n    ",
          "ModelObject = NULL,\n    ",
          "model_path = model_path,\n    ",
          "H2OStartUp = TRUE,\n    ",
          "H2OShutdown = TRUE,\n    ",
          "Threads = Threads,\n    ",
          "MaxMemory = MaxMemory)\n  ",
          "DataList[[temp_test]] <- TestData; rm(TestData); gc()\n",
          "}\n"))

      } else {
        shiny::incProgress(3/3, detail = 'Test Data is NULL')
      }

      # Update meta
      if(Debug) {print('FE Word2Vec_H2O 5'); print(names(DataList)); print(CodeList)}
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_train)}, error = function(x) DataList)
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_validate)}, error = function(x) DataList)
      DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_test)}, error = function(x) DataList)
      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))
    })
  } else {
    shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Text Columns is NULL or TrainData is NULL. Check to see if those inputs were filled out.', type = NULL, btn_labels = 'warning', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  }
}

#' @title Shiny.FE.DimReduction.AutoEncoder.H2O
#'
#' @description server.R observeEvent() for executing AutoEncoder_H2O() for given inputs from app
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.DimReduction.AutoEncoder.H2O <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Initialize List
  if(!exists('ArgsList')) ArgsList <- list()

  # Features
  Features <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_Features}, error = function(x) NULL), Type = 'character', Default = NULL)

  # Data
  if(Debug) print(input[['AutoEncoder_H2O_TrainData']])
  temp_train <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoEncoder_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TrainData <- DataList[[temp_train]][['data']]

  # ValidationData
  temp_validate <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoEncoder_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]][['data']] else ValidationData <- NULL

  # TestData
  temp_test <- DataMuse:::ReturnParam(xx = tryCatch({input[['AutoEncoder_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_test) > 0L) TestData <- DataList[[temp_test]][['data']] else TestData <- NULL


  # Create code
  if(Debug) print('FFE Word2Vec H2O 3')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# FE AutoEncoder H2O: Data\n",
    "Features <- ", DataMuse:::ExpandText(Features),"\n",
    "temp_train <- ", DataMuse:::CEP(temp_train),"\n",
    "TrainData <- DataList[[temp_train]]\n",
    "temp_validate <- ", DataMuse:::CEP(temp_validate),"\n",
    "if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]] else ValidationData <- NULL\n",
    "temp_test <- ", DataMuse:::CEP(temp_test),"\n",
    "if(length(temp_test) > 0L) TestData <- DataList[[temp_test]] else TestData <- NULL\n"))


  # Build
  if(length(Features) > 0L && length(TrainData) > 0L) {

    # AutoEncoder_H2O
    shiny::withProgress(message = 'AutoEncoder H2O has begun..', value = 0, {

      if(Debug) print('FE AutoEncoder H2O 2')

      # Non-Data Args
      AnomalyDetection <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_AnomalyDetection}, error = function(x) NULL), Type = 'logical', Default = FALSE)
      DimensionReduction <- TRUE
      per_feature <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_per_feature}, error = function(x) NULL), Type = 'logical', Default = FALSE)
      RemoveFeatures <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_RemoveFeatures}, error = function(x) NULL), Type = 'logical', Default = FALSE)
      LayerStructure <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_LayerStructure}, error = function(x) NULL), Type = 'numeric', Default = 2L)
      NodeShrinkRate <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_NodeShrinkRate}, error = function(x) NULL), Type = 'numeric', Default = sqrt(5)/2-0.5)
      ReturnLayer <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ReturnLayer}, error = function(x) NULL), Type = 'numeric', Default = 2L)
      Epochs <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_Epochs}, error = function(x) NULL), Type = 'numeric', Default = NULL)
      L2 <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_L2}, error = function(x) NULL), Type = 'numeric', Default = NULL)
      ElasticAveraging <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ElasticAveraging}, error = function(x) NULL), Type = 'logical', Default = NULL)
      ElasticAveragingMovingRate <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ElasticAveragingMovingRate}, error = function(x) NULL), Type = 'numeric', Default = NULL)
      ElasticAveragingRegularization <- DataMuse:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ElasticAveragingRegularization}, error = function(x) NULL), Type = 'numeric', Default = NULL)

      # Create Layer Structure
      LS <- c()
      Nodes <- length(Features)
      for(i in seq_len(LayerStructure)) LS <- c(LS, max(1, floor(Nodes * NodeShrinkRate ^ i)))
      LayerStructure <- LS

      # Args tracking
      ModelID <- 'temp'
      Models_Path <- getwd()
      NThreads <- max(1L, parallel::detectCores()-2L)
      MaxMem <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      # Create code
      if(Debug) print('FE AutoEncoder H2O 3')
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# AutoEncoder H2O Args\n",
        "AnomalyDetection <- ", DataMuse:::CEPP(AnomalyDetection),"\n",
        "DimensionReduction <- ", DataMuse:::CEPP(DimensionReduction),"\n",
        "per_feature <- ", DataMuse:::CEPP(per_feature), "\n",
        "RemoveFeatures <- ", DataMuse:::CEPP(RemoveFeatures),"\n",
        "LayerStructure <- ", DataMuse:::ExpandText(LayerStructure), "\n",
        "NodeShrinkRate <- ", DataMuse:::CEPP(NodeShrinkRate), "\n",
        "Epochs <- ", DataMuse:::CEPP(Epochs), "\n",
        "L2 <- ", DataMuse:::CEPP(L2), "\n",
        "ElasticAveraging <- ", DataMuse:::CEPP(ElasticAveraging), "\n",
        "ElasticAveragingMovingRate <- ", DataMuse:::CEPP(ElasticAveragingMovingRate), "\n",
        "ElasticAveragingRegularization <- ", DataMuse:::CEPP(ElasticAveragingRegularization), "\n",
        "ModelID <- ", DataMuse:::CEP(ModelID), "\n",
        "Models_Path <- ", DataMuse:::CEP(Models_Path), "\n",
        "NThreads <- ", DataMuse:::CEP(NThreads), "\n",
        "gc()\n",
        "MaxMem <- ", DataMuse:::CEP(MaxMem), "\n"))

      # Run function
      if(Debug) print('FE AutoEncoder H2O 3')
      TrainData <- Rodeo::ModelDataPrep(data=TrainData, Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(ValidationData) > 0L) ValidationData <- Rodeo::ModelDataPrep(data=ValidationData, Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(TestData) > 0L) TestData <- Rodeo::ModelDataPrep(data=TestData, Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# TrainData: Convert character columns to factors\n",
        "TrainData <- Rodeo::ModelDataPrep(\n  ",
        "data = TrainData,\n  ",
        "Impute = TRUE,\n  ",
        "CharToFactor = TRUE,\n  ",
        "FactorToChar = FALSE,\n  ",
        "IntToNumeric = TRUE,\n  ",
        "LogicalToBinary = TRUE,\n  ",
        "DateToChar = FALSE,\n  ",
        "IDateConversion = FALSE,\n  ",
        "RemoveDates = FALSE,\n  ",
        "MissFactor = 'missing',\n  ",
        "MissNum = -1,\n  ",
        "IgnoreCols=NULL)\n"))

      # Code
      if(length(ValidationData) > 0L) {
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ValidationData: Convert character columns to factors\n",
          "if(length(ValidationData) > 0L) {\n  ",
          "ValidationData <- Rodeo::ModelDataPrep(\n    ",
          "data = ValidationData,\n    ",
          "Impute = TRUE,\n    ",
          "CharToFactor = TRUE,\n    ",
          "FactorToChar = FALSE,\n    ",
          "IntToNumeric = TRUE,\n    ",
          "LogicalToBinary = TRUE,\n    ",
          "DateToChar = FALSE,\n    ",
          "IDateConversion = FALSE,\n    ",
          "RemoveDates = FALSE,\n    ",
          "MissFactor = 'missing',\n    ",
          "MissNum = -1,\n    ",
          "IgnoreCols=NULL)\n  ",
          "}\n"))
      }

      # Code
      if(length(TestData) > 0L) {
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# TestData: Convert character columns to factors\n",
          "if(length(TestData) > 0L) {\n  ",
          "TestData <- Rodeo::ModelDataPrep(\n    ",
          "data = TestData,\n    ",
          "Impute = TRUE,\n    ",
          "CharToFactor = TRUE,\n    ",
          "FactorToChar = FALSE,\n    ",
          "IntToNumeric = TRUE,\n    ",
          "LogicalToBinary = TRUE,\n    ",
          "DateToChar = FALSE,\n    ",
          "IDateConversion = FALSE,\n    ",
          "RemoveDates = FALSE,\n    ",
          "MissFactor = 'missing',\n    ",
          "MissNum = -1,\n    ",
          "IgnoreCols=NULL)\n  ",
          "}\n"))
      }

      if(Debug) print('AE ::: 1')

      # Metadata Args
      if(!exists('ModelID') || length(ModelID) == 0L) ModelID <- 'temp1'
      if(!exists('Models_Path') || length(Models_Path) == 0L) Models_Path <- getwd()

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Metadata Args\n",
        "if(length(ModelID) == 0L) ModelID <- 'temp1'\n",
        "if(length(Models_Path) == 0L) Models_Path <- getwd()\n"))

      if(Debug) print('AE ::: 2')

      # Metadata
      Start <- Sys.time()
      tempnames <- names(data.table::copy(TrainData))

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Metadata\n",
        "Start <- Sys.time()\n",
        "tempnames <- names(data.table::copy(TrainData))\n"
      ))

      # Run function
      TrainData <- Rodeo::H2OAutoencoder(

        # Select the service
        AnomalyDetection = AnomalyDetection,
        DimensionReduction = DimensionReduction,

        # Data related args
        data = TrainData,
        Features = Features,
        per_feature = per_feature,
        RemoveFeatures = RemoveFeatures,
        ModelID = ModelID,
        model_path = Models_Path,

        # H2O Environment
        NThreads = NThreads,
        MaxMem = MaxMem,
        H2OStart = TRUE,
        H2OShutdown = TRUE,

        # H2O ML Args
        LayerStructure = LayerStructure,
        NodeShrinkRate = NodeShrinkRate,
        ReturnLayer = length(LayerStructure),
        Activation = "Tanh",
        Epochs = Epochs,
        L2 = L2,
        ElasticAveraging = ElasticAveraging,
        ElasticAveragingMovingRate = ElasticAveragingMovingRate,
        ElasticAveragingRegularization = ElasticAveragingRegularization)

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Run function\n",
        "TrainData <- Rodeo::H2OAutoencoder(\n\n  ",
        "# Select services\n  ",
        "AnomalyDetection = AnomalyDetection,\n  ",
        "DimensionReduction = DimensionReduction,\n\n  ",
        "# Data related args\n  ",
        "data = TrainData,\n  ",
        "Features = Features,\n  ",
        "per_feature = per_feature,\n  ",
        "RemoveFeatures = RemoveFeatures,\n  ",
        "ModelID = ModelID,\n  ",
        "model_path = Models_Path,\n\n  ",
        "# H2O Environment\n  ",
        "NThreads = NThreads,\n  ",
        "MaxMem = MaxMem,\n  ",
        "H2OStart = TRUE,\n  ",
        "H2OShutdown = TRUE,\n\n  ",
        "# H2O ML Args\n  ",
        "LayerStructure = LayerStructure,\n  ",
        "NodeShrinkRate = NodeShrinkRate,\n  ",
        "ReturnLayer = length(LayerStructure),\n  ",
        "Activation = 'Tanh',\n  ",
        "Epochs = Epochs,\n  ",
        "L2 = L2,\n  ",
        "ElasticAveraging = ElasticAveraging,\n  ",
        "ElasticAveragingMovingRate = ElasticAveragingMovingRate,\n  ",
        "ElasticAveragingRegularization = ElasticAveragingRegularization)\n"))

      # Store Data
      DataList[[temp_train]][['data']] <- TrainData

      if(Debug) print('AE ::: 3')

      # New columns tracking
      NewColumns <- setdiff(names(data.table::copy(TrainData)), tempnames)
      rm(TrainData); gc()

      # Increment the progress bar, and update the detail text.
      shiny::incProgress(1/3, detail = 'Train Data is complete. Moving on to Validation Data')

      if(Debug) print('AE ::: 4')

      # Run time tracking
      End <- Sys.time()
      RunTime_Training <- difftime(End, Start, units = "mins")

      if(Debug) print('AE ::: 5')

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Store Data\n",
        "DataList[[temp_train]] <- TrainData\n",
        "gc()\n\n",
        "# Run time tracking\n",
        "End <- Sys.time()\n",
        "RunTime_Training <- difftime(End, Start, units = 'mins')\n"))

      # Score validation Data
      if(length(ValidationData) > 0L) {

        if(Debug) print('AE ::: 6')

        # Pause
        Sys.sleep(8L)

        # Score model
        ValidationData <- Rodeo::H2OAutoencoderScoring(

          # Select the service
          AnomalyDetection = AnomalyDetection,
          DimensionReduction = DimensionReduction,

          # Data related args
          data = ValidationData,
          Features = Features,
          per_feature = per_feature,
          RemoveFeatures = RemoveFeatures,
          ModelObject = NULL,
          ModelID = ModelID,
          model_path = Models_Path,

          # H2O args
          NThreads = NThreads,
          MaxMem = MaxMem,
          H2OStart = TRUE,
          H2OShutdown = TRUE,
          ReturnLayer = ReturnLayer)

        DataList[[temp_validate]][['data']] <- ValidationData
        rm(ValidationData); gc()

        # Code
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score Validation Data\n",
          "if(length(ValidationData) > 0L) {\n  ",
          "Sys.sleep(8L)\n\n  ",
          "# Score model\n  ",
          "ValidationData <- Rodeo::H2OAutoencoderScoring(\n\n    ",
          "# Select the service\n    ",
          "AnomalyDetection = AnomalyDetection,\n    ",
          "DimensionReduction = DimensionReduction,\n\n    ",
          "# Data related args\n    ",
          "data = ValidationData,\n    ",
          "Features = Features,\n    ",
          "per_feature = per_feature,\n    ",
          "RemoveFeatures = RemoveFeatures,\n    ",
          "ModelObject = NULL,\n    ",
          "ModelID = ModelID,\n    ",
          "model_path = Models_Path,\n\n    ",
          "# H2O args\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMem = MaxMem,\n    ",
          "H2OStart = TRUE,\n    ",
          "H2OShutdown = TRUE,\n    ",
          "ReturnLayer = length(LayerStructure))\n  ",
          "DataList[[temp_validate]] <- ValidationData\n  ",
          "rm(ValidationData); gc()\n",
          "}\n"))

        shiny::incProgress(2/3, detail = 'Validation data is complete. Moving on to Test Data')

      } else {
        shiny::incProgress(2/3, detail = 'Validation Data not supplied')
      }

      # Score Test Data
      if(length(TestData) > 0L) {

        if(Debug) print('AE ::: 7')

        # Pause
        Sys.sleep(8L)

        # Score model
        TestData <- Rodeo::H2OAutoencoderScoring(

          # Select the service
          AnomalyDetection = AnomalyDetection,
          DimensionReduction = DimensionReduction,

          # Data related args
          data = TestData,
          Features = Features,
          per_feature = per_feature,
          RemoveFeatures = RemoveFeatures,
          ModelObject = NULL,
          ModelID = ModelID,
          model_path = Models_Path,

          # H2O args
          NThreads = NThreads,
          MaxMem = MaxMem,
          H2OStart = TRUE,
          H2OShutdown = TRUE,
          ReturnLayer = ReturnLayer)

        # Store Data
        DataList[[temp_test]][['data']] <- TestData
        rm(TestData); gc()

        # Code
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score Test Data\n",
          "if(length(TestData) > 0L) {\n  ",
          "Sys.sleep(8L)\n\n  ",
          "# Score model\n  ",
          "TestData <- Rodeo::H2OAutoencoderScoring(\n\n    ",
          "# Select the service\n    ",
          "AnomalyDetection = AnomalyDetection,\n    ",
          "DimensionReduction = DimensionReduction,\n\n    ",
          "# Data related args\n    ",
          "data = TestData,\n    ",
          "Features = Features,\n    ",
          "per_feature = per_feature,\n    ",
          "RemoveFeatures = RemoveFeatures,\n    ",
          "ModelObject = NULL,\n    ",
          "ModelID = ModelID,\n    ",
          "model_path = Models_Path,\n\n    ",
          "# H2O args\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMem = MaxMem,\n    ",
          "H2OStart = TRUE,\n    ",
          "H2OShutdown = TRUE,\n    ",
          "ReturnLayer = length(LayerStructure))\n  ",
          "DataList[[temp_validate]] <- TestData\n  ",
          "rm(TestData); gc()\n",
          "}\n"))

        shiny::incProgress(3/3, detail = 'Test data is complete.')
      } else {
        shiny::incProgress(3/3, detail = 'Test Data not supplied')
      }

      # Collect Output
      if(Debug) print('FE AutoEncoder H2O 4')
    })

    # Return
    if(Debug) {print('FE Word2Vec_H2O 5'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_train)}, error = function(x) DataList)
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_validate)}, error = function(x) DataList)
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_test)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  } else {
    shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Text Columns is NULL or TrainData is NULL. Check to see if those inputs were filled out.', type = NULL, btn_labels = 'warning', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  }
}

#' @title Shiny.FE.AnomalyDetection.IsolationForest.H2O
#'
#' @description Utilize an H2O isolation forest to provide anomaly detection
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.AnomalyDetection.IsolationForest.H2O <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Initialize List
  if(!exists('ArgsList')) ArgsList <- list()

  # Features
  Features <- DataMuse:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_Features}, error = function(x) NULL), Type = 'character', Default = NULL)

  # Data
  if(Debug) print(input[['IsolationForest_H2O_TrainData']])
  temp_train <- DataMuse:::ReturnParam(xx = tryCatch({input[['IsolationForest_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # ValidationData
  temp_validate <- DataMuse:::ReturnParam(xx = tryCatch({input[['IsolationForest_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # TestData
  temp_test <- DataMuse:::ReturnParam(xx = tryCatch({input[['IsolationForest_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)


  # Create code
  if(Debug) print('FE IsolationForest H2O 3')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# FE IsolationForest H2O: Data\n",
    "Features <- ", DataMuse:::ExpandText(Features),"\n",
    "temp_train <- ", DataMuse:::CEP(temp_train),"\n",
    "temp_validate <- ", DataMuse:::CEP(temp_validate),"\n",
    "temp_test <- ", DataMuse:::CEP(temp_test),"\n"))

  # Build
  if(length(Features) > 0L && length(temp_train) > 0L) {

    # AutoEncoder_H2O
    shiny::withProgress(message = 'IsolationForest H2O has begun..', value = 0, {

      if(Debug) print('FE IsolationForest H2O 2')
      IDcols <- setdiff(names(DataList[[temp_train]][['data']]), Features)
      Threshold <- DataMuse:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_Threshold}, error = function(x) NULL), Type = 'numeric', Default = 0.95)
      NTrees <- DataMuse:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_NTrees}, error = function(x) NULL), Type = 'numeric', Default = 50)
      MaxDepth <- DataMuse:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_MaxDepth}, error = function(x) NULL), Type = 'numeric', Default = 20)
      MinRows <- DataMuse:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_MinRows}, error = function(x) NULL), Type = 'numeric', Default = 1)
      RowSampleRate <- DataMuse:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_RowSampleRate}, error = function(x) NULL), Type = 'numeric', Default = 1)
      ColSampleRate <- DataMuse:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_ColSampleRate}, error = function(x) NULL), Type = 'numeric', Default = 1)
      ColSampleRatePerLevel <- DataMuse:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_ColSampleRatePerLevel}, error = function(x) NULL), Type = 'numeric', Default = 1)
      ColSampleRatePerTree <- DataMuse:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_ColSampleRatePerTree}, error = function(x) NULL), Type = 'numeric', Default = 1)
      CategoricalEncoding <- 'AUTO'

      # Args tracking
      ModelID <- 'temp'
      SavePath <- getwd()
      NThreads <- max(1L, parallel::detectCores()-2L)
      MaxMem <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      # Create code
      if(Debug) print('FE IsolationForest H2O 3')
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# IsolationForest H2O Args\n",
        "IDcols <- ", DataMuse:::CEP(IDcols),"\n",
        "ModelID <- ", DataMuse:::CEP(ModelID),"\n",
        "Threshold <- ", DataMuse:::CEP(Threshold), "\n",
        "NTrees <- ", DataMuse:::CEP(NTrees),"\n",
        "MaxDepth <- ", DataMuse:::CEP(MaxDepth), "\n",
        "MinRows <- ", DataMuse:::CEP(MinRows), "\n",
        "RowSampleRate <- ", DataMuse:::CEP(RowSampleRate), "\n",
        "ColSampleRate <- ", DataMuse:::CEP(ColSampleRate), "\n",
        "ColSampleRatePerLevel <- ", DataMuse:::CEP(ColSampleRatePerLevel), "\n",
        "ColSampleRatePerTree <- ", DataMuse:::CEP(ColSampleRatePerTree), "\n",
        "CategoricalEncoding <- ", DataMuse:::CEP(CategoricalEncoding), "\n",
        "NThreads <- ", DataMuse:::CEP(NThreads), "\n",
        "gc()\n",
        "MaxMem <- ", DataMuse:::CEP(MaxMem), "\n"))

      # Run function
      if(Debug) print('FE IsolationForest H2O 3')
      DataList[[temp_train]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_train]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_validate) > 0L) DataList[[temp_validate]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_validate]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_test) > 0L) DataList[[temp_test]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_test]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_train]]: Convert character columns to factors\n",
        "DataList[[temp_train]] <- Rodeo::ModelDataPrep(\n  ",
        "data = DataList[[temp_train]],\n  ",
        "Impute = TRUE,\n  ",
        "CharToFactor = TRUE,\n  ",
        "FactorToChar = FALSE,\n  ",
        "IntToNumeric = TRUE,\n  ",
        "LogicalToBinary = TRUE,\n  ",
        "DateToChar = FALSE,\n  ",
        "IDateConversion = FALSE,\n  ",
        "RemoveDates = FALSE,\n  ",
        "MissFactor = 'missing',\n  ",
        "MissNum = -1,\n  ",
        "IgnoreCols=NULL)\n"))

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_validate]]: Convert character columns to factors\n",
        "if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {\n  ",
        "DataList[[temp_validate]] <- Rodeo::ModelDataPrep(\n    ",
        "data = DataList[[temp_validate]],\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"))

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_test]]: Convert character columns to factors\n",
        "if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {\n  ",
        "DataList[[temp_test]] <- Rodeo::ModelDataPrep(\n    ",
        "data = DataList[[temp_test]],\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"))

      if(Debug) print('AE ::: 1')

      # Run function
      DataList[[temp_train]][['data']] <- Rodeo::H2OIsolationForest(
        data = DataList[[temp_train]][['data']],
        Features = Features,
        IDcols = IDcols,
        ModelID = ModelID,
        SavePath = SavePath,
        NThreads = NThreads,
        MaxMem = MaxMem,
        Threshold = Threshold,
        NTrees = NTrees,
        MaxDepth = MaxDepth,
        MinRows = MinRows,
        RowSampleRate = RowSampleRate,
        ColSampleRate = ColSampleRate,
        ColSampleRatePerLevel = ColSampleRatePerLevel,
        ColSampleRatePerTree = ColSampleRatePerTree,
        CategoricalEncoding = CategoricalEncoding,
        Debug = Debug)

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_train]]: Convert character columns to factors\n",
        "if(length(DataList[[temp_train]]) > 0L) {\n  ",
        "DataList[[temp_train]] <- Rodeo::H2OIsolationForest(\n    ",
        "data = DataList[[temp_train]],\n    ",
        "Features = Features,\n    ",
        "IDcols = IDcols,\n    ",
        "ModelID = ModelID,\n    ",
        "SavePath = SavePath,\n    ",
        "NThreads = NThreads,\n    ",
        "MaxMem = MaxMem,\n    ",
        "Threshold = Threshold,\n    ",
        "NTrees = NTrees,\n    ",
        "MaxDepth = MaxDepth',\n    ",
        "MinRows = MinRows',\n    ",
        "RowSampleRate = RowSampleRate',\n    ",
        "ColSampleRate = ColSampleRate',\n    ",
        "ColSampleRatePerLevel = ColSampleRatePerLevel',\n    ",
        "ColSampleRatePerTree = ColSampleRatePerTree',\n    ",
        "CategoricalEncoding = CategoricalEncoding',\n    ",
        "Debug = Debug)\n",
        "}\n"))

      # Updates
      shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')

      if(Debug) print('AE ::: 2')

      # Score validation data
      if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {

        # Pause
        Sys.sleep(10L)

        # Score model
        DataList[[temp_validate]][['data']] <- Rodeo::H2OIsolationForestScoring(
          data = DataList[[temp_validate]][['data']],
          Features = Features,
          IDcols = IDcols,
          H2OStart = TRUE,
          H2OShutdown = TRUE,
          ModelID = ModelID,
          SavePath = SavePath,
          Threshold = Threshold,
          MaxMem = MaxMem,
          NThreads = NThreads,
          Debug = Debug)

        # Code
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score test data\n",
          "if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {\n  ",
          "DataList[[temp_validate]] <- Rodeo::H2OIsolationForestScoring(\n    ",
          "data = DataList[[temp_validate]],\n    ",
          "Features = Features,\n    ",
          "IDcols = IDcols,\n    ",
          "H2OStart = TRUE,\n    ",
          "H2OShutdown = TRUE,\n    ",
          "ModelID = ModelID,\n    ",
          "ModelObject = NULL,\n    ",
          "model_path = SavePath,\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMem = MaxMem)\n",
          "}\n"))

        # Updates
        shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')
      }

      # Score Test Data
      if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {

        # Pause
        Sys.sleep(10L)

        # Score model
        DataList[[temp_test]] <- Rodeo::H2OIsolationForestScoring(
          data = DataList[[temp_test]][['data']],
          Features = Features,
          IDcols = IDcols,
          H2OStart = TRUE,
          H2OShutdown = TRUE,
          ModelID = ModelID,
          SavePath = SavePath,
          Threshold = Threshold,
          MaxMem = MaxMem,
          NThreads = NThreads,
          Debug = Debug)

        # Code
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score test data\n",
          "if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {\n  ",
          "DataList[[temp_test]] <- Rodeo::H2OIsolationForestScoring(\n    ",
          "data = DataList[[temp_test]],\n    ",
          "Features = Features,\n    ",
          "IDcols = IDcols,\n    ",
          "H2OStart = TRUE,\n    ",
          "H2OShutdown = TRUE,\n    ",
          "ModelID = ModelID,\n    ",
          "ModelObject = NULL,\n    ",
          "model_path = model_path,\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMem = MaxMem)\n",
          "}\n"))
      }
    })

    # Return
    if(Debug) {print('FE IsolationForest H2O 6'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_train)}, error = function(x) DataList)
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_validate)}, error = function(x) DataList)
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_test)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  }
}

#' @title Shiny.FE.AnomalyDetection.IsolationForest.H2O
#'
#' @description Utilize an H2O isolation forest to provide anomaly detection
#'
#' @author Adrian Antico
#' @family FE
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
Shiny.FE.Clustering.Kmeans.H2O <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Features
  Features <- DataMuse:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_Features']]}, error = function(x) NULL), Type = 'character', Default = NULL)

  # Data
  if(Debug) print(input[['Kmeans_H2O_TrainData']])
  temp_train <- DataMuse:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # ValidationData
  temp_validate <- DataMuse:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # TestData
  temp_test <- DataMuse:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # Create code
  if(Debug) {
    print('FE Kmeans H2O 3')
    print(temp_train)
    print(Features)
  }
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# FE Kmeans H2O: Data\n",
    "Features <- ", DataMuse:::ExpandText(Features),"\n",
    "temp_train <- ", DataMuse:::CEP(temp_train),"\n",
    "temp_validate <- ", DataMuse:::CEP(temp_validate),"\n",
    "temp_test <- ", DataMuse:::CEP(temp_test),"\n"))

  if(Debug) print('FE Kmeans H2O 4')

  # Build
  if(length(Features) > 0L && length(temp_train) > 0L) {

    if(Debug) print('FE Kmeans H2O 5')

    # AutoEncoder_H2O
    shiny::withProgress(message = 'Kmeans H2O has begun..', value = 0, {

      if(Debug) print('FE Kmeans H2O 6')

      MaxClusters <- DataMuse:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_MaxClusters']]}, error = function(x) NULL), Type = 'numeric', Default = 50)
      ClusterMetric <- DataMuse:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_ClusterMetric']]}, error = function(x) NULL), Type = 'character', Default = 0.95)

      if(Debug) print('FE Kmeans H2O 7')

      # Args tracking
      ModelID <- 'temp'
      SavePath <- getwd()
      NThreads <- max(1L, parallel::detectCores()-2L)
      MaxMem <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      if(Debug) print('FE Kmeans H2O 8')

      # Create code
      if(Debug) print('FE Kmeans H2O 3')
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Kmeans H2O Args\n",
        "ModelID <- ", DataMuse:::CEP(ModelID),"\n",
        "NThreads <- ", DataMuse:::CEP(NThreads), "\n",
        "MaxClusters <- ", DataMuse:::CEP(MaxClusters), "\n",
        "ClusterMetric <- ", DataMuse:::CEP(ClusterMetric), "\n",
        "gc()\n",
        "MaxMem <- ", DataMuse:::CEP(MaxMem), "\n"))

      # Run function
      if(Debug) print('FE Kmeans H2O 9')
      DataList[[temp_train]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_train]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_validate) > 0L) DataList[[temp_validate]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_validate]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_test) > 0L) DataList[[temp_test]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_test]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)

      if(Debug) print('FE Kmeans H2O 10')

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_train]]: Convert character columns to factors\n",
        "DataList[[temp_train]] <- Rodeo::ModelDataPrep(\n  ",
        "data = DataList[[temp_train]],\n  ",
        "Impute = TRUE,\n  ",
        "CharToFactor = TRUE,\n  ",
        "FactorToChar = FALSE,\n  ",
        "IntToNumeric = TRUE,\n  ",
        "LogicalToBinary = TRUE,\n  ",
        "DateToChar = FALSE,\n  ",
        "IDateConversion = FALSE,\n  ",
        "RemoveDates = FALSE,\n  ",
        "MissFactor = 'missing',\n  ",
        "MissNum = -1,\n  ",
        "IgnoreCols=NULL)\n"))

      if(Debug) print('FE Kmeans H2O 11')

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_validate]]: Convert character columns to factors\n",
        "if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {\n  ",
        "DataList[[temp_validate]] <- Rodeo::ModelDataPrep(\n    ",
        "data = DataList[[temp_validate]],\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"))

      if(Debug) print('FE Kmeans H2O 12')

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_test]]: Convert character columns to factors\n",
        "if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {\n  ",
        "DataList[[temp_test]] <- Rodeo::ModelDataPrep(\n    ",
        "data = DataList[[temp_test]],\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"))

      if(Debug) print('Kmeans ::: 13')

      # Run function
      DataList[[temp_train]][['data']] <- Rodeo::AutoClustering(
        data = DataList[[temp_train]][['data']],
        FeatureColumns = Features,
        ModelID = 'temp_kmeans',
        SavePath = SavePath,
        NThreads = max(1L, parallel::detectCores()-2L),
        MaxMem = MaxMem,
        MaxClusters = MaxClusters,
        ClusterMetric = ClusterMetric,
        RunDimReduction = FALSE, ShrinkRate = NULL, Epochs = NULL, L2_Reg = NULL,
        ElasticAveraging = NULL, ElasticAveragingMovingRate = NULL, ElasticAveragingRegularization = NULL)

      # Code
      CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_train]]: Convert character columns to factors\n",
        "if(length(DataList[[temp_train]]) > 0L) {\n  ",
        "DataList[[temp_train]] <- Rodeo::AutoClustering(\n    ",
        "data = DataList[[temp_train]],\n    ",
        "FeatureColumns = Features,\n    ",
        "ModelID = 'temp_kmeans',\n    ",
        "SavePath = SavePath,\n    ",
        "NThreads = NThreads,\n    ",
        "MaxMem = MaxMem,\n    ",
        "MaxClusters = MaxClusters,\n    ",
        "ClusterMetric = ClusterMetric,\n    ",
        "RunDimReduction = FALSE)\n",
        "}\n"))

      if(Debug) print('FE Kmeans H2O 14')

      # Updates
      shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')

      if(Debug) print('FE Kmeans H2O 15')

      # Score validation data
      if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {

        if(Debug) print('FE Kmeans H2O 16')

        # Pause
        Sys.sleep(10L)

        # Score model
        DataList[[temp_validate]][['data']] <- Rodeo::AutoClusteringScoring(
          data = DataList[[temp_validate]][['data']],
          FeatureColumns = Features,
          ModelID = ModelID,
          SavePath = SavePath,
          NThreads = NThreads,
          MaxMemory = MaxMem,
          DimReduction = FALSE)

        if(Debug) print('FE Kmeans H2O 17')

        # Code
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score test data\n",
          "if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {\n  ",
          "DataList[[temp_validate]] <- Rodeo::H2OIsolationForestScoring(\n    ",
          "data = DataList[[temp_validate]],\n    ",
          "FeatureColumns = Features,\n    ",
          "IDcols = IDcols,\n    ",
          "ModelID = 'temp_kmeans',\n    ",
          "ModelObject = NULL,\n    ",
          "SavePath = SavePath,\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMemory = MaxMem)\n",
          "}\n"))

        # Updates
        shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')
      }

      if(Debug) print('FE Kmeans H2O 18')

      # Score Test Data
      if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {

        if(Debug) print('FE Kmeans H2O 19')

        # Pause
        Sys.sleep(10L)

        # Score model
        DataList[[temp_test]][['data']] <- Rodeo::AutoClusteringScoring(
          data = DataList[[temp_test]][['data']],
          FeatureColumns = Features,
          ModelID = ModelID,
          SavePath = SavePath,
          NThreads = NThreads,
          MaxMemory = MaxMem,
          DimReduction = FALSE)

        if(Debug) print('FE Kmeans H2O 20')

        # Code
        CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score test data\n",
          "if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {\n  ",
          "DataList[[temp_test]] <- Rodeo::H2OIsolationForestScoring(\n    ",
          "data = DataList[[temp_test]],\n    ",
          "FeatureColumns = Features,\n    ",
          "IDcols = IDcols,\n    ",
          "ModelID = 'temp_kmeans',\n    ",
          "ModelObject = NULL,\n    ",
          "SavePath = SavePath,\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMemory = MaxMem)\n",
          "}\n"))
      }
    })

    # Return
    if(Debug) {print('FE Kmeans H2O 6'); print(names(DataList)); print(CodeList)}
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_train)}, error = function(x) DataList)
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_validate)}, error = function(x) DataList)
    DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, temp_test)}, error = function(x) DataList)
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  }
}

#' @title FE.Impute.TimeSeriesFill
#'
#' @description Imputation of a forecasting target variable for TimeSeriesFill
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param input passthrough
#' @param output passthrough
#' @param session passthrough
#' @param DataList passthrough
#' @param CodeList passthrough
#' @param CacheDir passthrough
#' @param CacheName passthrough
#' @param Debug passthrough
#'
#' @export
FE.Impute.TimeSeriesFill <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Pull in values
  if(Debug) print('Time Series Fill 1')
  TSF_TargetColumn <- DataMuse:::ReturnParam(xx = tryCatch({input[['TSF_TargetColumn']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_DateColumnName <- DataMuse:::ReturnParam(xx = tryCatch({input[['TSF_DateColumnName']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_GroupVariables <- DataMuse:::ReturnParam(xx = tryCatch({input[['TSF_GroupVariables']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_TimeUnit <- DataMuse:::ReturnParam(xx = tryCatch({input[['TSF_TimeUnit']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_FillType <- DataMuse:::ReturnParam(xx = tryCatch({input[['TSF_FillType']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_MaxMissingPercentage <- DataMuse:::ReturnParam(xx = tryCatch({input[['TSF_MaxMissingPercentage']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  TSF_SimpleImpute <- DataMuse:::ReturnParam(xx = tryCatch({input[['TSF_SimpleImpute']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$TSF_SelectData}, error=function(x) NULL), VarName = 'TSF_SelectData', Type = 'character', Default = NULL, Debug = Debug)

  # if path is a character then data will be pulled inside the function
  #  otherwise you're passing data directly to function
  if(Debug) print('Time Series Fill 3')
  DataList[[SelectData]][['data']] <- Rodeo::TimeSeriesFill(
    data = DataList[[SelectData]][['data']],
    TargetColumn = TSF_TargetColumn,
    DateColumnName = TSF_DateColumnName,
    GroupVariables = TSF_GroupVariables,
    TimeUnit = TSF_TimeUnit,
    FillType = TSF_FillType,
    MaxMissingPercent = TSF_MaxMissingPercentage,
    SimpleImpute = TSF_SimpleImpute)

  # Create code
  if(Debug) print('Time Series Fill 6')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Lexical Diversity\n",
    "DataList[[", DataMuse:::CEP(SelectData), "]] <- Rodeo::TimeSeriesFill(\n  ",
    "data = DataList[[", DataMuse:::CEP(SelectData), "]],\n  ",
    "TargetColumn = ", DataMuse:::CEP(TSF_TargetColumn), ",\n  ",
    "DateColumnName = ", DataMuse:::CEP(TSF_DateColumnName), ",\n  ",
    "GroupVariables = ", DataMuse:::ExpandText(TSF_GroupVariables), ",\n  ",
    "TimeUnit = ", DataMuse:::CEP(TSF_TimeUnit), ",\n  ",
    "FillType = ", DataMuse:::CEP(TSF_FillType), ",\n  ",
    "MaxMissingPercent = ", DataMuse:::CEPP(TSF_MaxMissingPercentage), ",\n  ",
    "SimpleImpute = ", DataMuse:::CEPP(TSF_SimpleImpute),
    ")\n"))

  # Return
  if(Debug) {print('Time Series Fill 6'); print(names(DataList)); print(CodeList)}
  DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title FE.Impute.TimeSeriesFillRoll
#'
#' @description Back and front fill a panel data set with irregular start and end dates
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param input passthrough
#' @param output passthrough
#' @param session passthrough
#' @param DataList passthrough
#' @param CodeList passthrough
#' @param CacheDir passthrough
#' @param CacheName passthrough
#' @param Debug passthrough
#'
#' @export
FE.Impute.TimeSeriesFillRoll <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Pull in values
  if(Debug) print('Time Series Fill 1')
  Roll_NewName <- DataMuse:::ReturnParam(xx = tryCatch({input[['Roll_NewName']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_TimeUnit <- DataMuse:::ReturnParam(xx = tryCatch({input[['Roll_TimeUnit']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_DateColumnName <- DataMuse:::ReturnParam(xx = tryCatch({input[['Roll_DateColumnName']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_GroupVariables <- DataMuse:::ReturnParam(xx = tryCatch({input[['Roll_GroupVariables']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_SimpleImpute <- DataMuse:::ReturnParam(xx = tryCatch({input[['Roll_SimpleImpute']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_RollVars <- DataMuse:::ReturnParam(xx = tryCatch({input[['Roll_RollVars']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  Roll_NonRollVars <- DataMuse:::ReturnParam(xx = tryCatch({input[['Roll_NonRollVars']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  Roll_RollDirection <- DataMuse:::ReturnParam(xx = tryCatch({input[['Roll_RollDirection']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$TSF_SelectData}, error=function(x) NULL), VarName = 'TSF_SelectData', Type = 'character', Default = NULL, Debug = Debug)

  # if path is a character then data will be pulled inside the function
  #  otherwise you're passing data directly to function
  if(Debug) print('Time Series Fill 3')
  DataList[[if(length(Roll_NewName) > 0L) Roll_NewName else SelectData]][['data']] <- Rodeo::TimeSeriesFillRoll(
    data = DataList[[SelectData]][['data']],
    RollVars = Roll_RollVars,
    NonRollVars = Roll_NonRollVars,
    RollDirection = Roll_RollDirection,
    DateColumnName = Roll_DateColumnName,
    GroupVariables = Roll_GroupVariables,
    TimeUnit = Roll_TimeUnit,
    SimpleImpute = Roll_SimpleImpute)

  # Create code
  if(Debug) print('Time Series Fill 6')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Lexical Diversity\n",
    "DataList[[", DataMuse:::CEP(if(length(Roll_NewName) > 0L) Roll_NewName else SelectData), "]] <- Rodeo::TimeSeriesFillRoll(\n  ",
    "data = DataList[[", DataMuse:::CEP(SelectData), "]],\n  ",
    "RollVars = ", DataMuse:::ExpandText(Roll_RollVars), ",\n  ",
    "NonRollVars = ", DataMuse:::ExpandText(NonRollVars), ",\n  ",
    "RollDirection = ", DataMuse:::ExpandText(RollDirection), ",\n  ",
    "DateColumnName = ", DataMuse:::CEP(Roll_DateColumnName), ",\n  ",
    "GroupVariables = ", DataMuse:::ExpandText(Roll_GroupVariables), ",\n  ",
    "TimeUnit = ", DataMuse:::CEPP(Roll_TimeUnit), ",\n  ",
    "SimpleImpute = ", DataMuse:::CEPP(Roll_SimpleImpute),
    ")\n"))

  # Return
  if(Debug) {print('Time Series Fill 6'); print(names(DataList)); print(CodeList)}
  DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}


#' @title Shiny.FE.NLP.TextSummary
#'
#' @description NLP Text Summaries of Text Columns
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param input passthrough
#' @param output passthrough
#' @param session passthrough
#' @param DataList passthrough
#' @param CodeList passthrough
#' @param CacheDir passthrough
#' @param CacheName passthrough
#' @param Debug passthrough
#'
#' @export
Shiny.NLP.TextSummary <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Dispatch
  TextSummary_TextColumns <- DataMuse:::ReturnParam(xx = tryCatch({input[['TextSummary_TextColumns']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)

  # Pull in values
  if(Debug) print('NLP Text Summary 1')
  TextSummary_RemoveStats <- DataMuse:::ReturnParam(xx = tryCatch({input[['TextSummary_RemoveStats']]}, error=function(x) NULL), VarName = 'TextSummary_RemoveStats', Type = 'character', Default = NULL, Debug = Debug)
  SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$TextSummary_SelectData}, error=function(x) NULL), VarName = 'TextSummary_SelectData', Type = 'character', Default = NULL, Debug = Debug)

  # if path is a character then data will be pulled inside the function
  #  otherwise you're passing data directly to function
  if(Debug) print('NLP Text Summary 3')
  DataList[[SelectData]][['data']] <- AutoNLP::TextSummary(
    dt = DataList[[SelectData]][['data']],
    TextColumns = TextSummary_TextColumns,
    RemoveStats = TextSummary_RemoveStats)

  # Create code
  if(Debug) print('NLP Text Summary 6')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Text Summary\n",
    "DataList[[", DataMuse:::CEP(SelectData), "]] <- AutoNLP::TextSummary(\n  ",
    "DataList[[", DataMuse:::CEP(SelectData), "]],\n  ",
    "TextColumns = ", DataMuse:::ExpandText(TextSummary_TextColumns), ",\n  ",
    "RemoveStats = ", DataMuse:::CEPP(TextSummary_RemoveStats),
    ")\n"))

  # Return
  if(Debug) {print('NLP Text Summary 6'); print(names(DataList)); print(CodeList)}
  DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.FE.NLP.Sentiment
#'
#' @description NLP Sentiment of Text Columns
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param input passthrough
#' @param output passthrough
#' @param session passthrough
#' @param DataList passthrough
#' @param CodeList passthrough
#' @param CacheDir passthrough
#' @param CacheName passthrough
#' @param Debug passthrough
#'
#' @export
Shiny.NLP.Sentiment <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Pull in values
  if(Debug) print('NLP Sentiment 1')
  Sentiment_TextColumns <- DataMuse:::ReturnParam(xx = tryCatch({input[['Sentiment_TextColumns']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Sentiment_CombineTextGroupVar <- DataMuse:::ReturnParam(xx = tryCatch({input[['Sentiment_CombineTextGroupVar']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Sentiment_Response <- DataMuse:::ReturnParam(xx = tryCatch({input[['Sentiment_Response']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Sentiment_RemoveStopWords <- DataMuse:::ReturnParam(xx = tryCatch({input[['Sentiment_RemoveStopWords']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  Sentiment_Stemming <- DataMuse:::ReturnParam(xx = tryCatch({input[['Sentiment_Stemming']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$Sentiment_SelectData}, error=function(x) NULL), VarName = 'TextSummary_SelectData', Type = 'character', Default = NULL, Debug = Debug)

  # if path is a character then data will be pulled inside the function
  #  otherwise you're passing data directly to function
  if(Debug) print('NLP Sentiment 3')
  DataList[[SelectData]][['data']] <- AutoNLP::Sentiment(
    dt = DataList[[SelectData]][['data']],
    Response = Sentiment_Response,
    TextColumns = Sentiment_TextColumns,
    CombineTextGroupVar = Sentiment_CombineTextGroupVar,
    Language = "english",
    RemoveStopWords = Sentiment_RemoveStopWords,
    Stemming = Sentiment_Stemming)

  # Create code
  if(Debug) print('NLP Sentiment 6')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Sentiment\n",
    "DataList[[", DataMuse:::CEP(SelectData), "]] <- AutoNLP::Sentiment(\n  ",
    "DataList[[", DataMuse:::CEP(SelectData), "]],\n  ",
    "TextColumns = ", DataMuse:::ExpandText(Sentiment_TextColumns), ",\n  ",
    "CombineTextGroupVar = ", DataMuse:::CEP(Sentiment_CombineTextGroupVar), ",\n  ",
    "Response = ", DataMuse:::CEP(Sentiment_Response), ",\n  ",
    "Language = 'english',\n  ",
    "RemoveStopWords = ", DataMuse:::CEPP(Sentiment_RemoveStopWords), ",\n  ",
    "Stemming = ", DataMuse:::CEPP(Sentiment_Stemming),
    ")\n"))

  # Return
  if(Debug) {print('NLP Sentiment 6'); print(names(DataList)); print(CodeList)}
  DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.FE.NLP.Readability
#'
#' @description NLP Readability of Text Columns
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param input passthrough
#' @param output passthrough
#' @param session passthrough
#' @param DataList passthrough
#' @param CodeList passthrough
#' @param CacheDir passthrough
#' @param CacheName passthrough
#' @param Debug passthrough
#'
#' @export
Shiny.NLP.Readability <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Pull in values
  if(Debug) print('NLP Readability 1')
  Readability_TextColumns <- DataMuse:::ReturnParam(xx = tryCatch({input[['Readability_TextColumns']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Readability_Measures <- DataMuse:::ReturnParam(xx = tryCatch({input[['Readability_Measures']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Readability_RemoveHyphens <- DataMuse:::ReturnParam(xx = tryCatch({input[['Readability_RemoveHyphens']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  Readability_MinSentenceLength <- DataMuse:::ReturnParam(xx = tryCatch({input[['Readability_MinSentenceLength']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  Readability_MaxSentenceLength <- DataMuse:::ReturnParam(xx = tryCatch({input[['Readability_MaxSentenceLength']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  Readability_Intermediate <- DataMuse:::ReturnParam(xx = tryCatch({input[['Readability_Intermediate']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$Readability_SelectData}, error=function(x) NULL), VarName = 'Readability_SelectData', Type = 'character', Default = NULL, Debug = Debug)

  # if path is a character then data will be pulled inside the function
  #  otherwise you're passing data directly to function
  if(Debug) print('NLP Readability 3')
  DataList[[SelectData]][['data']] <- AutoNLP::Readability(
    dt = DataList[[SelectData]][['data']],
    Measures = Readability_Measures,
    TextColumns = Readability_TextColumns,
    RemoveHyphens = Readability_RemoveHyphens,
    MinSentenceLength = Readability_MinSentenceLength,
    MaxSentenceLength = Readability_MaxSentenceLength,
    Intermediate = Readability_Intermediate)

  # Create code
  if(Debug) print('NLP Readability 6')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Readability\n",
    "DataList[[", DataMuse:::CEP(SelectData), "]] <- AutoNLP::Readability(\n  ",
    "DataList[[", DataMuse:::CEP(SelectData), "]],\n  ",
    "Measures = ", DataMuse:::ExpandText(Readability_Measures), ",\n  ",
    "TextColumns = ", DataMuse:::ExpandText(Readability_TextColumns), ",\n  ",
    "RemoveHyphens = ", DataMuse:::CEPP(Readability_RemoveHyphens), ",\n  ",
    "MinSentenceLength = ", DataMuse:::CEPP(Readability_MinSentenceLength), ",\n  ",
    "MaxSentenceLength = ", DataMuse:::CEPP(Readability_MaxSentenceLength), ",\n  ",
    "Intermediate = ", DataMuse:::CEPP(Readability_Intermediate),
    ")\n"))

  # Return
  if(Debug) {print('NLP Readability 6'); print(names(DataList)); print(CodeList)}
  DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.NLP.LexicalDiversity
#'
#' @description NLP LexicalDiversity of Text Columns
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param input passthrough
#' @param output passthrough
#' @param session passthrough
#' @param DataList passthrough
#' @param CodeList passthrough
#' @param CacheDir passthrough
#' @param CacheName passthrough
#' @param Debug passthrough
#'
#' @export
Shiny.NLP.LexicalDiversity <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  # Pull in values
  if(Debug) print('NLP LexicalDiversity 1')
  LexicalDiversity_TextColumns <- DataMuse:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_TextColumns']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  LexicalDiversity_Measures <- DataMuse:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_Measures']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  LexicalDiversity_RemoveHyphens <- DataMuse:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_RemoveHyphens']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  LexicalDiversity_RemoveSymbols <- DataMuse:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_RemoveSymbols']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  LexicalDiversity_RemoveNumbers <- DataMuse:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_RemoveNumbers']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  LexicalDiversity_LogBase <- DataMuse:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_LogBase']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  LexicalDiversity_MATTR_Window <- DataMuse:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_MATTR_Window']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  LexicalDiversity_MSTTR_Segment <- DataMuse:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_MSTTR_Segment']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- DataMuse:::ReturnParam(xx = tryCatch({input$LexicalDiversity_SelectData}, error=function(x) NULL), VarName = 'LexicalDiversity_SelectData', Type = 'character', Default = NULL, Debug = Debug)

  # if path is a character then data will be pulled inside the function
  #  otherwise you're passing data directly to function
  if(Debug) print('NLP Lexical Diversity 3')
  DataList[[SelectData]][['data']] <- AutoNLP::LexicalDiversity(
    dt = DataList[[SelectData]][['data']],
    Measures = LexicalDiversity_Measures,
    TextColumns = LexicalDiversity_TextColumns,
    RemoveHyphens = LexicalDiversity_RemoveHyphens,
    RemoveSymbols = LexicalDiversity_RemoveSymbols,
    RemovePunctuation = LexicalDiversity_RemovePunctuation,
    RemoveNumbers = LexicalDiversity_RemoveNumbers,
    LogBase = LexicalDiversity_LogBase,
    MATTR_Window = LexicalDiversity_MATTR_Window,
    MSTTR_Segment = Readability_MSTTR_Segment)

  # Create code
  if(Debug) print('NLP Lexical Diversity 6')
  CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Lexical Diversity\n",
    "DataList[[", DataMuse:::CEP(SelectData), "]] <- AutoNLP::LexicalDiversity(\n  ",
    "DataList[[", DataMuse:::CEP(SelectData), "]],\n  ",
    "Measures = ", DataMuse:::ExpandText(LexicalDiversity_Measures), ",\n  ",
    "TextColumns = ", DataMuse:::ExpandText(LexicalDiversity_TextColumns), ",\n  ",
    "RemoveHyphens = ", DataMuse:::CEPP(LexicalDiversity_RemoveHyphens), ",\n  ",
    "RemoveSymbols = ", DataMuse:::CEPP(LexicalDiversity_RemoveSymbols), ",\n  ",
    "RemovePunctuation = ", DataMuse:::CEPP(LexicalDiversity_RemovePunctuation), ",\n  ",
    "RemoveNumbers = ", DataMuse:::CEPP(LexicalDiversity_RemoveNumbers), ",\n  ",
    "LogBase = ", DataMuse:::CEPP(LexicalDiversity_LogBase), ",\n  ",
    "MATTR_Window = ", DataMuse:::CEPP(LexicalDiversity_MATTR_Window), ",\n  ",
    "MSTTR_Segment = ", DataMuse:::CEPP(Readability_MSTTR_Segment),
    ")\n"))

  # Return
  if(Debug) {print('NLP Lexical Diversity 6'); print(names(DataList)); print(CodeList)}
  DataList <- tryCatch({DataMuse:::DM.DataListUpdate(DataList, SelectData)}, error = function(x) DataList)
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}
