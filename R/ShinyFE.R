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
  CalendarVar_DateVariables <- Quantico:::ReturnParam(xx = tryCatch({input[['CalendarVariables_DateVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(Debug) print(CalendarVar_DateVariables)

  # Pull in values
  CalendarVar_TimeUnits <- Quantico:::ReturnParam(xx = tryCatch({input[['CalendarVariables_TimeUnits']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$CalendarVariables_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

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
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Calendar Variables\n",
    "DataList[[", Quantico:::CEP(SelectData), "]] <- Rodeo::CreateCalendarVariables(\n  ",
    "data = DataList[[", Quantico:::CEP(SelectData), "]],\n  ",
    "DateCols = ", Quantico:::ExpandText(CalendarVar_DateVariables), ",\n  ",
    "AsFactor = FALSE,\n  ",
    "TimeUnits = ", Quantico:::ExpandText(CalendarVar_TimeUnits), ")\n"
  ))

  # Return
  if(Debug) {
    print('FE Calendar Variables 6')
    print(names(DataList))
    print(CodeList)
  }

  # Update meta
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
    HolidayVar_DateVariables <- Quantico:::ReturnParam(xx = tryCatch({input[['HolidayVariables_DateVariables']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)

    # Pull in values
    if(Debug) print('FE Holiday Variables 1')
    HolidayVar_HolidayGroups <- Quantico:::ReturnParam(xx = tryCatch({input[['HolidayVariables_HolidayGroups']]}, error=function(x) NULL), VarName = 'HolidayVariables_HolidayGroups', Type = 'character', Default = NULL, Debug = Debug)
    HolidayVar_LookbackDays <- Quantico:::ReturnParam(xx = tryCatch({input[['HolidayVariables_LookbackDays']]}, error=function(x) NULL), VarName = 'HolidayVariables_LookbackDays', Type = 'numeric', Default = 1, Debug = Debug)
    SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$HolidayVariables_SelectData}, error=function(x) NULL), VarName = 'HolidayVariables_SelectData', Type = 'character', Default = NULL, Debug = Debug)

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
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Holiday Variables\n",
      "DataList[[", Quantico:::CEP(SelectData), "]] <- Rodeo::CreateHolidayVariables(\n  ",
      "DataList[[", Quantico:::CEP(SelectData), "]],\n  ",
      "DateCols = ", Quantico:::ExpandText(HolidayVar_DateVariables), ",\n  ",
      "LookbackDays = ", Quantico:::CEP(HolidayVar_LookbackDays), ",\n  ",
      "HolidayGroups = ", Quantico:::ExpandText(HolidayVar_HolidayGroups),
      ")\n"))

    # Return
    if(Debug) {print('FE Holiday Variables 6'); print(names(DataList)); print(CodeList)}
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
    PercentRank_ColNames <- Quantico:::ReturnParam(xx = tryCatch({input[['PercentRank_ColNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(PercentRank_ColNames) > 0L) {

      # Args
      PercentRank_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$PercentRank_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_SelectValidationData <- Quantico:::ReturnParam(xx = tryCatch({input[['PercentRank_SelectValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_SelectTestData <- Quantico:::ReturnParam(xx = tryCatch({input[['PercentRank_SelectTestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_GroupVars <- Quantico:::ReturnParam(xx = tryCatch({input[['PercentRank_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_Granularity <- Quantico:::ReturnParam(xx = tryCatch({input[['PercentRank_Granularity']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)

      # Function
      if(length(PercentRank_SelectValidationData) == 0L && length(PercentRank_SelectTestData) == 0L) {
        DataList[[PercentRank_SelectData]][['data']] <- Rodeo::PercRank(data = DataList[[PercentRank_SelectData]][['data']], ColNames = PercentRank_ColNames, GroupVars = PercentRank_GroupVars, Granularity = PercentRank_Granularity)

        # Create code
        if(Debug) print('FE PercRank 6')
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Percent Rank: value --> percentile\n",
          "DataList[[", Quantico:::CEP(PercentRank_SelectData),"]] <- Rodeo::PercRank(\n  ",
          "data = DataList[[", Quantico:::CEP(PercentRank_SelectData), "]],\n  ",
          "ColNames = ", Quantico:::ExpandText(PercentRank_ColNames), ",\n  ",
          "GroupVars = ", Quantico:::ExpandText(PercentRank_GroupVars), ",\n  ",
          "Granularity = ", Quantico:::CEP(PercentRank_Granularity),")\n"))

      } else {

        Output <- Rodeo::PercRank(data = DataList[[PercentRank_SelectData]][['data']], ColNames = PercentRank_ColNames, GroupVars = PercentRank_GroupVars, Granularity = PercentRank_Granularity, ScoreTable = TRUE)
        DataList[[PercentRank_SelectData]][['data']] <- Output$data
        ScoreTable <- Output$ScoreTable

        # Create code
        if(Debug) print('FE PercRank 6')
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Percent Rank: value --> percentile\n",
          "Output <- Rodeo::PercRank(\n  ",
          "data = DataList[[", Quantico:::CEP(PercentRank_SelectData), "]],\n  ",
          "ColNames = ", Quantico:::ExpandText(PercentRank_ColNames), ",\n  ",
          "GroupVars = ", Quantico:::ExpandText(PercentRank_GroupVars), ",\n  ",
          "Granularity = ", Quantico:::CEP(PercentRank_Granularity), ",\n  ",
          "ScoreTable = TRUE)\n"))

        # Apply Standardization to Validation Data
        if(length(PercentRank_SelectValidationData) > 0L) {
          DataList[[PercentRank_SelectValidationData]][['data']] <- Rodeo::PercRankScoring(data = DataList[[PercentRank_SelectValidationData]][['data']], ScoreTable = ScoreTable, GroupVars = PercentRank_GroupVars, RollDirection = 'forward')
          CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
            "\n",
            "# Percent Rank: value --> percentile\n",
            "DataList[[",Quantico:::CEP(PercentRank_SelectValidationData),"]] <- Rodeo::PercRankScoring(\n  ",
            "data = DataList[[", Quantico:::CEP(PercentRank_SelectData), "]],\n  ",
            "ScoreTable = ScoreTable,\n  ",
            "RollDirection = 'forward',\n  ",
            "GroupVars = ", Quantico:::ExpandText(PercentRank_GroupVars),")\n"))
        }

        # Apply Standardization to Test Data
        if(length(PercentRank_SelectTestData) > 0L) {
          DataList[[PercentRank_SelectTestData]][['data']] <- Rodeo::PercRankScoring(data = DataList[[PercentRank_SelectTestData]][['data']], ScoreTable = ScoreTable, GroupVars = PercentRank_GroupVars, RollDirection = 'forward')
          CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
            "\n",
            "# Percent Rank: value --> percentile\n",
            "DataList[[",Quantico:::CEP(PercentRank_SelectTestData),"]] <- Rodeo::PercRankScoring(\n  ",
            "data = DataList[[", Quantico:::CEP(PercentRank_SelectData), "]],\n  ",
            "ScoreTable = ScoreTable,\n  ",
            "RollDirection = 'forward',\n  ",
            "GroupVars = ", Quantico:::ExpandText(PercentRank_GroupVars),")\n"))
        }
      }
    }

    # Return
    if(Debug) {print('FE PercRank 6'); print(names(DataList)); print(CodeList)}
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
    Standardize_ColNames <- Quantico:::ReturnParam(xx = tryCatch({input[['Standardize_ColNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(Standardize_ColNames) > 0L) {

      # Args
      Standardize_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$Standardize_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      Standardize_SelectValidationData <- Quantico:::ReturnParam(xx = tryCatch({input[['Standardize_SelectValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      Standardize_SelectTestData <- Quantico:::ReturnParam(xx = tryCatch({input[['Standardize_SelectTestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      Standardize_GroupVars <- Quantico:::ReturnParam(xx = tryCatch({input[['Standardize_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      Standardize_Center <- Quantico:::ReturnParam(xx = tryCatch({input[['Standardize_Center']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      Standardize_Scale <- Quantico:::ReturnParam(xx = tryCatch({input[['Standardize_Scale']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)

      # Function
      if(length(Standardize_SelectValidationData) == 0L && length(Standardize_SelectTestData) == 0L) {
        DataList[[Standardize_SelectData]][['data']] <- Rodeo::Standardize(data = DataList[[Standardize_SelectData]][['data']], ColNames = Standardize_ColNames, GroupVars = Standardize_GroupVars, Center = Standardize_Center, Scale = Standardize_Scale)

        # Create code
        if(Debug) print('FE Standardize 6')
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Standardize\n",
          "DataList[[", Quantico:::CEP(Standardize_SelectData),"]] <- Rodeo::Standardize(\n  ",
          "data = DataList[[", Quantico:::CEP(Standardize_SelectData), "]],\n  ",
          "ColNames = ", Quantico:::ExpandText(Standardize_ColNames), ",\n  ",
          "GroupVars = ", Quantico:::ExpandText(Standardize_GroupVars), ",\n  ",
          "Center = ", Quantico:::CEP(Standardize_Center),",\n  ",
          "Scale = ", Quantico:::CEP(Standardize_Scale),")\n"))

      } else {

        # Run
        Output <- Rodeo::Standardize(data = DataList[[Standardize_SelectData]][['data']], ColNames = Standardize_ColNames, GroupVars = Standardize_GroupVars, Granularity = Standardize_Granularity, ScoreTable = TRUE)
        DataList[[Standardize_SelectData]][['data']] <- Output$data
        ScoreTable <- Output$ScoreTable

        # Create code
        if(Debug) print('FE Standardize 6')
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Standardize\n",
          "Output <- Rodeo::Standardize(\n  ",
          "data = DataList[[", Quantico:::CEP(Standardize_SelectData), "]],\n  ",
          "ColNames = ", Quantico:::ExpandText(Standardize_ColNames), ",\n  ",
          "GroupVars = ", Quantico:::ExpandText(Standardize_GroupVars), ",\n  ",
          "Center = ", Quantico:::CEP(Standardize_Center),",\n  ",
          "Scale = ", Quantico:::CEP(Standardize_Scale),",\n  ",
          "ScoreTable = TRUE)\n"))

        # Apply Standardization to Validation Data
        if(length(Standardize_SelectValidationData) > 0L) {
          DataList[[Standardize_SelectValidationData]][['data']] <- Rodeo::StandardizeScoring(data = DataList[[Standardize_SelectValidationData]][['data']], ScoreTable = ScoreTable, Apply = 'apply', GroupVars = Standardize_GroupVars)
          CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
            "\n",
            "# Percent Rank: value --> percentile\n",
            "DataList[[",Quantico:::CEP(Standardize_SelectValidationData),"]] <- Rodeo::StandardizeScoring(\n  ",
            "data = DataList[[", Quantico:::CEP(Standardize_SelectData), "]],\n  ",
            "ScoreTable = ScoreTable,\n  ",
            "Apply = 'apply',\n  ",
            "GroupVars = ", Quantico:::ExpandText(Standardize_GroupVars),")\n"))
        }

        # Apply Standardization to Test Data
        if(length(Standardize_SelectTestData) > 0L) {
          DataList[[Standardize_SelectTestData]][['data']] <- Rodeo::StandardizeScoring(data = DataList[[Standardize_SelectTestData]][['data']], ScoreTable = ScoreTable, Apply = 'apply', GroupVars = Standardize_GroupVars)
          CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
            "\n",
            "# Percent Rank: value --> percentile\n",
            "DataList[[",Quantico:::CEP(Standardize_SelectTestData),"]] <- Rodeo::StandardizeScoring(\n  ",
            "data = DataList[[", Quantico:::CEP(Standardize_SelectData), "]],\n  ",
            "ScoreTable = ScoreTable,\n  ",
            "Apply = 'apply',\n  ",
            "GroupVars = ", Quantico:::ExpandText(Standardize_GroupVars),")\n"))
        }
      }
    }

    # Return
    if(Debug) {print('FE Standardize 6'); print(names(DataList)); print(CodeList)}
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
    AutoInteraction_NumericVars <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoInteraction_NumericVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoInteraction_NumericVars) != 0) {
      AutoInteraction_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$AutoInteraction_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoInteraction_InteractionDepth <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoInteraction_InteractionDepth']]}, error=function(x) NULL), Type = 'numeric', Default = 2, Debug = Debug)
      AutoInteraction_Scale <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoInteraction_Scale']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      AutoInteraction_Center <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoInteraction_Center']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
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
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Numeric Interaction: value --> percentile\n",
      "temp <- ", Quantico:::CEP(AutoInteraction_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::AutoInteraction(\n  ",
      "data = DataList[[temp]],\n  ",
      "NumericVars = ", Quantico:::ExpandText(AutoInteraction_NumericVars), ",\n  ",
      "InteractionDepth = ", Quantico:::CEP(AutoInteraction_InteractionDepth), ",\n  ",
      "Center = ", Quantico:::CEP(AutoInteraction_Center), ",\n  ",
      "Scale = ", Quantico:::CEP(AutoInteraction_Scale), ",\n  ",
      "SkipCols = NULL,\n  ",
      "Scoring = FALSE,\n  ",
      "File = NULL)\n"))

    # Return
    if(Debug) {print('FE Numeric Interaction 2'); print(names(DataList)); print(CodeList)}
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
    AutoTransformationCreate_ColumnNames <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoTransformationCreate_ColumnNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoTransformationCreate_ColumnNames) != 0) {
      AutoTransformationCreate_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$AutoTransformationCreate_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoTransformationCreate_Methods <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoTransformationCreate_Methods']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
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
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Numeric Transformation: value --> percentile\n",
      "temp <- ", Quantico:::CEP(AutoTransformationCreate_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::AutoTransformationCreate(\n  ",
      "data = DataList[[temp]],\n  ",
      "ColumnNames = ", Quantico:::ExpandText(AutoTransformationCreate_ColumnNames), ",\n  ",
      "Methods = ", Quantico:::ExpandText(AutoTransformationCreate_Methods), ",\n  ",
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
    DummifyDT_Cols <- Quantico:::ReturnParam(xx = tryCatch({input[['DummifyDT_Cols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(DummifyDT_Cols) != 0) {
      DummifyDT_TopN <- Quantico:::ReturnParam(xx = tryCatch({input[['DummifyDT_TopN']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      DummifyDT_KeepBaseCols <- Quantico:::ReturnParam(xx = tryCatch({input[['DummifyDT_KeepBaseCols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      DummifyDT_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input[['DummifyDT_SelectData']]}, error = function(x) NULL), VarName = 'DummifyDT_SelectData', Type = 'character', Default = NULL, Debug = Debug)
      DataList[[DummifyDT_SelectData]][['data']] <- Rodeo::DummifyDT(
        data = DataList[[DummifyDT_SelectData]][['data']],
        cols = DummifyDT_Cols,
        TopN = DummifyDT_TopN,
        KeepFactorCols = as.logical(DummifyDT_KeepBaseCols),
        OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=NULL, ImportFactorLevels=FALSE, FactorLevelsList=NULL, ClustScore=FALSE, ReturnFactorLevels=FALSE, GroupVar=FALSE)
    }

    # Create code
    if(Debug) print('FE Partial Dummies 1')
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Partial Dummify Variables\n",
      "temp <- ", Quantico:::CEP(DummifyDT_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::DummifyDT(\n  ",
      "data = DataList[[temp]],\n  ",
      "cols = ", Quantico:::ExpandText(DummifyDT_Cols), ",\n  ",
      "TopN = ", Quantico:::CEP(DummifyDT_TopN), ",\n  ",
      "KeepFactorCols = ", Quantico:::CEP(as.logical(DummifyDT_KeepBaseCols)), ",\n  ",
      "OneHot = FALSE,\n  ",
      "SaveFactorLevels = FALSE,\n  ",
      "SavePath = NULL,\n  ",
      "ImportFactorLevels = FALSE,\n  ",
      "FactorLevelsList = NULL,\n  ",
      "ReturnFactorLevels = FALSE)\n"))

    # Return
    if(Debug) {print('FE Partial Dummies 2'); print(names(DataList)); print(CodeList)}
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
    CategoricalEncoding_GroupVariables <- Quantico:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_GroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(CategoricalEncoding_GroupVariables) != 0) {

      if(Debug) print('here 1')

      CategoricalEncoding_TargetVariable <- Quantico:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_TargetVariable']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('here 2')
      CategoricalEncoding_Method <- Quantico:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_Method']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('here 3')
      temp_train <- Quantico:::ReturnParam(xx = tryCatch({input$CategoricalEncoding_TrainData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('here 4')
      x <- DataList[[temp_train]][['data']]
      if(Debug) print('here 5')
      temp_validate <- Quantico:::ReturnParam(xx = tryCatch({input$CategoricalEncoding_ValidationData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('here 6')
      if(length(temp_validate) > 0L) y <- DataList[[temp_validate]][['data']] else y <- NULL
      if(Debug) print('here 7')
      temp_test <- Quantico:::ReturnParam(xx = tryCatch({input$CategoricalEncoding_TestData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('here 8')
      if(length(temp_test) > 0L) z <- DataList[[temp_train]][['data']] else z <- NULL
      if(Debug) print('here 9')

      # Identify target type
      if(class(x[[eval(CategoricalEncoding_TargetVariable)]])[1L] %in% c('character','factor')) {
        MLType <- 'MultiClass'
      } else if(all(unique(x[[eval(CategoricalEncoding_TargetVariable)]]) %in% c(0,1))) {
        MLType <- 'Classification'
      } else {
        MLType <- 'Regression'
      }

      if(Debug) print('here 10')

      # Build Features
      Output <- Rodeo:::EncodeCharacterVariables(
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
      if(Debug) print(Output$TrainData)
      DataList[[temp_train]][['data']] <- Output$TrainData
      if(length(Output$ValidationData) > 0L) DataList[[temp_validate]][['data']] <- Output$ValidationData
      if(length(Output$TestData) > 0L) DataList[[temp_test]][['data']] <- Output$TestData
    }

    if(Debug) print('here 11')

    # Create code
    if(Debug) print('FE Categorical Encoding 1')
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Categorical Encoding\n",
      "temp_train <- ", Quantico:::CEP(temp_train),"\n",
      "if(class(x[[eval(CategoricalEncoding_TargetVariable)]])[1L] %in% c('character','factor')) {\n  ",
      "temp_validate <- ", Quantico:::CEP(temp_validate), "\n",
      "if(length(temp_validate) > 0L) y <- DataList[[temp_validate]] else y <- NULL\n",
      "temp_test <- ", Quantico:::CEP(temp_test), "\n",
      "if(length(temp_test) > 0L) z <- DataList[[temp_train]] else z <- NULL\n  ",
      "MLType <- 'multiclass'\n",
      "} else if(all(unique(x[[eval(CategoricalEncoding_TargetVariable)]]) %in% c(0,1))) {\n  ",
      "MLType <- 'classification'\n",
      "} else {\n  ",
      "MLType <- 'regression'\n",
      "}\n",
      "Output <- Quantico:::EncodeCharacterVariables(","\n  ",
      "RunMode = 'train',\n  ",
      "ModelType = MLType,\n  ",
      "TrainData = x,\n  ",
      "ValidationData = y,\n  ",
      "TestData = z,\n  ",
      "TargetVariableName = ", Quantico:::CEP(CategoricalEncoding_TargetVariable), ",\n  ",
      "CategoricalVariableNames = ", Quantico:::ExpandText(CategoricalEncoding_GroupVariables), ",\n  ",
      "EncodeMethod = ", Quantico:::CEP(CategoricalEncoding_Method), ",\n  ",
      "KeepCategoricalVariables = TRUE,\n  ",
      "ReturnMetaData = TRUE,\n  ",
      "MetaDataPath = NULL,\n  ",
      "MetaDataList = NULL,\n  ",
      "ImputeMissingValue = 0)","\n",
      "TrainData <- Output$TrainData\n",
      "ValidationData <- Output$ValidationData\n",
      "TestData <- Output$TestData; rm(Output)\n"))}, error = function(x) NULL)

    # Return
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
    AutoLagRollMode_Targets <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_Targets']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print('FE Auto Lag Roll Mode 1')
    AutoLagRollMode_SortDateName <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_SortDateName']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print('FE Auto Lag Roll Mode 6')
    if(Debug) print(input[['AutoLagRollMode_WindowingLag']])
    if(length(AutoLagRollMode_Targets) > 0 && length(AutoLagRollMode_SortDateName) > 0) {
      if(Debug) print('FE Auto Lag Roll Mode 2')
      AutoLagRollMode_Lags <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_Lags', Type = 'numeric']]}, error=function(x) NULL), Default = 1, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 3')
      AutoLagRollMode_ModePeriods <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_ModePeriods']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 4')
      AutoLagRollMode_GroupingVars <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_GroupingVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 5')
      AutoLagRollMode_WindowingLag <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_WindowingLag']]}, error=function(x) NULL), Type = 'numeric', Default = 1, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 7')
      AutoLagRollMode_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$AutoLagRollMode_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Auto Lag Roll Mode\n",
        "temp <- ", Quantico:::CEP(AutoLagRollMode_SelectData),"\n",
        "DataList[[temp]] <- Rodeo::AutoLagRollMode(\n  ",
        "data = DataList[[temp]],\n  ",
        "Targets = ", Quantico:::ExpandText(AutoLagRollMode_Targets), ",\n  ",
        "GroupingVars = ", Quantico:::ExpandText(AutoLagRollMode_GroupingVars), ",\n  ",
        "SortDateName = ", Quantico:::CEP(AutoLagRollMode_SortDateName), ",\n  ",
        "WindowingLag = ", Quantico:::CEP(AutoLagRollMode_WindowingLag), ",\n  ",
        "Lags = ", Quantico:::ExpandText(AutoLagRollMode_Lags), ",\n  ",
        "ModePeriods = ", Quantico:::ExpandText(AutoLagRollMode_ModePeriods), ",\n  ",
        "Type = 'Lag',\n  ",
        "SimpleImpute = TRUE)\n"))

      # Return
      if(Debug) {print('FE Auto Lag Roll Mode 2'); print(names(DataList)); print(CodeList)}
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
    AutoLagRollStats_Targets <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Targets']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoLagRollStats_DateColumn <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_DateColumn']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoLagRollStats_TimeUnits <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_TimeUnits']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoLagRollStats_Targets) != 0 && length(AutoLagRollStats_DateColumn) != 0 && length(AutoLagRollStats_TimeUnits) != 0) {
      AutoLagRollStats_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$AutoLagRollStats_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoLagRollStats_GroupVars <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(length(AutoLagRollStats_GroupVars) > 1L) {
        DataList[[AutoLagRollStats_SelectData]][['data']][, paste0(AutoLagRollStats_GroupVars, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(AutoLagRollStats_GroupVars)]
        AutoLagRollStats_GroupVars <- paste0(AutoLagRollStats_GroupVars, collapse = '_')
      }
      AutoLagRollStats_Lags <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Lags']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_RollOnLag1 <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_RollOnLag1']]}, error=function(x) NULL), Type = 'logical', Default = NULL, Debug = Debug)
      if(AutoLagRollStats_RollOnLag1 == 0) {
        AutoLagRollStats_RollOnLag1 <- FALSE
      } else {
        AutoLagRollStats_RollOnLag1 <- TRUE
      }
      AutoLagRollStats_MA_RollWindows <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_MA_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_SD_RollWindows <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_SD_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Skew_RollWindows <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Skew_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Kurt_RollWindows <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Kurt_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Quantile_RollWindows <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Quantile_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Quantiles_Selected <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Quantiles_Selected']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Auto Lag Roll Stats\n",
        "temp <- ", Quantico:::CEP(AutoLagRollStats_SelectData),"\n",
        "AutoLagRollStats_GroupVars <- ", Quantico:::ExpandText(AutoLagRollStats_GroupVars), "\n",
        "if(length(AutoLagRollStats_GroupVars) > 1L) {\n  ",
        "DataList[[temp]][, paste0(AutoLagRollStats_GroupVars, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(AutoLagRollStats_GroupVars)]\n  ",
        "AutoLagRollStats_GroupVars <- paste0(AutoLagRollStats_GroupVars, collapse = '_')\n",
        "}\n",
        "AutoLagRollStats_Targets <- ", Quantico:::ExpandText(AutoLagRollStats_Targets), "\n",
        "for(i in AutoLagRollStats_Targets) {",
        "DataList[[temp]] <- Rodeo::AutoLagRollStats(\n    ",
        "data <- DataList[[temp]],\n    ",
        "Targets = i,\n    ",
        "HierarchyGroups = NULL,\n    ",
        "IndependentGroups = AutoLagRollStats_GroupVars,\n    ",
        "DateColumn = ", Quantico:::CEP(AutoLagRollStats_DateColumn), ",\n    ",
        "TimeUnit = ", Quantico:::ExpandText(AutoLagRollStats_TimeUnits), ",\n    ",
        "TimeUnitAgg = ", Quantico:::ExpandText(AutoLagRollStats_TimeUnits), ",\n    ",
        "TimeGroups = ", Quantico:::ExpandText(AutoLagRollStats_TimeUnits), ",\n    ",
        "TimeBetween = NULL,\n    ",
        "RollOnLag1 = ", Quantico:::CEP(AutoLagRollStats_RollOnLag1), ",\n    ",
        "Type = 'Lag',\n    ",
        "SimpleImpute = TRUE,\n    ",
        "Lags = ", Quantico:::ExpandText(AutoLagRollStats_Lags), ",\n    ",
        "MA_RollWindows = ", Quantico:::ExpandText(AutoLagRollStats_MA_RollWindows), ",\n    ",
        "SD_RollWindows = ", Quantico:::ExpandText(AutoLagRollStats_SD_RollWindows), ",\n    ",
        "Skew_RollWindows = ", Quantico:::ExpandText(AutoLagRollStats_Skew_RollWindows), ",\n    ",
        "Kurt_RollWindows = ", Quantico:::ExpandText(AutoLagRollStats_Kurt_RollWindows),",\n    ",
        "Quantile_RollWindows = ", Quantico:::ExpandText(AutoLagRollStats_Quantile_RollWindows), ",\n    ",
        "Quantiles_Selected = ", Quantico:::ExpandText(AutoLagRollStats_Quantiles_Selected), ")\n",
        "}\n"))

      # Return
      if(Debug) {print('FE Auto Lag Roll Stats 2'); print(names(DataList)); print(CodeList)}
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
    AutoDiffLagN_DateVariable <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DateVariable']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoDiffLagN_NLag1 <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_NLag1']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
    AutoDiffLagN_NLag2 <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_NLag2']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
    if(length(AutoDiffLagN_DateVariable) != 0 &&
       length(AutoDiffLagN_NLag1) != 0 &&
       length(AutoDiffLagN_NLag2) != 0) {
      AutoDiffLagN_GroupVariables <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_GroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffVariables <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffDateVariables <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffDateVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffGroupVariables <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffGroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$AutoDiffLagN_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
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
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Differencing Variables\n",
      "temp <- ", Quantico:::CEP(AutoDiffLagN_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::AutoDiffLagN(\n  ",
      "data = DataList[[temp]],\n  ",
      "DateVariable = ", Quantico:::ExpandText(AutoDiffLagN_DateVariable), ",\n  ",
      "GroupVariables = ", Quantico:::ExpandText(AutoDiffLagN_GroupVariables), ",\n  ",
      "DiffVariables = ", Quantico:::ExpandText(AutoDiffLagN_DiffVariables), ",\n  ",
      "DiffDateVariables = ", Quantico:::ExpandText(AutoDiffLagN_DiffDateVariables), ",\n  ",
      "DiffGroupVariables = ", Quantico:::ExpandText(AutoDiffLagN_DiffGroupVariables), ",\n  ",
      "NLag1 = ", Quantico:::CEP(AutoDiffLagN_NLag1), ",\n  ",
      "NLag2 = ", Quantico:::CEP(AutoDiffLagN_NLag2), ",\n  ",
      "Sort = TRUE,\n  ",
      "RemoveNA = TRUE)\n"))

    # Return
    if(Debug) {print('FE Auto Diff Lag N 2'); print(names(DataList)); print(CodeList)}
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
    ModelDataPrep_IgnoreCols <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IgnoreCols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    ModelDataPrep_CharToFactor <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_CharToFactor']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_FactorToChar <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_FactorToChar']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_DateToChar <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_DateToChar']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_IDateConversion <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IDateConversion']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_RemoveDates <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_RemoveDates']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_IntToNumeric <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IntToNumeric']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_LogicalToBinary <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_LogicalToBinary']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_MissFactor <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_MissFactor']]}, error=function(x) NULL), Type = 'character', Default = FALSE, Debug = Debug)
    ModelDataPrep_MissNum <- Quantico:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_MissNum']]}, error=function(x) NULL), Type = 'numeric', Default = FALSE, Debug = Debug)
    ModelDataPrep_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$ModelDataPrep_SelectData}, error=function(x) NULL), Type = 'character', Default = FALSE, Debug = Debug)
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
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Model Data Prep\n",
      "temp <- ", Quantico:::CEP(ModelDataPrep_SelectData),"\n",
      "DataList[[temp]] <- Rodeo::ModelDataPrep(","\n  ",
      "DataList[[temp]],\n  ",
      "CharToFactor = ", Quantico:::CEP(ModelDataPrep_CharToFactor), ",\n  ",
      "FactorToChar = ", Quantico:::CEP(ModelDataPrep_FactorToChar), ",\n  ",
      "IntToNumeric = ", Quantico:::CEP(ModelDataPrep_IntToNumeric), ",\n  ",
      "LogicalToBinary = ", Quantico:::CEP(ModelDataPrep_LogicalToBinary), ",\n  ",
      "DateToChar = ", Quantico:::CEP(ModelDataPrep_DateToChar), ",\n  ",
      "IDateConversion <- ", Quantico:::CEP(ModelDataPrep_IDateConversion), ",\n  ",
      "RemoveDates <- ", Quantico:::CEP(ModelDataPrep_RemoveDates), ",\n  ",
      "MissFactor <- ", Quantico:::CEP(ModelDataPrep_MissFactor), ",\n  ",
      "MissNum <- ", Quantico:::CEP(ModelDataPrep_MissNum), ",\n  ",
      "IgnoreCols <- ", Quantico:::ExpandText(ModelDataPrep_IgnoreCols), ")\n"))

    # Return
    if(Debug) {print('FE Auto Diff Lag N 2'); print(names(DataList)); print(CodeList)}
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

    if(Debug) print('Shiny.FE.PartitionData 1')

    AutoDataPartition_NumDataSets <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_NumDataSets']]}, error=function(x) NULL), Type = 'numeric', Default = 3L, Debug = Debug)
    AutoDataPartition_Ratios_Train <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Train']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.70), Debug = Debug)
    AutoDataPartition_Ratios_Validation <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Validation']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.20), Debug = Debug)
    AutoDataPartition_Ratios_Test <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Test']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.10), Debug = Debug)
    AutoDataPartition_PartitionType <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_PartitionType']]}, error=function(x) NULL), Type = 'character', Default = "random", Debug = Debug)
    AutoDataPartition_StratifyColumnNames <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_StratifyColumnNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoDataPartition_TimeColumnName <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_TimeColumnName']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoDataPartition_SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$AutoDataPartition_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

    if(Debug) print('Shiny.FE.PartitionData 2')

    if(AutoDataPartition_NumDataSets == 2L) {
      xx <- round(AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation, 10) == 1
      AutoDataPartition_Ratios_Test <- NULL
    } else {
      xx <- round(AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation + AutoDataPartition_Ratios_Test, 10) == 1
    }

    if(Debug) {
      print('Shiny.FE.PartitionData 3')
      print(xx)
      print(AutoDataPartition_NumDataSets)
      print(c(AutoDataPartition_Ratios_Train, AutoDataPartition_Ratios_Validation, AutoDataPartition_Ratios_Test))
      print(AutoDataPartition_PartitionType)
      print(AutoDataPartition_StratifyColumnNames)
      print(AutoDataPartition_TimeColumnName)
    }

    if(xx) {

      if(Debug) print('Shiny.FE.PartitionData 4')

      DataSets <- Rodeo::AutoDataPartition(
        DataList[[AutoDataPartition_SelectData]][['data']],
        NumDataSets = AutoDataPartition_NumDataSets,
        Ratios = c(AutoDataPartition_Ratios_Train, AutoDataPartition_Ratios_Validation, AutoDataPartition_Ratios_Test),
        PartitionType = AutoDataPartition_PartitionType,
        StratifyColumnNames = AutoDataPartition_StratifyColumnNames,
        TimeColumnName = AutoDataPartition_TimeColumnName)
      DataList[[paste0(AutoDataPartition_SelectData, '_TrainData')]][['data']] <- DataSets[['TrainData']]
      DataList[[paste0(AutoDataPartition_SelectData, '_ValidationData')]][['data']] <- DataSets[['ValidationData']]
      DataList[[paste0(AutoDataPartition_SelectData, '_TestData')]][['data']] <- tryCatch({DataSets[['TestData']]}, error = function(x) NULL)
      rm(DataSets)

      if(Debug) print('Shiny.FE.PartitionData 5')

      # Create code
      if(Debug) print('FE Auto Diff Lag N 1')
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Data Partition\n",
        "temp <- ", Quantico:::CEP(AutoDataPartition_SelectData),"\n",
        "DataSets <- Rodeo::AutoDataPartition(\n  ",
        "DataList[[temp]],\n  ",
        "NumDataSets = ", Quantico:::CEP(AutoDataPartition_NumDataSets), ",\n  ",
        "Ratios = ", Quantico:::ExpandText(c(AutoDataPartition_Ratios_Train, AutoDataPartition_Ratios_Validation, AutoDataPartition_Ratios_Test)), ",\n  ",
        "PartitionType = ", Quantico:::CEP(AutoDataPartition_PartitionType), ",\n  ",
        "StratifyColumnNames = ", Quantico:::ExpandText(AutoDataPartition_StratifyColumnNames), ",\n  ",
        "TimeColumnName = ", Quantico:::CEP(AutoDataPartition_TimeColumnName), ")\n",
        "DataList[[paste0(temp, '_TrainData')]] <- DataSets[['TrainData']]\n",
        "DataList[[paste0(temp, '_ValidationData')]] <- DataSets[['ValidationData']]\n",
        "DataList[[paste0(temp, '_TestData')]] <- DataSets[['TestData']]\n",
        "rm(DataSets)\n"))

      if(Debug) print('Shiny.FE.PartitionData 6')

      # Add data to DataOutputSelection Page
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      Quantico::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

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
  stringCol <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_stringCol']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(Debug) print(input[['H2O_Word2Vec_TrainData']])
  temp_train <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TrainData <- DataList[[temp_train]][['data']]
  temp_validate <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]][['data']] else ValidationData <- NULL
  temp_test <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_test) > 0L) TestData <- DataList[[temp_test]][['data']] else TestData <- NULL
  if(Debug) print('FE Word2Vec H2O 2')

  # Create code
  if(Debug) print('FFE Word2Vec H2O 3')
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Word2Vec H2O: Data\n",
    "stringCol <- ", Quantico:::ExpandText(stringCol),"\n",
    "temp_train <- ", Quantico:::CEP(temp_train),"\n",
    "TrainData <- DataList[[temp_train]]\n",
    "temp_validate <- ", Quantico:::CEP(temp_validate),"\n",
    "if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]] else ValidationData <- NULL\n",
    "temp_test <- ", Quantico:::CEP(temp_test),"\n",
    "if(length(temp_test) > 0L) TestData <- DataList[[temp_test]] else TestData <- NULL\n"))

  # Build
  if(length(stringCol) > 0L && length(TrainData) > 0L) {

    # AutoEncoder_H2O
    shiny::withProgress(message = 'Word2Vec H2O has begun..', value = 0, {

      # Args
      BuildType <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_BuildType']]}, error=function(x) NULL), Type = 'character', Default = 'combined', Debug = Debug)
      KeepStringCol <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_KeepStringCol']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      vects <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_vects']]}, error=function(x) NULL), Type = 'numeric', Default = 30, Debug = Debug)
      H2O_Word2Vec_MinWords <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_MinWords']]}, error=function(x) NULL), Type = 'numeric', Default = 1, Debug = Debug)
      WindowSize <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_WindowSize']]}, error=function(x) NULL), Type = 'numeric', Default = 5, Debug = Debug)
      Epochs <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_Epochs']]}, error=function(x) NULL), Type = 'numeric', Default = 10, Debug = Debug)
      MinWords <- Quantico:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_MinWords']]}, error=function(x) NULL), Type = 'numeric', Default = 2, Debug = Debug)

      # Args tracking
      ModelID <- 'temp'
      model_path <- NULL
      Threads <- max(1L, parallel::detectCores()-2L)
      MaxMemory <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      # Create code
      if(Debug) print('WFE Word2Vec H2O 4')
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Word2Vec H2O Args\n",
        "BuildType <- ", Quantico:::CEP(BuildType),"\n",
        "KeepStringCol <- ", Quantico:::CEP(KeepStringCol),"\n",
        "vects <- ", Quantico:::CEP(vects), "\n",
        "H2O_Word2Vec_MinWords <- ", Quantico:::CEP(H2O_Word2Vec_MinWords),"\n",
        "WindowSize <- ", Quantico:::CEP(WindowSize), "\n",
        "Epochs <- ", Quantico:::CEP(Epochs), "\n",
        "MinWords <- ", Quantico:::CEP(MinWords), "\n",
        "ModelID <- ", Quantico:::CEP(ModelID), "\n",
        "model_path <- ", Quantico:::CEP(model_path), "\n",
        "Threads <- ", Quantico:::CEP(Threads), "\n",
        "gc()\n",
        "MaxMemory <- ", Quantico:::CEP(MaxMem), "\n"))

      # Run function
      if(Debug) print('FE Word2Vec H2O 3')

      # MetaData
      Start <- Sys.time()
      tempnames <- names(TrainData)

      # Code
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Update DataList\n",
        "DataList[[temp_train]] <- TrainData; rm(TrainData); gc()\n"))

      # Run time tracking
      End <- Sys.time()
      H2OWord2Vec_Training <- difftime(End, Start, units = "mins")

      # Code
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
  Features <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_Features}, error = function(x) NULL), Type = 'character', Default = NULL)

  # Data
  if(Debug) print(input[['AutoEncoder_H2O_TrainData']])
  temp_train <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoEncoder_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TrainData <- DataList[[temp_train]][['data']]

  # ValidationData
  temp_validate <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoEncoder_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]][['data']] else ValidationData <- NULL

  # TestData
  temp_test <- Quantico:::ReturnParam(xx = tryCatch({input[['AutoEncoder_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_test) > 0L) TestData <- DataList[[temp_test]][['data']] else TestData <- NULL


  # Create code
  if(Debug) print('FFE Word2Vec H2O 3')
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# FE AutoEncoder H2O: Data\n",
    "Features <- ", Quantico:::ExpandText(Features),"\n",
    "temp_train <- ", Quantico:::CEP(temp_train),"\n",
    "TrainData <- DataList[[temp_train]]\n",
    "temp_validate <- ", Quantico:::CEP(temp_validate),"\n",
    "if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]] else ValidationData <- NULL\n",
    "temp_test <- ", Quantico:::CEP(temp_test),"\n",
    "if(length(temp_test) > 0L) TestData <- DataList[[temp_test]] else TestData <- NULL\n"))


  # Build
  if(length(Features) > 0L && length(TrainData) > 0L) {

    # AutoEncoder_H2O
    shiny::withProgress(message = 'AutoEncoder H2O has begun..', value = 0, {

      if(Debug) print('FE AutoEncoder H2O 2')

      # Non-Data Args
      AnomalyDetection <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_AnomalyDetection}, error = function(x) NULL), Type = 'logical', Default = FALSE)
      DimensionReduction <- TRUE
      per_feature <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_per_feature}, error = function(x) NULL), Type = 'logical', Default = FALSE)
      RemoveFeatures <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_RemoveFeatures}, error = function(x) NULL), Type = 'logical', Default = FALSE)
      LayerStructure <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_LayerStructure}, error = function(x) NULL), Type = 'numeric', Default = 2L)
      NodeShrinkRate <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_NodeShrinkRate}, error = function(x) NULL), Type = 'numeric', Default = sqrt(5)/2-0.5)
      ReturnLayer <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ReturnLayer}, error = function(x) NULL), Type = 'numeric', Default = 2L)
      Epochs <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_Epochs}, error = function(x) NULL), Type = 'numeric', Default = NULL)
      L2 <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_L2}, error = function(x) NULL), Type = 'numeric', Default = NULL)
      ElasticAveraging <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ElasticAveraging}, error = function(x) NULL), Type = 'logical', Default = NULL)
      ElasticAveragingMovingRate <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ElasticAveragingMovingRate}, error = function(x) NULL), Type = 'numeric', Default = NULL)
      ElasticAveragingRegularization <- Quantico:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ElasticAveragingRegularization}, error = function(x) NULL), Type = 'numeric', Default = NULL)

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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# AutoEncoder H2O Args\n",
        "AnomalyDetection <- ", Quantico:::CEPP(AnomalyDetection),"\n",
        "DimensionReduction <- ", Quantico:::CEPP(DimensionReduction),"\n",
        "per_feature <- ", Quantico:::CEPP(per_feature), "\n",
        "RemoveFeatures <- ", Quantico:::CEPP(RemoveFeatures),"\n",
        "LayerStructure <- ", Quantico:::ExpandText(LayerStructure), "\n",
        "NodeShrinkRate <- ", Quantico:::CEPP(NodeShrinkRate), "\n",
        "Epochs <- ", Quantico:::CEPP(Epochs), "\n",
        "L2 <- ", Quantico:::CEPP(L2), "\n",
        "ElasticAveraging <- ", Quantico:::CEPP(ElasticAveraging), "\n",
        "ElasticAveragingMovingRate <- ", Quantico:::CEPP(ElasticAveragingMovingRate), "\n",
        "ElasticAveragingRegularization <- ", Quantico:::CEPP(ElasticAveragingRegularization), "\n",
        "ModelID <- ", Quantico:::CEP(ModelID), "\n",
        "Models_Path <- ", Quantico:::CEP(Models_Path), "\n",
        "NThreads <- ", Quantico:::CEP(NThreads), "\n",
        "gc()\n",
        "MaxMem <- ", Quantico:::CEP(MaxMem), "\n"))

      # Run function
      if(Debug) print('FE AutoEncoder H2O 3')
      TrainData <- Rodeo::ModelDataPrep(data=TrainData, Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(ValidationData) > 0L) ValidationData <- Rodeo::ModelDataPrep(data=ValidationData, Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(TestData) > 0L) TestData <- Rodeo::ModelDataPrep(data=TestData, Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)

      # Code
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Metadata Args\n",
        "if(length(ModelID) == 0L) ModelID <- 'temp1'\n",
        "if(length(Models_Path) == 0L) Models_Path <- getwd()\n"))

      if(Debug) print('AE ::: 2')

      # Metadata
      Start <- Sys.time()
      tempnames <- names(data.table::copy(TrainData))

      # Code
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
  Features <- Quantico:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_Features}, error = function(x) NULL), Type = 'character', Default = NULL)

  # Data
  if(Debug) print(input[['IsolationForest_H2O_TrainData']])
  temp_train <- Quantico:::ReturnParam(xx = tryCatch({input[['IsolationForest_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # ValidationData
  temp_validate <- Quantico:::ReturnParam(xx = tryCatch({input[['IsolationForest_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # TestData
  temp_test <- Quantico:::ReturnParam(xx = tryCatch({input[['IsolationForest_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)


  # Create code
  if(Debug) print('FE IsolationForest H2O 3')
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# FE IsolationForest H2O: Data\n",
    "Features <- ", Quantico:::ExpandText(Features),"\n",
    "temp_train <- ", Quantico:::CEP(temp_train),"\n",
    "temp_validate <- ", Quantico:::CEP(temp_validate),"\n",
    "temp_test <- ", Quantico:::CEP(temp_test),"\n"))

  # Build
  if(length(Features) > 0L && length(temp_train) > 0L) {

    # AutoEncoder_H2O
    shiny::withProgress(message = 'IsolationForest H2O has begun..', value = 0, {

      if(Debug) print('FE IsolationForest H2O 2')
      IDcols <- setdiff(names(DataList[[temp_train]][['data']]), Features)
      Threshold <- Quantico:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_Threshold}, error = function(x) NULL), Type = 'numeric', Default = 0.95)
      NTrees <- Quantico:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_NTrees}, error = function(x) NULL), Type = 'numeric', Default = 50)
      MaxDepth <- Quantico:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_MaxDepth}, error = function(x) NULL), Type = 'numeric', Default = 20)
      MinRows <- Quantico:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_MinRows}, error = function(x) NULL), Type = 'numeric', Default = 1)
      RowSampleRate <- Quantico:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_RowSampleRate}, error = function(x) NULL), Type = 'numeric', Default = 1)
      ColSampleRate <- Quantico:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_ColSampleRate}, error = function(x) NULL), Type = 'numeric', Default = 1)
      ColSampleRatePerLevel <- Quantico:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_ColSampleRatePerLevel}, error = function(x) NULL), Type = 'numeric', Default = 1)
      ColSampleRatePerTree <- Quantico:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_ColSampleRatePerTree}, error = function(x) NULL), Type = 'numeric', Default = 1)
      CategoricalEncoding <- 'AUTO'

      # Args tracking
      ModelID <- 'temp'
      SavePath <- getwd()
      NThreads <- max(1L, parallel::detectCores()-2L)
      MaxMem <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      # Create code
      if(Debug) print('FE IsolationForest H2O 3')
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# IsolationForest H2O Args\n",
        "IDcols <- ", Quantico:::CEP(IDcols),"\n",
        "ModelID <- ", Quantico:::CEP(ModelID),"\n",
        "Threshold <- ", Quantico:::CEP(Threshold), "\n",
        "NTrees <- ", Quantico:::CEP(NTrees),"\n",
        "MaxDepth <- ", Quantico:::CEP(MaxDepth), "\n",
        "MinRows <- ", Quantico:::CEP(MinRows), "\n",
        "RowSampleRate <- ", Quantico:::CEP(RowSampleRate), "\n",
        "ColSampleRate <- ", Quantico:::CEP(ColSampleRate), "\n",
        "ColSampleRatePerLevel <- ", Quantico:::CEP(ColSampleRatePerLevel), "\n",
        "ColSampleRatePerTree <- ", Quantico:::CEP(ColSampleRatePerTree), "\n",
        "CategoricalEncoding <- ", Quantico:::CEP(CategoricalEncoding), "\n",
        "NThreads <- ", Quantico:::CEP(NThreads), "\n",
        "gc()\n",
        "MaxMem <- ", Quantico:::CEP(MaxMem), "\n"))

      # Run function
      if(Debug) print('FE IsolationForest H2O 3')
      DataList[[temp_train]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_train]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_validate) > 0L) DataList[[temp_validate]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_validate]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_test) > 0L) DataList[[temp_test]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_test]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)

      # Code
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
  Features <- Quantico:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_Features']]}, error = function(x) NULL), Type = 'character', Default = NULL)

  # Data
  if(Debug) print(input[['Kmeans_H2O_TrainData']])
  temp_train <- Quantico:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # ValidationData
  temp_validate <- Quantico:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # TestData
  temp_test <- Quantico:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # Create code
  if(Debug) {
    print('FE Kmeans H2O 3')
    print(temp_train)
    print(Features)
  }
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# FE Kmeans H2O: Data\n",
    "Features <- ", Quantico:::ExpandText(Features),"\n",
    "temp_train <- ", Quantico:::CEP(temp_train),"\n",
    "temp_validate <- ", Quantico:::CEP(temp_validate),"\n",
    "temp_test <- ", Quantico:::CEP(temp_test),"\n"))

  if(Debug) print('FE Kmeans H2O 4')

  # Build
  if(length(Features) > 0L && length(temp_train) > 0L) {

    if(Debug) print('FE Kmeans H2O 5')

    # AutoEncoder_H2O
    shiny::withProgress(message = 'Kmeans H2O has begun..', value = 0, {

      if(Debug) print('FE Kmeans H2O 6')

      MaxClusters <- Quantico:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_MaxClusters']]}, error = function(x) NULL), Type = 'numeric', Default = 50)
      ClusterMetric <- Quantico:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_ClusterMetric']]}, error = function(x) NULL), Type = 'character', Default = 0.95)

      if(Debug) print('FE Kmeans H2O 7')

      # Args tracking
      ModelID <- 'temp'
      SavePath <- getwd()
      NThreads <- max(1L, parallel::detectCores()-2L)
      MaxMem <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      if(Debug) print('FE Kmeans H2O 8')

      # Create code
      if(Debug) print('FE Kmeans H2O 3')
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Kmeans H2O Args\n",
        "ModelID <- ", Quantico:::CEP(ModelID),"\n",
        "NThreads <- ", Quantico:::CEP(NThreads), "\n",
        "MaxClusters <- ", Quantico:::CEP(MaxClusters), "\n",
        "ClusterMetric <- ", Quantico:::CEP(ClusterMetric), "\n",
        "gc()\n",
        "MaxMem <- ", Quantico:::CEP(MaxMem), "\n"))

      # Run function
      if(Debug) print('FE Kmeans H2O 9')
      DataList[[temp_train]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_train]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_validate) > 0L) DataList[[temp_validate]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_validate]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_test) > 0L) DataList[[temp_test]][['data']] <- Rodeo::ModelDataPrep(data=DataList[[temp_test]][['data']], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)

      if(Debug) print('FE Kmeans H2O 10')

      # Code
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
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
  TSF_TargetColumn <- Quantico:::ReturnParam(xx = tryCatch({input[['TSF_TargetColumn']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_DateColumnName <- Quantico:::ReturnParam(xx = tryCatch({input[['TSF_DateColumnName']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_GroupVariables <- Quantico:::ReturnParam(xx = tryCatch({input[['TSF_GroupVariables']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_TimeUnit <- Quantico:::ReturnParam(xx = tryCatch({input[['TSF_TimeUnit']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_FillType <- Quantico:::ReturnParam(xx = tryCatch({input[['TSF_FillType']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  TSF_MaxMissingPercentage <- Quantico:::ReturnParam(xx = tryCatch({input[['TSF_MaxMissingPercentage']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  TSF_SimpleImpute <- Quantico:::ReturnParam(xx = tryCatch({input[['TSF_SimpleImpute']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$TSF_SelectData}, error=function(x) NULL), VarName = 'TSF_SelectData', Type = 'character', Default = NULL, Debug = Debug)

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
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Lexical Diversity\n",
    "DataList[[", Quantico:::CEP(SelectData), "]] <- Rodeo::TimeSeriesFill(\n  ",
    "data = DataList[[", Quantico:::CEP(SelectData), "]],\n  ",
    "TargetColumn = ", Quantico:::CEP(TSF_TargetColumn), ",\n  ",
    "DateColumnName = ", Quantico:::CEP(TSF_DateColumnName), ",\n  ",
    "GroupVariables = ", Quantico:::ExpandText(TSF_GroupVariables), ",\n  ",
    "TimeUnit = ", Quantico:::CEP(TSF_TimeUnit), ",\n  ",
    "FillType = ", Quantico:::CEP(TSF_FillType), ",\n  ",
    "MaxMissingPercent = ", Quantico:::CEPP(TSF_MaxMissingPercentage), ",\n  ",
    "SimpleImpute = ", Quantico:::CEPP(TSF_SimpleImpute),
    ")\n"))

  # Return
  if(Debug) {print('Time Series Fill 6'); print(names(DataList)); print(CodeList)}
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
  Roll_NewName <- Quantico:::ReturnParam(xx = tryCatch({input[['Roll_NewName']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_TimeUnit <- Quantico:::ReturnParam(xx = tryCatch({input[['Roll_TimeUnit']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_DateColumnName <- Quantico:::ReturnParam(xx = tryCatch({input[['Roll_DateColumnName']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_GroupVariables <- Quantico:::ReturnParam(xx = tryCatch({input[['Roll_GroupVariables']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_SimpleImpute <- Quantico:::ReturnParam(xx = tryCatch({input[['Roll_SimpleImpute']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Roll_RollVars <- Quantico:::ReturnParam(xx = tryCatch({input[['Roll_RollVars']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  Roll_NonRollVars <- Quantico:::ReturnParam(xx = tryCatch({input[['Roll_NonRollVars']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  Roll_RollDirection <- Quantico:::ReturnParam(xx = tryCatch({input[['Roll_RollDirection']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$TSF_SelectData}, error=function(x) NULL), VarName = 'TSF_SelectData', Type = 'character', Default = NULL, Debug = Debug)

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
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Lexical Diversity\n",
    "DataList[[", Quantico:::CEP(if(length(Roll_NewName) > 0L) Roll_NewName else SelectData), "]] <- Rodeo::TimeSeriesFillRoll(\n  ",
    "data = DataList[[", Quantico:::CEP(SelectData), "]],\n  ",
    "RollVars = ", Quantico:::ExpandText(Roll_RollVars), ",\n  ",
    "NonRollVars = ", Quantico:::ExpandText(NonRollVars), ",\n  ",
    "RollDirection = ", Quantico:::ExpandText(RollDirection), ",\n  ",
    "DateColumnName = ", Quantico:::CEP(Roll_DateColumnName), ",\n  ",
    "GroupVariables = ", Quantico:::ExpandText(Roll_GroupVariables), ",\n  ",
    "TimeUnit = ", Quantico:::CEPP(Roll_TimeUnit), ",\n  ",
    "SimpleImpute = ", Quantico:::CEPP(Roll_SimpleImpute),
    ")\n"))

  # Return
  if(Debug) {print('Time Series Fill 6'); print(names(DataList)); print(CodeList)}
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
  TextSummary_TextColumns <- Quantico:::ReturnParam(xx = tryCatch({input[['TextSummary_TextColumns']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)

  # Pull in values
  if(Debug) print('NLP Text Summary 1')
  TextSummary_RemoveStats <- Quantico:::ReturnParam(xx = tryCatch({input[['TextSummary_RemoveStats']]}, error=function(x) NULL), VarName = 'TextSummary_RemoveStats', Type = 'character', Default = NULL, Debug = Debug)
  SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$TextSummary_SelectData}, error=function(x) NULL), VarName = 'TextSummary_SelectData', Type = 'character', Default = NULL, Debug = Debug)

  # if path is a character then data will be pulled inside the function
  #  otherwise you're passing data directly to function
  if(Debug) print('NLP Text Summary 3')
  DataList[[SelectData]][['data']] <- AutoNLP::TextSummary(
    dt = DataList[[SelectData]][['data']],
    TextColumns = TextSummary_TextColumns,
    RemoveStats = TextSummary_RemoveStats)

  # Create code
  if(Debug) print('NLP Text Summary 6')
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Text Summary\n",
    "DataList[[", Quantico:::CEP(SelectData), "]] <- AutoNLP::TextSummary(\n  ",
    "DataList[[", Quantico:::CEP(SelectData), "]],\n  ",
    "TextColumns = ", Quantico:::ExpandText(TextSummary_TextColumns), ",\n  ",
    "RemoveStats = ", Quantico:::CEPP(TextSummary_RemoveStats),
    ")\n"))

  # Return
  if(Debug) {print('NLP Text Summary 6'); print(names(DataList)); print(CodeList)}
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
  Sentiment_TextColumns <- Quantico:::ReturnParam(xx = tryCatch({input[['Sentiment_TextColumns']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Sentiment_CombineTextGroupVar <- Quantico:::ReturnParam(xx = tryCatch({input[['Sentiment_CombineTextGroupVar']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Sentiment_Response <- Quantico:::ReturnParam(xx = tryCatch({input[['Sentiment_Response']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Sentiment_RemoveStopWords <- Quantico:::ReturnParam(xx = tryCatch({input[['Sentiment_RemoveStopWords']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  Sentiment_Stemming <- Quantico:::ReturnParam(xx = tryCatch({input[['Sentiment_Stemming']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$Sentiment_SelectData}, error=function(x) NULL), VarName = 'TextSummary_SelectData', Type = 'character', Default = NULL, Debug = Debug)

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
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Sentiment\n",
    "DataList[[", Quantico:::CEP(SelectData), "]] <- AutoNLP::Sentiment(\n  ",
    "DataList[[", Quantico:::CEP(SelectData), "]],\n  ",
    "TextColumns = ", Quantico:::ExpandText(Sentiment_TextColumns), ",\n  ",
    "CombineTextGroupVar = ", Quantico:::CEP(Sentiment_CombineTextGroupVar), ",\n  ",
    "Response = ", Quantico:::CEP(Sentiment_Response), ",\n  ",
    "Language = 'english',\n  ",
    "RemoveStopWords = ", Quantico:::CEPP(Sentiment_RemoveStopWords), ",\n  ",
    "Stemming = ", Quantico:::CEPP(Sentiment_Stemming),
    ")\n"))

  # Return
  if(Debug) {print('NLP Sentiment 6'); print(names(DataList)); print(CodeList)}
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
  Readability_TextColumns <- Quantico:::ReturnParam(xx = tryCatch({input[['Readability_TextColumns']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Readability_Measures <- Quantico:::ReturnParam(xx = tryCatch({input[['Readability_Measures']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  Readability_RemoveHyphens <- Quantico:::ReturnParam(xx = tryCatch({input[['Readability_RemoveHyphens']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  Readability_MinSentenceLength <- Quantico:::ReturnParam(xx = tryCatch({input[['Readability_MinSentenceLength']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  Readability_MaxSentenceLength <- Quantico:::ReturnParam(xx = tryCatch({input[['Readability_MaxSentenceLength']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  Readability_Intermediate <- Quantico:::ReturnParam(xx = tryCatch({input[['Readability_Intermediate']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$Readability_SelectData}, error=function(x) NULL), VarName = 'Readability_SelectData', Type = 'character', Default = NULL, Debug = Debug)

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
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Readability\n",
    "DataList[[", Quantico:::CEP(SelectData), "]] <- AutoNLP::Readability(\n  ",
    "DataList[[", Quantico:::CEP(SelectData), "]],\n  ",
    "Measures = ", Quantico:::ExpandText(Readability_Measures), ",\n  ",
    "TextColumns = ", Quantico:::ExpandText(Readability_TextColumns), ",\n  ",
    "RemoveHyphens = ", Quantico:::CEPP(Readability_RemoveHyphens), ",\n  ",
    "MinSentenceLength = ", Quantico:::CEPP(Readability_MinSentenceLength), ",\n  ",
    "MaxSentenceLength = ", Quantico:::CEPP(Readability_MaxSentenceLength), ",\n  ",
    "Intermediate = ", Quantico:::CEPP(Readability_Intermediate),
    ")\n"))

  # Return
  if(Debug) {print('NLP Readability 6'); print(names(DataList)); print(CodeList)}
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
  LexicalDiversity_TextColumns <- Quantico:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_TextColumns']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  LexicalDiversity_Measures <- Quantico:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_Measures']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
  LexicalDiversity_RemoveHyphens <- Quantico:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_RemoveHyphens']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  LexicalDiversity_RemoveSymbols <- Quantico:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_RemoveSymbols']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  LexicalDiversity_RemoveNumbers <- Quantico:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_RemoveNumbers']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  LexicalDiversity_LogBase <- Quantico:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_LogBase']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
  LexicalDiversity_MATTR_Window <- Quantico:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_MATTR_Window']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  LexicalDiversity_MSTTR_Segment <- Quantico:::ReturnParam(xx = tryCatch({input[['LexicalDiversity_MSTTR_Segment']]}, error=function(x) NULL), VarName = NULL, Type = 'logical', Default = NULL, Debug = Debug)
  SelectData <- Quantico:::ReturnParam(xx = tryCatch({input$LexicalDiversity_SelectData}, error=function(x) NULL), VarName = 'LexicalDiversity_SelectData', Type = 'character', Default = NULL, Debug = Debug)

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
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Lexical Diversity\n",
    "DataList[[", Quantico:::CEP(SelectData), "]] <- AutoNLP::LexicalDiversity(\n  ",
    "DataList[[", Quantico:::CEP(SelectData), "]],\n  ",
    "Measures = ", Quantico:::ExpandText(LexicalDiversity_Measures), ",\n  ",
    "TextColumns = ", Quantico:::ExpandText(LexicalDiversity_TextColumns), ",\n  ",
    "RemoveHyphens = ", Quantico:::CEPP(LexicalDiversity_RemoveHyphens), ",\n  ",
    "RemoveSymbols = ", Quantico:::CEPP(LexicalDiversity_RemoveSymbols), ",\n  ",
    "RemovePunctuation = ", Quantico:::CEPP(LexicalDiversity_RemovePunctuation), ",\n  ",
    "RemoveNumbers = ", Quantico:::CEPP(LexicalDiversity_RemoveNumbers), ",\n  ",
    "LogBase = ", Quantico:::CEPP(LexicalDiversity_LogBase), ",\n  ",
    "MATTR_Window = ", Quantico:::CEPP(LexicalDiversity_MATTR_Window), ",\n  ",
    "MSTTR_Segment = ", Quantico:::CEPP(Readability_MSTTR_Segment),
    ")\n"))

  # Return
  if(Debug) {print('NLP Lexical Diversity 6'); print(names(DataList)); print(CodeList)}
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}
