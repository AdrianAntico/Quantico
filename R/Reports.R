# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Single Series FC Reports                                        ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Run_SSFC_Report
#'
#' @description Run_SSFC_Report is an Rmarkdown report for FC
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param DataList DataList from app
#' @param MOL ModelObjectList from app
#' @param ModelID from app
#' @param OutputPath Path to directory where the html will be saved
#'
#' @noRd
Run_SSFC_Report <- function(DataList = NULL,
                            MOL = NULL,
                            ModelID = NULL,
                            OutputPath = NULL) {

  appDir <- system.file("r-markdowns", package = "Quantico")
  DataList <- DataList
  MOL <- MOL
  ModelID <- ModelID

  # Checks
  if(length(ModelID) == 0L) return(NULL)
  if(!exists("DataList")) return(NULL)

  EG <- paste0(ModelID, "_ExperimentGrid")
  FCD <- paste0(ModelID, "_Forecast")
  if(EG %in% names(DataList)) {
    GridTune_proc <- TRUE
  }
  if(FCD %in% names(DataList)) {
    Forecast_proc <- TRUE
  }

  # temp_model_rdata$ArgsList$TargetColumnName
  TargetColumnName <- MOL[["TargetColumnName"]]

  # temp_model_rdata$ArgsList$
  PredictionColumnName <- "Forecast"

  # DateCol
  DateColumnName <- MOL[["DateColumnName"]]

  # Collection List
  OutputList <- list()

  if(Forecast_proc) {

    # Plot with Prediction Intervals
    Forecast_Plot <- AutoPlots::Plot.Line(
      dt = DataList[[FCD]]$data,
      AggMethod = "mean",
      PreAgg = TRUE,
      XVar = DateColumnName,
      YVar = c(TargetColumnName,"Forecast","Low95","Low80","High80","High95"),
      DualYVar = NULL,
      GroupVar = NULL,
      YVarTrans = "Identity",
      DualYVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = "600px",
      Width = "975px",
      Title = "Forecast Line Plot",
      ShowLabels = FALSE,
      Title.YAxis = TargetColumnName,
      Title.XAxis = DateColumnName,
      EchartsTheme = "wef",
      TimeLine = TRUE,
      Alpha = 0.5,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      xaxis.fontSize = 14,
      yaxis.fontSize = 14,
      xaxis.rotate = 0,
      yaxis.rotate = 0,
      ContainLabel = TRUE,
      Debug = TRUE)

    # ----

    # ----

  } else {

    Forecast_Plot <- NULL

  }

  OutputPathName <- file.path(OutputPath, paste0('SSFCReport-', ModelID, '.html'))
  rmarkdown::render(
    input = file.path(appDir, 'SingleSeriesFC.Rmd'),
    output_file = OutputPathName)
}

#' @title SSFCReport
#'
#' @description SSFCReport is an Rmarkdown report for viewing FC results
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param DataList DataList from app
#' @param MOL ModelObjectList from app
#' @param ModelID from app
#' @param OutputPath List of output objects
#'
#' @export
SSFCReport <- function(DataList = NULL,
                       MOL = NULL,
                       ModelID = NULL,
                       OutputPath = NULL) {
  Run_SSFC_Report(
    DataList = DataList,
    MOL = MOL,
    ModelID = ModelID,
    OutputPath = OutputPath
  )
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Panel FC Reports                                                ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Run_PanelFC_Report
#'
#' @description Run_PanelFC_Report is an Rmarkdown report for FC
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param DataList DataList from app
#' @param MOL ModelObjectList from app
#' @param ModelID from app
#' @param OutputPath Path to directory where the html will be saved
#'
#' @noRd
Run_PanelFC_Report <- function(DataList = NULL,
                               MOL = NULL,
                               ModelID = NULL,
                               OutputPath = NULL) {

  appDir <- system.file("r-markdowns", package = "Quantico")
  DataList <- DataList
  MOL <- MOL
  ModelID <- ModelID

  # Checks
  if(length(ModelID) == 0L) return(NULL)

  # temp_model_rdata$ArgsList$TargetColumnName
  TargetColumnName <- MOL[["TargetColumnName"]]

  # temp_model_rdata$ArgsList$
  PredictionColumnName <- "Predict"

  # temp_model_rdata$ArgsList$FeatureColNames
  FeatureColumnNames <- MOL[["FeatureColNames"]]

  # DateCol
  DateColumnName <- MOL$DateColumnName

  # Algo
  temp_algo <- class(MOL$Model)[1L]
  if(temp_algo == "catboost.Model") {
    Algo <- "CatBoost"
  } else if(temp_algo == "xgb.Booster") {
    Algo <- "XGBoost"
  } else if(temp_algo == "lgb.Booster") {
    Algo <- "LightGBM"
  }

  # Group Var
  GroupVariableInclude <- "GroupVar"

  # Data sets
  if(tolower(Algo) == "catboost") {
    TestData <- DataList[[paste0("CatBoostFC_", ModelID, "_ScoringData")]]$data
  } else if(tolower(Algo) == "xgboost") {
    TestData <- DataList[[paste0("XGBoostFC_", ModelID, "_ScoringData")]]$data
  } else if(tolower(Algo) == "lightgbm") {
    TestData <- DataList[[paste0("LightGBMFC_", ModelID, "_ScoringData")]]$data
  }

  OutputList <- list()

  # Args
  PlotWidth <- "975px"
  PlotHeight <- "600px"

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Train Outputs                                                             ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  if(tolower(Algo) == "catboost") {
    ScoringData <- tryCatch({DataList[[paste0("CatBoostFC_", ModelID, "_ScoringData")]]$data}, error = function(x) NULL)
  } else if(tolower(Algo) == "xgboost") {
    ScoringData <- tryCatch({DataList[[paste0("XGBoostFC_", ModelID, "_ScoringData")]]$data}, error = function(x) NULL)
  } else if(tolower(Algo) == "lightgbm") {
    ScoringData <- tryCatch({DataList[[paste0("LightGBMFC_", ModelID, "_ScoringData")]]$data}, error = function(x) NULL)
  }

  OutputPathName <- file.path(OutputPath, paste0('PanelFCReport-', ModelID, '.html'))
  rmarkdown::render(
    input = file.path(appDir, 'PanelFC.Rmd'),
    output_file = OutputPathName)
}

#' @title PanelFCReport
#'
#' @description PanelFCReport is an Rmarkdown report for viewing FC results
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param DataList DataList from app
#' @param MOL ModelObjectList from app
#' @param ModelID from app
#' @param OutputPath List of output objects
#'
#' @export
PanelFCReport <- function(DataList = NULL,
                          MOL = NULL,
                          ModelID = NULL,
                          OutputPath = NULL) {
  Run_PanelFC_Report(
    DataList = DataList,
    MOL = MOL,
    ModelID = ModelID,
    OutputPath = OutputPath
  )
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# ML Reports                                                      ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Run_ModelInsightsReport
#'
#' @description Run_ModelInsightsReport is an Rmarkdown report for viewing the model insights generated by Quantico supervised learning functions
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param TrainDataInclude Default FALSE. If FALSE, no derived visuals or metrics for TrainData
#' @param FeatureColumnNames NULL
#' @param SampleSize Default 100000
#' @param ModelObject Output from a Quantico supervised learning function
#' @param ModelID ModelID used in the Quantico supervised learning function
#' @param SourcePath Path to directory with DataQuant Model Output
#' @param OutputPath Path to directory where the html will be saved
#'
#' @noRd
Run_ModelInsightsReport <- function(TrainDataInclude = FALSE,
                                    FeatureColumnNames = NULL,
                                    SampleSize = 100000,
                                    ModelObject = NULL,
                                    ModelID = NULL,
                                    SourcePath = NULL,
                                    OutputPath = NULL) {

  # Directory reference
  appDir <- system.file("r-markdowns", package = "Quantico")

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Globalize the parameters
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  # ModelObject
  ModelObject <<- ModelObject

  # Finalize Passthrough
  if(!is.null(ModelObject)) {

    # DataSets
    ModelObject[['TestData']] <- ModelObject[['TestData']][order(runif(.N))][seq_len(min(.N, SampleSize))]
    if(TrainDataInclude) {
      ModelObject[['TrainData']] <- ModelObject[['TrainData']][order(runif(.N))][seq_len(min(.N, SampleSize))]
    } else {
      ModelObject[['TrainData']] <- NULL
    }

    # Meta info
    TargetType <- ModelObject[["ArgsList"]][["TargetType"]]
    if(TargetType == "Binary Classification") TargetType <- "classification"
    TargetColumnName <- ModelObject[['ArgsList']][['TargetColumnName']]
    PredictionColumnName <- ModelObject[["ArgsList"]][["PredictionColumnName"]]
    TargetLevels <- ModelObject[["ArgsList"]][["TargetLevels"]]
    Algo <- tolower(ModelObject[["ArgsList"]][["Algo"]])
    if(is.null(FeatureColumnNames)) {
      FeatureColumnNames <- ModelObject[['ColNames']][[1L]]
    }
    ArgsList <- ModelObject[['ArgsList']]

  } else {
    return(NULL)
  }

  if(length(ModelObject[['TestData']]) == 0L && length(ModelObject[['TrainData']]) == 0L) return(NULL)

  # Metadata args
  TargetType <- TargetType
  TargetLevels <- TargetLevels
  ModelID <- ModelID
  Algo <- Algo
  SourcePath <- SourcePath
  OutputPath <- OutputPath
  OutputPathName <- file.path(OutputPath, paste0('MLReport-', ModelID, '-', TargetType, '.html'))

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Select Rmarkdown Report and Run it ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  # Regression Markdown
  if(tolower(TargetType) == 'regression') {
    rmarkdown::render(
      input = file.path(appDir, 'Regression_ModelInsights.Rmd'),
      output_file = OutputPathName)
  }

  # Classification Markdown
  if(tolower(TargetType) == 'classification') {
    rmarkdown::render(
      input = file.path(appDir, 'Classification_ModelInsights.Rmd'),
      output_file = OutputPathName)
  }

  # MultiClass Markdown
  if(tolower(TargetType) == 'multiclass') {
    rmarkdown::render(
      input = file.path(appDir, 'MultiClass_ModelInsights.Rmd'),
      output_file = OutputPathName)
  }
}

#' @title ModelInsightsReport
#'
#' @description ModelInsightsReport is an Rmarkdown report for viewing the model insights generated by Quantico supervised learning functions
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param TrainDataInclude Default FALSE. If FALSE, no derived visuals or metrics for TrainData
#' @param FeatureColumnNames NULL. Feature column names as character vector.
#' @param SampleSize Default 100000
#' @param ModelObject Returned output from regression, classificaiton, and multiclass Remix Auto_() models. Currenly supports CatBoost, XGBoost, and LightGBM models
#' @param ModelID ModelID used in the Quantico supervised learning function
#' @param SourcePath Path to directory with Quantico Model Output
#' @param OutputPath Path to directory where the html will be saved
#' @param GlobalVars ls() don't use
#' @param KeepOutput NULL A list of output names to select. Pass in as a character vector. E.g. c('Test_VariableImportance', 'Train_VariableImportance')
#'
#' @export
ModelInsightsReport <- function(TrainDataInclude = FALSE,
                                FeatureColumnNames = NULL,
                                SampleSize = 100000,
                                ModelObject = NULL,
                                ModelID = 'ModelTest',
                                SourcePath = NULL,
                                OutputPath = NULL,
                                KeepOutput = NULL,
                                GlobalVars = ls()) {

  # Run Function
  Run_ModelInsightsReport(
    TrainDataInclude = TrainDataInclude,
    FeatureColumnNames = FeatureColumnNames,
    SampleSize = SampleSize,
    ModelObject = ModelObject,
    ModelID = ModelID,
    SourcePath = SourcePath,
    OutputPath = OutputPath)

  # Remove objects
  GlobalVarsNew <- ls()
  rm(list = c(setdiff(GlobalVarsNew, c(GlobalVars, KeepOutput))))
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# EDA Reports                                                     ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Run_EDA_Report
#'
#' @description Run_EDA_Report is an Rmarkdown report for EDA
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param data NULL
#' @param DataName NULL
#' @param UnivariateVars NULL
#' @param CorrVars NULL
#' @param TrendVars NULL
#' @param TrendDateVar NULL
#' @param TrendGroupVar NULL
#' @param OutputPath Path to directory where the html will be saved
#'
#' @noRd
Run_EDA_Report <- function(data = NULL,
                           DataName = NULL,
                           UnivariateVars = NULL,
                           CorrVars = NULL,
                           TrendVars = NULL,
                           TrendDateVar = NULL,
                           TrendGroupVar = NULL,
                           OutputPath = NULL) {

  appDir <- system.file("r-markdowns", package = "Quantico")
  print(head(data))
  print(UnivariateVars)
  print(CorrVars)
  print(TrendVars)
  print(TrendDateVar)
  print(TrendGroupVar)


  data <- data
  UnivariateVars <- UnivariateVars
  CorrVars <- CorrVars
  TrendVars <- TrendVars
  TrendDateVar <- TrendDateVar
  TrendGroupVar <- TrendGroupVar

  if(length(UnivariateVars) > 50L) UnivariateVars <- UnivariateVars[seq_len(50L)]
  if(length(CorrVars) > 50L) CorrVars <- CorrVars[seq_len(50L)]
  if(length(TrendVars) > 50L) TrendVars <- TrendVars[seq_len(50L)]
  if(length(TrendDateVar) > 1L) {
    for(zzz in TrendDateVar) {
      if(class(data[[zzz]])[1L] %in% c("Date", "posix", "IDate", "IDateTime")) {
        TrendDateVar <- zzz
        break
      }
    }
  } else if(length(TrendDateVar) > 0L) {
    if(!class(data[[TrendDateVar]])[1L] %in% c("Date", "posix", "IDate", "IDateTime")) {
      x <- data[1L, get(TrendDateVar)]
      x1 <- lubridate::guess_formats(x, orders = c('mdY', 'BdY', 'Bdy', 'bdY', 'bdy', 'mdy', 'dby', 'Ymd', 'Ydm', 'dmy'))
      data[, eval(TrendDateVar) := as.Date(get(TrendDateVar), tryFormats = x1)]
      if(!class(data[[TrendDateVar]])[1L] %in% c("Date", "posix", "IDate", "IDateTime")) {
        TrendDateVar <- NULL
      }
    }
  }

  OutputPathName <- file.path(OutputPath, paste0('EDAReport-', DataName, '.html'))
  rmarkdown::render(
    input = file.path(appDir, 'EDA.Rmd'),
    output_file = OutputPathName)
}

#' @title EDAReport
#'
#' @description EDAReport is an Rmarkdown report for viewing EDA results
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param data NULL
#' @param DataName NULL
#' @param UnivariateVars NULL
#' @param CorrVars NULL
#' @param TrendVars NULL
#' @param TrendDateVar NULL
#' @param TrendGroupVar NULL
#' @param OutputPath List of output objects
#'
#' @export
EDAReport <- function(data = NULL,
                      DataName = NULL,
                      UnivariateVars = NULL,
                      CorrVars = NULL,
                      TrendVars = NULL,
                      TrendDateVar = NULL,
                      TrendGroupVar = NULL,
                      OutputPath = NULL) {

  Run_EDA_Report(
    data = data,
    DataName = DataName,
    UnivariateVars = UnivariateVars,
    CorrVars = CorrVars,
    TrendVars = TrendVars,
    TrendDateVar = TrendDateVar,
    TrendGroupVar = TrendGroupVar,
    OutputPath = OutputPath)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Plotting Reports                                                ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Run_Plotting_Report
#'
#' @description Run_Plotting_Report is an Rmarkdown report for Plot Output
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param PlotOutputList NULL
#' @param OutputPath Path to directory where the html will be saved
#'
#' @noRd
Run_Plotting_Report <- function(PlotOutputList = NULL,
                                OutputPath = NULL) {

  appDir <- system.file("r-markdowns", package = "Quantico")
  PlotOutputList <- PlotOutputList
  OutputPathName <- file.path(OutputPath, paste0('Plotting-', gsub(pattern = ":", "_", Sys.time()), '.html'))
  rmarkdown::render(
    input = file.path(appDir, 'Plotting.Rmd'),
    output_file = OutputPathName)
}

#' @title PlottingReport
#'
#' @description PlottingReport is an Rmarkdown report for viewing Plotting results
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param PlotOutputList NULL
#' @param OutputPath List of output objects
#'
#' @export
PlottingReport <- function(PlotOutputList = NULL,
                           OutputPath = NULL) {

  Run_Plotting_Report(
    PlotOutputList = PlotOutputList,
    OutputPath = OutputPath)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Tables Reports                                                  ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Run_Tables_Report
#'
#' @description Run_Tables_Report is an Rmarkdown report for Table Output
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param DataOuts NULL
#' @param DataOutputList NULL
#' @param OutputPath Path to directory where the html will be saved
#'
#' @noRd
Run_Tables_Report <- function(DataOuts = NULL,
                              DataList = NULL,
                              OutputPath = NULL) {

  appDir <- system.file("r-markdowns", package = "Quantico")
  DataList <- DataList
  DataOuts <- DataOuts
  OutputPathName <- file.path(OutputPath, paste0('Tables-', gsub(pattern = ":", "_", Sys.time()), '.html'))
  rmarkdown::render(
    input = file.path(appDir, 'Tables.Rmd'),
    output_file = OutputPathName)
}

#' @title TablesReport
#'
#' @description TablesReport is an Rmarkdown report for viewing Tables results
#'
#' @author Adrian Antico
#' @family Reports
#'
#' @param DataOuts NULL
#' @param DataList NULL
#' @param OutputPath List of output objects
#'
#' @export
TablesReport <- function(DataOuts = NULL,
                         DataList = NULL,
                         OutputPath = NULL) {

  Run_Tables_Report(
    DataOuts = DataOuts,
    DataList = DataList,
    OutputPath = OutputPath)
}

# ----

# ----
