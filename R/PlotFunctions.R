#' @title Shiny.Plot.Build
#'
#' @description Step 1 in building plots
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param input From App
#' @param output From App
#' @param session From App
#' @param PlotNumberss From App
#' @param DataList From App
#' @param CodeList From App
#' @param OutputPage From App
#' @param PlotEngine = PE from App
#' @param Timeline = TL from App
#' @param EchartsTheme = EchartsTheme From App
#' @param ColorFont = "#e2e2e2"
#' @param NumLevelsDisplay = numeric
#' @param NumberOfBins histograms
#' @param GlobalChange TRUE
#' @param Args NULL
#' @param SubsetList NULL
#' @param Debug From App
#'
#' @keywords internal
Shiny.Plot.Build <- function(input,
                             output,
                             session,
                             PlotNumbers,
                             DataList,
                             Debug,
                             PlotMap,
                             CodeList,
                             EchartsTheme,
                             OutputPage = 1,
                             PlotEngine = "Plotly",
                             Timeline = TRUE,
                             ColorFont = "#e2e2e2",
                             NumLevelsDisplay = 50L,
                             NumberOfBins = 30L,
                             GlobalChange = FALSE,
                             PlotHeight = "860px",
                             PlotWidth = "1450px",
                             Args = NULL,
                             SubsetList = NULL) {

  if(missing(CodeList)) CodeList <- list()

  DefaultArgs <- function(default, x) {
    xx <- tryCatch({x}, error = function(x) NULL)
    if(length(xx) > 0L) {
      xx
    } else {
      default
    }
  }

  # New collection list each time
  #  Results are saved to a different list in App
  #  This preserves ordering if user requests a diff order
  PlotList <- list()
  incre <- 0L
  tempPlotType <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('PlotID_', PlotNumbers[1L])]]}, error = function(x) NULL), Type='character', Default = PlotMap[PlotNumber == eval(PlotNumbers[1L])][["PlotType"]], Debug = TRUE)
  if(length(DataList) > 0L) {
    for(PlotNumberss in PlotNumbers) {

      # Basis
      PlotType <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('PlotID_', PlotNumberss)]]}, error = function(x) NULL), Type='character', Default = PlotMap[PlotNumber == eval(PlotNumberss)][["PlotType"]], Debug = TRUE)

      if(PlotType %in% c("Autocorrelation","PartialAutocorr")) {
        DateVariable <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('PlottingDateVar', PlotNumberss)]]}, error = function(x) NULL), Type='character', Default = PlotMap[PlotNumber == eval(PlotNumberss)][["PlotType"]], Debug = TRUE)
        MaxLagsVariable <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('PlottingMaxLags', PlotNumberss)]]}, error = function(x) NULL), Type='character', Default = PlotMap[PlotNumber == eval(PlotNumberss)][["PlotType"]], Debug = TRUE)
        TimeUnitVariable <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('PlottingTimeUnit', PlotNumberss)]]}, error = function(x) NULL), Type='character', Default = PlotMap[PlotNumber == eval(PlotNumberss)][["PlotType"]], Debug = TRUE)
      }

      # Continuation Logic
      if(length(PlotType) > 0L) {

        # Plotting variables
        SampleSize <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('SampleSize', PlotNumberss)]]}, error=function(x) NULL), Type='numeric', Default = DefaultArgs(default = 30000, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('SampleSize', PlotNumberss)]]))
        if(length(SampleSize) == 0L) {
          SampleSize <- 15000L
        } else if(SampleSize <= 1L) {
          SampleSize <- 15000L
        }

        AggMethod <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('AggMethod', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = 'mean', x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('AggMethod', PlotNumberss)]]))
        YVar <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('YVar', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('YVar', PlotNumberss)]]))
        DualYVar <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('DualYVar', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('DualYVar', PlotNumberss)]]))
        if(!PlotType %in% c("LinePlot", "StepPlot", "AreaPlot")) DualYVar <- NULL
        XVar <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('XVar', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('XVar', PlotNumberss)]]))
        ZVar <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('ZVar', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('ZVar', PlotNumberss)]]))

        if(length(YVar) > 0L && any(YVar == "No Data Loaded")) YVar <- NULL
        if(length(DualYVar) > 0L && any(DualYVar == "No Data Loaded")) DualYVar <- NULL
        if(length(XVar) > 0L && any(XVar == "No Data Loaded")) XVar <- NULL
        if(length(ZVar) > 0L && any(ZVar == "No Data Loaded")) ZVar <- NULL

        YVarTrans <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('YVarTrans', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = "Identity", x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('YVarTrans', PlotNumberss)]]))
        DualYVarTrans <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('DualYVarTrans', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = "Identity", x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('DualYVarTrans', PlotNumberss)]]))
        XVarTrans <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('XVarTrans', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = "Identity", x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('XVarTrans', PlotNumberss)]]))
        ZVarTrans <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('ZVarTrans', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = "Identity", x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('ZVarTrans', PlotNumberss)]]))

        if(PlotType %in% c("ConfusionMatrixHeatmap","ROCPlot","LiftPlot","GainsPlot","Residuals","ResidScatter","CalibrationLine","CalibrationBox")) {
          XVar <- ZVar; ZVar <- NULL
          XVarTrans <- ZVarTrans; ZVarTrans <- "Identity"
        }

        if(Debug) print(rep("GroupVar", 5))
        if(Debug) print(rep(input[[paste0('GroupVars', PlotNumberss)]], 5))
        GroupVars <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('GroupVars', PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('GroupVars', PlotNumberss)]]))

        if(Debug) print(rep(GroupVars, 5))

        Levels1 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('Levels_',PlotNumberss,'_1')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('Levels_',PlotNumberss,'_1')]]))
        Levels2 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('Levels_',PlotNumberss,'_2')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('Levels_',PlotNumberss,'_2')]]))
        Levels3 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('Levels_',PlotNumberss,'_3')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('Levels_',PlotNumberss,'_3')]]))

        if(length(GroupVars) > 0L && any(GroupVars == "No Data Loaded")) GroupVars <- NULL

        FacetRows <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FacetRows', PlotNumberss)]]}, error=function(x) NULL), Type='numeric', Default = DefaultArgs(default = 1, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FacetRows', PlotNumberss)]]))
        if(length(FacetRows) > 0L) FacetRows <- as.numeric(FacetRows)
        FacetCols <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FacetCols', PlotNumberss)]]}, error=function(x) NULL), Type='numeric', Default = DefaultArgs(default = 1, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FacetCols', PlotNumberss)]]))
        if(length(FacetCols) > 0L) FacetCols <- as.numeric(FacetCols)

        Title <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('Title',PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('Title', PlotNumberss)]]))
        ShowLabels <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('ShowLabels',PlotNumberss)]]}, error=function(x) NULL), Type='logical', Default = DefaultArgs(default = FALSE, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('ShowLabels', PlotNumberss)]]))
        YAxisTitle <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('YAxisTitle',PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('YAxisTitle', PlotNumberss)]]))
        XAxisTitle <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('XAxisTitle',PlotNumberss)]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('XAxisTitle', PlotNumberss)]]))

        FilterVar1 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',PlotNumberss, '_1')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterVariable_',PlotNumberss, '_1')]]))
        FilterVar2 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',PlotNumberss, '_2')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterVariable_',PlotNumberss, '_2')]]))
        FilterVar3 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',PlotNumberss, '_3')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterVariable_',PlotNumberss, '_3')]]))
        FilterVar4 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',PlotNumberss, '_4')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterVariable_',PlotNumberss, '_4')]]))
        FilterLogic1 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',PlotNumberss, '_1')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterLogic_',PlotNumberss, '_1')]]))
        FilterLogic2 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',PlotNumberss, '_2')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterLogic_',PlotNumberss, '_2')]]))
        FilterLogic3 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',PlotNumberss, '_3')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterLogic_',PlotNumberss, '_3')]]))
        FilterLogic4 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',PlotNumberss, '_4')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterLogic_',PlotNumberss, '_4')]]))
        FilterValue_1_1 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',PlotNumberss, '_1_1')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterValue_',PlotNumberss, '_1_1')]]))
        FilterValue_1_2 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',PlotNumberss, '_1_2')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterValue_',PlotNumberss, '_1_2')]]))
        FilterValue_2_1 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',PlotNumberss, '_2_1')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterValue_',PlotNumberss, '_2_1')]]))
        FilterValue_2_2 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',PlotNumberss, '_2_2')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterValue_',PlotNumberss, '_2_2')]]))
        FilterValue_3_1 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',PlotNumberss, '_3_1')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterValue_',PlotNumberss, '_3_1')]]))
        FilterValue_3_2 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',PlotNumberss, '_3_2')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterValue_',PlotNumberss, '_3_2')]]))
        FilterValue_4_1 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',PlotNumberss, '_4_1')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterValue_',PlotNumberss, '_4_1')]]))
        FilterValue_4_2 <- DataMuse:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',PlotNumberss, '_4_2')]]}, error=function(x) NULL), Type='character', Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('FilterValue_',PlotNumberss, '_4_2')]]))

        if(length(FilterValue_1_1) > 0L && any(FilterValue_1_1 == 'No Data Loaded')) FilterValue_1_1 <- NULL

        if(Debug) print(rep(FilterVar1, 5))
        if(Debug) print(rep(FilterValue_1_1, 5))

        if(length(FilterValue_1_2) > 0L && any(FilterValue_1_2 == 'No Data Loaded')) FilterValue_1_2 <- NULL
        if(length(FilterValue_2_1) > 0L && any(FilterValue_2_1 == 'No Data Loaded')) FilterValue_2_1 <- NULL
        if(length(FilterValue_2_2) > 0L && any(FilterValue_2_2 == 'No Data Loaded')) FilterValue_2_2 <- NULL
        if(length(FilterValue_3_1) > 0L && any(FilterValue_3_1 == 'No Data Loaded')) FilterValue_3_1 <- NULL
        if(length(FilterValue_3_2) > 0L && any(FilterValue_3_2 == 'No Data Loaded')) FilterValue_3_2 <- NULL
        if(length(FilterValue_4_1) > 0L && any(FilterValue_4_1 == 'No Data Loaded')) FilterValue_4_1 <- NULL
        if(length(FilterValue_4_2) > 0L && any(FilterValue_4_2 == 'No Data Loaded')) FilterValue_4_2 <- NULL

        if(length(FilterVar1) > 0L && any(FilterVar1 == "No Data Loaded")) FilterVar1 <- NULL
        if(length(FilterVar2) > 0L && any(FilterVar2 == "No Data Loaded")) FilterVar2 <- NULL
        if(length(FilterVar3) > 0L && any(FilterVar3 == "No Data Loaded")) FilterVar3 <- NULL
        if(length(FilterVar4) > 0L && any(FilterVar4 == "No Data Loaded")) FilterVar4 <- NULL

        # Special: Sidebar color picker input
        FontColor <- ColorFont
        GridColor <- ColorFont
        NumberBins <- NumberOfBins

        if(ShowLabels == "1") {
          ShowLabels <- TRUE
        } else {
          ShowLabels <- FALSE
        }

        if(length(Title) == 0L || Title == "") {
          Title <- PlotType
        } else if(tolower(Title) == "none") {
          Title <- NULL
        }

        if(length(XAxisTitle) == 0L || XAxisTitle == "") {
          XAxisTitle <- XVar
        } else if(tolower(XAxisTitle) == "none") {
          XAxisTitle <- NULL
        }

        if(length(YAxisTitle) == 0L || YAxisTitle == "") {
          YAxisTitle <- paste0(YVar, collapse = " | ")
        } else if(tolower(YAxisTitle) == "none") {
          YAxisTitle <- NULL
        }

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Logic Check to Build Plots           ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Define data
        dName <- DataMuse::ReturnParam(xx = input[[paste0('Plot', PlotNumberss, '_SelectData')]], Type = "character", Default = DefaultArgs(default = NULL, x = Args[[paste0(PlotType, PlotNumberss)]][[paste0('Plot',PlotNumberss, '_SelectData')]]))
        Output <- DataMuse:::Shiny.Plot.DM.data1(DataList,PlotNumberss,PlotType,CodeList,Debug,DataName=dName)
        if(length(Output) > 0L) {
          data1 <- Output$data1; CodeList <- Output$CodeList
          CodeList <<- CodeList

          # ABC Order
          if((FacetRows > 1L || FacetCols > 1L) && length(GroupVars) > 0L && length(Levels1) == 0L) {
            Levels1 <- as.character(unique(data1[[GroupVars[1L]]]))
            if(length(Levels1) > 0L) Levels1 <- sort(Levels1)[seq_len(min(length(Levels1), as.numeric(FacetRows) * as.numeric(FacetCols)))]
          }

          # Check that user inputs are correct for standard plots
          Output <- DataMuse:::Shiny.Plot.StandardPlotChecks(d=data1, y=YVar, x=XVar, z=ZVar, Type=PlotType)
          ValidCheck <- Output$ValidCheck; msg = Output$msg; print(msg)

        } else {
          ValidCheck <- FALSE
        }

      } else {
        ValidCheck <- FALSE
      }

      # If ValidCheck is FALSE, send error message back to user
      if(!ValidCheck) {

        if(!exists("msg")) msg <- "Inputs are missing"
        shinyWidgets::sendSweetAlert(session, title = NULL, text = msg, type = NULL, btn_labels = "error", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

      } else {

        if(Debug) print("Continuation Logic passed 30")

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Check for request changes in Data    ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Check to redo reduction steps: stores a state of args and uses them for comparison to determine if Reduction steps neeed to be redone
        if(!(exists('SubsetList') && length(SubsetList[[paste0('PlotNumberss', PlotNumberss)]]) > 0L && SubsetList[[paste0('PlotNumberss', PlotNumberss)]] >= 1L)) {

          # SubsetList <- list()
          SubsetList[["PlotEngine"]] <- PlotEngine
          SubsetList[["EchartsTheme"]] <- EchartsTheme
          SubsetList[["Timeline"]] <- Timeline
          SubsetList[["ColorFont"]] <- ColorFont
          SubsetList[[paste0("SampleSize", PlotNumberss)]] <- SampleSize

          SubsetList[["YVar"]] <- YVar
          SubsetList[["DualYVar"]] <- DualYVar
          SubsetList[["XVar"]] <- XVar
          SubsetList[["ZVar"]] <- ZVar

          SubsetList[["YVarTrans"]] <- YVarTrans
          SubsetList[["DualYVarTrans"]] <- DualYVarTrans
          SubsetList[["XVarTrans"]] <- XVarTrans
          SubsetList[["ZVarTrans"]] <- ZVarTrans
          SubsetList[["FacetRows"]] <- FacetRows
          SubsetList[["FacetCols"]] <- FacetCols

          SubsetList[[paste0('PlotNumberss', PlotNumberss)]] <- 1L
          SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          SubsetList[[paste0('GroupVars', PlotNumberss)]] <- GroupVars
          SubsetList[[paste0('Levels_', PlotNumberss, '_1')]] <- Levels1
          SubsetList[[paste0('Levels_', PlotNumberss, '_2')]] <- Levels2
          SubsetList[[paste0('Levels_', PlotNumberss, '_3')]] <- Levels3

          # Filter Variables
          SubsetList[[paste0('FilterVariable_', PlotNumberss, '_1')]] <- if(length(FilterVar1) != 0 && FilterVar1 != 'None') stringr::str_remove(string = FilterVar1, pattern = 'ModelVar-') else 'None'
          SubsetList[[paste0('FilterVariable_', PlotNumberss, '_2')]] <- if(length(FilterVar2) != 0 && FilterVar2 != 'None') stringr::str_remove(string = FilterVar2, pattern = 'ModelVar-') else 'None'
          SubsetList[[paste0('FilterVariable_', PlotNumberss, '_3')]] <- if(length(FilterVar3) != 0 && FilterVar3 != 'None') stringr::str_remove(string = FilterVar3, pattern = 'ModelVar-') else 'None'
          SubsetList[[paste0('FilterVariable_', PlotNumberss, '_4')]] <- if(length(FilterVar4) != 0 && FilterVar4 != 'None') stringr::str_remove(string = FilterVar4, pattern = 'ModelVar-') else 'None'

          # Filter Logic
          SubsetList[[paste0('FilterLogic_', PlotNumberss, '_1')]] <- FilterLogic1
          SubsetList[[paste0('FilterLogic_', PlotNumberss, '_2')]] <- FilterLogic2
          SubsetList[[paste0('FilterLogic_', PlotNumberss, '_3')]] <- FilterLogic3
          SubsetList[[paste0('FilterLogic_', PlotNumberss, '_3')]] <- FilterLogic4

          # Filter Values
          SubsetList[[paste0('FilterValue_', PlotNumberss, '_1_1')]] <- FilterValue_1_1
          SubsetList[[paste0('FilterValue_', PlotNumberss, '_1_2')]] <- FilterValue_1_2
          SubsetList[[paste0('FilterValue_', PlotNumberss, '_2_1')]] <- FilterValue_2_1
          SubsetList[[paste0('FilterValue_', PlotNumberss, '_2_2')]] <- FilterValue_2_2
          SubsetList[[paste0('FilterValue_', PlotNumberss, '_3_1')]] <- FilterValue_3_1
          SubsetList[[paste0('FilterValue_', PlotNumberss, '_3_2')]] <- FilterValue_3_2
          SubsetList[[paste0('FilterValue_', PlotNumberss, '_4_1')]] <- FilterValue_4_1
          SubsetList[[paste0('FilterValue_', PlotNumberss, '_4_2')]] <- FilterValue_4_2

          # Store Globally
          SubsetList <<- SubsetList
          UpdateData <- TRUE

        } else {

          # MetaData
          SubsetList[[paste0('PlotNumberss', PlotNumberss)]] <- PlotNumberss + 1L
          SubsetList[[paste0('DataPrep', PlotNumberss)]] <- FALSE

          # Group Variables

          # Sample Size
          if(!all(SubsetList[[paste0("SampleSize", PlotNumberss)]] == SampleSize)) {
            SubsetList[[paste0("SampleSize", PlotNumberss)]] <- SampleSize; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          # Check values
          if(!all(SubsetList[[paste0('GroupVars', PlotNumberss)]] == GroupVars)) {
            SubsetList[[paste0('GroupVars', PlotNumberss)]] <- GroupVars
            SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          # Levels
          if(Debug) {print("Levels"); print(paste0("SubsetList DataPrep = ", SubsetList[[paste0('DataPrep', PlotNumberss)]]))}
          if(!all(SubsetList[[paste0('Levels_', PlotNumberss, '_1')]] == Levels1)) {
            SubsetList[[paste0('Levels_', PlotNumberss, '_1')]] <- Levels1; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('Levels_', PlotNumberss, '_2')]] == Levels2)) {
            SubsetList[[paste0('Levels_', PlotNumberss, '_2')]] <- Levels2; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('Levels_', PlotNumberss, '_3')]] == Levels3)) {
            SubsetList[[paste0('Levels_', PlotNumberss, '_3')]] <- Levels3; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(Debug) {print("Standard Vars");print(paste0("SubsetList DataPrep = ", SubsetList[[paste0('DataPrep', PlotNumberss)]]))}
          if(!all(SubsetList[["YVar"]] == YVar)) {
            SubsetList[["YVar"]] <- YVar; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(Debug) {print("Standard Vars");print(paste0("SubsetList DataPrep = ", SubsetList[[paste0('DataPrep', PlotNumberss)]]))}
          if(!all(SubsetList[["DualYVar"]] == DualYVar)) {
            SubsetList[["DualYVar"]] <- DualYVar; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(!all(SubsetList[["XVar"]] == XVar)) {
            SubsetList[["XVar"]] <- XVar; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(!all(SubsetList[["ZVar"]] == ZVar)) {
            SubsetList[["ZVar"]] <- ZVar; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(!all(SubsetList[["YVarTrans"]] == YVarTrans)) {
            SubsetList[["YVarTrans"]] <- YVarTrans; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(!all(SubsetList[["DualYVarTrans"]] == DualYVarTrans)) {
            SubsetList[["DualYVarTrans"]] <- DualYVarTrans; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(!all(SubsetList[["XVarTrans"]] == XVarTrans)) {
            SubsetList[["XVarTrans"]] <- XVarTrans; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(!all(SubsetList[["ZVarTrans"]] == ZVarTrans)) {
            SubsetList[["ZVarTrans"]] <- ZVarTrans; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(!all(SubsetList[["FacetRows"]] == FacetRows)) {
            SubsetList[["FacetRows"]] <- FacetRows; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          if(!all(SubsetList[["FacetCols"]] == FacetCols)) {
            SubsetList[["FacetCols"]] <- FacetCols; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          # Filter Variables
          if(!all(SubsetList[[paste0('FilterVariable_', PlotNumberss, '_1')]] == FilterVar1)) {
            SubsetList[[paste0('FilterVariable_', PlotNumberss, '_1')]] <- FilterVar1; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', PlotNumberss, '_2')]] == FilterVar2)) {
            SubsetList[[paste0('FilterVariable_', PlotNumberss, '_2')]] <- FilterVar2; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', PlotNumberss, '_3')]] == FilterVar3)) {
            SubsetList[[paste0('FilterVariable_', PlotNumberss, '_3')]] <- FilterVar3; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', PlotNumberss, '_4')]] == FilterVar4)) {
            SubsetList[[paste0('FilterVariable_', PlotNumberss, '_4')]] <- FilterVariable4; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          # Filter Logic
          if(Debug) {print("Filter Logic");print(paste0("SubsetList DataPrep = ", SubsetList[[paste0('DataPrep', PlotNumberss)]]))}
          if(!all(SubsetList[[paste0('FilterLogic_', PlotNumberss, '_1')]] == FilterLogic1)) {
            SubsetList[[paste0('FilterLogic_', PlotNumberss, '_1')]] <- FilterLogic1; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', PlotNumberss, '_2')]] == FilterLogic2)) {
            SubsetList[[paste0('FilterLogic_', PlotNumberss, '_2')]] <- FilterLogic2; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', PlotNumberss, '_3')]] == FilterLogic3)) {
            SubsetList[[paste0('FilterLogic_', PlotNumberss, '_3')]] <- FilterLogic3; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', PlotNumberss, '_4')]] == FilterLogic4)) {
            SubsetList[[paste0('FilterLogic_', PlotNumberss, '_4')]] <- FilterLogic4; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }

          # Filter Values
          if(Debug) {print("Filter Values");print(paste0("SubsetList DataPrep = ", SubsetList[[paste0('DataPrep', PlotNumberss)]]))}
          if(!all(SubsetList[[paste0('FilterValue_', PlotNumberss, '_1_1')]] == FilterValue_1_1)) {
            SubsetList[[paste0('FilterValue_', PlotNumberss, '_1_1')]] <- FilterValue_1_1; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', PlotNumberss, '_1_2')]] == FilterValue_1_2)) {
            SubsetList[[paste0('FilterValue_', PlotNumberss, '_1_2')]] <- FilterValue_1_2; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', PlotNumberss, '_2_1')]] == FilterValue_2_1)) {
            SubsetList[[paste0('FilterValue_', PlotNumberss, '_2_1')]] <- FilterValue_2_1; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', PlotNumberss, '_2_2')]] == FilterValue_2_2)) {
            SubsetList[[paste0('FilterValue_', PlotNumberss, '_2_2')]] <- FilterValue_2_2; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', PlotNumberss, '_3_1')]] == FilterValue_3_1)) {
            SubsetList[[paste0('FilterValue_', PlotNumberss, '_3_1')]] <- FilterValue_3_1; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', PlotNumberss, '_3_2')]] == FilterValue_3_2)) {
            SubsetList[[paste0('FilterValue_', PlotNumberss, '_3_2')]] <- FilterValue_3_2; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', PlotNumberss, '_4_1')]] == FilterValue_4_1)) {
            SubsetList[[paste0('FilterValue_', PlotNumberss, '_4_1')]] <- FilterValue_4_1; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', PlotNumberss, '_4_2')]] == FilterValue_4_2)) {
            SubsetList[[paste0('FilterValue_', PlotNumberss, '_4_2')]] <- FilterValue_4_2; SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE
          }
          SubsetList <<- SubsetList
          if(SubsetList[[paste0('DataPrep', PlotNumberss)]]) {
            UpdateData <- TRUE
          } else {
            UpdateData <- FALSE
          }
        }

        if(Debug) {print("SubsetList[[paste0('DataPrep', PlotNumberss)]]");print(SubsetList[[paste0('DataPrep', PlotNumberss)]])}
        if(SubsetList[[paste0('DataPrep', PlotNumberss)]]) {
          CollectCode <- TRUE
        } else {
          CollectCode <- FALSE
        }

        SubsetList[[paste0('DataPrep', PlotNumberss)]] <- TRUE

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Data Preparation                     ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Subset Columns 1
        if(PlotType == "ShapleyImportance") YVar <- names(data1)[names(data1) %like% "Shap_"]
        if(length(YVar) == 1L) {
          yclass <- class(data1[[YVar]])[1L]
        } else {
          yclass <- "numeric"
        }

        # Conditions to proceed:
        #   1. MultiClass Model Eval Plots are handled in plot functions
        MultiClass_ModelEval_Check <- if((yclass %in% c("factor","character") && PlotType %in% c(
          'PartialDependenceLine','PartialDependenceBox','PartialDependenceHeatMap',
          'CalibrationLine','CalibrationBox','ShapleyImportance',
          'Residuals','ResidScatter','ResidualsCopulaPlot',
          'VariableImportance','ConfusionMatrixHeatmap',
          'GainsPlot','LiftPlot','ROCPlot')) || PlotType %in% c('Autocorrelation','PartialAutocorr')) TRUE else FALSE

        if(!MultiClass_ModelEval_Check) {# && !Stocks_Check) {
          Keep <- unique(c(YVar, DualYVar, XVar, ZVar, GroupVars, FilterVar1, FilterVar2, FilterVar3, FilterVar4))
          data1 <- data1[, ..Keep]
          fv1 <- length(FilterVar1) > 0L && length(FilterLogic1) > 0L
          fv2 <- length(FilterVar2) > 0L && length(FilterLogic2) > 0L
          fv3 <- length(FilterVar3) > 0L && length(FilterLogic3) > 0L
          fv4 <- length(FilterVar4) > 0L && length(FilterLogic4) > 0L
          gv1 <- fv1 && length(GroupVars) > 0L && length(Levels1) > 0L
          fv1a <- fv1 && (fv2 || gv1)
          fv2a <- fv2 && (fv3 || gv1)
          fv3a <- fv3 && (fv4 || gv1)

          # Plotting Code for GroupVar levels, Filter Variables, and Filter Values
          # CodeList <- DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
          CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
            "\n",
            "# Plotting Variables\n",
            "GroupVars <- ", DataMuse:::CEP(GroupVars), "\n",
            "FilterVar1 <- ", DataMuse:::CEP(FilterVar1), "\n",
            "FilterVar2 <- ", DataMuse:::CEP(FilterVar2), "\n",
            "FilterVar3 <- ", DataMuse:::CEP(FilterVar3), "\n",
            "FilterVar4 <- ", DataMuse:::CEP(FilterVar4), "\n",
            "FilterValue_1_1 <- ",
            if(!is.numeric(FilterValue_1_1)) {
              DataMuse:::CEP(FilterValue_1_1)
            } else {
              DataMuse:::ExpandText(FilterValue_1_1)
            },
            "\n",
            "FilterValue_1_2 <- ",
            if(!is.numeric(FilterValue_1_2)) {
              DataMuse:::CEP(FilterValue_1_2)
            } else {
              DataMuse:::ExpandText(FilterValue_1_2)
            },
            "\n",
            "FilterValue_2_1 <- ",
            if(!is.numeric(FilterValue_2_1)) {
              DataMuse:::CEP(FilterValue_2_1)
            } else {
              DataMuse:::ExpandText(FilterValue_2_1)
            },
            "\n",
            "FilterValue_2_2 <- ",
            if(!is.numeric(FilterValue_2_2)) {
              DataMuse:::CEP(FilterValue_2_2)
            } else {
              DataMuse:::ExpandText(FilterValue_2_2)
            },
            "\n",
            "FilterValue_3_1 <- ",
            if(!is.numeric(FilterValue_3_1)) {
              DataMuse:::CEP(FilterValue_3_1)
            } else {
              DataMuse:::ExpandText(FilterValue_3_1)
            },
            "\n",
            "FilterValue_3_2 <- ",
            if(!is.numeric(FilterValue_3_2)) {
              DataMuse:::CEP(FilterValue_3_2)
            } else {
              DataMuse:::ExpandText(FilterValue_3_2)
            },
            "\n",
            "FilterValue_4_1 <- ",
            if(!is.numeric(FilterValue_4_1)) {
              DataMuse:::CEP(FilterValue_4_1)
            } else {
              DataMuse:::ExpandText(FilterValue_4_1)
            },
            "\n",
            "FilterValue_4_2 <- ",
            if(!is.numeric(FilterValue_4_2)) {
              DataMuse:::CEP(FilterValue_4_2)
            } else {
              DataMuse:::ExpandText(FilterValue_4_2)
            },
            "\n",
            "Levels1 <- ", DataMuse:::ExpandText(Levels1), "\n",
            "Levels2 <- ", DataMuse:::ExpandText(Levels2), "\n",
            "Levels3 <- ", DataMuse:::ExpandText(Levels3), "\n")
          )}, error = function(x) CodeList)

          # FV1
          if(fv1 && FilterLogic1 %in% c('<','>','<=','>=','%in%','%like%')) {
            if(fv1a) {
              FV1 <- paste0("get(FilterVar1) ", FilterLogic1, " eval(FilterValue_1_1) & \n  ")
            } else {
              FV1 <- paste0("get(FilterVar1) ", FilterLogic1, " eval(FilterValue_1_1)\n  ")
            }
          } else if(fv1 && FilterLogic1 %in% '%between%') {
            if(fv1a) {
              FV1 <- paste0("get(FilterVar1) > eval(FilterValue_1_1) & get(FilterVar1) < eval(FilterValue_1_2) &\n  ")
            } else {
              FV1 <- paste0("get(FilterVar1) > eval(FilterValue_1_1) & get(FilterVar1) < eval(FilterValue_1_2)\n  ")
            }
          } else if(fv1 && FilterLogic1 %in% c('not %between%')) {
            if(fv1a) {
              FV1 <- paste0("get(FilterVar1) < eval(FilterValue_1_1) & get(FilterVar1) > eval(FilterValue_1_2) & \n  ")
            } else {
              FV1 <- paste0("get(FilterVar1) < eval(FilterValue_1_1) & get(FilterVar1) > eval(FilterValue_1_2)\n  ")
            }
          }

          # FV2
          if(fv2 && FilterLogic2 %in% c('<','>','<=','>=','%in%','%like%')) {
            if(fv2a) {
              FV2 <- paste0("get(FilterVar2) ", FilterLogic2, " eval(FilterValue_2_1) & \n  ")
            } else {
              FV2 <- paste0("get(FilterVar2) ", FilterLogic2, " eval(FilterValue_2_1)\n  ")
            }
          } else if(fv2 && FilterLogic2 %in% '%between%') {
            if(fv2a) {
              FV2 <- paste0("get(FilterVar2) > eval(FilterValue_2_1) & get(FilterVar2) < eval(FilterValue_2_2) & \n  ")
            } else {
              FV2 <- paste0("get(FilterVar2) > eval(FilterValue_2_1) & get(FilterVar2) < eval(FilterValue_2_2)\n  ")
            }
          } else if(fv2 && FilterLogic2 %in% c('not %between%')) {
            if(fv2a) {
              FV2 <- paste0("get(FilterVar2) < eval(FilterValue_2_1) & get(FilterVar2) > eval(FilterValue_2_2) & \n  ")
            } else {
              FV2 <- paste0("get(FilterVar2) < eval(FilterValue_2_1) & get(FilterVar2) > eval(FilterValue_2_2)\n  ")
            }
          }

          if(fv3 && FilterLogic3 %in% c('<','>','<=','>=','%in%','%like%')) {
            if(fv3a) {
              FV3 <- paste0("get(FilterVar3) ", FilterLogic3, " eval(FilterValue_3_1) & \n  ")
            } else {
              FV3 <- paste0("get(FilterVar3) ", FilterLogic3, " eval(FilterValue_3_1)\n  ")
            }

          } else if(fv3 && FilterLogic3 %in% '%between%') {
            if(fv3a) {
              FV3 <- paste0("get(FilterVar3) > eval(FilterValue_3_1) & get(FilterVar3) < eval(FilterValue_3_2) & \n  ")
            } else {
              FV3 <- paste0("get(FilterVar3) > eval(FilterValue_3_1) & get(FilterVar3) < eval(FilterValue_3_2)\n  ")
            }

          } else if(fv3 && FilterLogic3 %in% c('not %between%')) {
            if(fv3a) {
              FV3 <- paste0("get(FilterVar3) < eval(FilterValue_3_1) & get(FilterVar3) > eval(FilterValue_3_2) & \n  ")
            } else {
              FV3 <- paste0("get(FilterVar3) < eval(FilterValue_3_1) & get(FilterVar3) > eval(FilterValue_3_2)\n  ")
            }

          }

          if(fv4 && FilterLogic4 %in% c('<','>','<=','>=','%in%','%like%')) {
            if(length(GroupVars) > 0L && length(Levels1) > 0L) {
              FV4 <- paste0("get(FilterVar4) ", FilterLogic4, " eval(FilterValue_4_1) & \n  ")
            } else {
              FV4 <- paste0("get(FilterVar4) ", FilterLogic4, " eval(FilterValue_4_1)\n  ")
            }
          } else if(fv4 && FilterLogic4 %in% '%between%') {
            FV4 <- paste0("get(FilterVar4) > eval(FilterValue_4_1) & get(FilterVar4) < eval(FilterValue_4_2)\n  ")
          } else if(fv4 && FilterLogic4 %in% c('not %between%')) {
            FV4 <- paste0("get(FilterVar4) < eval(FilterValue_4_1) & get(FilterVar4) > eval(FilterValue_4_2)\n  ")
          }

          SDCOLS <- c()
          BYCOLS <- c()
          PlotAggs <- c('PiePlot','LinePlot','BarPlot','StackedBarPlot','StepPlot','AreaPlot','RiverPlot')
          AggCheck <- PlotType %in% PlotAggs
          if(PlotType %in% PlotAggs) ZVar <- NULL

          if(!AggCheck) {

            SDCOLS <- c(XVar,YVar,DualYVar,ZVar,GroupVars)
            BYCOLS <- NULL
            GroupVarSwitch <- FALSE
          } else {

            # If any variable in GroupVar is part of XVar, then remove it from GroupVar
            if(any(GroupVars %in% XVar)) {
              GroupVars <- GroupVars[!GroupVars %in% XVar]
              if(length(GroupVars) == 0L) GroupVars <- NULL
            }

            if(length(YVar) > 0L) Y_Class <- tryCatch({class(data1[[YVar[1L]]])[1L]}, error = function(x) "NULL") else Y_Class <- "NULL"
            if(length(XVar) > 0L) X_Class <- tryCatch({class(data1[[XVar[1L]]])[1L]}, error = function(x) "NULL") else X_Class <- "NULL"
            if(length(ZVar) > 0L) Z_Class <- tryCatch({class(data1[[ZVar[1L]]])[1L]}, error = function(x) "NULL") else Z_Class <- "NULL"

            if(Y_Class %in% c("numeric","integer")) {
              SDCOLS <- c(SDCOLS, YVar, DualYVar)
            } else if(Y_Class == "NULL") {
              1
            } else {
              BYCOLS <- c(BYCOLS, YVar, DualYVar)
            }

            if(X_Class %in% c("numeric","integer")) {
              SDCOLS <- c(SDCOLS, XVar)
            } else if(X_Class == "NULL") {
              1
            } else {
              BYCOLS <- c(BYCOLS, XVar)
            }

            if(Z_Class %in% c("numeric","integer")) {
              SDCOLS <- c(SDCOLS, ZVar)
            } else if(Z_Class == "NULL") {
              1
            } else {
              BYCOLS <- c(BYCOLS, ZVar)
            }

            if(length(GroupVars) == 1L) {
              BYCOLS <- c(BYCOLS, GroupVars)
              GroupVarSwitch <- FALSE
            } else if(length(GroupVars) > 1L) {
              GroupVars <- "GroupVariables"
              GroupVarSwitch <- TRUE
            } else {
              GroupVarSwitch <- FALSE
            }
          }

          # Data Prep
          DataPrepare <- paste0(
            "\n",
            "# Data Prep\n",
            "data1 <- data1[\n  ",
            if(fv1) FV1,
            if(fv2) FV2,
            if(fv3) FV3,
            if(fv4) FV4,
            if(length(GroupVars) > 0L && length(Levels1) > 0L) {
              "get(GroupVars[1L]) %chin% c(eval(Levels1))\n  "
            },
            if(length(GroupVars) > 0L && !is.na(GroupVars[2L]) && length(Levels2) > 0L) {
              "& get(GroupVars[2L]) %chin% c(eval(Levels2))\n  "
            },
            if(length(GroupVars) > 0L && !is.na(GroupVars[3L]) && length(Levels3) > 0L) {
              "& get(GroupVars[3L]) %chin% c(eval(Levels3)),\n  "
            }  else {
              ",\n  "
            },

            # c(YVar, ZVar)
            if((length(GroupVars) > 0L || length(XVar) > 0L) && AggCheck) {
              if(AggMethod == "mean") {
                paste0("lapply(.SD, mean, na.rm = TRUE),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ", DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod %in% c("count","proportion")) {
                paste0("list(Count = .N),\n  by = ", DataMuse:::ExpandText(BYCOLS), "\n\n]")
              } else if(AggMethod == "sum") {
                paste0("lapply(.SD, sum, na.rm = TRUE),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "median") {
                paste0("lapply(.SD, median, na.rm = TRUE),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "sd") {
                paste0("lapply(.SD, sd, na.rm = TRUE),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "skewness") {
                paste0("lapply(.SD, e1071::skewness, na.rm = TRUE),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "kurtosis") {
                paste0("lapply(.SD, e1071::kurtosis, na.rm = TRUE),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ", DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "CoeffVar") {
                paste0("lapply(.SD, FUN = function(x) {sd(x, na.rm=TRUE) / mean(x, na.rm = TRUE)}),\n  .SDcols = ",DataMuse:::ExpandText(SDCOLS),",\n  by = ", DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "sumabs") {
                paste0("lapply(.SD, FUN = function(x) sum(abs(x), na.rm=TRUE)),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "meanabs") {
                paste0("lapply(.SD, FUN = function(x) mean(abs(x), na.rm=TRUE)),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "medianabs") {
                paste0("lapply(.SD, FUN = function(x) median(abs(x), na.rm=TRUE)),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "sdabs") {
                paste0("lapply(.SD, FUN = function(x) sd(abs(x), na.rm=TRUE)),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "skewnessabs") {
                paste0("lapply(.SD, FUN = function(x) e1071::skewness(abs(x), na.rm=TRUE)),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "kurtosisabs") {
                paste0("lapply(.SD, FUN = function(x) e1071::kurtosis(abs(x), na.rm=TRUE)),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              } else if(AggMethod == "CoeffVarabs") {
                paste0("lapply(.SD, FUN = function(x) {sd(abs(x), na.rm=TRUE) / mean(abs(x), na.rm = TRUE)}),\n  .SDcols = ", DataMuse:::ExpandText(SDCOLS),",\n  by = ",DataMuse:::ExpandText(BYCOLS),"\n]")
              }

            } else {
              paste0(".SD, .SDcols = ", DataMuse:::ExpandText(SDCOLS),"\n]")
            }
          )

          if(AggMethod == "count") {
            YVar <- "Count"
            YAxisTitle <- "Counts"
          }

          if(Debug) print("Data Prepare Done")
          if(PlotType %in% PlotAggs) {
            PreAgg <- TRUE
          } else {
            PreAgg <- FALSE
          }

          # Group Variable Combine: Up to 3 group vars merged into 1; levels pasted together
          #   separated by " :: "
          # Run Code
          # print("Before Subset -------------------------------")
          # print(data1)
          # print(GroupVarSwitch)
          # print(length(Levels1))
          # print(length(Levels2))
          # print(length(Levels3))
          if(GroupVarSwitch) {
            eval(parse(text = DataPrepare))
            if(length(Levels1) == 0L && length(Levels2) == 0L && length(Levels3) == 0L) {
              data1[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVars)]
              levs <- as.character(data1[, .N, by = "GroupVariables"][order(-N)][seq_len(min(NumLevelsDisplay, .N))][['GroupVariables']])
              data1 <- data1[GroupVariables %chin% c(eval(levs))]
              data1[, (GroupVars) := NULL]
              GroupVars <- "GroupVariables"
            }
          } else if(length(GroupVars) == 1L) {
            eval(parse(text = DataPrepare))
            if(length(Levels1) == 0L && length(Levels2) == 0L && length(Levels3) == 0L) {
              levs <- as.character(data1[, .N, by = c(GroupVars)][order(-N)][seq_len(min(NumLevelsDisplay, .N))][[GroupVars]])
              data1 <- data1[get(GroupVars) %chin% c(eval(levs))]
            }
          } else {
            eval(parse(text = DataPrepare))
          }

          # print("After Subset -------------------------------")
          # print(data1)

          # Code Collect
          if(length(YVar) > 1L || length(YVar) == 0) yclass <- "numeric" else yclass <- class(data1[[YVar]])[1L]
          if(CollectCode) {
            CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, DataPrepare)}, error = function(x) CodeList)
          } else {
            CodeList[['Code']] <- CodeList[['Code']][seq_len(length(CodeList[['Code']])-1L)]
            CodeList[['TimeStamp']] <- CodeList[['TimeStamp']][seq_len(length(CodeList[['TimeStamp']])-1L)]
          }

          # Proportion Only
          if(AggMethod == "proportion") {
            data1[, Proportion := Count / sum(Count, na.rm = TRUE)][, Count := NULL]
            YVar <- "Proportion"
            YAxisTitle <- "Proportion"
            CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
              "\n\n# Convert Count to a Proportion\n",
              "data1[, Proportion := Count / sum(Count, na.rm = TRUE)][, Count := NULL]\n"
            ))}, error = function(x) CodeList)
          }
        }

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Create Plots                         ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Check to see if Plot already exists
        if(!GlobalChange && !UpdateData && length(PlotList[[paste0('Plot', PlotNumberss)]]) > 0L) {
          Build <- FALSE
        } else {
          Build <- TRUE
        }

        if(Debug) print(paste0("Build = ", Build))

        # Build or not to build
        if(Build) {

          # Build
          if(PlotType %chin% c('HistogramPlot',  'BarPlot',   'LinePlot',      'HeatMapPlot',
                               'BoxPlot',        'CopulaPlot','ScatterPlot3D', 'CopulaPlot3D',
                               'DensityPlot',    'AreaPlot',  'StepPlot',      'RiverPlot','',
                               'ViolinPlot',     'BarPlot3D', 'StackedBarPlot','CorrelogramPlot',
                               'PiePlot',        'BarPlotD',  'CopulaPlotD',   'ScatterPlot',
                               'ScatterPlotD',   'DonutPlot', 'RosetypePlot',  'PolarPlot')) {

            # Plot.StandardPlots()
            incre <- incre + 1L # starts at zero and initialized just above main for loop
            if(Debug) {
              print("Build = TRUE Plot.StandardPlots() run")
              print(PlotHeight[incre])
              print(PlotWidth[incre])
              print("Args for Plot.StandardPlots")
              print(paste0("AggMethod = ", AggMethod))
              print(paste0("SampleSize = ", SampleSize))
              print(paste0("PreAgg = ", PreAgg))
              print(paste0("YVar = ", YVar))
              print(paste0("DualYVar = ", DualYVar))
              print(paste0("XVar = ", XVar))
              print(paste0("ZVar = ", ZVar))
              print(paste0("GroupVar = ", GroupVars))
              print(paste0("YVarTrans = ", YVarTrans))
              print(paste0("DualYVarTrans = ", DualYVarTrans))
              print(paste0("XVarTrans = ", XVarTrans))
              print(paste0("ZVarTrans = ", ZVarTrans))
              print(paste0("FacetRows = ", FacetRows))
              print(paste0("FacetCols = ", FacetCols))
              print(paste0("FacetLevels = ", Levels1))
            }

            # Plot
            PlotList[[paste0('Plot', PlotNumberss)]] <- AutoPlots::Plot.StandardPlots(
              dt = data1,
              AggMethod = AggMethod,
              SampleSize = SampleSize,
              PreAgg = PreAgg,
              YVar=YVar, XVar=XVar,
              DualYVar = DualYVar,
              ZVar=ZVar,
              GroupVar=GroupVars,
              YVarTrans = YVarTrans,
              DualYVarTrans = DualYVarTrans,
              XVarTrans = XVarTrans,
              ZVarTrans = ZVarTrans,
              FacetRows = FacetRows,
              FacetCols = FacetCols,
              FacetLevels = Levels1,
              PlotType = PlotType,
              Title = Title,
              ShowLabels = ShowLabels,
              Title.YAxis = YAxisTitle,
              Title.XAxis = XAxisTitle,
              NumberBins = NumberBins,
              Height = PlotHeight[incre],
              Width = PlotWidth[incre],
              EchartsTheme = EchartsTheme,
              TimeLine = Timeline,
              TextColor = ColorFont,
              FontSize = 14,
              Debug = Debug)

            # Code Collection
            if(CollectCode) {
              CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
                "\n\n",
                "# Build Plot\n",
                "AutoPlots::Plot.StandardPlots(", "\n  ",
                "PlotType = ", DataMuse:::CEP(PlotType), ",\n  ",
                "dt = data1", ",\n  ",
                "AggMethod = ", DataMuse:::CEP(AggMethod), ",\n  ",
                "SampleSize = ", DataMuse:::CEPP(SampleSize), ",\n  ",
                "PreAgg = ", DataMuse:::CEPP(PreAgg), ",\n  ",
                "YVar = ", DataMuse:::ExpandText(YVar), ",\n  ",
                "DualYVar = ", DataMuse:::ExpandText(DualYVar), ",\n  ",
                "XVar = ", DataMuse:::ExpandText(XVar), ",\n  ",
                "ZVar = ", DataMuse:::ExpandText(ZVar), ",\n  ",
                "GroupVar = ", DataMuse:::ExpandText(GroupVars), ",\n  ",
                "YVarTrans = ", DataMuse:::CEP(YVarTrans), ",\n  ",
                "DualYVarTrans = ", DataMuse:::CEP(DualYVarTrans), ",\n  ",
                "XVarTrans = ", DataMuse:::CEP(XVarTrans), ",\n  ",
                "ZVarTrans = ", DataMuse:::CEP(ZVarTrans), ",\n  ",
                "FacetRows = ", DataMuse:::CEPP(FacetRows), ",\n  ",
                "FacetCols = ", DataMuse:::CEPP(FacetCols), ",\n  ",
                "FacetLevels = ", DataMuse:::ExpandText(Levels1), ",\n  ",
                "NumberBins = ", DataMuse:::CEP(NumberBins), ",\n  ",
                "Title = ", DataMuse:::CEP(Title), ",\n  ",
                "ShowLabels = ", DataMuse:::CEPP(ShowLabels), ",\n  ",
                "Title.YAxis = ", DataMuse:::CEP(YAxisTitle), ",\n  ",
                "Title.XAxis = ", DataMuse:::CEP(XAxisTitle), ",\n  ",
                "EchartsTheme = ", DataMuse:::CEP(EchartsTheme), ",\n  ",
                "TimeLine = ", DataMuse:::CEPP(Timeline), ",\n  ",
                "FontSize = 14,\n  ",
                "TextColor = ", DataMuse:::CEP(FontColor), ")\n  "))}, error = function(x) CodeList)
              CodeList <<- CodeList
            }

          } else if(PlotType == "WordCloud") {

            incre <- incre + 1L
            PlotList[[paste0('Plot', PlotNumberss)]] <- AutoPlots::Plot.WordCloud(
              dt = data1,
              YVar = YVar,
              Height = PlotHeight[incre],
              Width = PlotWidth[incre],
              Title = Title,
              EchartsTheme = EchartsTheme,
              TextColor = ColorFont,
              Debug = Debug)

            CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
              "\n\n",
              "# Build Plot\n",
              "AutoPlots::Plot.WordCloud(", "\n  ",
              "dt = data1,\n  ",
              "YVar = ", DataMuse:::ExpandText(YVar), ",\n  ",
              "Title = ", DataMuse:::CEP(Title), ",\n  ",
              "TextColor = ", DataMuse:::CEP(ColorFont), "\n  ",
              "EchartsTheme = ", DataMuse:::CEP(EchartsTheme), ")\n"))}, error = function(x) NULL)

          } else if(PlotType == "ProbabilityPlot") {

            incre <- incre + 1L
            PlotList[[paste0('Plot', PlotNumberss)]] <- AutoPlots::Plot.ProbabilityPlot(
              dt = data1,
              YVar = YVar,
              YVarTrans = YVarTrans,
              Height = PlotHeight[incre],
              Width = PlotWidth[incre],
              Title = Title,
              EchartsTheme = EchartsTheme,
              TextColor = ColorFont,
              Debug = Debug)

            CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
              "\n\n",
              "# Build Plot\n",
              "AutoPlots::Plot.ProbabilityPlot(", "\n  ",
              "dt = data1,\n  ",
              "YVar = ", DataMuse:::ExpandText(YVar), ",\n  ",
              "Title = ", DataMuse:::CEP(Title), ",\n  ",
              "TextColor = ", DataMuse:::CEP(ColorFont), "\n  ",
              "EchartsTheme = ", DataMuse:::CEP(EchartsTheme), ")\n"))}, error = function(x) NULL)

          } else if(PlotType == "ParallelPlot") {

            incre <- incre + 1L

            PlotList[[paste0('Plot', PlotNumberss)]] <- AutoPlots::Plot.Parallel(
              dt = data1,
              SampleSize = SampleSize,
              CorrVars = YVar,
              Height = PlotHeight[incre],
              Width = PlotWidth[incre],
              Title = Title,
              EchartsTheme = EchartsTheme,
              TextColor = ColorFont,
              Debug = Debug)

            CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
              "\n\n",
              "# Build Plot\n",
              "AutoPlots::Plot.Parallel(", "\n  ",
              "dt = data1,\n  ",
              "SampleSize = ", DataMuse:::CEPP(SampleSize), ",\n  ",
              "CorrVars = ", DataMuse:::ExpandText(YVar), ",\n  ",
              "Title = ", DataMuse:::CEP(Title), ",\n  ",
              "TextColor = ", DataMuse:::CEP(ColorFont), "\n  ",
              "EchartsTheme = ", DataMuse:::CEP(EchartsTheme), ")\n"))}, error = function(x) NULL)

          } else if(PlotType == "RadarPlot") {

            incre <- incre + 1L

            PlotList[[paste0('Plot', PlotNumberss)]] <- AutoPlots::Plot.Radar(
              dt = data1,
              PreAgg = PreAgg,
              AggMethod = AggMethod,
              YVar = YVar,
              YVarTrans = YVarTrans,
              GroupVar = GroupVars,
              Height = PlotHeight[incre],
              Width = PlotWidth[incre],
              Title = Title,
              EchartsTheme = EchartsTheme,
              TextColor = ColorFont,
              Debug = Debug)

            CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
              "\n\n",
              "# Build Plot\n",
              "AutoPlots::Plot.Radar(", "\n  ",
              "dt = data1,\n  ",
              "PreAgg = ", DataMuse:::CEPP(PreAgg), ",\n  ",
              "AggMethod = ", DataMuse:::CEP(AggMethod), ",\n  ",
              "YVar = ", DataMuse:::ExpandText(YVar), ",\n  ",
              "YVarTrans = ", DataMuse:::CEP(YVarTrans), ",\n  ",
              "GroupVar = ", DataMuse:::ExpandText(GroupVars), ",\n  ",
              "Title = ", DataMuse:::CEP(Title), ",\n  ",
              "TextColor = ", DataMuse:::CEP(ColorFont), "\n  ",
              "EchartsTheme = ", DataMuse:::CEP(EchartsTheme), ")\n"))}, error = function(x) NULL)

          } else if(PlotType == 'Autocorrelation') {

            incre <- incre + 1L

            if(Debug) {
              print(paste0("YVar = ", YVar))
              print(paste0("DateVar = ", DateVariable))
              print(paste0("TimeUnit = ", TimeUnitVariable))
              print(paste0("MaxLags = ", MaxLagsVariable))
              print(paste0("YVarTrans = ", YVarTrans))
              print(paste0("AggMethod = ", "sum"))
              print(paste0("Height = ", PlotHeight[incre]))
              print(paste0("Width = ", PlotWidth[incre]))
              print(paste0("Title = ", "Autocorrelation Plot"))
              print(paste0("EchartsTheme = ", EchartsTheme))
              print(paste0("TextColor = ", ColorFont))
              print(paste0("Debug = ", Debug))
            }

            PlotList[[paste0('Plot', PlotNumberss)]] <- AutoPlots::Plot.ACF(
              dt = data1,
              YVar = YVar,
              DateVar = DateVariable,
              TimeUnit = TimeUnitVariable,
              MaxLags = MaxLagsVariable,
              YVarTrans = YVarTrans,
              AggMethod = "sum",
              Height = PlotHeight[incre],
              Width = PlotWidth[incre],
              Title = Title,
              EchartsTheme = EchartsTheme,
              TextColor = ColorFont,
              Debug = Debug)

            CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
              "\n\n",
              "# Build Plot\n",
              "AutoPlots::Plot.ACF(", "\n  ",
              "dt = data1,\n  ",
              "YVar = ", DataMuse:::ExpandText(YVar), ",\n  ",
              "DateVar = ", DataMuse:::CEP(DateVariable), ",\n  ",
              "TimeUnit = ", DataMuse:::CEP(TimeUnitVariable), ",\n  ",
              "MaxLags = ", DataMuse:::CEPP(MaxLagsVariable), ",\n  ",
              "YVarTrans = ", DataMuse:::CEP(YVarTrans), ",\n  ",
              "AggMethod = 'sum',\n  ",
              "Title = 'Autocorrelation Plot',\n  ",
              "TextColor = ", DataMuse:::CEP(ColorFont), "\n  ",
              "EchartsTheme = ", DataMuse:::CEP(EchartsTheme), ")\n"))}, error = function(x) NULL)

          } else if(PlotType == 'PartialAutocorr') {

              incre <- incre + 1L

              if(Debug) {
                print(paste0("YVar = ", YVar))
                print(paste0("DateVar = ", DateVariable))
                print(paste0("TimeUnit = ", TimeUnitVariable))
                print(paste0("MaxLags = ", MaxLagsVariable))
                print(paste0("YVarTrans = ", YVarTrans))
                print(paste0("AggMethod = ", "sum"))
                print(paste0("Height = ", PlotHeight[incre]))
                print(paste0("Width = ", PlotWidth[incre]))
                print(paste0("Title = ", "Partial Autocorrelation Plot"))
                print(paste0("EchartsTheme = ", EchartsTheme))
                print(paste0("TextColor = ", ColorFont))
                print(paste0("Debug = ", Debug))
              }

              PlotList[[paste0('Plot', PlotNumberss)]] <- AutoPlots::Plot.PACF(
                dt = data1,
                YVar = YVar,
                DateVar = DateVariable,
                TimeUnit = TimeUnitVariable,
                MaxLags = MaxLagsVariable,
                YVarTrans = YVarTrans,
                AggMethod = "sum",
                Height = PlotHeight[incre],
                Width = PlotWidth[incre],
                Title = Title,
                EchartsTheme = EchartsTheme,
                TextColor = ColorFont,
                Debug = Debug)

              CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
                "\n\n",
                "# Build Plot\n",
                "AutoPlots::Plot.PACF(", "\n  ",
                "dt = data1,\n  ",
                "YVar = ", DataMuse:::ExpandText(YVar), ",\n  ",
                "DateVar = ", DataMuse:::CEP(DateVariable), ",\n  ",
                "TimeUnit = ", DataMuse:::CEP(TimeUnitVariable), ",\n  ",
                "MaxLags = ", DataMuse:::CEPP(MaxLagsVariable), ",\n  ",
                "YVarTrans = ", DataMuse:::CEP(YVarTrans), ",\n  ",
                "AggMethod = 'sum',\n  ",
                "Title = 'Partial Autocorrelation Plot',\n  ",
                "TextColor = ", DataMuse:::CEP(ColorFont), "\n  ",
                "EchartsTheme = ", DataMuse:::CEP(EchartsTheme), ")\n"))}, error = function(x) NULL)


          } else {

            incre <- incre + 1L

            # Build plot
            if(Debug) {
              print(PlotType)
              print(AggMethod)
              print(SampleSize)
              print(YVar)
              print(ZVar)
              print(XVar)
              print(GroupVars)
              print(FacetRows)
              print(FacetCols)
              print(Levels1)
              print(NumberBins)
              print(EchartsTheme)
              print(Timeline)
              print(FontColor)
            }
            PlotList[[paste0('Plot', PlotNumberss)]] <- AutoPlots::Plots.ModelEvaluation(
              PlotType = PlotType,
              dt = data1,
              AggMethod = AggMethod,
              SampleSize = SampleSize,
              YVar = YVar,
              ZVar = ZVar,
              XVar = XVar,
              GroupVar = GroupVars,
              YVarTrans = YVarTrans,
              XVarTrans = XVarTrans,
              ZVarTrans = ZVarTrans,
              Title = Title,
              FacetRows = FacetRows,
              FacetCols = FacetCols,
              FacetLevels = Levels1,
              Height = PlotHeight[incre],
              Width = PlotWidth[incre],
              ShowLabels = ShowLabels,
              Title.YAxis = NULL,
              Title.XAxis = XAxisTitle,
              NumberBins = NumberBins,
              NumLevels_X = NumberBins,
              NumLevels_Y = NumberBins,
              EchartsTheme = EchartsTheme,
              TimeLine = Timeline,
              TextColor = FontColor,
              Debug = Debug)

            # Code Collection
            if(CollectCode) {
              CodeList <- tryCatch({DataMuse:::Shiny.CodePrint.Collect(CodeList, paste0(
                "\n\n",
                "# Build Plot\n",
                "AutoPlots::Plots.ModelEvaluation(\n  ",
                "dt = data1,\n  ",
                "EchartsTheme = ", DataMuse:::CEP(EchartsTheme), ",\n  ",
                "TimeLine = ", DataMuse:::CEPP(Timeline), ",\n  ",
                "PlotType = ", DataMuse:::CEP(PlotType), ",\n  ",
                "SampleSize = ", DataMuse:::CEPP(SampleSize), ",\n  ",
                "YVar = ", DataMuse:::ExpandText(YVar), ",\n  ",
                "ZVar = ", DataMuse:::ExpandText(ZVar), ",\n  ",
                "XVar = ", DataMuse:::ExpandText(XVar), ",\n  ",
                "GroupVar = ", DataMuse:::ExpandText(GroupVars), ",\n  ",
                "YVarTrans = ", DataMuse:::CEP(YVarTrans), ",\n  ",
                "XVarTrans = ", DataMuse:::CEP(XVarTrans), ",\n  ",
                "ZVarTrans = ", DataMuse:::CEP(ZVarTrans), ",\n  ",
                "FacetRows = ", DataMuse:::CEPP(FacetRows), ",\n  ",
                "FacetCols = ", DataMuse:::CEPP(FacetCols), ",\n  ",
                "NumberBins = ", DataMuse:::CEP(NumberBins), ",\n  ",
                "TextColor = ", DataMuse:::CEP(FontColor), ")\n"))}, error = function(x) CodeList)
              CodeList <<- CodeList
            }
          }
        }
      }
    }

    # Return Lists
    return(list(
      PlotList = PlotList,
      CodeList = CodeList,
      SubsetList = SubsetList
    ))
  } else {
    if(!length(tempPlotType) > 0L) {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Inputs not available", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
    return(list(
      PlotList = PlotList,
      CodeList = CodeList,
      SubsetList = SubsetList
    ))
  }
}

#' @title RenderOutput
#'
#' @description Display multiple plots in single output
#'
#' @family helpers
#' @author Adrian Antico
#'
#' @param ... Plots
#' @param NumCols Number of columns
#' @param Title character
#' @param BoxNum number
#' @param FontColor white
#' @param Debug logic
#'
#' @noRd
RenderOutput <- function(...,
                         NumCols = 1,
                         Title = NULL,
                         BoxNum = 1,
                         FontColor = "white",
                         Debug = FALSE) {

  pl <- as.list(...)
  if(length(NumCols) == 0L) NumCols <- 1
  if(NumCols <= 1) {
    rows <- length(pl)
    if(NumCols < 1) NumCols <- 1
  } else {
    rows <- ceiling(length(pl) / NumCols)
  }
  w <- "-xs"
  if(!isTRUE(getOption("knitr.in.progress"))) {
    w <- ""
  }
  x <- 0

  # Grid Output Columns:
  #  if cols == 1, build individual boxes, textAreaInputs, and single plot outputs and
  #    then stack them on top of each other, via tagAppendChild()
  #  if cols > 1, then shrink columns, plot sizes are already shrunk inside plot functions
  #  if cols == 5 and at least 5 plots still need to be displayed, add a column
  #     to start of width 1 so that the rest can be filled with witdth = 2, with 1
  #     column remaining
  #  if remainder > 0, fill in the row using grid cols == remainder to size correctly
  if(NumCols == 1L) {
    r <- htmltools::div(class = "row", style="padding-right: 0px;")
    l <- shiny::tagList(
      DataMuse:::cust_box(
        id = paste0("cust_box", BoxNum),
        title = Title,
        solidHeader = TRUE,
        collapsible = TRUE,
        status = 'warning',
        width = 12L,
        style = "text-align: left; border; padding-left: 7px; padding-right: 15px",
        TitleColor = FontColor,
        shiny::fluidRow(
          style = "padding-bottom: 10px",
          shiny::column(
            12L,
            align = "left",
            shiny::tags$head(shiny::tags$style(shiny::HTML(paste0("#textArea text-align: left;"))))
          )
        ),
        shiny::fluidRow(
          style = "padding-bottom: 20px;",
          shiny::column(
            12L,
            align = "left",
            style = "padding-bottom: 25px;",
            pl))
      ),
      DataMuse:::BlankRow(12L)
    )
    r <- htmltools::tagAppendChild(r, shiny::tagList(l))
    return(htmltools::browsable(r))

  } else if(NumCols > 1L) {
    tgList <- list()
    for(i in seq_len(rows)) {
      remaining_length <- length(names(pl)[(x+1):length(names(pl))])
      if(NumCols == 5 && remaining_length >= 5) {
        tg <- shiny::tagList(
          DataMuse:::BlankRow(12L), # child 1
          DataMuse:::cust_box(      # child 2
            id = paste0("cust_box", i),
            title = "Grouped Output",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = 'warning',
            width = NULL,
            style = "text-align: left; border; padding-left: 7px; padding-right: 15px",
            TitleColor = FontColor,
            shiny::fluidRow( # child 1
              style = "padding-bottom: 10px;",
              shiny::column(
                12L,
                align = "left",
                shiny::tags$head(shiny::tags$style(shiny::HTML(paste0("#textArea text-align: left; width: 100%;"))))
              )
            ),
            shiny::fluidRow(shiny::column(1))
          )
        ) # child 2

      } else if(i == 1L) {
        tg <- shiny::tagList(
          DataMuse:::cust_box(
            id = paste0("cust_box", i),
            title = "Grouped Output",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = 'warning',
            width = NULL,
            style = "text-align: left; border; padding-left: 7px; padding-right: 15px",
            TitleColor = FontColor,
            shiny::fluidRow(
              style = "padding-bottom: 10px;",
              shiny::column(
                12L,
                align = "left",
                shiny::tags$head(shiny::tags$style(shiny::HTML(paste0("#textArea text-align: left; width: 100%;"))))
              )
            ),
            shiny::fluidRow()
          )
        )

      } else {
        tg <- shiny::tagList(
          DataMuse:::BlankRow(12L), # child 1
          DataMuse:::cust_box(      # child 2
            id = paste0("cust_box", i),
            title = "Grouped Output",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = 'warning',
            width = NULL,
            style = "text-align: left; border; padding-left: 7px; padding-right: 15px",
            TitleColor = FontColor,
            shiny::fluidRow( # child 1
              style = "padding-bottom: 10px;",
              shiny::column(
                12L,
                align = "left",
                shiny::tags$head(shiny::tags$style(shiny::HTML(paste0("#textArea text-align: left; width: 100%;"))))
              )
            ),
            shiny::fluidRow())) # child 2
      }
      g <- list()
      for(j in seq_len(min(NumCols, remaining_length))) { # j = 1
        x <- x + 1L
        if(x <= length(pl)) {
          g[[j]] <- shiny::column(
            floor(12/min(NumCols, remaining_length)),
            align = "left",
            style = "padding-bottom: 10px;",
            pl[[x]])
        }
      }
      if(i == 1 && !(NumCols == 5 && remaining_length >= 5)) {
        tg[[1L]][["children"]][[1L]][["children"]][[2L]][["children"]][[2L]] <- htmltools::tagAppendChild(tag = tg[[1L]][["children"]][[1L]][["children"]][[2L]][["children"]][[2L]], child = g)
      } else {
        tg[[2L]][["children"]][[1L]][["children"]][[2L]][["children"]][[2L]] <- htmltools::tagAppendChild(tag = tg[[2L]][["children"]][[1L]][["children"]][[2L]][["children"]][[2L]], child = g)
      }
      tgList[[i]] <- tg
    }
    htmltools::browsable(shiny::tagList(tgList))
  }
}

#' @title Shiny.Display
#'
#' @description Output display of plots for shiny apps.
#'
#' @details
#' Output as many plots as you desire. Mix and match plotting backends. Alter sizing, spacing, and styling.
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param input From App
#' @param output From App
#' @param session From App
#' @param PlotList Named list \code{list("Plot 1" = "Plot 1", "Updated Name" = "Updated Name", ...)} containing names of plot button objects
#' @param Debug  From App
#' @param OutputId From App
#' @param Cols Multiplot output number of columns. Defaults to 1
#' @param FontColor white
#' @param PM = NULL
#'
#' @keywords internal
Shiny.Display <- function(input,
                          output,
                          session,
                          PlotList,
                          Debug,
                          OutputId,
                          Cols = 1,
                          FontColor = "white",
                          PM = NULL) {

  # Return NULL if empty PlotList
  if(length(PlotList) == 0L) return(NULL)

  # Build plist
  plist <- list()
  if(Cols == 1) {
    for(p in seq_along(PlotList)) {# p = 1
      if(Debug) print(p)
      plist[[p]] <- DataMuse:::RenderOutput(
        PlotList[[p]],
        NumCols = 1,
        Title = if(length(PM) == 0L) "Grid Output" else PM[p],
        BoxNum = p,
        FontColor = FontColor,
        Debug = Debug)
    }
  } else {
    plist <- DataMuse:::RenderOutput(
      PlotList,
      NumCols = Cols,
      Title = PM,
      BoxNum = 1,
      FontColor = FontColor,
      Debug = Debug)
  }

  # Return
  output[[OutputId]] <- shiny::renderUI({
    eval(plist)
  })
}

#' @noRd
cust_box <- function(...,
                     title = NULL,
                     footer = NULL,
                     status = NULL,
                     solidHeader = FALSE,
                     background = NULL,
                     width = 6,
                     height = NULL,
                     collapsible = FALSE,
                     TitleColor = "white",
                     collapsed = FALSE) {

  boxClass <- "box"
  if(solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if(!is.null(status)) {
    shinydashboard:::validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if(collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if(!is.null(background)) {
    shinydashboard:::validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if(!is.null(height)) {
    style <- paste0(
      "height: ", htmltools::validateCssUnit(height))
  }
  titleTag <- NULL
  if(!is.null(title)) {
    titleTag <- shiny::h3(
      class = "box-title",
      style = paste0("color: ", TitleColor, ";"),
      title)
  }
  collapseTag <- NULL
  if(collapsible) {
    buttonStatus <- if(length(status) > 0L) status else "default"
    collapseIcon <- if(collapsed) "plus" else "minus"
    collapseTag <- shiny::div(
      class = "box-tools pull-right",
      shiny::tags$button(
        class = paste0("btn btn-box-tool"),
        `data-widget` = "collapse",
        shiny::icon(collapseIcon)))
  }
  headerTag <- NULL
  if(!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- shiny::div(
      class = "box-header",
      titleTag,
      collapseTag)
  }
  shiny::div(
    class = if(!is.null(width)) paste0("col-sm-", width),
    shiny::div(
      class = boxClass,
      style = if(!is.null(style)) style,
      headerTag,
      shiny::div(class = "box-body", ...),
      if(!is.null(footer)) shiny::div(class = "box-footer", footer)
    )
  )
}

#' @title BlankEchart
#'
#' @param Type "Poincare", "Divergence", "Lips", "Tenet", "Space"
#' @param Theme Echarts
#'
#' @noRd
BlankEchart <- function(Type = "Poincare", Theme = "macarons") {

  if(grepl(pattern = "Poincare", x = Type)) {
    poin <- ceiling(runif(1)*9)
    Type <- paste0("poincare", poin)
  }

  # # Echarts Example from Website
  if(Type == "Divergence") {

    # data.frame with x, y as columns. Cross join
    vals <- expand.grid(
      x = seq(-1,1,0.249),
      y = seq(-1,1,0.249))

    mu <- runif(1)/5
    vals$sx <- vals$y
    vals$sy <-  -1 * (runif(1)+0.5) * (mu * (1 - vals$x^2) * vals$y - vals$x)
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    valss <- data.table::setorderv(x = data.table::copy(vals), cols = names(vals)[2], -1)
    vals <- cbind(vals, x = data.table::setnames(valss, names(valss), paste0("sss", names(valss))))

    vals[[ncol(vals)]] <- NULL
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(
        y,
        sx,
        sy,
        color,
        particleSize = ceiling(runif(1L) * 20),
        particleDensity = ceiling(runif(1))*60,
        itemStyle = list(
          opacity = 0.87
        )) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.8,
        dimension = 8,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(
          color = c("white","blue","#8e9cab")
        )) |>
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "Lips") {

    # data.frame with x, y as columns. Cross join
    vals <- expand.grid(
      x = seq(-1,1,0.249),
      y = seq(-1,1,0.249))

    mu <- 15
    vals$sx <- vals$y
    vals$sy <-  -1 * (mu * (1 - vals$x^1) * vals$y - vals$x)
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(
        y,
        sx,
        sy,
        color,
        particleSize = ceiling(runif(1L) * 20),
        particleDensity = ceiling(runif(1))*60,
        itemStyle = list(
          opacity = 0.87
        )) |>
      echarts4r::e_visual_map(
        min = 0, max = 10,
        dimension = 4,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("black","darkred","red","#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "Space") {

    # data.frame with x, y as columns. Cross join
    vals <- expand.grid(
      x = seq(-1,1,0.249),
      y = seq(-1,1,0.249))

    mu <- 25
    vals$sx <- vals$y
    vals$sy <-  1 / (mu * (1 - vals$x^2) / vals$y^2 - vals$x^1)

    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(
        y,
        sx,
        sy,
        color,
        particleSize = ceiling(runif(1L) * 20),
        particleDensity = ceiling(runif(1))*60,
        itemStyle = list(
          opacity = 0.87
        )) |>
      echarts4r::e_visual_map(
        min = 0, max = 10,
        dimension = 4,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("darkblue","white","black","#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "Tenet") {

    # data.frame with x, y as columns. Cross join
    vals <- expand.grid(
      x = seq(-1,1,0.249),
      y = seq(-1,1,0.249))

    mu <- 0.0
    vals$sx <- vals$y
    vals$sy <-  -1 / (15000+mu * (1 - vals$x^2) / vals$y^2 - vals$x^1)
    vals$color <- runif(nrow(vals), 1, 10)/10
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 1.3,
        dimension = 0.5,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("black","#8e9cab","white"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "poincare1") {

    # Hyperbolica example
    library(gyro)         # to use the `changesOfSign` function
    library(arrangements) # to use the `permutations` function
    x <- c(
      (1 + 2*sqrt(2)) / 2,
      (1 + sqrt(2)) / 2,
      1/2)
    vertices <- changesOfSign(
      cbind(
        t(apply(permutations(3L), 1L, function(perm) x[perm])),
        1/2
      ))

    R <- sqrt(c(crossprod(vertices[1L, ])))
    library(cxhull)
    hull <- cxhull(vertices)
    edges <- hull[["edges"]]
    cells <- hull[["facets"]]
    ridges <- hull[["ridges"]]
    cubicCells <- Filter(function(cell) length(cell[["vertices"]]) == 8L, cells)
    polygonize <- function(edges){
      nedges <- nrow(edges)
      indices <- edges[1L, ]
      i <- indices[2L]
      edges <- edges[-1L, ]
      for(. in 1L:(nedges-2L)){
        j <- which(apply(edges, 1L, function(e) i %in% e))
        i <- edges[j, ][which(edges[j, ] != i)]
        indices <- c(indices, i)
        edges <- edges[-j, ]
      }
      indices
    }
    squares <- t(vapply(
      do.call(c, lapply(cubicCells, `[[`, "ridges")),
      function(r) polygonize(ridges[[r]][["edges"]]),
      integer(4L)
    ))
    verts3D <- t(apply(vertices, 1L, function(v){
      v[1L:3L] / (R - v[4L])
    }))
    vals <- data.table::as.data.table(cbind(verts3D, verts3D[2]))
    data.table::setnames(vals, names(vals), c("x","y","sx","sy"))
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.0,
        dimension = 2,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "poincare2") {

    # Hyperbolica example
    library(gyro)         # to use the `changesOfSign` function
    library(arrangements) # to use the `permutations` function
    x <- c(
      (1 + 3*sqrt(2)) / 2,
      (1 + sqrt(2)) / 2,
      1/2)
    vertices <- changesOfSign(
      cbind(
        t(apply(permutations(3L), 1L, function(perm) x[perm])),
        1/2
      ))

    R <- sqrt(c(crossprod(vertices[1L, ])))
    library(cxhull)
    hull <- cxhull(vertices)
    edges <- hull[["edges"]]
    cells <- hull[["facets"]]
    ridges <- hull[["ridges"]]
    cubicCells <- Filter(function(cell) length(cell[["vertices"]]) == 8L, cells)
    polygonize <- function(edges){
      nedges <- nrow(edges)
      indices <- edges[1L, ]
      i <- indices[2L]
      edges <- edges[-1L, ]
      for(. in 1L:(nedges-2L)){
        j <- which(apply(edges, 1L, function(e) i %in% e))
        i <- edges[j, ][which(edges[j, ] != i)]
        indices <- c(indices, i)
        edges <- edges[-j, ]
      }
      indices
    }
    squares <- t(vapply(
      do.call(c, lapply(cubicCells, `[[`, "ridges")),
      function(r) polygonize(ridges[[r]][["edges"]]),
      integer(4L)
    ))
    verts3D <- t(apply(vertices, 1L, function(v){
      v[1L:3L] / (R - v[4L])
    }))
    vals <- data.table::as.data.table(cbind(verts3D, verts3D[2]))
    data.table::setnames(vals, names(vals), c("x","y","sx","sy"))
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.0,
        dimension = 2,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "poincare3") {

    # Hyperbolica example
    library(gyro)         # to use the `changesOfSign` function
    library(arrangements) # to use the `permutations` function
    x <- c(
      (1 + 2*sqrt(3)) / 2,
      (1 + sqrt(2)) / 2,
      1/2)
    vertices <- changesOfSign(
      cbind(
        t(apply(permutations(3L), 1L, function(perm) x[perm])),
        1/2
      ))

    R <- sqrt(c(crossprod(vertices[1L, ])))
    library(cxhull)
    hull <- cxhull(vertices)
    edges <- hull[["edges"]]
    cells <- hull[["facets"]]
    ridges <- hull[["ridges"]]
    cubicCells <- Filter(function(cell) length(cell[["vertices"]]) == 8L, cells)
    polygonize <- function(edges){
      nedges <- nrow(edges)
      indices <- edges[1L, ]
      i <- indices[2L]
      edges <- edges[-1L, ]
      for(. in 1L:(nedges-2L)){
        j <- which(apply(edges, 1L, function(e) i %in% e))
        i <- edges[j, ][which(edges[j, ] != i)]
        indices <- c(indices, i)
        edges <- edges[-j, ]
      }
      indices
    }
    squares <- t(vapply(
      do.call(c, lapply(cubicCells, `[[`, "ridges")),
      function(r) polygonize(ridges[[r]][["edges"]]),
      integer(4L)
    ))
    verts3D <- t(apply(vertices, 1L, function(v){
      v[1L:3L] / (R - v[4L])
    }))
    vals <- data.table::as.data.table(cbind(verts3D, verts3D[2]))
    data.table::setnames(vals, names(vals), c("x","y","sx","sy"))
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.0,
        dimension = 2,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "poincare4") {

    # Hyperbolica example
    library(gyro)         # to use the `changesOfSign` function
    library(arrangements) # to use the `permutations` function
    x <- c(
      (1 + 2*sqrt(2)) / 2,
      (1 + sqrt(3)) / 7,
      1/2)
    vertices <- changesOfSign(
      cbind(
        t(apply(permutations(3L), 1L, function(perm) x[perm])),
        1/2
      ))

    R <- sqrt(c(crossprod(vertices[1L, ])))
    library(cxhull)
    hull <- cxhull(vertices)
    edges <- hull[["edges"]]
    cells <- hull[["facets"]]
    ridges <- hull[["ridges"]]
    cubicCells <- Filter(function(cell) length(cell[["vertices"]]) == 8L, cells)
    polygonize <- function(edges){
      nedges <- nrow(edges)
      indices <- edges[1L, ]
      i <- indices[2L]
      edges <- edges[-1L, ]
      for(. in 1L:(nedges-2L)){
        j <- which(apply(edges, 1L, function(e) i %in% e))
        i <- edges[j, ][which(edges[j, ] != i)]
        indices <- c(indices, i)
        edges <- edges[-j, ]
      }
      indices
    }
    squares <- t(vapply(
      do.call(c, lapply(cubicCells, `[[`, "ridges")),
      function(r) polygonize(ridges[[r]][["edges"]]),
      integer(4L)
    ))
    verts3D <- t(apply(vertices, 1L, function(v){
      v[1L:3L] / (R - v[4L])
    }))
    vals <- data.table::as.data.table(cbind(verts3D, verts3D[2]))
    data.table::setnames(vals, names(vals), c("x","y","sx","sy"))
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.0,
        dimension = 2,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "poincare5") {

    # Hyperbolica example
    library(gyro)         # to use the `changesOfSign` function
    library(arrangements) # to use the `permutations` function
    x <- c(
      (1 + 2*sqrt(2)) / 2,
      (1 + sqrt(3)) / 4,
      1/2)
    vertices <- changesOfSign(
      cbind(
        t(apply(permutations(3L), 1L, function(perm) x[perm])),
        1/2
      ))

    R <- sqrt(c(crossprod(vertices[1L, ])))
    library(cxhull)
    hull <- cxhull(vertices)
    edges <- hull[["edges"]]
    cells <- hull[["facets"]]
    ridges <- hull[["ridges"]]
    cubicCells <- Filter(function(cell) length(cell[["vertices"]]) == 8L, cells)
    polygonize <- function(edges){
      nedges <- nrow(edges)
      indices <- edges[1L, ]
      i <- indices[2L]
      edges <- edges[-1L, ]
      for(. in 1L:(nedges-2L)){
        j <- which(apply(edges, 1L, function(e) i %in% e))
        i <- edges[j, ][which(edges[j, ] != i)]
        indices <- c(indices, i)
        edges <- edges[-j, ]
      }
      indices
    }
    squares <- t(vapply(
      do.call(c, lapply(cubicCells, `[[`, "ridges")),
      function(r) polygonize(ridges[[r]][["edges"]]),
      integer(4L)
    ))
    verts3D <- t(apply(vertices, 1L, function(v){
      v[1L:3L] / (R - v[4L])
    }))
    vals <- data.table::as.data.table(cbind(verts3D, verts3D[2]))
    data.table::setnames(vals, names(vals), c("x","y","sx","sy"))
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.0,
        dimension = 2,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "poincare6") {

    # Hyperbolica example
    library(gyro)         # to use the `changesOfSign` function
    library(arrangements) # to use the `permutations` function
    x <- c(
      (1 + 2*sqrt(2)) / 2,
      (1 + sqrt(3)) / 12,
      1/0.2)
    vertices <- changesOfSign(
      cbind(
        t(apply(permutations(3L), 1L, function(perm) x[perm])),
        1/2
      ))

    R <- sqrt(c(crossprod(vertices[1L, ])))
    library(cxhull)
    hull <- cxhull(vertices)
    edges <- hull[["edges"]]
    cells <- hull[["facets"]]
    ridges <- hull[["ridges"]]
    cubicCells <- Filter(function(cell) length(cell[["vertices"]]) == 8L, cells)
    polygonize <- function(edges){
      nedges <- nrow(edges)
      indices <- edges[1L, ]
      i <- indices[2L]
      edges <- edges[-1L, ]
      for(. in 1L:(nedges-2L)){
        j <- which(apply(edges, 1L, function(e) i %in% e))
        i <- edges[j, ][which(edges[j, ] != i)]
        indices <- c(indices, i)
        edges <- edges[-j, ]
      }
      indices
    }
    squares <- t(vapply(
      do.call(c, lapply(cubicCells, `[[`, "ridges")),
      function(r) polygonize(ridges[[r]][["edges"]]),
      integer(4L)
    ))
    verts3D <- t(apply(vertices, 1L, function(v){
      v[1L:3L] / (R - v[4L])
    }))
    vals <- data.table::as.data.table(cbind(verts3D, verts3D[2]))
    data.table::setnames(vals, names(vals), c("x","y","sx","sy"))
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.0,
        dimension = 2,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "poincare7") {

    # Hyperbolica example
    library(gyro)         # to use the `changesOfSign` function
    library(arrangements) # to use the `permutations` function
    x <- c(
      (1 + 2*sqrt(2)) / 2,
      (1 + sqrt(3)) / 12,
      1.61)
    vertices <- changesOfSign(
      cbind(
        t(apply(permutations(3L), 1L, function(perm) x[perm])),
        1/2
      ))

    R <- sqrt(c(crossprod(vertices[1L, ])))
    library(cxhull)
    hull <- cxhull(vertices)
    edges <- hull[["edges"]]
    cells <- hull[["facets"]]
    ridges <- hull[["ridges"]]
    cubicCells <- Filter(function(cell) length(cell[["vertices"]]) == 8L, cells)
    polygonize <- function(edges){
      nedges <- nrow(edges)
      indices <- edges[1L, ]
      i <- indices[2L]
      edges <- edges[-1L, ]
      for(. in 1L:(nedges-2L)){
        j <- which(apply(edges, 1L, function(e) i %in% e))
        i <- edges[j, ][which(edges[j, ] != i)]
        indices <- c(indices, i)
        edges <- edges[-j, ]
      }
      indices
    }
    squares <- t(vapply(
      do.call(c, lapply(cubicCells, `[[`, "ridges")),
      function(r) polygonize(ridges[[r]][["edges"]]),
      integer(4L)
    ))
    verts3D <- t(apply(vertices, 1L, function(v){
      v[1L:3L] / (R - v[4L])
    }))
    vals <- data.table::as.data.table(cbind(verts3D, verts3D[2]))
    data.table::setnames(vals, names(vals), c("x","y","sx","sy"))
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.0,
        dimension = 2,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "poincare8") {

    # Hyperbolica example
    library(gyro)         # to use the `changesOfSign` function
    library(arrangements) # to use the `permutations` function
    x <- c(
      (1 + 2*sqrt(2)) / 2,
      (1 + sqrt(2)) / 2,
      0)
    vertices <- changesOfSign(
      cbind(
        t(apply(permutations(3L), 1L, function(perm) x[perm])),
        1/2
      ))

    R <- sqrt(c(crossprod(vertices[1L, ])))
    library(cxhull)
    hull <- cxhull(vertices)
    edges <- hull[["edges"]]
    cells <- hull[["facets"]]
    ridges <- hull[["ridges"]]
    cubicCells <- Filter(function(cell) length(cell[["vertices"]]) == 8L, cells)
    polygonize <- function(edges){
      nedges <- nrow(edges)
      indices <- edges[1L, ]
      i <- indices[2L]
      edges <- edges[-1L, ]
      for(. in 1L:(nedges-2L)){
        j <- which(apply(edges, 1L, function(e) i %in% e))
        i <- edges[j, ][which(edges[j, ] != i)]
        indices <- c(indices, i)
        edges <- edges[-j, ]
      }
      indices
    }
    squares <- t(vapply(
      do.call(c, lapply(cubicCells, `[[`, "ridges")),
      function(r) polygonize(ridges[[r]][["edges"]]),
      integer(4L)
    ))
    verts3D <- t(apply(vertices, 1L, function(v){
      v[1L:3L] / (R - v[4L])
    }))
    vals <- data.table::as.data.table(cbind(verts3D, verts3D[2]))
    data.table::setnames(vals, names(vals), c("x","y","sx","sy"))
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.0,
        dimension = 2,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "poincare9") {

    # Hyperbolica example
    library(gyro)         # to use the `changesOfSign` function
    library(arrangements) # to use the `permutations` function
    x <- c(
      (1 + 2*sqrt(2)) / 2,
      (0 + sqrt(2)) / 2,
      0)
    vertices <- changesOfSign(
      cbind(
        t(apply(permutations(3L), 1L, function(perm) x[perm])),
        1/2
      ))

    R <- sqrt(c(crossprod(vertices[1L, ])))
    library(cxhull)
    hull <- cxhull(vertices)
    edges <- hull[["edges"]]
    cells <- hull[["facets"]]
    ridges <- hull[["ridges"]]
    cubicCells <- Filter(function(cell) length(cell[["vertices"]]) == 8L, cells)
    polygonize <- function(edges){
      nedges <- nrow(edges)
      indices <- edges[1L, ]
      i <- indices[2L]
      edges <- edges[-1L, ]
      for(. in 1L:(nedges-2L)){
        j <- which(apply(edges, 1L, function(e) i %in% e))
        i <- edges[j, ][which(edges[j, ] != i)]
        indices <- c(indices, i)
        edges <- edges[-j, ]
      }
      indices
    }
    squares <- t(vapply(
      do.call(c, lapply(cubicCells, `[[`, "ridges")),
      function(r) polygonize(ridges[[r]][["edges"]]),
      integer(4L)
    ))
    verts3D <- t(apply(vertices, 1L, function(v){
      v[1L:3L] / (R - v[4L])
    }))
    vals <- data.table::as.data.table(cbind(verts3D, verts3D[2]))
    data.table::setnames(vals, names(vals), c("x","y","sx","sy"))
    vals$color <- sort(runif(nrow(vals), 1, 10)/10)
    vals |>
      echarts4r::e_charts(x) |>
      echarts4r::e_flow_gl(y, sx, sy, color) |>
      echarts4r::e_visual_map(
        min = 0, max = 0.0,
        dimension = 2,
        scale = echarts4r::e_scale,
        show = FALSE,
        inRange = list(color = c("#8e9cab"))) |> # "#626d78"))) |> #"darkorange" # c("white","darkred", "#626d78")
      echarts4r::e_theme(name = Theme) |>
      echarts4r::e_x_axis(splitLine = list(show = FALSE), show = FALSE) |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE), show = FALSE)

  } else if(Type == "Globe") {
    BackgroundsImages <- c(
      "composite 4k",
      "galaxy",
      "jupiter",
      "starfield",
      "world",
      "world dark",
      "world night")
    bg <- sample(x = BackgroundsImages, size = 1, replace = FALSE, prob = rep(1/length(BackgroundsImages), length(BackgroundsImages)))
    flights <- data.table::fread(file = system.file("shiny-apps","DataMuse","www", "flights.csv", package = "DataMuse"))
    flights |>
      echarts4r::e_charts() |>
      echarts4r::e_globe(
        environment = echarts4r.assets::ea_asset(bg), #("starfield"),
        base_texture = echarts4r.assets::ea_asset("world topo"),
        height_texture = echarts4r.assets::ea_asset("world topo"),
        displacementScale = 0.05
      ) |>
      echarts4r::e_lines_3d(
        start_lon,
        start_lat,
        end_lon,
        end_lat,
        name = "flights",
        effect = list(show = TRUE)
      ) |>
      echarts4r::e_legend(FALSE)
  }
}

# EchartThemes <- c(
#   "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark","dark-bold","eduardo",
#   "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired",
#   "jazz","london","macarons2","mint","purple-passion","red-velvet","red","roma","royal",
#   "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland")

#' @title rgba2hex
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param Color hex or rgba value
#'
#' @return hex value
#' @keywords internal
rgba2hex <- function(Color) {
  fl <- substr(x = Color, 1, 1)
  if(fl == "r") {
    rgba2rgb <- function(background_RGB, color_RGBA) {
      alpha=color_RGBA[4]
      new_col=matrix(c(
        (1 - alpha) * background_RGB[1] + alpha * color_RGBA[1],
        (1 - alpha) * background_RGB[2] + alpha * color_RGBA[2],
        (1 - alpha) * background_RGB[3] + alpha * color_RGBA[3]),
        nrow=3,ncol=1,dimnames=list(c("red","green","blue")))
      return(new_col)
    }
    rgb2hex <- function(x) {
      rgb(x[1L], x[2L], x[3L], maxColorValue = 255)
    }
    flv <- Color #input$color
    rv <- c()
    flv <- eval(parse(text = gsub(pattern = "rgba", "c", x = flv)))
    vlf <- 255 - flv
    x <- col2rgb('black')
    for(i in seq_along(x)) x[[i]] <- flv[i]
    x <- as.numeric(x)
    y <- col2rgb('black')
    for(i in seq_along(y)) y[[i]] <- vlf[i]
    y <- as.numeric(y)
    flv <- rgb2hex(x = x)
    vlf <- rgb2hex(x = y)
    return(list(flv = flv, vlf = vlf))
  } else {
    return(list(flv = Color, vlf = "#ff9900"))
  }
}

#' @title PlotSize_
#'
#' @param PlotOrder should be a seq of 1:n, with n being the total number of plots to be displayed
#' @param NumCols Number of grid columns in the output pane
#' @param OutputPage From App
#'
#' @noRd
PlotSize_ <- function(input, OutputPage, NumCols, PlotOrder) {

  # PlotOrder should be a seq of 1:n, with n being the total number of plots to be displayed
  # NumCols

  # Setup
  NumPlots <- length(PlotOrder)
  remainder <- NumPlots %% NumCols
  h <- c(); w <- c()
  height <- DataMuse::ReturnParam(xx = tryCatch({input[[paste0('PlotHeight',OutputPage)]]}, error = function(x) NULL), Type = 'numeric', Default = 860)
  width <- DataMuse::ReturnParam(xx = tryCatch({input[[paste0('PlotWidth', OutputPage)]]}, error = function(x) NULL), Type = 'numeric', Default = 1450)

  # Fill in full rows first --> then fill in remainder in 2nd for-loop
  for(i in seq_len(NumPlots - remainder)) {
    if(length(height) > 0L & is.numeric(height)) {
      h[i] <- paste0(as.character(floor(height / sqrt(NumCols))), "px")
    } else {
      h[i] <- paste0(as.character(floor(775 / sqrt(NumCols))), "px")
    }

    if(length(width) > 0L & is.numeric(width)) {
      w[i] <- paste0(as.character(width / NumCols), "px")
    } else {
      w[i] <- paste0(as.character(1450 / NumCols), "px")
    }
  }

  # Fill in remainder plots in 2nd for-loop.
  # > If Grid is 3x3 and there are 7 plots, then there is 1 remainder plot
  # > Make size adjustments relative to remainder
  #   counts and number of grid columns
  #
  # Note: DataMuse::RenderOutput() will modify grid column width for the remainder rows
  #       E.g. If you have a 4 column grid and 2 remainder plots then RenderOutput
  #            will create 2 columns with each having width 6L, to fill the entire row.
  #            Therefore, there isn't a need to make size adjustments in light of NumCols;
  #            only for remainder counts.
  for(j in seq_len(remainder)) {
    h[length(h) + 1L] <- paste0(height / remainder^0.42, "px")
    w[length(w) + 1L] <- paste0(width / remainder, "px")
  }
  return(list(PlotHeight = h[PlotOrder], PlotWidth = w[PlotOrder]))
}

#' @examples
#' \dontrun{
#' DataMuse:::transp_('#fff', alpha = 0.50)
#' }
#' @noRd
transp_ <- function(hex.color.list,alpha) sprintf("%s%02X",hex.color.list, floor(alpha*256))

#' @title LevelsHex
#'
#' @description Returns a vector of N color hex values with transparency added, where N is the number of unique levels in gv. Uses RColorBrewer to return base colors and then they are modified with transparency.
#'
#' @examples
#' \dontrun{
#' DataMuse:::LevelsHex(dt,gv,'Blues')
#' }
#' @keywords internal
LevelsHex <- function(dt, gv, name = 'Blues') {
  N <- max(8L, length(dt[, unique(get(gv))]))
  g <- RColorBrewer::brewer.pal(name = 'Blues', n = max(3L, N))
  return(DataMuse:::transp_(hex.color.list = g, alpha = 0.62))
}

#' @title Shiny.Plot.StandardPlotChecks
#'
#' @description Ensure variables are defined correctly for standard plots, if applicable
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param d From App
#' @param y From App
#' @param x From App
#' @param z From App
#' @param Type From App
#'
#' @keywords internal
Shiny.Plot.StandardPlotChecks <- function(d=NULL,
                                          y=NULL,
                                          x=NULL,
                                          z=NULL,
                                          Type=NULL) {

  # Mistakes
  if(length(Type) == 0L) return(list(ValidCheck = FALSE, msg = 'Type is NULL. This is an error from the app. Please submit an issue'))

  # Facts
  Type <- tolower(Type[1L])
  Check <- FALSE
  nam <- tryCatch({names(d)}, error = function(x) NULL)

  # Ensure data was selected
  if(length(nam) == 0L) {
    return(list(ValidCheck = FALSE, msg = "Please select a data set on the first tab"))
  }

  # Variable types
  if(length(x) > 0L) x_class <- class(d[[x[1L]]])[1L] else x_class <- NULL
  if(length(y) > 0L) y_class <- class(d[[y[1L]]])[1L] else y_class <- NULL
  if(length(z) > 0L) z_class <- class(d[[z[1L]]])[1L] else z_class <- NULL

  # Single Variable Plots
  if(Type %in% c('boxplot','violinplot','histogram','densityplot')) {
    if(length(y) > 0L) {
      Check <- TRUE
    } else if(length(x) > 0L) {
      Check <- TRUE
    } else {
      msg <- "Y-Axis and X-Axis variables are both empty. Please select one numeric variable for the Y-Axis input"
      return(list(ValidCheck = FALSE, msg = msg))
    }

    # Ensure data types are correct
    if(y_class %in% c('character','factor','NULL') && x_class %in% c('character','factor','NULL')) {
      Check <- FALSE
      if(length(y) > 0L && length(x) > 0L) {
        return(list(ValidCheck = Check, msg = "Both variables have class %in% c('character','factor'). Please select one numeric variable."))
      } else if( (length(y) == 0L && length(x) > 0L) || (length(y) > 0L && length(x) == 0L) ) {
        return(list(ValidCheck = Check, msg = "Your variable selected has class %in% c('character','factor'). Please select a numeric variable."))
      }
    }
  }

  # Numeric Relationships
  if(Type %in% c('scatterplot','copulaplot','scatterplot3d','copulaplot3d')) {

    # Ensure at least two variables were selected
    if(length(y) > 0L && length(x) > 0L) {
      Check <- TRUE
    } else {
      return(list(ValidCheck = FALSE, msg = "For these plot types you need to have both the X-Axis and Y-Axis variables defined"))
    }

    # Ensure data types are correct
    if(y_class %in% c('character','factor','NULL') || x_class %in% c('character','factor','NULL')) {
      Check <- FALSE
      return(list(ValidCheck = Check, msg = "Both your Y-Axis and X-Axis variables have to be a Numeric type"))
    }
  }

  # BarPlot
  if(Type %in% c('barplot','stackedbarplot')) {

    # Ensure at least two variables were selected
    if(length(y) > 0L && length(x) > 0L) {
      Check <- TRUE
    } else {
      return(list(ValidCheck = FALSE, msg = "For these plot types you need to have both the X-Axis and Y-Axis variables defined"))
    }

    # Ensure data types are correct
    if(y_class %in% c('character','factor','NULL') && x_class %in% c('character','factor','NULL')) {
      Check <- FALSE
      return(list(ValidCheck = Check, msg = "Only one of your X-Axis and Y-Axis variables can be a non-numeric type"))
    }
  }

  # Line plot
  if(Type %in% c('lineplot','stepplot','areaplot','riverplot')) {

    # Ensure at least two variables were selected
    if(length(y) > 0L && length(x) > 0L) {
      Check <- TRUE
    } else {
      return(list(ValidCheck = FALSE, msg = "For these plot types you need to have both the X-Axis and Y-Axis variables defined"))
    }

    # Ensure data types are correct
    if(!y_class %in% c('numeric','integer')) {
      Check <- FALSE
      return(list(ValidCheck = Check, msg = "You Y-Axis variable has to be a Numeric type and your X-Axis variable has to be a Date type"))
    }
  }

  # Correlogram
  if(Type %in% c('correlogram')) {

    # Ensure at least two variables were selected
    if(length(y) > 0L && length(x) > 0L) {
      msg <- "Please select more than one Y-Axis variables for the CorrMatrix plot"
      return(list(ValidCheck = FALSE, msg = msg))
    }

    # Ensure that only one was selected
    if(length(y) > 0L || length(x) > 0L) {
      Check <- TRUE
    } else {
      msg <- "You need to define either the X-Axis or Y-Axis variables, but not both"
      return(list(ValidCheck = FALSE, msg = msg))
    }

    # Ensure data types are correct
    if(length(y) > 0L) {
      g <- c()
      for(i in seq_along(y)) g <- c(g, class(d[[y[i]]])[[1L]])
      if(!all(g %in% c('numeric','integer'))) {
        msg <- "For your CorrMatrix plot, some of your Y-Axis variables are not of a numeric or integer type"
        return(list(ValidCheck = FALSE, msg = msg))
      } else {
        return(list(ValidCheck = TRUE, msg = NULL))
      }
    }
  }

  # Non Standard Plot Type
  return(list(ValidCheck = TRUE, msg = NULL))
}

#' @title Shiny.Plot.DM.data1
#'
#' @description Create data1 within plotter OE for app
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param DL DataList From App. Using DL because I'm not returning DataList
#' @param run From App
#' @param PlotType From App
#' @param CodeList From App
#' @param Debug From App
#' @param DataName From App, input[[paste0('Plot', run, '_SelectData')]]
#'
#' @keywords internal
Shiny.Plot.DM.data1 <- function(DL,run,PlotType,CodeList,Debug,DataName=NULL) {
  if(length(DataName) > 0L && length(DL) > 0L) {
    data1 <- data.table::copy(DL[[DataName]][['data']])
    CodeList <- DataMuse:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Plotting: ", PlotType,"\n",
      "data1 <- DataList[[", DataMuse:::CEP(DataName), "]]\n"))

    # Return
    return(list(
      data1 = data1,
      CodeList = CodeList
    ))
  } else {
    return(NULL)
  }
}

# ----

# ----

