
# CATBOOST ----

# Load CSV
if(!exists('DataList')) DataList <- list()
filename <- basename('ModelData.csv')
DataList[[filename]] <- DataMuse:::LoadCSV(file.choose())

# ML Data
ArgsList <- list()
ArgsList[['data']] <- data.table::copy(DataList[['ModelData.csv']])

# ML Set Metadata Parameters
ArgsList[['TargetColumnName']] <- "Regression Target" # 'Binary Classification Target' # 'MultiClass Target' #  #
ArgsList[['FeatureColNames']] <- c('CombinedGroups','day_LAG_1_Daily Margin','TimeTrend')

# Additional Metadata Parameters
ArgsList[['OutputSelection']] <- c('Importances','EvalMetrics','Score_TrainData')
ArgsList[['TrainOnFull']] <- FALSE
ArgsList[['ModelID']] <- 'ML1'
ArgsList[['DebugMode']] <- FALSE
ArgsList[['model_path']] <- 'C:/Users/Bizon/AppData/Local/R/win-library/4.2/DataMuse/shiny-apps/DataMuse'
ArgsList[['metadata_path']] <- 'C:/Users/Bizon/AppData/Local/R/win-library/4.2/DataMuse/shiny-apps/DataMuse'
ArgsList[['SaveModelObjects']] <- FALSE
ArgsList[['ReturnModelObjects']] <- TRUE
ArgsList[['NumOfParDepPlots']] <- 1

# CatBoost ML Parameters
ArgsList[['Trees']] <- 50
ArgsList[['Depth']] <- 6
ArgsList[['LearningRate']] <- NULL
ArgsList[['L2_Leaf_Reg']] <- NULL
ArgsList[['model_size_reg']] <- 0.5
ArgsList[['langevin']] <- TRUE
ArgsList[['diffusion_temperature']] <- 10000
ArgsList[['RandomStrength']] <- 1
ArgsList[['BorderCount']] <- 256
ArgsList[['RSM']] <- 1
ArgsList[['BootStrapType']] <- 'No'
ArgsList[['GrowPolicy']] <- 'SymmetricTree'
ArgsList[['feature_border_type']] <- 'GreedyLogSum'
ArgsList[['subsample']] <- 1
ArgsList[['score_function']] <- 'Cosine'
ArgsList[['min_data_in_leaf']] <- 1
ArgsList[['sampling_unit']] <- 'Object'

# CatBoost Evaluation Parameters
#ArgsList[['LossFunction']] <- 'Logloss'
ArgsList[['loss_function']] <- "RMSE" # 'MultiClassOneVsAll' ##
#ArgsList[['EvalMetric']] <- 'MCC'
ArgsList[['eval_metric']] <- "RMSE" # 'MultiClassOneVsAll'# 'MCC'##
ArgsList[['grid_eval_metric']] <- "rmse" #'Utility'##'microauc'
ArgsList[['MetricPeriods']] <- 10

# ML Build Model
ArgsList[['PrimaryDateColumn']] <- ArgsList[['PrimaryDateColumn']]
ArgsList[['EncodeMethod']] <- ArgsList[['EncodeMethod']]
# ModelOutputList <- do.call(AutoQuant::AutoCatBoostClassifier, ArgsList)
ModelOutputList <- do.call(AutoQuant::AutoCatBoostRegression, ArgsList)
# ModelOutputList <- do.call(AutoQuant::AutoCatBoostMultiClass, ArgsList)


# H2O ---
# Load CSV
if(!exists('DataList')) DataList <- list()
filename <- basename('ModelData.csv')
DataList[[filename]] <- DataMuse:::LoadCSV(file.choose())

# ML Data
if(!exists('ArgsList')) ArgsList <- list()
ArgsList[['data']] <- data.table::copy(DataList[['ModelData.csv']])

# ML Set Metadata Parameters
ArgsList[['TargetColumnName']] <- c('Binary Classification Target')
ArgsList[['FeatureColNames']] <- c('CombinedGroups','day_LAG_1_Daily Margin','TimeTrend')

# Additional Metadata Parameters
ArgsList[['OutputSelection']] <- c('Importances','EvalMetrics','Score_TrainData')
ArgsList[['TrainOnFull']] <- FALSE
ArgsList[['ModelID']] <- 'ML1'
ArgsList[['DebugMode']] <- TRUE
ArgsList[['model_path']] <- 'C:/Users/Bizon/AppData/Local/R/win-library/4.2/DataMuse/shiny-apps/DataMuse'
ArgsList[['metadata_path']] <- 'C:/Users/Bizon/AppData/Local/R/win-library/4.2/DataMuse/shiny-apps/DataMuse'
ArgsList[['SaveModelObjects']] <- FALSE
ArgsList[['ReturnModelObjects']] <- TRUE
ArgsList[['H2OStartUp']] <- TRUE
ArgsList[['H2OShutdown']] <- TRUE
ArgsList[['NumOfParDepPlots']] <- 1

# H2O-GBM ML Parameters
ArgsList[['Trees']] <- 20
ArgsList[['MaxDepth']] <- 20
ArgsList[['SampleRate']] <- 0.61
ArgsList[['ColSampleRatePerTree']] <- 1
ArgsList[['MinRows']] <- 1
ArgsList[['NBinsCats']] <- 1024
ArgsList[['NBinsTopLevel']] <- 1024
ArgsList[['HistogramType']] <- 'AUTO'
ArgsList[['CategoricalEncoding']] <- 'AUTO'
ArgsList[['StoppingRounds']] <- 1
ArgsList[['LearnRate']] <- 0.1
ArgsList[['LearnRateAnnealing']] <- 1
ArgsList[['Distribution']] <- 'AUTO'

# H2O-GBM Evaluation Parameters
ArgsList[['eval_metric']] <- 'binary_logloss'

# ML Build Model
ArgsList[['SaveInfoToPDF']] <- FALSE
cw0 <- 1
cw1 <- 1
ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
ArgsList[['eval_metric']] <- 'logloss'
ModelOutputList <- do.call(AutoQuant::AutoH2oGBMClassifier, ArgsList)

# ML Reports ----

SampleSize <- 100000

TrainData <- ModelOutputList$TrainData
TrainData <- TrainData[order(runif(.N))][seq_len(min(.N, eval(SampleSize)))]
TestData <- ModelOutputList$TestData
TestData <- TestData[order(runif(.N))][seq_len(min(.N, eval(SampleSize)))]

# Checks
if(!exists("OutputList")) OutputList <- NULL

ModelID = "ML_ML1"

# temp_model_rdata$ArgsList$TargetColumnName
TargetColumnName <- ModelOutputList[["ArgsList"]][["TargetColumnName"]]

# temp_model_rdata$ArgsList$
PredictionColumnName <- ModelOutputList[["ArgsList"]][["PredictionColumnName"]]

# temp_model_rdata$ArgsList$FeatureColNames
FeatureColumnNames <- ModelOutputList[["ArgsList"]][["FeatureColNames"]]
PDPVariables <- FeatureColumnNames

# NEED
TargetType <- ModelOutputList[["ArgsList"]][["TargetType"]]

# Group Var
GroupVariable <- NULL#"CombinedGroups"

# NEED
Algo <- ModelOutputList[["ArgsList"]][["Algo"]]

print("ML Reports 4")

# All Metrics Table
if(TargetType == "Regression") {
  MetricsTable <- DataList[["ML_RegressionMetrics"]][["data"]]
} else if(TargetType == "Binary Classification") {
  MetricsTable <- DataList[["ML_ClassificationMetrics"]][["data"]]
} else if(TargetType == "MultiClass") {
  MetricsTable <- DataList[["ML_MultiClassMetrics"]][["data"]]
}

print("ML Reports 5")

# Variable Importance
if(tolower(Algo) == "catboost") {
  Test_Importance <- ModelOutputList[["VariableImportance"]][["Test_Importance"]]
  Validation_Importance <- ModelOutputList[["VariableImportance"]][["Validation_Importance"]]
  Train_Importance <- ModelOutputList[["VariableImportance"]][["Train_Importance"]]
} else {
  Test_Importance <- ModelOutputList[["VariableImportance"]]
}

print("ML Reports 6")

# Interaction Importances
if(tolower(Algo) == 'catboost' && TargetType != "MultiClass") {

  Test_Interaction <- ModelOutputList[["InteractionImportance"]][["Test_Interaction"]]
  Validation_Interaction <- NULL
  Train_Interaction <- NULL

  # Update Colnames
  if(!is.null(Test_Interaction)) data.table::setnames(Test_Interaction, old = 'score', new = 'Test_Importance', skip_absent = TRUE)
  if(!is.null(Validation_Interaction)) data.table::setnames(Validation_Interaction, old = 'score', new = 'Validation_Importance', skip_absent = TRUE)
  if(!is.null(Train_Interaction)) data.table::setnames(Train_Interaction, old = 'score', new = 'Train_Importance', skip_absent = TRUE)

  # CatBoost only
  if(is.null(Test_Interaction) && is.null(Validation_Interaction) && is.null(Train_Interaction)) {
    All_Interaction <- NULL
  } else if(!is.null(Test_Interaction) && !is.null(Validation_Interaction) && !is.null(Train_Interaction)) {
    All_Interaction <- merge(Test_Interaction, Validation_Interaction, by = c('Features1','Features2'), all = TRUE)
    All_Interaction <- merge(All_Interaction, Train_Interaction, by = c('Features1','Features2'), all = TRUE)
    data.table::setorderv(x = All_Interaction, cols = names(All_Interaction)[3L], order = -1)
  } else if(!is.null(Test_Interaction) && !is.null(Validation_Interaction) && is.null(Train_Interaction)) {
    All_Interaction <- merge(Test_Interaction, Validation_Interaction, by = c('Features1','Features2'), all = TRUE)
    data.table::setorderv(x = All_Interaction, cols = names(All_Interaction)[3L], order = -1)
  } else if(!is.null(Test_Interaction) && is.null(Validation_Interaction) && !is.null(Train_Interaction)) {
    All_Interaction <- merge(Test_Interaction, Train_Interaction, by = c('Features1','Features2'), all = TRUE)
    data.table::setorderv(x = All_Interaction, cols = names(All_Interaction)[3L], order = -1)
  } else if(is.null(Test_Interaction) && !is.null(Validation_Interaction) && !is.null(Train_Interaction)) {
    All_Interaction <- merge(Validation_Interaction, Train_Interaction, by = c('Features1','Features2'), all = TRUE)
    data.table::setorderv(x = All_Interaction, cols = names(All_Interaction)[3L], order = -1)
  } else if(is.null(Test_Interaction) && is.null(Validation_Interaction) && !is.null(Train_Interaction)) {
    All_Interaction <- Train_Interaction
  } else if(is.null(Test_Interaction) && !is.null(Validation_Interaction) && is.null(Train_Interaction)) {
    All_Interaction <- Validation_Interaction
  } else if(!is.null(Test_Interaction) && is.null(Validation_Interaction) && is.null(Train_Interaction)) {
    All_Interaction <- Test_Interaction
  } else {
    All_Interaction <- NULL
  }
} else {
  All_Interaction <- NULL
}

PlotHeighta = "600px"
PlotWidtha = "1100px"
Theme = "dark"
FontColor <- "dark"

MLOutputSelection = c("Model Comparison Metrics", "Evaluation Plots", "Partial Dependence Plots")

if("Model Comparison Metrics" %in% MLOutputSelection) {

  ## Model_VarImportanceTable
  if(tolower(Algo) == 'catboost') {

    # Update Colnames
    if(!is.null(Test_Importance)) data.table::setnames(Test_Importance, old = 'Importance', new = 'Test_Importance', skip_absent = TRUE)
    if(!is.null(Validation_Importance)) data.table::setnames(Validation_Importance, old = 'Importance', new = 'Validation_Importance', skip_absent = TRUE)
    if(!is.null(Train_Importance)) data.table::setnames(Train_Importance, old = 'Importance', new = 'Train_Importance', skip_absent = TRUE)

    # CatBoost only
    if(is.null(Test_Importance) && is.null(Validation_Importance) && is.null(Train_Importance)) {
      All_Importance <- NULL
    } else if(!is.null(Test_Importance) && !is.null(Validation_Importance) && !is.null(Train_Importance)) {
      All_Importance <- merge(Test_Importance, Validation_Importance, by = 'Variable', all = TRUE)
      All_Importance <- merge(All_Importance, Train_Importance, by = 'Variable', all = TRUE)
    } else if(!is.null(Test_Importance) && !is.null(Validation_Importance) && is.null(Train_Importance)) {
      All_Importance <- merge(Test_Importance, Validation_Importance, by = 'Variable', all = TRUE)
    } else if(!is.null(Test_Importance) && is.null(Validation_Importance) && !is.null(Train_Importance)) {
      All_Importance <- merge(Test_Importance, Train_Importance, by = 'Variable', all = TRUE)
    } else if(is.null(Test_Importance) && !is.null(Validation_Importance) && !is.null(Train_Importance)) {
      All_Importance <- merge(Validation_Importance, Train_Importance, by = 'Variable', all = TRUE)
    } else if(is.null(Test_Importance) && is.null(Validation_Importance) && !is.null(Train_Importance)) {
      All_Importance <- Train_Importance
    } else if(is.null(Test_Importance) && !is.null(Validation_Importance) && is.null(Train_Importance)) {
      All_Importance <- Validation_Importance
    } else if(!is.null(Test_Importance) && is.null(Validation_Importance) && is.null(Train_Importance)) {
      All_Importance <- Test_Importance
    } else {
      All_Importance <- NULL
    }

  } else {
    All_Importance <- Test_Importance
  }

  OutputList[["Variable Importance"]] <- reactable::reactable(
    data = All_Importance,
    compact = TRUE,
    defaultPageSize = 10,
    wrap = TRUE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    selection = "multiple",
    showPagination = TRUE,
    showSortable = TRUE,
    showSortIcon = TRUE,
    sortable = TRUE,
    striped = TRUE,
    theme = reactable::reactableTheme(
      color = "white",
      backgroundColor = "#4f4f4f26",
      borderColor = "#dfe2e5",
      stripedColor = "#4f4f4f8f",
      highlightColor = "#8989898f",
      cellPadding = "8px 12px",
      style = list(
        fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
      ),
      searchInputStyle = list(width = "100%")
    )
  )

  # Interaction Importance Table
  if(data.table::is.data.table(All_Interaction)) {
    OutputList[["Interaction Importance"]] <- reactable::reactable(
      data = All_Interaction,
      compact = TRUE,
      defaultPageSize = 10,
      wrap = TRUE,
      filterable = TRUE,
      fullWidth = TRUE,
      highlight = TRUE,
      pagination = TRUE,
      resizable = TRUE,
      searchable = TRUE,
      selection = "multiple",
      showPagination = TRUE,
      showSortable = TRUE,
      showSortIcon = TRUE,
      sortable = TRUE,
      striped = TRUE,
      theme = reactable::reactableTheme(
        color = "white",
        backgroundColor = "#4f4f4f26",
        borderColor = "#dfe2e5",
        stripedColor = "#4f4f4f8f",
        highlightColor = "#8989898f",
        cellPadding = "8px 12px",
        style = list(
          fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
        ),
        searchInputStyle = list(width = "100%")
      )
    )
  }
}

print("ML Reports 9")

# Evaluation Plots
if("Evaluation Plots" %in% MLOutputSelection) {

  print("ML Reports 9.1")

  OutputList[["TestData Residual Histogram"]] <- AutoPlots::Plot.Residuals.Histogram(
    dt = TestData,
    AggMethod = "mean",
    SampleSize = 30000,
    XVar = 'Predict',
    YVar = TargetColumnName,
    GroupVar = GroupVariable,
    YVarTrans = "Identity",
    XVarTrans = "Identity",
    FacetRows = 1,
    FacetCols = 1,
    FacetLevels = NULL,
    NumberBins = 20,
    Height = PlotHeighta,
    Width = PlotWidtha,
    Title = "Residuals Histogram",
    ShowLabels = FALSE,
    Title.YAxis = TargetColumnName,
    Title.XAxis = 'Predict',
    EchartsTheme = Theme,
    TimeLine = FALSE,
    X_Scroll = TRUE,
    Y_Scroll = FALSE,
    TextColor = "white",
    title.fontSize = 22,
    title.fontWeight = "bold",
    title.textShadowColor = "#63aeff",
    title.textShadowBlur = 3,
    title.textShadowOffsetY = 1,
    title.textShadowOffsetX = -1,
    xaxis.fontSize = 14,
    yaxis.fontSize = 14,
    Debug = FALSE)

  # dt = TestData
  # AggMethod = "mean"
  # SampleSize = 30000
  # XVar = 'Predict'
  # YVar = TargetColumnName
  # GroupVar = GroupVariable
  # YVarTrans = "Identity"
  # XVarTrans = "Identity"
  # FacetRows = 1
  # FacetCols = 1
  # FacetLevels = NULL
  # NumberBins = 20
  # Height = PlotHeighta
  # Width = PlotWidtha
  # Title = "Residuals Histogram"
  # ShowLabels = FALSE
  # Title.YAxis = TargetColumnName
  # Title.XAxis = 'Predict'
  # EchartsTheme = Theme
  # TimeLine = FALSE
  # X_Scroll = TRUE
  # Y_Scroll = TRUE
  # TextColor = "white"
  # title.fontSize = 22
  # title.fontWeight = "bold"
  # title.textShadowColor = "#63aeff"
  # title.textShadowBlur = 3
  # title.textShadowOffsetY = 1
  # title.textShadowOffsetX = -1
  # xaxis.fontSize = 14
  # yaxis.fontSize = 14
  # Debug = FALSE

  ### Test
  OutputList[["TestData Calibration Plot"]] <- AutoPlots::Plot.Calibration.Line(
    dt = TestData,
    AggMethod = "mean",
    XVar = 'Predict', # 'p1',
    YVar = TargetColumnName,
    GroupVar = GroupVariable,
    YVarTrans = "Identity",
    XVarTrans = "Identity",
    FacetRows = 1,
    FacetCols = 1,
    FacetLevels = NULL,
    NumberBins = 21,
    Height = PlotHeighta,
    Width = PlotWidtha,
    Title = "Calibration Line Plot",
    ShowLabels = FALSE,
    Title.YAxis = TargetColumnName,
    Title.XAxis = "Predict",
    EchartsTheme = Theme,
    TimeLine = FALSE,
    X_Scroll = TRUE,
    Y_Scroll = TRUE,
    TextColor = "white",
    Debug = FALSE)

  # dt = TestData
  # AggMethod = "mean"
  # XVar = 'p1'#'Predict'
  # YVar = TargetColumnName
  # GroupVar = GroupVariable
  # YVarTrans = "Identity"
  # XVarTrans = "Identity"
  # FacetRows = 1
  # FacetCols = 1
  # FacetLevels = NULL
  # NumberBins = 21
  # Height = PlotHeighta
  # Width = PlotWidtha
  # Title = "Calibration Line Plot"
  # ShowLabels = FALSE
  # Title.YAxis = TargetColumnName
  # Title.XAxis = "Predict"
  # EchartsTheme = Theme
  # TimeLine = FALSE
  # X_Scroll = TRUE
  # Y_Scroll = TRUE
  # TextColor = "white"
  # Debug = FALSE

  print("ML Reports 9.2")

  ### Train
  if(!is.null(TrainData) && TrainData[,.N] > 100L) {
    OutputList[["TrainData Calibration Plot"]] <- AutoPlots::Plot.Calibration.Line(
      dt = TrainData,
      AggMethod = "mean",
      XVar = "p1", #'Predict',
      YVar = TargetColumnName,
      GroupVar = GroupVariable,
      YVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      NumberBins = 21,
      Height = PlotHeighta,
      Width = PlotWidtha,
      Title = "Calibration Line Plot",
      ShowLabels = FALSE,
      Title.YAxis = TargetColumnName,
      Title.XAxis = "Predict",
      EchartsTheme = Theme,
      TimeLine = FALSE,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = "white",
      Debug = FALSE)
  }

  print("ML Reports 9.3")

  ## ROC
  OutputList[["TestData ROC Plot"]] <- AutoPlots::Plot.ROC(
    dt = TestData,
    AggMethod = "mean",
    SampleSize = 30000,
    XVar = "p1", #"Predict",
    YVar = TargetColumnName,
    GroupVar = GroupVariable,
    YVarTrans = "Identity",
    XVarTrans = "Identity",
    FacetRows = 1,
    FacetCols = 1,
    FacetLevels = NULL,
    Height = PlotHeighta,
    Width = PlotWidtha,
    Title = "ROC Plot",
    ShowLabels = FALSE,
    Title.YAxis = "True Positive Rate",
    Title.XAxis = "1 - False Positive Rate",
    EchartsTheme = Theme,
    TimeLine = FALSE,
    X_Scroll = TRUE,
    Y_Scroll = TRUE,
    TextColor = "white",
    Debug = FALSE)

  print("ML Reports 9.4")

  if(length(TrainData) > 0L) {
    OutputList[["TrainData ROC Plot"]] <- AutoPlots::Plot.ROC(
      dt = TrainData,
      AggMethod = "mean",
      SampleSize = 30000,
      XVar = "p1",#"Predict",
      YVar = TargetColumnName,
      GroupVar = GroupVariable,
      YVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeighta,
      Width = PlotWidtha,
      Title = "ROC Plot",
      ShowLabels = FALSE,
      Title.YAxis = "True Positive Rate",
      Title.XAxis = "1 - False Positive Rate",
      EchartsTheme = Theme,
      TimeLine = FALSE,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = "white",
      Debug = FALSE)
  }

  dt = TrainData
  AggMethod = "mean"
  SampleSize = 30000
  XVar = "Predict"
  YVar = TargetColumnName
  GroupVar = NULL
  YVarTrans = "Identity"
  XVarTrans = "Identity"
  FacetRows = 1
  FacetCols = 1
  FacetLevels = NULL
  Height = PlotHeighta
  Width = PlotWidtha
  Title = "ROC Plot"
  ShowLabels = FALSE
  Title.YAxis = "True Positive Rate"
  Title.XAxis = "1 - False Positive Rate"
  EchartsTheme = Theme
  TimeLine = TRUE
  X_Scroll = TRUE
  Y_Scroll = TRUE
  TextColor = "white"
  Debug = FALSE

  print("ML Reports 9.5")

  ## Lift
  print(head(TestData))
  print(TargetColumnName)
  OutputList[["TestData Lift Plot"]] <- AutoPlots::Plot.Lift(
    dt = TestData,
    PreAgg = FALSE,
    XVar = "p1", #"Predict",
    YVar = TargetColumnName,
    GroupVar = GroupVariable,
    YVarTrans = "Identity",
    XVarTrans = "Identity",
    FacetRows = 1,
    FacetCols = 1,
    FacetLevels = NULL,
    NumberBins = 21,
    Height = PlotHeighta,
    Width = PlotWidtha,
    Title = "Lift Plot",
    ShowLabels = FALSE,
    Title.YAxis = "Lift",
    Title.XAxis = "% Positive Classified",
    EchartsTheme = Theme,
    TimeLine = FALSE,
    X_Scroll = TRUE,
    Y_Scroll = TRUE,
    TextColor = "white",
    Debug = FALSE)

  dt = TestData
  PreAgg = FALSE
  XVar = "p1"
  YVar = TargetColumnName
  GroupVar = GroupVariable
  YVarTrans = "Identity"
  XVarTrans = "Identity"
  FacetRows = 1
  FacetCols = 1
  FacetLevels = NULL
  NumberBins = 21
  Height = PlotHeighta
  Width = PlotWidtha
  Title = "Lift Plot"
  ShowLabels = FALSE
  Title.YAxis = "Lift"
  Title.XAxis = "% Positive Classified"
  EchartsTheme = Theme
  TimeLine = FALSE
  X_Scroll = TRUE
  Y_Scroll = TRUE
  TextColor = "white"
  Debug = FALSE

  print("ML Reports 9.6")

  if(length(TrainData) > 0L) {
    OutputList[["TrainData Lift Plot"]] <- AutoPlots::Plot.Lift(
      dt = TrainData,
      PreAgg = TRUE,
      XVar = "Predict",
      YVar = TargetColumnName,
      ZVar = NULL,
      GroupVar = GroupVariable,
      YVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeighta,
      Width = PlotWidtha,
      Title = "Lift Plot",
      ShowLabels = FALSE,
      Title.YAxis = "Lift",
      Title.XAxis = "% Positive Classified",
      EchartsTheme = Theme,
      TimeLine = FALSE,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = "white",
      Debug = FALSE)
  }

  print("ML Reports 9.7")

  ## Gains
  OutputList[["TestData Gains Plot"]] <- AutoPlots::Plot.Gains(
    dt = TestData,
    PreAgg = FALSE,
    XVar = "p1", #"Predict",
    YVar = TargetColumnName,
    GroupVar = GroupVariable,
    YVarTrans = "Identity",
    XVarTrans = "Identity",
    FacetRows = 1,
    FacetCols = 1,
    FacetLevels = NULL,
    Height = PlotHeighta,
    Width = PlotWidtha,
    Title = "Gains Plot",
    ShowLabels = FALSE,
    Title.YAxis = TargetColumnName,
    Title.XAxis = g,
    EchartsTheme = Theme,
    TimeLine = FALSE,
    X_Scroll = TRUE,
    Y_Scroll = TRUE,
    TextColor = "white",
    Debug = FALSE)

  print("ML Reports 9.8")

  if(length(TrainData) > 0L) {
    OutputList[["TrainData Gains Plot"]] <- AutoPlots::Plot.Gains(
      dt = TrainData,
      PreAgg = TRUE,
      XVar = "Predict",
      YVar = TargetColumnName,
      ZVar = NULL,
      GroupVar = GroupVariable,
      YVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeighta,
      Width = PlotWidtha,
      Title = "Gains Plot",
      ShowLabels = FALSE,
      Title.YAxis = TargetColumnName,
      Title.XAxis = "Predict",
      EchartsTheme = Theme,
      TimeLine = FALSE,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = "white",
      Debug = FALSE)
  }
}

print("ML Reports 10")

# Model Interpretation
if("Partial Dependence Plots" %in% MLOutputSelection) {

  # Add Plots
  if(!is.null(TestData) && !is.null(PDPVariables)) {
    for(g in PDPVariables) { # g = "day_LAG_1_Daily Margin"
      if(is.numeric(TestData[[g]])) {
        OutputList[[paste0('TestData Partial Dependence Line Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Line(
          dt = TestData,
          XVar = g,
          YVar = TargetColumnName,
          ZVar = 'Predict',
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          ZVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          GroupVar = GroupVariable,
          NumberBins = 20,
          AggMethod = "mean",
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Partial Dependence Line",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = g,
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = FALSE)

        dt = TestData
        XVar = g
        YVar = TargetColumnName
        ZVar = 'Predict'
        YVarTrans = "Identity"
        XVarTrans = "Identity"
        ZVarTrans = "Identity"
        FacetRows = 1
        FacetCols = 1
        FacetLevels = NULL
        GroupVar = GroupVariable
        NumberBins = 20
        AggMethod = "mean"
        Height = PlotHeighta
        Width = PlotWidtha
        Title = "Partial Dependence Line"
        ShowLabels = FALSE
        Title.YAxis = TargetColumnName
        Title.XAxis = g
        EchartsTheme = Theme
        TimeLine = FALSE
        X_Scroll = TRUE
        Y_Scroll = TRUE
        TextColor = "white"
        Debug = FALSE

      }
    }
  }

  # Add Plots
  if(!is.null(TestData) && !is.null(PDPVariables)) {
    for(g in PDPVariables) {# g = PDPVariables[2]
      if(is.numeric(TestData[[g]])) {
        print(paste0("g = ", g))
        OutputList[[paste0('TestData Partial Dependence Box Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Box(
          dt = TestData,
          XVar = g,
          YVar = TargetColumnName,
          ZVar = 'Predict',
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          ZVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          GroupVar = GroupVariableInclude,
          NumberBins = 20,
          AggMethod = "mean",
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Partial Dependence Line",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = g,
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = TRUE)

        dt = TestData
        XVar = g
        YVar = TargetColumnName
        ZVar = 'Predict'
        YVarTrans = "Identity"
        XVarTrans = "Identity"
        ZVarTrans = "Identity"
        FacetRows = 1
        FacetCols = 1
        FacetLevels = NULL
        GroupVar = GroupVariableInclude
        NumberBins = 20
        AggMethod = "mean"
        Height = PlotHeighta
        Width = PlotWidtha
        Title = "Partial Dependence Line"
        ShowLabels = FALSE
        Title.YAxis = TargetColumnName
        Title.XAxis = g
        EchartsTheme = Theme
        TimeLine = FALSE
        X_Scroll = TRUE
        Y_Scroll = TRUE
        TextColor = "white"
        Debug = TRUE
      }
    }
  }


  # Add Plots
  if(!is.null(TrainData) && TrainData[,.N] > 100L && !is.null(PDPVariables)) {
    for(g in PDPVariables) {
      if(is.numeric(TrainData[[g]])) {
        OutputList[[paste0('TrainData Partial Dependence Line Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Line(
          dt = TrainData,
          XVar = g,
          YVar = TargetColumnName,
          ZVar = 'Predict',
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          ZVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          GroupVar = GroupVariable,
          NumberBins = 20,
          AggMethod = "mean",
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Partial Dependence Line",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = g,
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = FALSE)
      }
    }
  }

  # Add Plots
  if(!is.null(TestData) && !is.null(PDPVariables)) {
    for(g in PDPVariables) {# g = PDPVariables[2]
      if(!is.numeric(TestData[[g]])) {
        OutputList[[paste0('TestData Partial Dependence Heatmap: ', g)]] <- AutoPlots::Plot.PartialDependence.HeatMap(
          dt = TestData,
          XVar = g,
          YVar = TargetColumnName,
          ZVar = 'Predict',
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          ZVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          GroupVar = GroupVariable,
          NumberBins = 20,
          AggMethod = "mean",
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Partial Dependence Line",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = g,
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = TRUE)

        dt = TestData
        XVar = g
        YVar = TargetColumnName
        ZVar = 'Predict'
        YVarTrans = "Identity"
        XVarTrans = "Identity"
        ZVarTrans = "Identity"
        FacetRows = 1
        FacetCols = 1
        FacetLevels = NULL
        GroupVar = GroupVariable
        NumberBins = 20
        AggMethod = "mean"
        Height = PlotHeighta
        Width = PlotWidtha
        Title = "Partial Dependence Line"
        ShowLabels = FALSE
        Title.YAxis = TargetColumnName
        Title.XAxis = g
        EchartsTheme = Theme
        TimeLine = FALSE
        X_Scroll = TRUE
        Y_Scroll = TRUE
        TextColor = "white"
        Debug = TRUE
      }
    }
  }

  # Add Plots
  if(!is.null(TrainData) && TrainData[,.N] > 100L && !is.null(PDPVariables)) {
    for(g in PDPVariables) {
      if(!is.numeric(TrainData[[g]])) {
        OutputList[[paste0('TrainData Partial Dependence Heatmap: ', g)]] <- AutoPlots::Plot.PartialDependence.HeatMap(
          dt = TrainData,
          XVar = g,
          YVar = TargetColumnName,
          ZVar = 'Predict',
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          ZVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          GroupVar = GroupVariable,
          NumberBins = 20,
          AggMethod = "mean",
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Partial Dependence Line",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = g,
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = FALSE)
      }
    }
  }

}
