# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  #xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE),
  Diff = c(TRUE, FALSE))

# Other tests
QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
QA_Results[, TaskType := data.table::fifelse(runif(.N) < 0.5, "hist", "hist")]
QA_Results[, Success := "Failure"]
QA_Results[, RunTime := 123.456]
QA_Results[, DateTime := Sys.time()]
QA_Results[, Mixed := data.table::fifelse(runif(.N) < 0.5, TRUE, FALSE)]
QA_Results[, MeanPred := -555]
QA_Results[, SDPred := -555]
# QA_Results[, SaveModel := data.table::fifelse(runif(.N) < 0.5, TRUE, FALSE)]

FillNow <- FALSE

# NOT Train On FULL TOF
# run = 16
# run = 32
# run = 45
# run = 53
# run = 61
# run = 69
# run = 101
# run = 109
# run = 117
# run = 125
# run = 128
# run = 92
for(run in seq_len(QA_Results[,.N])) {

  data <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/FakeBevData.csv")
  data <- data[Date < "2022-01-01"]

  if(QA_Results[run][["Group"]] == 0L) {
    GroupVars <- NULL
  } else if(QA_Results[run][["Group"]] == 1L) {
    GroupVars <- "Customer"
  } else if(QA_Results[run][["Group"]] == 2L) {
    GroupVars <- c("Customer",'Brand')
  } else if(QA_Results[run][["Group"]] == 3L) {
    GroupVars <- c("Customer",'Brand','Category')
  }

  if(QA_Results[run, Diff]) Diff <- TRUE else Diff <- FALSE
  if(QA_Results[run, Trans]) Trans <- TRUE else Trans <- FALSE
  if(QA_Results[run, TOF]) TOF <- TRUE else TOF <- FALSE
  TimeWeights <- QA_Results[run, TimeWeights]
  TaskType <- QA_Results[run, TaskType]

  Start <- Sys.time()

  # Build forecast ----
  TestModel <- tryCatch({AutoQuant::AutoXGBoostCARMA(

    SaveModel = FALSE,
    ArgsList = NULL,
    NThreads = -1,
    SaveDataPath = getwd(),
    ModelID = "aa",
    TVT = NULL,

    # data args
    data = data,
    XREGS = NULL,
    TimeWeights = TimeWeights,
    TargetColumnName = "Daily Margin",
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = GroupVars,
    TimeUnit = "days",
    TimeGroups = c("days"),

    # Production args
    TrainOnFull = TOF,
    SplitRatios = c(0.70,0.30),
    PartitionType = "random",
    FC_Periods = 5,
    Timer = TRUE,
    DebugMode = TRUE,

    # Target variable transformations
    TargetTransformation = Trans,
    Methods = "LogPlus1",
    Difference = Diff,
    NonNegativePred = FALSE,
    RoundPreds = FALSE,

    # Calendar-related features
    CalendarVariables = c("wom","month","quarter"),
    HolidayVariable = c("USPublicHolidays"),
    HolidayLags = NULL,
    HolidayMovingAverages = NULL,
    HolidayLookback = 7,

    # Lags, moving averages, and other rolling stats
    Lags = c(1,7),
    MA_Periods = 7,
    SD_Periods = NULL,
    Skew_Periods = NULL,
    Kurt_Periods = NULL,
    Quantile_Periods = NULL,
    Quantiles_Selected = NULL,

    # Bonus features
    EncodingMethod = 'credibility',
    ZeroPadSeries = "maxmax",
    AnomalyDetection = NULL,
    FourierTerms = 0,
    TimeTrendVariable = FALSE,
    DataTruncate = FALSE,

    # ML grid tuning args
    GridTune = FALSE,
    ModelCount = 5,
    MaxRunsWithoutNewWinner = 50,
    MaxRunMinutes = 60*60,
    GridEvalMetric = "rmse",

    # ML eval args
    TreeMethod = TaskType,
    EvalMetric = 'RMSE',
    LossFunction = 'reg:squarederror',
    EarlyStoppingRounds = 50,
    num_parallel_tree = 2,

    # ML args
    alpha = 0.10,
    lambda = 0.10,
    NTrees = 300,
    LearningRate = 0.3,
    MaxDepth = 9L,
    MinChildWeight = 1.0,
    SubSample = 1.0,
    ColSampleByTree = 1.0)}, error = function(x) NULL)

  # Timer
  End <- Sys.time()
  QA_Results[run, RunTime := as.numeric(difftime(time1 = End, Start))]

  if(length(TestModel) > 0L) {
    if(length(TestModel$Forecast) > 0) {
      QA_Results <- QA_Results[eval(run), MeanPred := TestModel$Forecast[is.na(`Daily Margin`), round(mean(Predictions), 1)]]
      QA_Results <- QA_Results[eval(run), SDPred := TestModel$Forecast[is.na(`Daily Margin`), round(sd(Predictions), 1)]]
    }

    QA_Results[run, Success := "Success"]
  }

  # Outcome
  rm(TestModel)
  AutoQuant:::Post_Append_Helper(QA_Results,'AutoXGBoostCARMA_QA')
  Sys.sleep(5)
}


# Build Model ----
# library(AutoQuant)
# library(data.table)
# library(lubridate)
# devtools::load_all(path = "C:/Users/Bizon/Documents/GitHub/AutoQuant/R")
# library(Rodeo)
#
# run = 16
#
# data <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/FakeBevData.csv")
# data <- data[Date < "2022-01-01"]
#
# # Collection data.table
# QA_Results <- data.table::CJ(
#   Group = c(0,1,2,3),
#   #xregs = c(0,1,2,3),
#   TOF = c(TRUE, FALSE),
#   Trans = c(TRUE, FALSE),
#   Diff = c(TRUE, FALSE))
#
# # Other tests
# QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
# QA_Results[, TaskType := data.table::fifelse(runif(.N) < 0.5, "hist", "hist")]
# QA_Results[, Success := "Failure"]
# QA_Results[, RunTime := 123.456]
# QA_Results[, DateTime := Sys.time()]
# QA_Results[, Mixed := data.table::fifelse(runif(.N) < 0.5, TRUE, FALSE)]
# QA_Results[, MeanPred := -555]
# QA_Results[, SDPred := -555]
#
# if(QA_Results[run][["Group"]] == 0L) {
#   GroupVars <- NULL
# } else if(QA_Results[run][["Group"]] == 1L) {
#   GroupVars <- "Customer"
# } else if(QA_Results[run][["Group"]] == 2L) {
#   GroupVars <- c("Customer",'Brand')
# } else if(QA_Results[run][["Group"]] == 3L) {
#   GroupVars <- c("Customer",'Brand','Category')
# }
#
# if(QA_Results[run, Diff]) Diff <- TRUE else Diff <- FALSE
# if(QA_Results[run, Trans]) Trans <- TRUE else Trans <- FALSE
# if(QA_Results[run, TOF]) TOF <- TRUE else TOF <- FALSE
# TimeWeights <- QA_Results[run, TimeWeights]
# TaskType <- QA_Results[run, TaskType]
#
# Start <- Sys.time()
#
# FillNow <- FALSE
#
# data <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/FakeBevData.csv")
# data <- data[Date < "2022-01-01"]
#
# SaveDataPath = getwd()
# SaveModel = FALSE
# ArgsList = NULL
# data = data
# XREGS = NULL
# TimeWeights = TimeWeights
# TargetColumnName = "Daily Margin"
# DateColumnName = "Date"
# HierarchGroups = NULL
# GroupVariables = GroupVars
# TimeUnit = "days"
# TimeGroups = c("days")
# TrainOnFull = TOF
# SplitRatios = c(0.70,0.30)
# PartitionType = "random"
# FC_Periods = 5
# ModelID = "aa"
# Timer = TRUE
# DebugMode = TRUE
# TargetTransformation = Trans
# Methods = "LogPlus1"
# Difference = Diff
# NonNegativePred = FALSE
# RoundPreds = FALSE
# CalendarVariables = c("wom","month","quarter")
# HolidayVariable = c("USPublicHolidays")
# HolidayLookback = 7
# HolidayLags = NULL
# HolidayMovingAverages = NULL
# Lags = c(1,7)
# MA_Periods = 7
# SD_Periods = NULL
# Skew_Periods = NULL
# Kurt_Periods = NULL
# Quantile_Periods = NULL
# Quantiles_Selected = NULL
# EncodingMethod = 'credibility'
# ZeroPadSeries = "maxmax"
# AnomalyDetection = NULL
# FourierTerms = 0
# TimeTrendVariable = FALSE
# DataTruncate = FALSE
# GridTune = FALSE
# ModelCount = 5
# GridEvalMetric = "rmse"
# MaxRunsWithoutNewWinner = 50
# MaxRunMinutes = 60*60
# EarlyStoppingRounds = 50
# TreeMethod = TaskType
# NThreads = -1
# EvalMetric = 'RMSE'
# LossFunction = 'reg:squarederror'
# NTrees = 300
# LearningRate = 0.3
# MaxDepth = 9L
# MinChildWeight = 1.0
# SubSample = 1.0
# ColSampleByTree = 1.0
# num_parallel_tree = 2
# TVT = NULL
# alpha = 0.10
# lambda = 0.10

# CarmaDifferencing ----
# GroupVariables.=GroupVariables
# Difference.=Difference
# data.=data
# TargetColumnName.=TargetColumnName
# FC_Periods.=FC_Periods

# DiffData ----
# data = data.
# ColumnsToDiff = eval(TargetColumnName.)
# CARMA = TRUE
# TargetVariable = eval(TargetColumnName.)
# GroupingVariable = NULL

# CarmaTimeSeriesFeatures ----
# data.=data
# TargetColumnName.=TargetColumnName
# DateColumnName.=DateColumnName
# GroupVariables.=GroupVariables
# HierarchGroups.=HierarchGroups
# Difference.=Difference
# TimeGroups.=TimeGroups
# TimeUnit.=TimeUnit
# Lags.=Lags
# MA_Periods.=MA_Periods
# SD_Periods.=SD_Periods
# Skew_Periods.=Skew_Periods
# Kurt_Periods.=Kurt_Periods
# Quantile_Periods.=Quantile_Periods
# Quantiles_Selected.=Quantiles_Selected
# HolidayVariable.=HolidayVariable
# HolidayLags.=HolidayLags
# HolidayMovingAverages.=HolidayMovingAverages
# DebugMode.=DebugMode

# AutoLagRollStats ----
# data                 = data.
# DateColumn           = eval(DateColumnName.)
# Targets              = eval(TargetColumnName.)
# HierarchyGroups      = HierarchSupplyValue
# IndependentGroups    = IndependentSupplyValue
# TimeBetween          = NULL
# TimeUnit             = TimeUnit.
# TimeUnitAgg          = TimeUnit.
# TimeGroups           = TimeGroups.
# RollOnLag1           = TRUE
# Type                 = 'Lag'
# SimpleImpute         = TRUE
# Lags                  = Lags.
# MA_RollWindows        = MA_Periods.
# SD_RollWindows        = SD_Periods.
# Skew_RollWindows      = Skew_Periods.
# Kurt_RollWindows      = Kurt_Periods.
# Quantile_RollWindows  = Quantile_Periods.
# Quantiles_Selected    = Quantiles_Selected.
# ShortName             = TRUE
# Debug = TRUE

# DT_GDL_Feature_Engineering ----
# data
# lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags
# periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows
# SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows
# Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows
# Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows
# Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows
# statsFUNs       = RollFunctions
# targets         = Targets
# groupingVars    = Fact
# sortDateName    = DateColumn
# timeDiffTarget  = TimeBetween
# timeAgg         = timeaggs
# WindowingLag    = RollOnLag1
# Type            = Type
# ShortName       = ShortName
# SimpleImpute    = SimpleImpute


# CatBoost Regression ----
# task_type = TaskType
# NumGPUs = NumGPU
# OutputSelection = if(TOF) NULL else c('Importances', 'EvalMetrics', 'Score_TrainData')
# ReturnShap = ReturnShap
# ModelID = 'CatBoost'
# model_path = getwd()
# metadata_path = getwd()
# SaveModelObjects = FALSE
# ReturnModelObjects = TRUE
# SaveInfoToPDF = FALSE
# data = train
# TrainOnFull = TOF
# ValidationData = valid
# TestData = test
# TargetColumnName = TargetVariable
# FeatureColNames = ModelFeatures
# PrimaryDateColumn = eval(DateColumnName)
# WeightsColumnName = if('Weights' %in% names(train)) 'Weights' else NULL
# IDcols = IDcols
# EncodeMethod = EncodingMethod
# TransformNumericColumns = NULL
# Methods = NULL
# eval_metric = EvalMetric
# eval_metric_value = EvalMetricValue
# loss_function = LossFunction
# loss_function_value = LossFunctionValue
# MetricPeriods = 10L
# NumOfParDepPlots = NumOfParDepPlots
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = ModelCount
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = 60*60
# BaselineComparison = 'default'
# langevin = Langevin
# diffusion_temperature = DiffusionTemperature
# Trees = NTrees
# Depth = Depth
# LearningRate = LearningRate
# L2_Leaf_Reg = L2_Leaf_Reg
# RandomStrength = RandomStrength
# BorderCount = BorderCount
# RSM = if(TaskType == 'GPU') NULL else RSM
# BootStrapType = BootStrapType
# GrowPolicy = GrowPolicy
# model_size_reg = ModelSizeReg
# feature_border_type = FeatureBorderType
# sampling_unit = SamplingUnit
# subsample = SubSample
# score_function = ScoreFunction
# min_data_in_leaf = MinDataInLeaf
# DebugMode = DebugMode


# Regression DataPrep ----
# OutputSelection.=OutputSelection
# ModelType="regression"
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# PrimaryDateColumn.=PrimaryDateColumn
# WeightsColumnName.=WeightsColumnName
# IDcols.=IDcols
# TrainOnFull.=TrainOnFull
# SaveModelObjects.=SaveModelObjects
# TransformNumericColumns.=TransformNumericColumns
# Methods.=Methods
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=LossFunction
# EncodeMethod. = EncodeMethod
# EvalMetric.=EvalMetric

# Regression Data Conversion ----
# CatFeatures.=CatFeatures
# dataTrain.=dataTrain
# dataTest.=dataTest
# TestData.=TestData
# TrainTarget.=TrainTarget
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TrainOnFull.=TrainOnFull
# Weights.=WeightsColumnName


# Carma score ----
# Type = 'xgboost'
# i.=i
# N.=N
# GroupVariables.=GroupVariables
# ModelFeatures.=ModelFeatures
# HierarchGroups.=HierarchGroups
# DateColumnName.=DateColumnName
# Difference.=Difference
# TargetColumnName.=TargetColumnName
# Step1SCore.=Step1SCore
# Model.=Model
# FutureDateData.=FutureDateData
# NonNegativePred.=NonNegativePred
# UpdateData.=UpdateData
# FactorList.= TestModel$FactorLevelsList
# EncodingMethod. = TestModel$FactorLevelsList$EncodingMethod
# dt = data
# RoundPreds. = FALSE
# Debug = FALSE

# Update Features (date is <NA>) ----
# RollingVars. = TRUE
# UpdateData.=UpdateData
# GroupVariables.=GroupVariables
# CalendarFeatures.=CalendarFeatures
# CalendarVariables.=CalendarVariables
# GroupVarVector.=GroupVarVector
# DateColumnName.=DateColumnName
# XREGS.=XREGS
# FourierTerms.=FourierTerms
# FourierFC.=FourierFC
# TimeGroups.=TimeGroups
# TimeTrendVariable.=TimeTrendVariable
# N.=N
# TargetColumnName.=TargetColumnName
# HolidayVariable.=HolidayVariable
# HolidayLookback.=HolidayLookback
# TimeUnit.=TimeUnit
# AnomalyDetection.=AnomalyDetection
# i.=i
# Debug = DebugMode

# Rolling Stats Update (NA's returned) ----
# ModelType='catboost'
# DebugMode.=DebugMode
# UpdateData.=UpdateData
# GroupVariables.=GroupVariables
# Difference.=Difference
# CalendarVariables.=CalendarVariables
# HolidayVariable.=HolidayVariable
# IndepVarPassTRUE.=IndepentVariablesPass
# data.=data
# CalendarFeatures.=CalendarFeatures
# XREGS.=XREGS
# HierarchGroups.=HierarchGroups
# GroupVarVector.=GroupVarVector
# TargetColumnName.=TargetColumnName
# DateColumnName.=DateColumnName
# Preds.=Preds
# HierarchSupplyValue.=HierarchSupplyValue
# IndependentSupplyValue.=IndependentSupplyValue
# TimeUnit.=TimeUnit
# TimeGroups.=TimeGroups
# Lags.=Lags
# MA_Periods.=MA_Periods
# SD_Periods.=SD_Periods
# Skew_Periods.=Skew_Periods
# Kurt_Periods.=Kurt_Periods
# Quantile_Periods.=Quantile_Periods
# Quantiles_Selected.=Quantiles_Selected
# HolidayLags.=HolidayLags
# HolidayMovingAverages.=HolidayMovingAverages

# Carma Diff
# GroupVariables.=GroupVariables
# Difference.=Difference
# data.=data
# TargetColumnName.=TargetColumnName
# FC_Periods.=FC_Periods

# UpdateFeatures
# RollingVars. = FALSE
# UpdateData.=Step1SCore
# GroupVariables.=GroupVariables
# CalendarFeatures.=UpdateData
# CalendarVariables.=CalendarVariables
# GroupVarVector.=GroupVarVector
# DateColumnName.=DateColumnName
# XREGS.=XREGS
# FourierTerms.=FourierTerms
# FourierFC.=FourierFC
# TimeGroups.=TimeGroups
# TimeTrendVariable.=TimeTrendVariable
# N.=N
# TargetColumnName.=TargetColumnName
# HolidayVariable.=HolidayVariable
# HolidayLookback.=HolidayLookback
# TimeUnit.=TimeUnit
# AnomalyDetection.=AnomalyDetection
# i.=1
# Debug = DebugMode




# Return data ----
# UpdateData.=UpdateData
# FutureDateData.=FutureDateData
# dataStart.=dataStart
# DateColumnName.=DateColumnName
# TargetColumnName.=TargetColumnName
# GroupVariables.=GroupVariables
# Difference.=Difference
# TargetTransformation.=TargetTransformation
# TransformObject.=TransformObject
# NonNegativePred.=NonNegativePred
# MaxDate. = MaxDate
# DiffTrainOutput.=DiffTrainOutput
# Debug = TRUE

# Carma Partition
# data.=data
# SplitRatios.=SplitRatios
# TrainOnFull.=TrainOnFull
# NumSets.=NumSets
# PartitionType.=PartitionType
# GroupVariables.=GroupVariables
# DateColumnName.=DateColumnName
# TVT.=TVT

# GroupVarVector.=GroupVarVector
# UpdateData.=UpdateData
# GroupVariables.=GroupVariables
# CalendarFeatures.=CalendarFeatures
# CalendarVariables.=CalendarVariables
# DateColumnName.=DateColumnName
# XREGS.=XREGS
# FourierTerms.=FourierTerms
# FourierFC.=FourierFC
# TimeGroups.=TimeGroups
# TimeTrendVariable.=TimeTrendVariable
# N.=N
# TargetColumnName.=TargetColumnName
# HolidayVariable.=HolidayVariable
# HolidayLookback.=HolidayLookback
# TimeUnit.=TimeUnit
# AnomalyDetection.=AnomalyDetection
# i.=i
# Debug = DebugMode




# # Time Series Fill
# data
# DateColumnName=eval(DateColumnName)
# GroupVariables=GroupVariables
# TimeUnit=TimeUnit
# FillType=ZeroPadSeries
# MaxMissingPercent=0.95
# SimpleImpute=TRUE


# CarmaTimeSeriesFeatures ----
# data.=data
# TargetColumnName.=TargetColumnName
# DateColumnName.=DateColumnName
# GroupVariables.=GroupVariables
# HierarchGroups.=HierarchGroups
# Difference.=Difference
# TimeGroups.=TimeGroups
# TimeUnit.=TimeUnit
# Lags.=Lags
# MA_Periods.=MA_Periods
# SD_Periods.=SD_Periods
# Skew_Periods.=Skew_Periods
# Kurt_Periods.=Kurt_Periods
# Quantile_Periods.=Quantile_Periods
# Quantiles_Selected.=Quantiles_Selected
# HolidayVariable.=HolidayVariable
# HolidayLags.=HolidayLags
# HolidayMovingAverages.=HolidayMovingAverages
# DebugMode.=DebugMode



# Data ----
# data                 = Temporary
# RowNumsID            = "ID"
# RowNumsKeep          = 1
# DateColumn           = eval(DateColumnName.)
# Targets              = eval(TargetColumnName.)
# HierarchyGroups      = NULL
# IndependentGroups    = NULL
# # Service
# TimeBetween          = NULL
# TimeUnit             = TimeUnit.
# TimeUnitAgg          = TimeGroups.[1]
# TimeGroups           = TimeGroups.
# RollOnLag1           = TRUE
# Type                 = "Lag"
# SimpleImpute         = TRUE
# # Calculated Column
# Lags                 = Lags.
# MA_RollWindows       = MA_Periods.
# SD_RollWindows       = SD_Periods.
# Skew_RollWindows     = Skew_Periods.
# Kurt_RollWindows     = Kurt_Periods.
# Quantile_RollWindows = Quantile_Periods.
# Quantiles_Selected   = Quantiles_Selected.
# Debug                = DebugMode.



# Keep GDL Features ----
# IndepVarPassTRUE = NULL
# data.
# UpdateData.
# CalendarFeatures.
# XREGS.
# Difference.
# HierarchGroups.
# GroupVariables.,
# GroupVarVector.
# CalendarVariables=CalVar
# HolidayVariable=HolVar
# TargetColumnName.,DateColumnName.
# Preds.




# Catboost scoring ----
# i == 1
# TargetType = 'regression'
# ScoringData = Step1SCore.
# FeatureColumnNames = ModelFeatures.
# FactorLevelsList = FactorList.
# IDcols = IDcols
# OneHot = FALSE
# ModelObject = Model.
# ModelPath = getwd()
# ReturnShapValues = FALSE
# MultiClassTargetLevels = NULL
# RemoveModel = FALSE
# ModelID = 'ModelTest'
# ReturnFeatures = TRUE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TransformationObject = NULL
# TransID = NULL
# TransPath = NULL
# TargetColumnName = NULL
# MDP_Impute = FALSE
# MDP_CharToFactor = FALSE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = '0'
# MDP_MissNum = -1




# i == 2 ----
# TargetType = 'regression'
# ScoringData = temp
# FeatureColumnNames = ModelFeatures.
# FactorLevelsList = FactorList.
# ReturnShapValues = FALSE
# IDcols = IDcols
# OneHot = FALSE
# ModelObject = Model.
# ModelPath = getwd()
# ModelID = 'ModelTest'
# ReturnFeatures = FALSE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TargetColumnName = NULL
# TransformationObject = NULL
# TransID = NULL
# TransPath = NULL
# MDP_Impute = FALSE
# MDP_CharToFactor = FALSE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = '0'
# MDP_MissNum = -1











# ----

# ----
