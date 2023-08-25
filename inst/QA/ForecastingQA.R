
# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Train From Scratch         ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----

options(warn = 0)

# Build Query String
query <- Rappture::QueryBuilder(
  DB = 'KompsProcessed',
  Table = 'POS_Processed_Long_Daily_backward',
  Columns = c('ARTICLE','BRAND','CHILLED_Margin_PerDay','CUSTOMER_COD_char','DATE_ISO'),
  GroupByColumns = NULL,
  AggStat = 'AVG',
  Limit = NULL)

# Load Data from PostGRE
if(!exists('DataList')) DataList <- list()
DataList[['POS_Processed_Long_Daily_backward']] <- RemixAutoML::PostGRE_Query(
  Query = query,
  Host = 'localhost',
  CloseConnection = TRUE,
  DBName = 'KompsProcessed',
  User = 'postgres',
  Port = 5432,
  Password = 'Aa1028#@')$data

# Subset Data
if(!exists('DataList')) DataList <- list()
DataList[['FC_Train']] <- Rappture:::FilterLogicData(
  DataList[['POS_Processed_Long_Daily_backward']],
  FilterLogic = '<',
  FilterVariable = 'DATE_ISO',
  FilterValue = '2022-01-01',
  FilterValue2 = NULL)

# Forecasting: Data Args
ModelID <- 'FC001'
if(!exists('ArgsList')) ArgsList <- list()
ArgsList[['data']] <- data.table::copy(DataList[['FC_Train']])
ValidationData <- NULL

ArgsList[['TargetColumnName']] <- 'CHILLED_Margin_PerDay'
ArgsList[['DateColumnName']] <- 'DATE_ISO'
ArgsList[['GroupVariables']] <- c('CUSTOMER_COD_char','ARTICLE','BRAND')
ArgsList[['TimeUnit']] <- 'day'
ArgsList[['TimeGroups']] <- ArgsList[['TimeUnit']]
ArgsList[['FC_Periods']] <- 5
ArgsList[['TimeWeights']] <- NULL
ArgsList[['TargetTransformation']] <- TRUE
ArgsList[['Methods']] <- 'LogPlus1'
ArgsList[['Difference']] <- FALSE

# Forecasting: Production Args
ArgsList[['TaskType']] <- NULL
ArgsList[['NumGPU']] <- NULL
ArgsList[['RoundPreds']] <- FALSE
ArgsList[['NonNegativePred']] <- FALSE
ArgsList[['SaveModel']] <- FALSE
ArgsList[['DebugMode']] <- TRUE
ArgsList[['ZeroPadSeries']] <- 'dynamic:meow'

# Forecasting: Feature Engineering Args
ArgsList[['CalendarVariables']] <- c('wday')
ArgsList[['HolidayVariable']] <- NULL
ArgsList[['HolidayLookback']] <- NULL
ArgsList[['Quantiles_Selected']] <- NULL
ArgsList[['Lags']] <- c(1)
ArgsList[['MA_Periods']] <- NULL
ArgsList[['SD_Periods']] <- NULL
ArgsList[['Kurt_Periods']] <- NULL
ArgsList[['Skew_Periods']] <- NULL
ArgsList[['Quantile_Periods']] <- NULL
ArgsList[['DataTruncate']] <- FALSE
ArgsList[['TimeTrend']] <- NULL
AnomalyDetection_LowThreshold <- NULL
AnomalyDetection_HighThreshold <- NULL
if(length(AnomalyDetection_HighThreshold) > 0L && length(AnomalyDetection_LowThreshold) > 0L) {
  ArgsList[['AnomalyDetection']] <- list('tstat_high' = NULL, 'tstat_low' = NULL)
}

# Forecasting: Feature Enginnering Args
ArgsList[['NTrees']] <- 500
ArgsList[['Depth']] <- NULL
ArgsList[['Langevin']] <- NULL
ArgsList[['DiffusionTemperature']] <- NULL
ArgsList[['L2_Leaf_Reg']] <- NULL
ArgsList[['ModelSizeReg']] <- NULL
ArgsList[['MinDataInLeaf']] <- NULL
ArgsList[['LearningRate']] <- 0.2
ArgsList[['BootStrapType']] <- NULL
ArgsList[['GrowPolicy']] <- NULL
ArgsList[['RandomStrength']] <- NULL
ArgsList[['RSM']] <- NULL
ArgsList[['SubSample']] <- 0.85
ArgsList[['BorderCount']] <- NULL
ArgsList[['FeatureBorderType']] <- NULL
ArgsList[['ScoreFunction']] <- NULL

# Ensure date column is correct date type
Output <- Rappture::FC.DateCast(ArgsList, VD = ValidationData)
ArgsList <- Output$ArgsList; ValidationData <- Output$VD; rm(Output); gc()

# Train ML Model
dtc <- data.table::copy(ArgsList[['data']])
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)
ArgsList <- Output$ArgsList
ArgsList[['FC001_Meta']] <- Output$TestModel
if(!(length(ArgsList$data) > 0 && data.table::is.data.table(ArgsList$data))) {
  if(length(VD) > 0L) {
    ArgsList[['data']] <- unique(data.table::rbindlist(list(dtc, VD), use.names = TRUE, fill = TRUE))
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Retrain Only               ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
query <- Rappture::QueryBuilder(
  DB = 'KompsProcessed',
  Table = 'POS_Processed_Long_Daily_backward',
  Columns = c('BRAND','ARTICLE','CUSTOMER_COD_char','DATE_ISO','CHILLED_Margin_PerDay'),#,'CHILLED_Net_Revenue_PerDay'), # SIZE
  GroupByColumns = NULL,
  AggStat = NULL,
  Limit = NULL)

# Load Data from PostGRE
if(!exists('DataList')) DataList <- list()
data <- RemixAutoML::PostGRE_Query(
  Query = query,
  Host = 'localhost',
  CloseConnection = TRUE,
  DBName = 'KompsProcessed',
  User = 'postgres',
  Port = 5432,
  Password = 'Aa1028#@')$data




#  ----

#  ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Forecast Only              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
query <- Rappture::QueryBuilder(
  DB = 'KompsProcessed', Table = 'POS_Processed_Long_Daily_backward',
  Columns = c('BRAND','ARTICLE','CUSTOMER_COD_char','DATE_ISO','CHILLED_Margin_PerDay'),#,'CHILLED_Net_Revenue_PerDay'), # SIZE
  GroupByColumns = NULL, AggStat = NULL, Limit = NULL)

# Load Data from PostGRE
if(!exists('DataList')) DataList <- list()
data <- RemixAutoML::PostGRE_Query(
  Query = query, Host = 'localhost', CloseConnection = TRUE,
  DBName = 'KompsProcessed', User = 'postgres', Port = 5432,
  Password = 'Aa1028#@')$data

# Forecasting: Data Args
ModelID = 'FC1'

# Load Trained Model
ArgsList <- readRDS(file = "C:/Users/Bizon/Documents/GitHub/CatBoostFC1.rds")

# Remove data (data from training)
ArgsList$data <- NULL

# Forecast
Output <- RemixAutoML::AutoCatBoostCARMA(data = data, ArgsList = ArgsList)

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Retrain Model Only         ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
query <- Rappture::QueryBuilder(
  DB = 'KompsProcessed', Table = 'POS_Processed_Long_Daily_backward',
  Columns = c('BRAND','ARTICLE','CUSTOMER_COD_char','DATE_ISO','CHILLED_Margin_PerDay'),
  GroupByColumns = NULL, AggStat = NULL, Limit = NULL)

# Load Data from PostGRE
if(!exists('DataList')) DataList <- list()
data <- RemixAutoML::PostGRE_Query(
  Query = query, Host = 'localhost', CloseConnection = TRUE,
  DBName = 'KompsProcessed', User = 'postgres', Port = 5432,
  Password = 'Aa1028#@')$data

# Load Trained Model
ArgsList <- readRDS(file = "C:/Users/Bizon/Documents/GitHub/CatBoostFC1.rds")

# Remove data (data from training)
ArgsList$data <- NULL
ArgsList$Model <- NULL

# Retrain with new data
Output <- RemixAutoML::AutoCatBoostCARMA(data = data, TrainOnFull = FALSE, SaveModel = TRUE, ArgsList = ArgsList)

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Retrain + Forecast         ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
query <- Rappture::QueryBuilder(
  DB = 'KompsProcessed', Table = 'POS_Processed_Long_Daily_backward',
  Columns = c('BRAND','ARTICLE','CUSTOMER_COD_char','DATE_ISO','CHILLED_Margin_PerDay'),#,'CHILLED_Net_Revenue_PerDay'), # SIZE
  GroupByColumns = NULL, AggStat = NULL, Limit = NULL)

# Load Data from PostGRE
if(!exists('DataList')) DataList <- list()
data <- RemixAutoML::PostGRE_Query(
  Query = query, Host = 'localhost', CloseConnection = TRUE,
  DBName = 'KompsProcessed', User = 'postgres', Port = 5432,
  Password = 'Aa1028#@')$data

# Load Trained Model
ArgsList <- readRDS(file = "C:/Users/Bizon/Documents/GitHub/CatBoostFC1.rds")

# Remove data (data from training)
ArgsList$data <- NULL
ArgsList$Model <- NULL

# Retrain with new data
Output <- RemixAutoML::AutoCatBoostCARMA(data = data, TrainOnFull = TRUE, SaveModel = TRUE, ArgsList = ArgsList)

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Backtest Basic Loop        ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----


# *******************************
#       CatBoost     ************
# *******************************

dlist <- list()
for(run in 1:20) { # run = 1

  # Build Query String
  Debug <- TRUE
  query <- Rappture::QueryBuilder(
    DB = 'KompsProcessed',
    Table = 'POS_Processed_Long_Daily_backward',
    Columns = c('ARTICLE','BRAND','CHILLED_Margin_PerDay','CUSTOMER_COD_char','DATE_ISO'),
    GroupByColumns = NULL,
    AggStat = 'AVG',
    Limit = NULL)

  # Load Data from PostGRE
  if(!exists('DataList')) DataList <- list()
  DataList[['POS_Processed_Long_Daily_backward']] <- RemixAutoML::PostGRE_Query(
    Query = query,
    Host = 'localhost',
    CloseConnection = TRUE,
    DBName = 'KompsProcessed',
    User = 'postgres',
    Port = 5432,
    Password = 'Aa1028#@')$data

  # Subset Data
  if(!exists('DataList')) DataList <- list()
  DataList[['FC_Train']] <- Rappture:::FilterLogicData(
    DataList[['POS_Processed_Long_Daily_backward']],
    FilterLogic = '<',
    FilterVariable = 'DATE_ISO',
    FilterValue = '2022-01-01',
    FilterValue2 = NULL)

  # Forecasting: Data Args
  ModelID <- 'FC001'
  if(!exists('ArgsList')) ArgsList <- list()
  ArgsList[['data']] <- data.table::copy(DataList[['FC_Train']])
  ValidationData <- data.table::copy(DataList[['POS_Processed_Long_Daily_backward']])

  ArgsList[['TargetColumnName']] <- 'CHILLED_Margin_PerDay'
  ArgsList[['DateColumnName']] <- 'DATE_ISO'
  ArgsList[['GroupVariables']] <- c('CUSTOMER_COD_char','ARTICLE','BRAND')
  ArgsList[['TimeUnit']] <- 'day'
  ArgsList[['TimeGroups']] <- ArgsList[['TimeUnit']]
  ArgsList[['FC_Periods']] <- 14
  ArgsList[['TimeWeights']] <- 1
  ArgsList[['TargetTransformation']] <- TRUE
  ArgsList[['Methods']] <- NULL
  ArgsList[['Difference']] <- FALSE

  # Forecasting: Production Args
  ArgsList[['TaskType']] <- 'CPU'
  ArgsList[['NumGPU']] <- 1
  ArgsList[['RoundPreds']] <- FALSE
  ArgsList[['NonNegativePred']] <- FALSE
  ArgsList[['SaveModel']] <- FALSE
  ArgsList[['DebugMode']] <- TRUE
  ArgsList[['ZeroPadSeries']] <- 'dynamic:meow'

  # Forecasting: Feature Engineering Args
  ArgsList[['CalendarVariables']] <- c('wday','wom')
  ArgsList[['HolidayVariable']] <- c('EasterGroup','ChristmasGroup','OtherEcclesticalFeasts')
  ArgsList[['HolidayLookback']] <- 1
  ArgsList[['Quantiles_Selected']] <- NULL
  ArgsList[['Lags']] <- c(1,7)
  ArgsList[['MA_Periods']] <- 0
  ArgsList[['SD_Periods']] <- NULL
  ArgsList[['Kurt_Periods']] <- NULL
  ArgsList[['Skew_Periods']] <- NULL
  ArgsList[['Quantile_Periods']] <- NULL
  ArgsList[['DataTruncate']] <- FALSE
  ArgsList[['TimeTrend']] <- NULL
  AnomalyDetection_LowThreshold <- NULL
  AnomalyDetection_HighThreshold <- NULL
  if(length(AnomalyDetection_HighThreshold) > 0L && length(AnomalyDetection_LowThreshold) > 0L) {
    ArgsList[['AnomalyDetection']] <- list('tstat_high' = NULL, 'tstat_low' = NULL)
  }

  # Forecasting: Feature Enginnering Args
  ArgsList[['NTrees']] <- 500
  ArgsList[['Depth']] <- 6
  ArgsList[['Langevin']] <- FALSE
  ArgsList[['DiffusionTemperature']] <- 10000
  ArgsList[['L2_Leaf_Reg']] <- 5
  ArgsList[['ModelSizeReg']] <- 1.2
  ArgsList[['MinDataInLeaf']] <- 1
  ArgsList[['LearningRate']] <- 0.1
  ArgsList[['BootStrapType']] <- 'No'
  ArgsList[['GrowPolicy']] <- 'SymmetricTree'
  ArgsList[['RandomStrength']] <- 1
  ArgsList[['RSM']] <- 1
  ArgsList[['SubSample']] <- 0.66
  ArgsList[['BorderCount']] <- 128
  ArgsList[['FeatureBorderType']] <- 'GreedyLogSum'
  ArgsList[['ScoreFunction']] <- 'Cosine'

  # Ensure date column is correct date type
  Output <- Rappture::FC.DateCast(ArgsList, VD = ValidationData)
  ArgsList <- Output$ArgsList; ValidationData <- Output$VD; rm(Output); gc()

  # CatBoostFC Backtest Simple Loop
  ArgsList$TrainOnFull <- TRUE
  ArgsList$Model <- NULL
  ArgsList$SaveModel <- FALSE

  # Collection table
  N <- 10
  if(!exists('LoopMetrics')) {
    LoopMetrics <- data.table::data.table(
      'Methods' = rep(FALSE, N),
      'Difference' = rep(FALSE, N),
      'EncodingMethod' = rep(FALSE, N),
      'ZeroPadSeries' = rep(FALSE, N),
      'CalendarVariables' = rep(FALSE, N),
      'HolidayVariable' = rep(FALSE, N),
      'TimeWeights' = rep(FALSE, N),
      'AnomalyDetection' = rep(FALSE, N),
      'TimeTrendVariable' = rep(FALSE, N),
      'Lags' = rep(FALSE, N),
      'AvgError On' = rep(-5, N),
      'AvgError Off' = rep(-5, N),
      'RMSE On' = rep(-5, N),
      'RMSE Off' = rep(-5, N),
      'MAE On' = rep(-5, N),
      'MAE Off' = rep(-5, N))
  }

  # Encoding and Time Series fill
  Plan <- list()
  Plan[['Methods']] <- list(Method = 'Methods', ArgsOn = c('LogPlus1'), ArgsOff = "Identity")
  Plan[['EncodingMethod']] <- list(Method = 'EncodingMethod', ArgsOn = 'meow', ArgsOff = 'target_encoding')
  Plan[['ZeroPadSeries']] <- list(Method = 'ZeroPadSeries', ArgsOn = 'dynamic:meow', ArgsOff = 'dynamic:target_encoding')
  Plan[['CalendarVariables']] <- list(Method = 'CalendarVariables', ArgsOn = c(ArgsList$CalendarVariables))
  Plan[['HolidayVariable']] <- list(Method = 'HolidayVariable', ArgsOn = if(length(ArgsList$HolidayVariable) == 0L) c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts') else ArgsList$HolidayVariable)
  Plan[['TimeWeights']] <- list(Method = 'TimeWeights', ArgsOn = if(ArgsList$TimeWeights == 1) 0.9995 else ArgsList$TimeWeights, ArgsOff = 1)
  Plan[['AnomalyDetection']] <- list(Method = 'AnomalyDetection', ArgsOn = list('tstat_high' = 6.25, 'tstat_low' = -6.25))
  Plan[['TimeTrendVariable']] <- list(Method = 'TimeTrendVariable', ArgsOn = c(TRUE))
  Plan[['Lags']] <- list(Method = 'Lags', ArgsOn = if(length(ArgsList$Lags) == 0 || all(ArgsList$Lags == 0)) 1 else ArgsList$Lags, ArgsOff = 0)
  Plan[['Difference']] <- list(Method = 'Difference', ArgsOn = c(TRUE))

  # Reset args
  ArgsList$Methods <- NULL
  ArgsList$TargetTransformation <- FALSE
  ArgsList$Difference <- FALSE
  ArgsList$EncodingMethod <- 'target_encoding'
  ArgsList$ZeroPadSeries <- 'dynamic:target_encoding'
  ArgsList$CalendarVariables <- NULL
  ArgsList$HolidayVariable <- NULL
  ArgsList$TimeWeights <- 1
  ArgsList$AnomalyDetection <- NULL
  ArgsList$TimeTrendVariable <- FALSE
  ArgsList$Lags <- NULL
  ArgsList$MA_Periods <- NULL

  # Loop through builds
  if(Debug) print('Shiny.FC.Panel.Backest.FeatureEval 2: Begin Loop')
  # svs = 1
  CodeList <- list()
  for(svs in seq_along(names(Plan))) { # svs = 2
    Output <- Rappture:::BasicLoop.SingleTest(LoopMetrics, Plan, ArgsList, DataList, CodeList, ModelID, ValidationData, Debug=TRUE, Algo = 'catboost', Test = names(Plan)[svs])
    ArgsList <- Output$ArgsList; LoopMetrics <- Output$LoopMetrics
  }
  LoopMetrics[, Run := eval(run)]
  dlist[[run]] <- data.table::copy(LoopMetrics)
  rm(LoopMetrics, ArgsList, DataList, CodeList, ModelID, ValidationData, Plan, svs)
  Sys.sleep(8)
}

FinalMetrics <- data.table::rbindlist(dlist)




svs = 2


Algo = 'catboost'
Test = names(Plan)[svs]
VD = ValidationData
LM = LoopMetrics


if(Debug) print(paste0('BasicLoop.SingleTest 1: test = ', Test))

if(!data.table::is.data.table(VD)) {print('BasicLoop.SingleTest(): VD is NOT a data.table'); return(list(LoopMetrics = LM, ArgsList = ArgsList))}

if(missing(ModelID) || length(ModelID) == 0L || is.na(ModelID)) {
  ModelID <- Rappture:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ModelID')]]}, error=function(x) NULL), Type='character', Default='FC001')
  ArgsList$ModelID <- ModelID
}

# Ensure no model is found
ArgsList[['Model']] <- NULL

RunCheck <- LM[`RMSE On` > -5, .N]
if(RunCheck > 0L) RunCheck <- 1L else RunCheck <- 2L

# Build
if(Debug) print(paste0('BasicLoop.SingleTest 2'))

# trial = 1
# trial = 2
for(trial in seq_len(RunCheck)) {
  if(Debug) print(paste0('trial number: ', trial))

  ArgsList$DebugMode <- TRUE

  if(trial == 1L) ArgsList[[Test]] <- Plan[[Test]]$ArgsOn else ArgsList[[Test]] <- Plan[[Test]]$ArgsOff
  print(ArgsList)
  Output <- Rappture::Shiny.FC.Panel.Backtest(ArgsList, CodeList, DataList, ModelID, VD = VD, Debug = Debug, Algo = Algo)
  DataList <- Output$DataList; CodeList <- Output$CodeList; ArgsList <- Output$ArgsList; dlname <- Output$dlname
  totals <- DataList[[paste0(ModelID, dlname)]][['data']]
  if(trial == 1L) update_row <- LM[`RMSE On` > -5][, .N] + 1L
  print(paste0('rowNum = ', update_row))
  if(trial == 1L) {

    data.table::set(x = LM, i = eval(update_row), j = 'AvgError On', value = totals[.N, AvgError])
    data.table::set(x = LM, i = eval(update_row), j = 'RMSE On', value = totals[.N, RMSE])
    data.table::set(x = LM, i = eval(update_row), j = 'MAE On', value = totals[.N, MAE])

    if(RunCheck == 1L) {
      rowval <- LM[`RMSE Off` > -5, .N]
      data.table::set(x = LM, i = eval(update_row), j = 'AvgError Off', value = min(LM[eval(rowval), abs(`AvgError On`)], LM[eval(rowval), abs(`AvgError Off`)]))
      data.table::set(x = LM, i = eval(update_row), j = 'RMSE Off', value = min(LM[eval(rowval), `RMSE On`], LM[eval(rowval), `RMSE Off`]))
      data.table::set(x = LM, i = eval(update_row), j = 'MAE Off', value = min(LM[eval(rowval), `MAE On`], LM[eval(rowval), `MAE Off`]))
    }

  } else {
    data.table::set(x = LM, i = eval(update_row), j = 'AvgError Off', value = totals[.N, AvgError])
    data.table::set(x = LM, i = eval(update_row), j = 'RMSE Off', value = totals[.N, RMSE])
    data.table::set(x = LM, i = eval(update_row), j = 'MAE Off', value = totals[.N, MAE])
  }
}

# Make permanent for remainder of Loop & Update LM
act_lift <- LM[eval(update_row), `MAE On`] - LM[eval(update_row), `MAE Off`]
if(act_lift < Lift) {
  ArgsList[[Test]] <- Plan[[Test]]$ArgsOn
  n <- LM[,.N]
  data.table::set(x = LM, i = c(eval(update_row:n)), j = eval(Test), value = TRUE)
} else {
  ArgsList[[Test]] <- Plan[[Test]]$ArgsOff
}

#
# # Best model
# data.table::setorderv(x = LoopMetrics, cols = c('RMSE On'), order = 1, na.last = TRUE)
#
# #Store LoopMetrics in DataList
# mcn <- paste0(ModelID, '_ModelCompare')
# DataList[[mcn]][['data']] <- LoopMetrics
# DataList <- Rappture:::DM.DataListUpdate(dl = DataList, dn = mcn, Sample = FALSE)

# *******************************
#       XGBoost      ************
# *******************************


# *******************************
#       LightGBM     ************
# *******************************

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Backtest Cross Eval        ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# Build Query String
query <- Rappture::QueryBuilder(
  DB = 'KompsProcessed',
  Table = 'POS_Processed_Long_Daily_backward',
  Columns = c('ARTICLE','BRAND','CHILLED_Margin_PerDay','CUSTOMER_COD_char','DATE_ISO'),
  GroupByColumns = NULL,
  AggStat = 'AVG',
  Limit = NULL)

# Load Data from PostGRE
if(!exists('DataList')) DataList <- list()
DataList[['POS_Processed_Long_Daily_backward']] <- RemixAutoML::PostGRE_Query(
  Query = query,
  Host = 'localhost',
  CloseConnection = TRUE,
  DBName = 'KompsProcessed',
  User = 'postgres',
  Port = 5432,
  Password = 'Aa1028#@')$data

# Subset Data
if(!exists('DataList')) DataList <- list()
DataList[['FC_Train']] <- Rappture:::FilterLogicData(
  DataList[['POS_Processed_Long_Daily_backward']],
  FilterLogic = '<',
  FilterVariable = 'DATE_ISO',
  FilterValue = '2022-01-01',
  FilterValue2 = NULL)

# Forecasting: Data Args
ModelID <- 'FC001'
if(!exists('ArgsList')) ArgsList <- list()
ArgsList[['data']] <- data.table::copy(DataList[['FC_Train']])
ValidationData <- data.table::copy(DataList[['POS_Processed_Long_Daily_backward']])

ArgsList[['TargetColumnName']] <- 'CHILLED_Margin_PerDay'
ArgsList[['DateColumnName']] <- 'DATE_ISO'
ArgsList[['GroupVariables']] <- c('CUSTOMER_COD_char','ARTICLE','BRAND')
ArgsList[['TimeUnit']] <- 'day'
ArgsList[['TimeGroups']] <- ArgsList[['TimeUnit']]
ArgsList[['FC_Periods']] <- 30
ArgsList[['TimeWeights']] <- 0.9999
ArgsList[['TargetTransformation']] <- TRUE
ArgsList[['Methods']] <- 'LogPlus1'
ArgsList[['Difference']] <- FALSE

# Forecasting: Production Args
ArgsList[['TaskType']] <- 'CPU'
ArgsList[['NumGPU']] <- 1
ArgsList[['RoundPreds']] <- FALSE
ArgsList[['NonNegativePred']] <- FALSE
ArgsList[['SaveModel']] <- FALSE
ArgsList[['DebugMode']] <- TRUE
ArgsList[['ZeroPadSeries']] <- 'dynamic:meow'

# Forecasting: Feature Engineering Args
ArgsList[['CalendarVariables']] <- c('wday','wom','month','quarter')
ArgsList[['HolidayVariable']] <- NULL
ArgsList[['HolidayLookback']] <- NULL
ArgsList[['Quantiles_Selected']] <- NULL
ArgsList[['Lags']] <- c(1)
ArgsList[['MA_Periods']] <- NULL
ArgsList[['SD_Periods']] <- NULL
ArgsList[['Kurt_Periods']] <- NULL
ArgsList[['Skew_Periods']] <- NULL
ArgsList[['Quantile_Periods']] <- NULL
ArgsList[['DataTruncate']] <- FALSE
ArgsList[['TimeTrend']] <- NULL
AnomalyDetection_LowThreshold <- NULL
AnomalyDetection_HighThreshold <- NULL
if(length(AnomalyDetection_HighThreshold) > 0L && length(AnomalyDetection_LowThreshold) > 0L) {
  ArgsList[['AnomalyDetection']] <- list('tstat_high' = NULL, 'tstat_low' = NULL)
}

# Forecasting: Feature Enginnering Args
ArgsList[['NTrees']] <- 500
ArgsList[['Depth']] <- 6
ArgsList[['Langevin']] <- FALSE
ArgsList[['DiffusionTemperature']] <- 10000
ArgsList[['L2_Leaf_Reg']] <- 5
ArgsList[['ModelSizeReg']] <- 1.2
ArgsList[['MinDataInLeaf']] <- 1
ArgsList[['LearningRate']] <- 0.1
ArgsList[['BootStrapType']] <- 'No'
ArgsList[['GrowPolicy']] <- 'SymmetricTree'
ArgsList[['RandomStrength']] <- 1
ArgsList[['RSM']] <- 1
ArgsList[['SubSample']] <- 0.66
ArgsList[['BorderCount']] <- 128
ArgsList[['FeatureBorderType']] <- 'GreedyLogSum'
ArgsList[['ScoreFunction']] <- 'Cosine'

# Ensure date column is correct date type
Output <- Rappture::FC.DateCast(ArgsList, VD = ValidationData)
ArgsList <- Output$ArgsList; ValidationData <- Output$VD; rm(Output); gc()

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Reconcile FC_Periods and set to TrainOnFull == TRUE
ArgsList <- Rappture::FC.FCPeriods(ArgsList, VD = ValidationData)

# Generate Forecast for Backtest
Output <- do.call(what = RemixAutoML::AutoCatBoostCARMA, args = ArgsList)

# Store Output Data
if(!exists('CrossEval')) CrossEval <- FALSE
if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'
DataList[[paste0(ModelID, dlname)]] <- Output$Forecast
DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]
data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]
DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]

# Error Metrics Raw: Raw data granularity and DateColumnName still exist
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, Debug = Debug)

# Error Metrics: Total Full Period
DataList <- Rappture::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)

# Plotting: BoxPlot
data1 <- DataList[['FC001_CE_Raw']]
data1 <- Rappture:::PreparePlotData(
  SubsetOnly = TRUE,
  data = data1,
  Aggregate = 'mean',
  TargetVariable = 'AvgError',
  DateVariable = 'DATE_ISO',
  GroupVariables = NULL,
  G1Levels = NULL,
  G2Levels = NULL,
  G3Levels = NULL)

# Build Plot
Rappture:::AutoPlotter(
  dt = data1,
  PlotType = 'BoxPlot',
  YVar = c('AvgError'),
  XVar = c('DATE_ISO'),
  ZVar = NULL,
  Bins = 30,
  ColorVariables = NULL,
  FacetVar1 = NULL,
  YTicks = 'Default',
  XTicks = 'Default',
  OutlierSize = 0.01,
  OutlierColor = 'white',
  FillColor = '#88ff00',
  Density = FALSE,
  BarPlotAggMethod = 'mean',
  GamFitScatter = FALSE,
  TextSize = 14,
  TextColor = 'white',
  AngleX = 90,
  AngleY = 0,
  ChartColor = '#000000bf',
  BorderColor = 'white',
  GridColor = 'white',
  BackGroundColor = '#17108db8')


# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# QA                         ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# library(RemixAutoML)
# library(data.table)
# library(lubridate)
#
# FillMissingDates = FALSE
#
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CalendarTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CrossRowOperations.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CARMA-HelperFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
#
# data = ArgsList[['data']]
# XREGS = NULL
#
# TargetColumnName <- 'CHILLED_Margin_PerDay'
# DateColumnName <- 'DATE_ISO'
# GroupVariables <- c('CUSTOMER_COD_char','ARTICLE','BRAND') # c('BRAND','ARTICLE','CUSTOMER_COD_char')   # c('CUSTOMER_COD_char','ARTICLE','BRAND') I think this is wrong
# TimeUnit <- 'day'
# TimeGroups <- ArgsList[['TimeUnit']]
# FC_Periods <- 120
# TimeWeights <- 1
# TargetTransformation <- TRUE
# Methods <- 'LogPlus1'
# Difference <- FALSE
# TaskType <- 'CPU'
# NumGPU <- 1L
# RoundPreds <- FALSE
# NonNegativePred <- FALSE
# SaveModel <- FALSE
# DebugMode <- TRUE
# ZeroPadSeries <- 'dynamic:meow'
# CalendarVariables <- c('wday','wom','month','quarter')
# HolidayVariable <- c('EasterGroup','ChristmasGroup','OtherEcclesticalFeasts')
# HolidayLookback <- 2
# Quantiles_Selected <- NULL
# SplitRatios <- c(0.84,0.08,0.08)
# Lags <- c(1L,7L)
# MA_Periods <- 7L
# DataTruncate <- FALSE
# TimeTrend <- FALSE
# AnomalyDetection <- list('tstat_high' = 6, 'tstat_low' = -6)
# Langevin <- FALSE
# DiffusionTemperature <- 10000
# NTrees <- 500
# Depth <- 6
# L2_Leaf_Reg <- 5
# ModelSizeReg <- 1.2
# MinDataInLeaf <- 1
# LearningRate <- 0.1
# SubSample <- 0.66
# RandomStrength <- 1
# RSM <- 1
# BorderCount <- 128
# BootStrapType <- 'No'
# GrowPolicy <- 'SymmetricTree'
# FeatureBorderType <- 'GreedyLogSum'
# ScoreFunction <- 'Cosine'
# SD_Periods <- NULL
# Kurt_Periods <- NULL
# Skew_Periods <- NULL
# Quantiles_Periods <- NULL
#
# HolidayLags = NULL
# HolidayMovingAverages = NULL
# Quantile_Periods = NULL
# HierarchGroups <- NULL
# PartitionType <- 'random'
# FourierTerms = 0
# TimeTrendVariable = FALSE
# SaveDataPath = NULL
# LossFunction = 'RMSE'
# EvalMetric = 'RMSE'
# PDFOutputPath = NULL
# GridTune = FALSE
# SamplingUnit = 'Group'
# ModelCount = 30
# NumOfParDepPlots = 0
# PassInGrid = NULL
# ReturnShap = FALSE
# EncodingMethod = 'MEOW'
# EvalMetricValue = 1.5
# LossFunctionValue = 1.5
# MaxRunsWithoutNewWinner = 20
# Timer = TRUE
#
# # CarmaScore
# Type = 'catboost'
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
# RoundPreds.=RoundPreds
# UpdateData.=UpdateData
# FactorList.=TestModel$FactorLevelsList
# EncodingMethod.=EncodingMethod
# dt = data




# ----

# ----









