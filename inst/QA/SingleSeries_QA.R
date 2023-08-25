# QA Single Series
library(data.table)

ArgsList <- list()
ArgsList[['data']] <- data.table::fread("C:/Users/Bizon/Documents/GitHub/Sample_Data_-_Price_Data_3.csv")
ArgsList[['TargetColumnName']] <- "Price"
ArgsList[['DateColumnName']] <- "Date"
ArgsList[['MaxLags']] <- 4
ArgsList[['MaxMovingAverages']] <- 4
ArgsList[['MaxSeasonalPeriods']] <- 1
ArgsList[['TimeUnit']] <- "day"
ArgsList[['FCPeriods']] <- 14
ArgsList[['EvaluationMetric']] <- "MSE"
ArgsList[['TrainWeighting']] <- 0.50

check <- class(ArgsList[['data']][[ArgsList[['DateColumnName']]]])[1L]
if(check %in% c("numeric", "integer", "character", "factor", "logical")) {
  if(!(tolower(ArgsList[['TimeUnit']]) %chin% c('1min','5min','10min','15min','30min','hour'))) {
    x <- ArgsList[['data']][1L, get(ArgsList[['DateColumnName']])]
    x1 <- lubridate::guess_formats(x, orders = c('mdY', 'BdY', 'Bdy', 'bdY', 'bdy', 'mdy', 'dby', 'Ymd', 'Ydm'))
    ArgsList[['data']][, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]), tryFormats = x1)]
  } else {
    ArgsList[['data']][, eval(ArgsList[['DateColumnName']]) := as.POSIXct(get(ArgsList[['DateColumnName']]))]
  }
}

data <- ArgsList[['data']]
TBATS_Artifacts_Build <- tryCatch({
  AutoQuant:::TimeSeriesDataPrepare(
    data = data,
    TargetName = ArgsList[['TargetColumnName']],
    DateName = ArgsList[['DateColumnName']],
    Lags = ArgsList[['MaxLags']],
    SeasonalLags = 0,
    MovingAverages = ArgsList[['MaxMovingAverages']],
    SeasonalMovingAverages = ArgsList[['MaxSeasonalPeriods']],
    TimeUnit = ArgsList[['TimeUnit']],
    FCPeriods = 1,
    HoldOutPeriods = ArgsList[['FCPeriods']],
    TSClean = TRUE,
    ModelFreq = TRUE,
    FinalBuild = FALSE)
}, error = function(x) {
  print("AutoQuant:::TimeSeriesDataPrepare failed")
  NULL
})

# data = data
# TargetName = ArgsList[['TargetColumnName']]
# DateName = ArgsList[['DateColumnName']]
# Lags = ArgsList[['MaxLags']]
# SeasonalLags = 0
# MovingAverages = ArgsList[['MaxMovingAverages']]
# SeasonalMovingAverages = ArgsList[['MaxSeasonalPeriods']]
# TimeUnit = ArgsList[['TimeUnit']]
# FCPeriods = 1
# HoldOutPeriods = ArgsList[['FCPeriods']]
# TSClean = TRUE
# ModelFreq = TRUE
# FinalBuild = FALSE

cores <- parallel::detectCores()
if(cores >= 4L) {
  NumberCores <- 4L
} else {
  NumberCores <- cores
}

ExperimentGrid <- tryCatch({
  AutoQuant:::ParallelAutoTBATS(
    MetricSelection = ArgsList[['EvaluationMetric']],
    Output = TBATS_Artifacts_Build,
    NumCores = NumberCores,
    TrainValidateShare = ArgsList[['TrainWeighting']])
}, error = function(x) {
  print("AutoQuant:::ParallelAutoETS failed")
  NULL
})


################ Forecasting
FCPeriods = 5
TBATS_Artifacts_Score <- tryCatch({
  AutoQuant:::TimeSeriesDataPrepare(
    data = data,
    TargetName = ArgsList[['TargetColumnName']],
    DateName = ArgsList[['DateColumnName']],
    Lags = as.integer(ExperimentGrid[1L, Lags]),
    SeasonalLags = 0,
    MovingAverages = as.integer(ExperimentGrid[1L, MovingAverages]),
    SeasonalMovingAverages = 0,
    TimeUnit = ArgsList[['TimeUnit']],
    FCPeriods = FCPeriods,
    HoldOutPeriods = 0,
    TSClean = TRUE,
    ModelFreq = TRUE,
    FinalBuild = TRUE)
}, error = function(x) {
  print("AutoQuant:::TimeSeriesDataPrepare failed")
  print(paste0("data[,.N] = ", tryCatch({data[,.N]}, error = function(x) NULL)))
  print(paste0("TargetName = ", ArgsList[['TargetColumnName']]))
  print(paste0("DateName = ", ArgsList[['DateColumnName']]))
  print(paste0("Lags = ", as.integer(ExperimentGrid[1L, Lags])))
  print(paste0("SeasonalLags = ", 0))
  print(paste0("MovingAverages = ", as.integer(ExperimentGrid[1L, MovingAverages])))
  print(paste0("SeasonalMovingAverages = ", 0))
  print(paste0("TimeUnit = ", ArgsList[['TimeUnit']]))
  print(paste0("FCPeriods = ", FCPeriods))
  print(paste0("HoldOutPeriods = ", 0))
  print(paste0("TSClean = ", TRUE))
  print(paste0("ModelFreq = ", TRUE))
  print(paste0("FinalBuild = ", TRUE))
  NULL
})


# Generate Final TBATS Forecast ----
# repeat so long as there isn't a forecast generated or no more models left to use
# upon generating a final forecast, a new model is built utilizing 100% of training data
# Sometimes, this new model doesn't get built
#   If it doesn't get built then the next best version will be attempted (based on ExperimentGrid data.table)
#   the ExperimentGrid is sorted by paste('Blended_', ArgsList[['EvaluationMetric']])
counter <- ExperimentGrid[, .N] + 1L
repeat {
  counter <- counter - 1L
  if(counter == 0) break
  ForecastOutput <- tryCatch({
    AutoQuant:::FinalBuildTBATS(
      ModelOutputGrid = ExperimentGrid,
      SavePath = NULL,
      TimeSeriesPrepareOutput = TBATS_Artifacts_Score,
      FCPeriods = FCPeriods,
      NumberModelsScore = 1,
      MetricSelection = ArgsList[['EvaluationMetric']],
      DebugMode = DebugFC,
      ByDataType = FALSE)
  }, error = function(x) {
    print("AutoQuant:::FinalBuildTBATS failed")
    print(paste0("counter = ", counter))
    print(paste0("ModelOutputGrid = ", ExperimentGrid))
    print(paste0("SavePath = NULL"))
    print(paste0("TimeSeriesPrepareOutput = ", TBATS_Artifacts_Score))
    print(paste0("FCPeriods = ", FCPeriods))
    print(paste0("NumberModelsScore = 1"))
    print(paste0("MetricSelection = ", ArgsList[['EvaluationMetric']]))
    print(paste0("DebugMode = ", DebugFC))
    print(paste0("ByDataType = FALSE"))
    NULL
  })
  if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][["Forecast"]])) {
    break
  } else {
    if(length(ForecastOutput) == 0L) {
      print("length(ForecastOutput) == 0L")
    } else if(!is.na(ForecastOutput[.N][["Forecast"]])) {
      print("!is.na(ForecastOutput[.N][['Forecast']]) == FALSE")
    }
  }
}








