


data <- data.table::fread('https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1')
data[, IsHoliday := NULL]

data <- Rodeo::TimeSeriesFill(
  data = data,
  TargetColumn = "Weekly_Sales",
  DateColumnName = "Date",
  GroupVariables = c("Store","Dept"),
  TimeUnit = "weeks",
  FillType = "maxmax",
  MaxMissingPercent = 0.05,
  SimpleImpute = FALSE)

data.table::fwrite(data, file = "C:/Users/Bizon/Documents/GitHub/rappwd/WalmartFC.csv")


data = data
TargetColumn = "Weekly_Sales"
DateColumnName = "Date"
GroupVariables = c("Store","Dept")
TimeUnit = "days"
FillType = "maxmax"
MaxMissingPercent = 0.05
SimpleImpute = FALSE



AutoQuant::AutoCatBoostRegression(
  OutputSelection = c('Importances', 'Score_TrainData'),
  data = data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/POS_Processed_Long_Daily_backward.csv"),
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Daily Margin",
  FeatureColNames = c("Customer","Brand","Category","Beverage Flavor","Daily Liters","Daily Units"),
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = names(data)[!names(data) %in% c("Daily Margin","Customer","Brand","Category","Beverage Flavor","Daily Liters","Daily Units")],
  EncodeMethod = 'credibility',
  TransformNumericColumns = "Daily Margin",
  Methods = 'LogPlus1',
  TrainOnFull = FALSE,
  task_type = 'GPU',
  NumGPUs = 1,
  DebugMode = FALSE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  ModelID = 'FirstModel',
  model_path = NULL,
  metadata_path = NULL,
  SaveInfoToPDF = FALSE,
  eval_metric = 'RMSE',
  eval_metric_value = 1.5,
  loss_function = 'RMSE',
  loss_function_value = 1.5,
  grid_eval_metric = 'r2',
  NumOfParDepPlots = 0L,
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  BaselineComparison = 'default',
  MetricPeriods = 10L,
  Trees = 500L,
  Depth = 9,
  L2_Leaf_Reg = 3.0,
  RandomStrength = 1,
  BorderCount = 254,
  LearningRate = NULL,
  RSM = 1,
  BootStrapType = NULL,
  GrowPolicy = 'SymmetricTree',
  langevin = FALSE,
  diffusion_temperature = 10000,
  model_size_reg = 0.5,
  feature_border_type = 'GreedyLogSum',
  sampling_unit = 'Object',
  subsample = NULL,
  score_function = 'Cosine',
  min_data_in_leaf = 1)

devtools::load_all(path = "C:/Users/Bizon/Documents/GitHub/AutoQuant/R")
library(Rodeo)

OutputSelection = c('Importances', 'Score_TrainData')
data = data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/Fake_Beverage_Data.csv")
ReturnShap = TRUE
ValidationData = NULL
TestData = NULL
TargetColumnName = "Daily Margin"
FeatureColNames = c("Customer","Brand","Category","Beverage Flavor","Daily Liters","Daily Units")
PrimaryDateColumn = NULL
WeightsColumnName = NULL
IDcols = names(data)[!names(data) %in% c("Daily Margin","Customer","Brand","Category","Beverage Flavor","Daily Liters","Daily Units")]
EncodeMethod = 'credibility'
TransformNumericColumns = "Daily Margin"
Methods = 'LogPlus1'
TrainOnFull = FALSE
task_type = 'GPU'
NumGPUs = 1
DebugMode = FALSE
ReturnModelObjects = TRUE
SaveModelObjects = FALSE
ModelID = 'FirstModel'
model_path = NULL
metadata_path = NULL
SaveInfoToPDF = FALSE
eval_metric = 'RMSE'
eval_metric_value = 1.5
loss_function = 'RMSE'
loss_function_value = 1.5
grid_eval_metric = 'r2'
NumOfParDepPlots = 0L
PassInGrid = NULL
GridTune = FALSE
MaxModelsInGrid = 30L
MaxRunsWithoutNewWinner = 20L
MaxRunMinutes = 24L*60L
BaselineComparison = 'default'
MetricPeriods = 10L
Trees = 500L
Depth = 9
L2_Leaf_Reg = 3.0
RandomStrength = 1
BorderCount = 254
LearningRate = NULL
RSM = 1
BootStrapType = NULL
GrowPolicy = 'SymmetricTree'
langevin = FALSE
diffusion_temperature = 10000
model_size_reg = 0.5
feature_border_type = 'GreedyLogSum'
sampling_unit = 'Object'
subsample = NULL
score_function = 'Cosine'
min_data_in_leaf = 1


# TrainData preds and actuals not being transformed back
ModelType='regression'
TrainOnFull.=TRUE
TestDataCheck=FALSE
FinalTestTarget.=FinalTestTarget
TestTarget.=TestTarget
TrainTarget.=TrainTarget
TrainMerge.=TrainMerge
TestMerge.=TestMerge
dataTest.=NULL
data.=dataTrain
predict.=predict
TargetColumnName.=TargetColumnName
SaveModelObjects. = SaveModelObjects
metadata_path.=metadata_path
model_path.=model_path
ModelID.=ModelID
LossFunction.=NULL
TransformNumericColumns.=NULL
GridTune.=GridTune
TransformationResults.=NULL
TargetLevels.=NULL


# R-SQ issue
SaveModelObjects.=FALSE
data.=data
ValidationData.=TrainData
TrainOnFull.=TrainOnFull
LossFunction.=LossFunction
EvalMetric.=EvalMetric
TargetColumnName.=TargetColumnName
ModelID.=ModelID
model_path.=model_path
metadata_path.=metadata_path



