# Regression ----
ModelObject <- AutoQuant::AutoXGBoostRegression(
  OutputSelection = c('Importances', 'EvalMetrics', 'Score_TrainData'),
  data = data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/FakeBevData.csv"),
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Daily Margin",
  FeatureColNames = c("Customer","Brand","Category","Beverage Flavor","Daily Liters","Daily Units"),
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = names(data)[!names(data) %in% c("Daily Margin","Customer","Brand","Category","Beverage Flavor","Daily Liters","Daily Units")],
  model_path = NULL,
  metadata_path = NULL,
  DebugMode = FALSE,
  SaveInfoToPDF = FALSE,
  ModelID = 'FirstModel',
  EncodingMethod = 'credibility',
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  TransformNumericColumns = NULL,
  Methods = c('Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
  Verbose = 0L,
  NumOfParDepPlots = 3L,
  NThreads = parallel::detectCores(),
  LossFunction = 'reg:squarederror',
  eval_metric = 'rmse',
  grid_eval_metric = 'r2',
  TreeMethod = 'hist',
  GridTune = FALSE,
  BaselineComparison = 'default',
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  PassInGrid = NULL,
  early_stopping_rounds = 100L,
  Trees = 50L,
  num_parallel_tree = 1,
  eta = NULL,
  max_depth = NULL,
  min_child_weight = NULL,
  subsample = NULL,
  colsample_bytree = NULL,
  alpha = 0,
  lambda = 1)

ModelObject$VariableImportance
ModelObject$


data[, ClassTarget := data.table::fifelse(`Daily Margin` > 25, 1, 0)]
data.table::fwrite(data, file = "C:/Users/Bizon/Documents/GitHub/rappwd/Fake_Beverage_Data.csv")


# Load CSV
#filename <- basename('POS_Processed_Long_Daily_backward.csv')
#DataList[[filename]] <- Rappture:::LoadCSV(Infile = file.choose())
ArgsList <- list()
# ML Data
ArgsList[['data']] <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/Fake_Beverage_Data.csv")

# ML Set Metadata Parameters
ArgsList[['TargetColumnName']] <- c('Daily Margin')
ArgsList[['FeatureColNames']] <- c('Customer','Brand','Category','Beverage Flavor','Daily Liters','Daily Units')
# Additional Metadata Parameters
ArgsList[['OutputSelection']] <- c('Importances','EvalMetrics','Score_TrainData')
ArgsList[['TrainOnFull']] <- FALSE
ArgsList[['ModelID']] <- 'Model1'
ArgsList[['DebugMode']] <- TRUE
ArgsList[['model_path']] <- 'C:/Users/Bizon/AppData/Local/R/win-library/4.2/Rappture/shiny-apps/Rappture'
ArgsList[['metadata_path']] <- 'C:/Users/Bizon/AppData/Local/R/win-library/4.2/Rappture/shiny-apps/Rappture'
ArgsList[['SaveModelObjects']] <- FALSE
ArgsList[['ReturnModelObjects']] <- TRUE
ArgsList[['NumOfParDepPlots']] <- 1

# XGBoost ML Parameters
ArgsList[['Trees']] <- 10
ArgsList[['max_depth']] <- 8
ArgsList[['eta']] <- 0.1
ArgsList[['min_child_weight']] <- 1
ArgsList[['subsample']] <- 1
ArgsList[['colsample_bytree']] <- 1

# XGBoost Evaluation Parameters
ArgsList[['LossFunction']] <- 'reg:squarederror'
ArgsList[['eval_metric']] <- 'rmse'
ArgsList[['grid_eval_metric']] <- 'r2'

# ML Build Model
ArgsList[['PrimaryDateColumn']] <- NULL
ArgsList[['SaveInfoToPDF']] <- FALSE
ModelOutputList <- do.call(AutoQuant::AutoXGBoostRegression, ArgsList)
ModelOutputList$VariableImportance


devtools::load_all(path = "C:/Users/Bizon/Documents/GitHub/AutoQuant/R")
library(Rodeo)

td <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/POS_Processed_Long_Daily_backward.csv")
output <- Rodeo::AutoDataPartition(data = td, NumDataSets = 3, Ratios = c(0.7,0.2,0.1), PartitionType = "random")
td <- output$TrainData
vd <- output$ValidationData
ted <- output$TestData


OutputSelection = c('Importances','Score_TrainData')
data = td
TrainOnFull = FALSE
ValidationData = vd
TestData = ted
TargetColumnName = "Daily Margin"
FeatureColNames = c("Customer","Brand","Category","Beverage Flavor","Daily Liters","Daily Units")
PrimaryDateColumn = NULL
WeightsColumnName = NULL
IDcols = names(data)[!names(data) %in% c("Daily Margin","Customer","Brand","Category","Beverage Flavor","Daily Liters","Daily Units")]
model_path = NULL
metadata_path = NULL
DebugMode = TRUE
SaveInfoToPDF = FALSE
ModelID = 'FirstModel'
EncodingMethod = 'credibility'
ReturnFactorLevels = TRUE
ReturnModelObjects = TRUE
SaveModelObjects = FALSE
TransformNumericColumns = "Daily Margin"
Methods = 'LogPlus1'
Verbose = 0L
NumOfParDepPlots = 0L
NThreads = parallel::detectCores()
LossFunction = 'reg:squarederror'
eval_metric = 'rmse'
grid_eval_metric = 'r2'
TreeMethod = 'hist'
GridTune = FALSE
BaselineComparison = 'default'
MaxModelsInGrid = 10L
MaxRunsWithoutNewWinner = 20L
MaxRunMinutes = 24L*60L
PassInGrid = NULL
early_stopping_rounds = 100L
Trees = 50L
num_parallel_tree = 1
eta = NULL
max_depth = NULL
min_child_weight = NULL
subsample = NULL
colsample_bytree = NULL
alpha = 0
lambda = 1

# Data Prep
Algo='xgboost'
ModelType='regression'
data.=data
ValidationData.=ValidationData
TestData.=TestData
TargetColumnName.=TargetColumnName
FeatureColNames.=FeatureColNames
WeightsColumnName.=WeightsColumnName
IDcols.=IDcols
TransformNumericColumns.=TransformNumericColumns
Methods.=Methods
ModelID.=ModelID
model_path.=model_path
TrainOnFull.=TrainOnFull
SaveModelObjects.=SaveModelObjects
ReturnFactorLevels.=ReturnFactorLevels
EncodingMethod.=EncodingMethod
DebugMode.=DebugMode


# Transform
data = data.
ColumnNames = TransformNumericColumns.
Methods = Methods.
Path = model_path.
TransID = ModelID.
SaveOutput = SaveModelObjects.



# XGBoostValidationData
model.=model
TestData.=NULL
ModelType='regression'
TrainOnFull.=TRUE
TestDataCheck=FALSE
FinalTestTarget.=FinalTestTarget
TestTarget.=TestTarget
TrainTarget.=TrainTarget
TrainMerge.=TrainMerge
TestMerge.=TestMerge
dataTest.=dataTest
data.=dataTrain
predict.=predict
TargetColumnName.=TargetColumnName
SaveModelObjects. = SaveModelObjects
metadata_path.=metadata_path
model_path.=model_path
ModelID.=ModelID
LossFunction.=NULL
TransformNumericColumns.=TransformNumericColumns
GridTune.=GridTune
TransformationResults.=TransformationResults
TargetLevels.=NULL

######################################################################################

# Classification ----

######################################################################################

# MultiClass ----
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# Run function
ModelObject <- AutoQuant::AutoXGBoostMultiClass(

  # GPU or CPU
  TreeMethod = "hist",
  NThreads = parallel::detectCores(),

  # Metadata args
  OutputSelection = c("Importances","EvalMetrics","Score_TrainData"),
  model_path = normalizePath("./"),
  metadata_path = normalizePath("./"),
  ModelID = "Test_Model_1",
  EncodingMethod = "binary",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,

  # Data args
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in%
                                  c("IDcol_1", "IDcol_2","Adrian")],
  WeightsColumnName = NULL,
  IDcols = c("IDcol_1","IDcol_2"),

  # Model evaluation args
  eval_metric = "merror",
  LossFunction = 'multi:softprob',
  grid_eval_metric = "accuracy",
  NumOfParDepPlots = 3L,

  # Grid tuning args
  PassInGrid = NULL,
  GridTune = FALSE,
  BaselineComparison = "default",
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  Verbose = 1L,
  DebugMode = FALSE,

  # ML args
  Trees = 50L,
  eta = 0.05,
  max_depth = 4L,
  min_child_weight = 1.0,
  subsample = 0.55,
  colsample_bytree = 0.55)








