# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 10000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Copy data
data1 <- data.table::copy(data)

# Define features names
Features <- c(names(data1)[!names(data1) %in% c('IDcol_1','IDcol_2','Adrian')])

# Run function
ModelObject <- AutoQuant::AutoCatBoostRegression(

  # GPU or CPU and the number of available GPUs
  task_type = 'GPU',
  NumGPUs = 1,
  NumOfParDepPlots = length(Features),
  Trees = 100,

  # Metadata args
  OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
  ModelID = 'Test_Model_1',
  model_path = getwd(),
  metadata_path = getwd(),
  ReturnModelObjects = TRUE,

  # Data args
  data = data1,
  TargetColumnName = 'Adrian',
  FeatureColNames = Features,
  IDcols = c('IDcol_1','IDcol_2'),
  TransformNumericColumns = 'Adrian',
  Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'))

# Build report
AutoQuant::ModelInsightsReport(

  # Meta info
  TargetColumnName = 'Adrian',
  PredictionColumnName = 'Predict',
  FeatureColumnNames = Features,
  DateColumnName = NULL,

  # Control options
  TargetType = 'regression',
  ModelID = 'Test_Model_1',
  Algo = 'catboost',
  OutputPath = "C:/Users/Bizon/Documents/GitHub",
  ModelObject = ModelObject)

#####################################################################################################

# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = TRUE,
  MultiClass = FALSE)

# Copy data (used for scoring below``)
data1 <- data.table::copy(data)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Feature Colnames
Features <- names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")]

# AutoCatBoostClassifier
TestModel <- AutoQuant::AutoCatBoostClassifier(

  # GPU or CPU and the number of available GPUs
  task_type = "CPU",
  NumGPUs = 1,

  # Metadata arguments
  OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
  ModelID = "Test_Model_1",
  model_path = normalizePath("./"),
  metadata_path = normalizePath("./"),
  SaveModelObjects = FALSE,
  ReturnModelObjects = TRUE,
  SaveInfoToPDF = FALSE,

  # Data arguments
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = Features,
  PrimaryDateColumn = "DateTime",
  WeightsColumnName = "Weights",
  ClassWeights = c(1L,1L),
  IDcols = c("IDcol_1","IDcol_2","DateTime"),

  # Model evaluation
  CostMatrixWeights = c(2,0,0,1),
  EvalMetric = "MCC",
  LossFunction = "Logloss",
  grid_eval_metric = "Utility",
  MetricPeriods = 10L,
  NumOfParDepPlots = 3,

  # Grid tuning arguments
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  BaselineComparison = "default",

  # ML args
  Trees = 100L,
  Depth = 4L,
  LearningRate = NULL,
  L2_Leaf_Reg = NULL,
  RandomStrength = 1,
  BorderCount = 128,
  RSM = 0.80,
  BootStrapType = "Bayesian",
  GrowPolicy = "SymmetricTree",
  langevin = FALSE,
  diffusion_temperature = 10000,
  model_size_reg = 0.5,
  feature_border_type = "GreedyLogSum",
  sampling_unit = "Object",
  subsample = NULL,
  score_function = "Cosine",
  min_data_in_leaf = 1,
  DebugMode = TRUE)


# Insights Report
AutoQuant::ModelInsightsReport(

  # Meta info
  TargetColumnName = 'Adrian',
  PredictionColumnName = 'p1',
  FeatureColumnNames = Features,
  DateColumnName = NULL,

  # Control options
  TargetType = 'classification',
  ModelID = 'Test_Model_1',
  Algo = 'catboost',
  OutputPath = "C:/Users/Bizon/Documents/GitHub",
  ModelObject = TestModel)

#####################################################################################################

# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = FALSE,
  MultiClass = TRUE)

# Copy data (used for scoring below``)
data1 <- data.table::copy(data)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Feature Colnames
Features <- names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","Adrian","DateTime")]

# Run function
TestModel <- AutoQuant::AutoCatBoostMultiClass(

  # GPU or CPU and the number of available GPUs
  task_type = "GPU",
  NumGPUs = 1,

  # Metadata arguments
  OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
  ModelID = "Test_Model_1",
  model_path = normalizePath("./"),
  metadata_path = normalizePath("./"),
  SaveModelObjects = FALSE,
  ReturnModelObjects = TRUE,

  # Data arguments
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = Features,
  PrimaryDateColumn = "DateTime",
  WeightsColumnName = "Weights",
  ClassWeights = c(1L,1L,1L,1L,1L),
  IDcols = c("IDcol_1","IDcol_2","DateTime"),

  # Model evaluation
  eval_metric = "MCC",
  loss_function = "MultiClassOneVsAll",
  grid_eval_metric = "Accuracy",
  MetricPeriods = 10L,

  # Grid tuning arguments
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  BaselineComparison = "default",

  # ML args
  Trees = 100L,
  Depth = 4L,
  LearningRate = 0.01,
  L2_Leaf_Reg = 1.0,
  RandomStrength = 1,
  BorderCount = 128,
  langevin = FALSE,
  diffusion_temperature = 10000,
  RSM = 0.80,
  BootStrapType = "Bayesian",
  GrowPolicy = "SymmetricTree",
  model_size_reg = 0.5,
  feature_border_type = "GreedyLogSum",
  sampling_unit = "Group",
  subsample = NULL,
  score_function = "Cosine",
  min_data_in_leaf = 1,
  DebugMode = TRUE)


# Insights Report
AutoQuant::ModelInsightsReport(

  # Meta info
  TargetColumnName = 'Adrian',
  PredictionColumnName = 'Predict',
  FeatureColumnNames = Features,
  DateColumnName = NULL,

  # Control options
  TargetType = 'multiclass',
  ModelID = 'Test_Model_1',
  Algo = 'catboost',
  OutputPath = "C:/Users/Bizon/Documents/GitHub",
  ModelObject = TestModel)
