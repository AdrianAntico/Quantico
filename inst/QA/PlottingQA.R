# Query postgres
dt <- Rappture::DM.pgQuery(
  Host = 'localhost',
  DataBase = 'KompsProcessed',
  SELECT = c('ARTICLE',
             'BRAND',
             'Category',
             'CHILLED_Liters_PerDay',
             'CHILLED_Margin_PerDay',
             'CHILLED_Net_Revenue_PerDay',
             'CHILLED_Units_PerDay',
             'Total_Liters_PerDay',
             'Total_Margin_PerDay',
             'Total_Net_Revenue_PerDay',
             'Total_Units_PerDay',
             'CUSTOMER_COD_char',
             'DATE_ISO'),
  AggStat = 'AVG',
  FROM = 'POS_Processed_Long_Daily_backward',
  GroupBy = NULL,
  SamplePercent = 1,
  User = 'postgres',
  Port = 5432,
  Password = 'Aa1028#@')

dt[, OutlierHigh := quantile(CHILLED_Margin_PerDay, probs = 0.95), by = ARTICLE]
dt[, BinaryTarget := data.table::fifelse(CHILLED_Margin_PerDay > OutlierHigh, 1, 0)]
dt[, OutlierHigh := NULL]
data.table::setcolorder(dt, c(ncol(dt), 1:(ncol(dt)-1)))
data.table::fwrite(dt, file.choose())

x <- AutoQuant:::BinaryMetrics(
  TargetColumnName. = "BinaryTarget",
  CostMatrixWeights. = c(0,1,1,0),
  ValidationData. = data,
  SaveModelObjects. = FALSE)

Metrics = c("Utility","MCC","Accuracy","F1_Score","F2_Score","F0.5_Score","ThreatScore","TPR","TNR","FNR","FPR","FDR","FOR")
xx <- data.table::melt.data.table(
  data = x,
  id.vars = "Threshold",
  measure.vars = Metrics)


dt <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/rappwd/BinaryMetrics.csv"))

# Stocks data
# tick <- "TSLA"
# 1 second to pull
start <- Sys.time()
StockSymbolsData <- Rappture:::LoadCSV(Infile = system.file(package = "Rappture", "shiny-apps", "Rappture", "ticker_data.csv"))
tryCatch({Rappture::DM.pgRemoveTable(DataBase = "RemixAutoML", Table = "stocks", Host = "localhost", User = "postgres", Port = 5432, Password = "Aa1028#@")}, error = function(x) print("table didn't exist"))
for(tick in StockSymbolsData$ticker) {# tick <- "TSLA"
  Sys.sleep(12L)
  print(paste0("Ticker: ", tick, " :: is currently running"))
  nam <- StockSymbolsData[ticker == eval(tick)]$name
  StockDataOutput <- AutoPlots::StockData(
    PolyOut = NULL,
    Symbol = tick,
    CompanyName = nam,
    Metric = 'Stock Price',
    TimeAgg = 'days',
    StartDate = '2022-01-01',
    EndDate = '2022-12-31',
    APIKey = 'hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20',
    timeElapsed = 0)

  AutoPlots::Plot.Stock(
    StockDataOutput,
    Type = 'candlestick',
    Metric = "Stock Price",
    PlotEngineType = "Echarts",
    Width = NULL,
    Height = NULL,
    EchartsTheme = "macarons",
    TextColor = "white",
    ShadowBlur = 0,
    ShadowColor = "black",
    ShadowOffsetX = 0,
    ShadowOffsetY = 0,
    title.fontSize = 22,
    title.fontWeight = "bold",
    title.textShadowColor = '#63aeff',
    title.textShadowBlur = 3,
    title.textShadowOffsetY = 1,
    title.textShadowOffsetX = -1,
    Color = "green",
    Color0 = "red",
    BorderColor = "transparent",
    BorderColor0 = "transparent",
    BorderColorDoji = "transparent",
    xaxis.fontSize = 14,
    yaxis.fontSize = 14)


StockDataOutput
Type = 'candlestick'
Metric = "Stock Price"
PlotEngineType = "Echarts"
Width = NULL
Height = NULL
EchartsTheme = "macarons"
TextColor = "white"
ShadowBlur = 0
ShadowColor = "black"
ShadowOffsetX = 0
ShadowOffsetY = 0
title.fontSize = 14
title.fontWeight = "bold"
title.textShadowColor = '#63aeff'
title.textShadowBlur = 3
title.textShadowOffsetY = 1
title.textShadowOffsetX = -1
Color = "green"
Color0 = "red"
BorderColor = "transparent"
BorderColor0 = "transparent"
BorderColorDoji = "transparent"
xaxis.fontSize = 14
yaxis.fontSize = 14




  dt <- StockDataOutput$results
  dt[, CompanyName := nam]
  dt[, Ticker := StockDataOutput$Symbol]
  dt <- merge(dt, StockSymbolsData, by.x = "Ticker", by.y = "ticker", all = FALSE)
  data.table::set(dt, j = c("last_updated_utc","name"), value = NULL)
  data.table::setcolorder(x = dt, neworder = c(
   13,14,12,1,10,11,9,2:8
  ))
  print(paste0("Accumulated run time: ", round(difftime(end, start, "min")), " minutes"))
}


Rappture::DM.pgAppend(
  data = dt,
  DataBase = "RemixAutoML",
  Table = "stocks",
  Append = TRUE,
  Host = "localhost",
  User = "postgres",
  Port = 5432,
  Password = "Aa1028#@")

Rappture::DM.pgQuery(SELECT = "v", FROM = "stocks",
  #data = dt,
  DataBase = "RemixAutoML",
  #Table = "stocks",
  #Append = TRUE,
  Host = "localhost",
  User = "postgres",
  Port = 5432,
  Password = "Aa1028#@")

AutoPlots::Plot.Stock(
  StockDataOutput = StockDataOutput$results,
  PlotEngineType = "Echarts",
  Type = "Candlestick",
  Metric = "%Log Returns",
  Width = "850px",
  Height = "455px",
  EchartsTheme = "macarons",
  TextColor = "white")

PolyOut = NULL #StockDataOutput$results
Symbol = 'TSLA'
CompanyName = 'Tesla Inc. Common Stock'
Metric = 'Stock Price'
TimeAgg = 'days'
StartDate = '2021-01-01'
EndDate = '2022-01-01'
APIKey = 'hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20'

Type = 'candlestick'
Metric = "Stock Price"
PlotEngineType = "Echarts"
Width = "1450px"
Height = "600px"
EchartsTheme = "macarons"
TextColor = "white"
ShadowBlur = 0
ShadowColor = "black"
ShadowOffsetX = 0
ShadowOffsetY = 0
title.fontSize = 22
title.fontWeight = "bold"
title.textShadowColor = '#63aeff'
title.textShadowBlur = 3
title.textShadowOffsetY = 1
title.textShadowOffsetX = -1
Color = "green"
Color0 = "red"
BorderColor = "transparent"
BorderColor0 = "transparent"
BorderColorDoji = "transparent"
xaxis.fontSize = 14
yaxis.fontSize = 14

# Variable Importance data
dt <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/CatBoost_ML1_Test_VI_Data.csv")


# Regression Data
dt <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/CatBoost_ML_Regression_ScoringData.csv")

# Classification Data
dt <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/CatBoost_ML1_ScoringData.csv")

# MultiClass Data
dt <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/CatBoost_MC1_ScoringData.csv")

# Plotting Data
dt <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/FakeBevData.csv")

p1 <- AutoPlots:::Plot.Histogram(
  dt = dt,
  SampleSize = 30000,
  XVar = NULL,
  YVar = "Daily Margin",
  GroupVar = "Brand",
  YVarTrans = "Identity",
  XVarTrans = "Identity",
  FacetRows = 2,
  FacetCols = 2,
  FacetLevels = unique(dt$Brand)[1:4],
  NumberBins = 20,
  Width = "1100px",
  Height = "600px",
  Title = "hist",
  ShowLabels = FALSE,
  Title.YAxis = NULL,
  Title.XAxis = NULL,
  X_Scroll = TRUE,
  Y_Scroll = TRUE,
  EchartsTheme = "macarons",
  TimeLine = FALSE,
  TextColor = "black",
  title.fontSize = 14,
  Debug = TRUE)



dt = dt
SampleSize = 30000
XVar = NULL
YVar = "Daily Margin"
GroupVar = "Brand"
YVarTrans = "Identity"
XVarTrans = "Identity"
FacetRows = 2
FacetCols = 2
FacetLevels = unique(dt$Brand)[1:4]
NumberBins = 20
Width = "1100px"
Height = "600px"
Title = "hist"
ShowLabels = FALSE
Title.YAxis = NULL
title.fontWeight = "bold"
Title.XAxis = NULL
X_Scroll = TRUE
Y_Scroll = TRUE
EchartsTheme = "macarons"
TimeLine = FALSE
TextColor = "black"
title.fontSize = 14
Debug = TRUE
title.fontSize = 22
title.fontWeight = "bold"
title.textShadowColor = '#63aeff'
title.textShadowBlur = 3
title.textShadowOffsetY = 1
title.textShadowOffsetX = -1
xaxis.fontSize = 14
yaxis.fontSize = 14





# Step Through Function
library(Rappture)
library(data.table)

PreAgg = FALSE
DataReady = FALSE
PlotEngineType = Engine =  "Echarts" # "Plotly"
TimeLine = FALSE
EchartsTheme = "purple-passion"
GroupVar = NULL# "ARTICLE" # "BRAND" #c("BRAND",'ARTICLE')
GroupVars = NULL# c("BRAND",'ARTICLE')
CorrVars = c("CHILLED_Net_Revenue_PerDay","CHILLED_Margin_PerDay","CHILLED_Liters_PerDay","CHILLED_Units_PerDay")
Method = 'spearman'
NumLevelsDisplay = 50L
AggMethod = 'mean'
NumberBins = 20
ZeroLineColor = '#ffff'
ZeroLineWidth = 1.25
Title = 'Bar Plot'
SampleSize = 100000
FillColor = "#0066ff"
FillColorReverse = "#97ff00"
BackGroundColor = "#6a6969"
ChartColor = '#001534'
GridColor = 'white'
TextColor = 'white'
X_Scroll = TRUE
Y_Scroll = TRUE
NumLevels_X = 15
NumLevels_Y = 15
Debug = FALSE
Alpha = 0.8
Smooth = TRUE
yaxis.fontSize = 14
xaxis.fontSize = 14
title.fontSize = 16
title.fontWeight = "bold" # normal
title.textShadowColor = '#63aeff'
title.textShadowBlur = 3
title.textShadowOffsetY = 1
title.textShadowOffsetX = -1
Width = "600px"
Height = "400px"
ShowSymbol = FALSE
Timeline = FALSE
PlotEngine = "Echarts"
PlotWidth = "600px"
PlotHeight = "400px"
incre = 1
ColorBackground = BackGroundColor =  "#6a6969"
ColorChart = ChartColor =       "#001534"
ColorFill = FillColor =        "#0066ff"
ColorFillReverse = GridColor =        "white"
ColorFont = TextColor =        "white"
FontSize = 14
xaxis.rotate = 90
yaxis.rotate = 0
ContainLabel = TRUE

YVarTrans <- "Identity"
XVarTrans <- "Identity"
ZVarTrans <- "Identity"
aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

FacetRows = 1
FacetCols = 1

Title.XAxis = NULL
Title.YAxis = NULL



# StepPlot
XVar <-  NULL#"Date"#NULL
GroupVar <- NULL#$"Brand"#"ARTICLE"
YVar <- "Daily Margin"
ZVar <- NULL
FacetLevels = NULL#sort(unique(dt[["Brand"]]))#Levels1 = NULL#dt[, unique(get(GroupVar))][1:9]

ShowLabels = FALSE
LabelValues = NULL#dt[, list(mean(get(YVar)), .N), by = XVar][order(-V1)][,N]


# BarPlot
XVar <-  "Customer"#NULL
GroupVar <- "Brand"#"ARTICLE"
YVar <- "Daily Margin"
ZVar <- NULL
FacetLevels = sort(unique(dt[["Brand"]]))#Levels1 = NULL#dt[, unique(get(GroupVar))][1:9]

ShowLabels = TRUE
LabelValues = dt[, list(mean(get(YVar)), .N), by = XVar][order(-V1)][,N]




# Heatmap
XVar <- "Variable"#"Category"
YVar <- "Importance"
ZVar <- NULL#"Daily Margin"
GroupVar <- NULL# "ARTICLE"

ShowLabels = FALSE

# Scatter 3D
YVar = "Daily Margin"
XVar = "Category"
ZVar = "Daily Units"
GroupVar <- "Brand"# "ARTICLE"

AutoPlots::Plot.Scatter(dt = dt,
                        SampleSize = 10000,
                        XVarTrans = "Identity",
                        YVarTrans = "Identity",
                        YVar = "Daily Margin",
                        XVar = "Daily Revenue",
                        FacetRows = 1,
                        FacetCols = 1,
                        FacetLevels = NULL,
                        GroupVar = "Brand",
                        Height = NULL,
                        Width = NULL,
                        Title = 'Copula 3D',
                        ShowLabels = FALSE,
                        Title.YAxis = NULL,
                        Title.XAxis = NULL,
                        Engine = "Echarts",
                        EchartsTheme = "dark-blue",
                        TimeLine = FALSE,
                        BackGroundColor =  "#6a6969",
                        ChartColor =       "#001534",
                        FillColor =        "#0066ff",
                        FillColorReverse = "#97ff00",
                        GridColor =        "white",
                        TextColor =        "white",
                        ZeroLineColor = '#ffff',
                        ZeroLineWidth = 1.25,
                        title.fontSize = 22,
                        title.fontWeight = "bold", # normal
                        title.textShadowColor = '#63aeff',
                        title.textShadowBlur = 3,
                        title.textShadowOffsetY = 1,
                        title.textShadowOffsetX = -1,
                        yaxis.fontSize = 14,
                        xaxis.fontSize = 14,
                        Debug = FALSE)



# StackedBar
XVar <- "Daily Liters"
YVar <- "Daily Margin"
GroupVar <- "Daily"
FacetLevels = Levels1 = NULL#$dt[, unique(get(GroupVar))][1:5]
FacetRows = 1
FacetCols = 1

Title = "Stacked Barf"
Title.YAxis = "Sucks"
Title.XAxis = "Adrian"
ShowLabels = TRUE

# Histogram
PreAgg <- FALSE
YVar <- "CHILLED_Units_PerDay"
XVar <- "BRAND"
YVarTrans = 'Identity' # 'LogPlus1'
XVarTrans = NULL
GroupVar <- "Category"
FacetLevels = NULL#dt[, unique(get(GroupVar))][1:9]


AutoPlots::Plot.StandardPlots(
  dt = dt,
  PreAgg = FALSE,
  PlotType = "BarPlot",
  XVar = XVar,
  YVar =  YVar,
  ZVar = ZVar,
  Debug = TRUE,
  YVarTrans = "LogPlus1",
  PlotEngineType = "Echarts")


# Run from Rappture PlotFunctions.R
data1 <- dt
GroupVars = GroupVar
PlotType = "HeatMapPlot"
PlotType = "BarPlot"
PlotType = "BoxPlot"
PlotType = "StackedBarPlot"
PlotType = "RiverPlot"
PlotType = "LinePlot"
PlotType = "HistogramPlot"
PlotType = "Residuals1"
PlotType = "CalibrationLine"
PlotType = "PartialDependenceLine"
PlotType = "PartialDependenceHeatMap"
PlotType = "LiftPlot"
ColorBackground = BackGroundColor =  "#6a6969"
ColorChart = ChartColor =       "#001534"
ColorFill = FillColor =        "#0066ff"
ColorFillReverse = GridColor =        "white"
ColorFont = TextColor =        "white"

PlotList= list()
PlotList[[1]] <- AutoPlots::Plots.ModelEvaluation(
  PlotType = PlotType,
  dt = data1,
  AggMethod = AggMethod,
  TargetLevel = TargetLevel,
  SampleSize = SampleSize,
  YVar = YVar,
  ZVar = ZVar,
  XVar = XVar,
  GroupVar = GroupVars,
  YVarTrans = YVarTrans,
  XVarTrans = XVarTrans,
  ZVarTrans = ZVarTrans,
  FacetRows = FacetRows,
  FacetCols = FacetCols,
  FacetLevels = Levels1,
  Height = PlotHeight[incre],
  Width = PlotWidth[incre],
  NumberBins = NumberBins,
  NumLevels_X = NumberBins,
  NumLevels_Y = NumberBins,
  PlotEngineType = PlotEngine,
  EchartsTheme = EchartsTheme,
  TimeLine = Timeline,
  BackGroundColor = BackGroundColor,
  ChartColor = ChartColor,
  FillColor = FillColor,
  FillColorReverse = ColorFillReverse,
  GridColor = GridColor,
  TextColor = TextColor,
  Debug = TRUE)


# Partial Dep Heatmap
XVar <- c("CHILLED_Units_PerDay","CHILLED_Liters_PerDay")
YVar <- "CHILLED_Margin_PerDay"
ZVar <- "Predict"
GroupVar <- NULL# "ARTICLE"


# ScatterPlot
XVar <- "Total_Margin_PerDay" #NULL
GroupVar <- "BRAND"
YVar <- "CHILLED_Margin_PerDay"
ZVar <- NULL
FacetLevels = dt[, unique(get(GroupVar))][1:9]

# Scatter 3D
XVar <- "CHILLED_Margin_PerDay"
YVar <- "CHILLED_Net_Revenue_PerDay"
ZVar <- "CHILLED_Liters_PerDay"
GroupVar <- NULL# "ARTICLE"

# Line
XVar <- "DATE_ISO"
YVar <- "CHILLED_Margin_PerDay"
GroupVar <- c("BRAND")
YVarTrans = 'Identity' # 'LogPlus1'
XVarTrans = NULL





# 3D Scatter
XVar = "CHILLED_Units_PerDay"
YVar =  "CHILLED_Margin_PerDay"
ZVar = "CHILLED_Liters_PerDay"

# River
XVar <- "Date"
YVar <- c("Daily Margin","Daily Units","Daily Revenue")
GroupVar <- NULL# "ARTICLE"
FacetLevels <- NULL

# BoxPlot (no faceting)
XVar <- "DATE_ISO" #NULL
GroupVar <- "CUSTOMER_COD_char"
YVar <- "CHILLED_Margin_PerDay"
ZVar <- NULL
FacetLevels = dt[, unique(get(GroupVar))][1:9]

# Variable iMportacne
YVar <- "Variable"
XVar <- "Importance"
GroupVar <- NULL
FacetLevels = NULL

# Calibration HeatMap
XVar <- c("CHILLED_Liters_PerDay","CHILLED_Units_PerDay")
ZVar = "Predict"
YVar <- "CHILLED_Margin_PerDay"
GroupVar <- "BRAND"
FacetLevels = Levels1 = dt[, unique(get(GroupVar))][1:9]
dt <- dt[, .SD, .SDcols = c(XVar,YVar,ZVar,GroupVar)]

# Calibration Dep Heatmap
ZVar <- NULL#"Predict"
YVar <- "BRAND"#"BinaryTarget" # YVar <- "BinaryTarget"
XVar <- "Predict"
GroupVar <- "Category"#"BRAND"
FacetLevels = Levels1 = dt[, unique(get(GroupVar))][1:5]

# Partial Dep Heatmap
ZVar <- "Predict"
YVar <- "BRAND"#"BinaryTarget" # YVar <- "BinaryTarget"
XVar <- "ARTICLE"# "CHILLED_Margin_PerDay"
GroupVar <- NULL#"Category"#"BRAND"
FacetLevels = Levels1 = NULL#$dt[, unique(get(GroupVar))][1:5]

# ROC
ZVar <- NULL
YVar <- "BRAND"#"BinaryTarget" # YVar <- "BinaryTarget"
XVar <- "Predict"# "CHILLED_Margin_PerDay"
GroupVar <- NULL# "Category"#"BRAND"
FacetLevels = Levels1 = NULL#dt[, unique(get(GroupVar))][1:5]

# 3D Scatter
XVar = "BRAND"
YVar =  "CUSTOMER_COD_char"
ZVar = "CHILLED_Liters_PerDay"







